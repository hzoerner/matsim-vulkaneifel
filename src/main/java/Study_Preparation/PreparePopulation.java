package Study_Preparation;

import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.locationtech.jts.geom.Geometry;
import org.matsim.api.core.v01.TransportMode;
import org.matsim.api.core.v01.population.*;
import org.matsim.contrib.drt.routing.DrtRoute;
import org.matsim.contrib.drt.routing.DrtRouteFactory;
import org.matsim.core.config.Config;
import org.matsim.core.config.ConfigUtils;
import org.matsim.core.population.PopulationUtils;
import org.matsim.core.population.io.PopulationWriter;
import org.matsim.core.router.TripStructureUtils;
import org.matsim.core.utils.geometry.geotools.MGC;
import org.matsim.core.utils.gis.ShapeFileReader;
import org.opengis.feature.simple.SimpleFeature;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

public class PreparePopulation {

    private static final String workingDirectory = "./scenario/open-vulkaneifel-scenario/";
    private static final String pathToConfig = workingDirectory + "vulkaneifel-v1.0-25pct.config.xml";
    private static final String pathToDilutionArea = workingDirectory + "vulkaneifel-v1.0-25pct/dilutionArea/dilutionArea.shp";

    private static final Logger log =  LogManager.getLogger(PreparePopulation.class);

    public static void main(String[] args) {

        Config config = ConfigUtils.loadConfig(pathToConfig);
        String runId = config.controler().getRunId();

        Geometry dilutionArea = (Geometry) ShapeFileReader.getAllFeatures(pathToDilutionArea).stream().
                map(SimpleFeature::getDefaultGeometry).
                findFirst().
                get();

        log.info("+++++ Start to change leg modes from pt to drt +++++");
        Population noPtButDrtPlans = changeLegModeToDrt(config, Set.of(TransportMode.pt), dilutionArea);

        log.info("+++++ Start to change leg modes from pt, car and ride to drt +++++");
        Population noMIVButDrtPlans = changeLegModeToDrt(config, Set.of(TransportMode.pt, TransportMode.car, TransportMode.ride), dilutionArea);
        log.info("+++++ Finished mode changing +++++");

        log.info("+++++ Start to write new plans to file +++++");
        new PopulationWriter(noPtButDrtPlans).write(workingDirectory + runId + ".plans-no-pt-drt.xml.gz");
        new PopulationWriter(noMIVButDrtPlans).write(workingDirectory + runId + ".plans-drt-only.xml.gz");
    }

    private static Population changeLegModeToDrt(Config config, Set<String> modes,
                                           Geometry dilutionArea) {
        int counter = 0;

        Population population = PopulationUtils.readPopulation(config.plans().getInputFile());
        PopulationFactory factory = population.getFactory();
        factory.getRouteFactories().setRouteFactory(DrtRoute.class, new DrtRouteFactory());

        for (var person : population.getPersons().values()) {
            for (var plan : person.getPlans()) {

                //check if Plan consist any trips which completely happens within shape area
                if (TripStructureUtils.getTrips(plan).stream().noneMatch(trip -> isTripInDilutionArea(trip, dilutionArea)))
                    continue;

                List<PlanElement> planElementList = plan.getPlanElements();

                if (planElementList.stream().
                        filter(planElement -> planElement instanceof Leg).
                        anyMatch(planElement -> modes.contains(((Leg) planElement).getMode()))) {

                    //simplest way to set drt as leg mode will be to recreate plans with pt main mode without pt interaction events and walk legs
                    Plan drtPlan = factory.createPlan();
                    drtPlan.setPerson(person);
                    drtPlan.setType(plan.getType());

                    List<Activity> drtPlanActivities = plan.getPlanElements().stream().
                            filter(planElement -> planElement instanceof Activity).
                            filter(planElement -> !((Activity) planElement).getType().endsWith("interaction")).
                            map(planElement -> (Activity) planElement).
                            collect(Collectors.toList());

                    for (Activity activity : drtPlanActivities) {

                        drtPlan.addActivity(activity);

                        //if last activity of plan is reached there will be no trip starting at actitvity
                        if(drtPlanActivities.indexOf(activity) < (drtPlanActivities.size() -1)) {
                            TripStructureUtils.Trip trip = TripStructureUtils.findTripStartingAtActivity(activity, plan);

                            if (modeToChangeInTrip(trip, modes) && isTripInDilutionArea(trip, dilutionArea)) {

                                Leg drtLeg = factory.createLeg("Leg");
                                drtLeg.setMode(TransportMode.drt);
                                TripStructureUtils.setRoutingMode(drtLeg, TransportMode.drt);

                                drtPlan.addLeg(drtLeg);

                                if (counter++ % 100 == 0) log.info("+++++ Modified " + counter + " plans +++++");
                            } else {
                                trip.getLegsOnly().forEach(drtPlan::addLeg);
                            }
                        }
                    }
                }
            }
        }
        return population;
    }

    private static boolean modeToChangeInTrip(TripStructureUtils.Trip trip, Set<String> modes){

        return trip.getLegsOnly().stream().anyMatch(leg -> modes.contains(leg.getMode()));
    }

    private static boolean isTripInDilutionArea(TripStructureUtils.Trip trip, Geometry dilutionArea){

        return dilutionArea.covers(MGC.coord2Point(trip.getOriginActivity().getCoord())) &&
                dilutionArea.covers(MGC.coord2Point(trip.getDestinationActivity().getCoord()));
    }
}
