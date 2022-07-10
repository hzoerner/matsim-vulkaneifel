package Study_Preparation;

import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.locationtech.jts.geom.Geometry;
import org.matsim.api.core.v01.TransportMode;
import org.matsim.api.core.v01.population.Leg;
import org.matsim.api.core.v01.population.Population;
import org.matsim.core.config.Config;
import org.matsim.core.config.ConfigUtils;
import org.matsim.core.population.PopulationUtils;
import org.matsim.core.population.io.PopulationWriter;
import org.matsim.core.router.TripStructureUtils;
import org.matsim.core.utils.geometry.geotools.MGC;
import org.matsim.core.utils.gis.ShapeFileReader;
import org.opengis.feature.simple.SimpleFeature;

import java.util.Set;

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
                                           Geometry dilutionArea){
        int counter = 0;

        Population noDrtPlans = PopulationUtils.readPopulation(config.plans().getInputFile());
        Population drtPlans = PopulationUtils.createPopulation(ConfigUtils.createConfig());
        drtPlans.setName(noDrtPlans.getName());

        for(var person: noDrtPlans.getPersons().values()){
            for(var plan: person.getPlans()){
                for(var trip: TripStructureUtils.getTrips(plan)){

                    if(isTripInDilutionArea(trip, dilutionArea)){

                        for(Leg leg: trip.getLegsOnly()){
                            if (modes.contains(leg.getMode())) {
                                leg.setMode(TransportMode.drt);
                                if(counter++ % 100 == 0) log.info("Total Number of edited legs: " + counter);
                            }
                        }
                    }
                }
            }
            drtPlans.addPerson(person);
        }

        return drtPlans;
    }

    private static boolean isTripInDilutionArea(TripStructureUtils.Trip trip, Geometry dilutionArea){

        return dilutionArea.covers(MGC.coord2Point(trip.getOriginActivity().getCoord())) &&
                dilutionArea.covers(MGC.coord2Point(trip.getDestinationActivity().getCoord()));
    }
}
