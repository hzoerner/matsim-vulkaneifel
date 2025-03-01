package drtSchoolTransportStudy.prepare;

import drtSchoolTransportStudy.run.CaseStudyTool;
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import org.apache.log4j.Logger;
import org.locationtech.jts.geom.Geometry;
import org.matsim.api.core.v01.Coord;
import org.matsim.api.core.v01.Id;
import org.matsim.api.core.v01.TransportMode;
import org.matsim.api.core.v01.network.Link;
import org.matsim.api.core.v01.network.Network;
import org.matsim.api.core.v01.population.Activity;
import org.matsim.api.core.v01.population.Person;
import org.matsim.api.core.v01.population.Population;
import org.matsim.api.core.v01.population.PopulationWriter;
import org.matsim.application.MATSimAppCommand;
import org.matsim.application.options.ShpOptions;
import org.matsim.contrib.common.util.DistanceUtils;
import org.matsim.contrib.drt.run.MultiModeDrtConfigGroup;
import org.matsim.contrib.dvrp.path.VrpPaths;
import org.matsim.contrib.dvrp.trafficmonitoring.QSimFreeSpeedTravelTime;
import org.matsim.core.config.Config;
import org.matsim.core.config.ConfigUtils;
import org.matsim.core.network.NetworkUtils;
import org.matsim.core.population.PopulationUtils;
import org.matsim.core.router.TripStructureUtils;
import org.matsim.core.router.costcalculators.OnlyTimeDependentTravelDisutility;
import org.matsim.core.router.speedy.SpeedyALTFactory;
import org.matsim.core.router.util.LeastCostPathCalculator;
import org.matsim.core.router.util.TravelTime;
import org.matsim.core.utils.geometry.CoordUtils;
import org.matsim.core.utils.geometry.geotools.MGC;
import picocli.CommandLine;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

/**
 * This script is used to modify the departure time of school children's plan based on school starting time.
 * <p>
 * There are some unrealistic/inappropriate plans from the SNZ data. Some modification is required to make the
 * output of the runs more meaningful
 */
public class ModifySchoolChildrenPlan implements MATSimAppCommand {
    private static final Logger log = Logger.getLogger(ModifySchoolChildrenPlan.class);

    @CommandLine.Option(names = "--config", description = "path to config file", required = true)
    private String configPath;

    @CommandLine.Option(names = "--plans", description = "path to input plan file", required = true)
    private String plansPath;

    @CommandLine.Option(names = "--network", description = "path to network file", required = true)
    private String networkPath;

    @CommandLine.Option(names = "--alpha", description = "travel time alpha", defaultValue = "2.0")
    private double alpha;

    @CommandLine.Option(names = "--beta", description = "travel time beta", defaultValue = "1200.0")
    private double beta;

    @CommandLine.Option(names = "--output", description = "path to output plans file", required = true)
    private String outputPath;

    @CommandLine.Option(names = "--school-starting-time", description = "path to output plans file", defaultValue = "UNIFORM")
    private CaseStudyTool.SchoolStartingTime schoolStartingTimeType;

    @CommandLine.Option(names = "--drt-stops", description = "DRT stops location csv file", defaultValue = "")
    private String drtStops;

    @CommandLine.Mixin
    private ShpOptions shp = new ShpOptions();

    private final Random random = new Random(1234);

    public static void main(String[] args) {
        new ModifySchoolChildrenPlan().execute(args);
    }

    @Override
    public Integer call() throws Exception {
        Config config = ConfigUtils.loadConfig(configPath);
        double walkingSpeed = config.plansCalcRoute().getTeleportedModeSpeeds().get(TransportMode.walk);
        log.info("Walking speed is " + walkingSpeed + " m/s");

        MultiModeDrtConfigGroup drtConfigGroup = ConfigUtils.addOrGetModule(config, MultiModeDrtConfigGroup.class);
        double stopDuration = drtConfigGroup.getModalElements().iterator().next().getStopDuration(); // Use the first Drt Config Group by default
        log.info("DRT Stop Duration is: " + stopDuration + " s");

        Network network = NetworkUtils.readNetwork(networkPath);
        Population plans = PopulationUtils.readPopulation(plansPath);

        // For stop-based service, there are 2 types of plan: original plans and adapted plans
        // In the adapted plan, most people depart earlier to accommodate for the longer walking time
        boolean adaptToDrtStops = false;
        List<Link> drtStopLinks = null;
        if (!drtStops.equals("")) {
            adaptToDrtStops = true;
            drtStopLinks = new ArrayList<>();
            // Read CSV file and add DRT stops links to the list
            try (CSVParser parser = new CSVParser(Files.newBufferedReader(Path.of(drtStops)),
                    CSVFormat.DEFAULT.withDelimiter(',').withFirstRecordAsHeader())) {
                for (CSVRecord record : parser.getRecords()) {
                    Id<Link> linkId = Id.createLinkId(record.get(1));
                    drtStopLinks.add(network.getLinks().get(linkId));
                }
            }
        }

        SchoolStartingTimeIdentifier schoolStartTimeCalculator;
        if (schoolStartingTimeType == CaseStudyTool.SchoolStartingTime.UNIFORM) {
            schoolStartTimeCalculator = new UniformSchoolStartingTimeIdentification();
        } else if (schoolStartingTimeType == CaseStudyTool.SchoolStartingTime.TWO_SCHOOL_STARTING_TIME) {
            schoolStartTimeCalculator = new TwoSchoolStartingTimeIdentification(shp.getGeometry());
        } else {
            throw new RuntimeException("Unknown school starting time type!");
        }

        double timeStepSize = config.qsim().getTimeStepSize();
        TravelTime travelTime = new QSimFreeSpeedTravelTime(timeStepSize);
        LeastCostPathCalculator router = new SpeedyALTFactory().createPathCalculator(network, new OnlyTimeDependentTravelDisutility(travelTime), travelTime);

        for (Person person : plans.getPersons().values()) {
            List<TripStructureUtils.Trip> trips = TripStructureUtils.getTrips(person.getSelectedPlan());
            assert trips.size() == 1 : "There should only be 1 trip per children (i.e., going to school in the morning)";
            for (TripStructureUtils.Trip trip : trips) {
                Activity homeActivity = trip.getOriginActivity();
                Activity schoolActivity = trip.getDestinationActivity();

                homeActivity.setStartTime(0);
                Link homeLink = NetworkUtils.getNearestLink(network, homeActivity.getCoord());
                homeActivity.setLinkId(homeLink.getId());
                if (CoordUtils.calcEuclideanDistance(homeLink.getToNode().getCoord(), homeActivity.getCoord()) >= 200) {
                    homeActivity.setCoord(homeLink.getToNode().getCoord());
                }

                Link toLink = NetworkUtils.getNearestLink(network, schoolActivity.getCoord());
                double originalDepartureTime = homeActivity.getEndTime().orElseThrow(RuntimeException::new);

                double estDirectTravelTime;
                double walkingTime;
                if (adaptToDrtStops) {
                    Link closestDrtStopLink = findClosestDrtStopLink(homeActivity.getCoord(), drtStopLinks);
                    Link schoolDrtStopLink = findClosestDrtStopLink(schoolActivity.getCoord(), drtStopLinks);
                    walkingTime = Math.floor(DistanceUtils.calculateDistance(homeActivity.getCoord(), closestDrtStopLink.getToNode().getCoord()) / walkingSpeed);
                    estDirectTravelTime = VrpPaths.calcAndCreatePath(closestDrtStopLink, schoolDrtStopLink, originalDepartureTime + walkingTime, router, travelTime).getTravelTime();
                } else {
                    walkingTime = Math.floor(DistanceUtils.calculateDistance(homeActivity.getCoord(), homeLink.getToNode().getCoord()) / walkingSpeed);
                    estDirectTravelTime = VrpPaths.calcAndCreatePath(homeLink, toLink, originalDepartureTime + walkingTime, router, travelTime).getTravelTime();
                }

                double schoolStartingTime = schoolStartTimeCalculator.getSchoolStartingTime(schoolActivity);
                schoolActivity.setStartTime(schoolStartingTime);

                double travelTimeAllowance = alpha * estDirectTravelTime + beta + walkingTime + stopDuration;
                double updatedDepartureTime = schoolStartingTime - travelTimeAllowance;
                homeActivity.setEndTime(updatedDepartureTime);
            }
        }

        PopulationWriter populationWriter = new PopulationWriter(plans);
        populationWriter.write(outputPath);

        return 0;
    }

    private Link findClosestDrtStopLink(Coord homeCoord, List<Link> drtStopLinks) {
        double minDistance = Double.MAX_VALUE;
        Link closestDrtStopLink = null;
        for (Link link : drtStopLinks) {
            Coord stopCoord = link.getToNode().getCoord();
            double distance = DistanceUtils.calculateDistance(homeCoord, stopCoord);
            if (distance < minDistance) {
                minDistance = distance;
                closestDrtStopLink = link;
            }
        }
        return closestDrtStopLink;
    }

    interface SchoolStartingTimeIdentifier {
        double getSchoolStartingTime(Activity schoolActivity);
    }

    // Initial implementation: Uniform school starting time at 8:00.
    static class UniformSchoolStartingTimeIdentification implements SchoolStartingTimeIdentifier {
        private final static double schoolStartingTime = 28800;  //08:00

        @Override
        public double getSchoolStartingTime(Activity schoolActivity) {
            return schoolStartingTime;
        }
    }

    static class TwoSchoolStartingTimeIdentification implements SchoolStartingTimeIdentifier {
        private final Geometry earlyStartArea;
        private final static int schoolStartingTime1 = 27000; // 07:30
        private final static int schoolStartingTime2 = 28800; // 08:00

        TwoSchoolStartingTimeIdentification(Geometry earlyStartArea) {
            this.earlyStartArea = earlyStartArea;
        }

        @Override
        public double getSchoolStartingTime(Activity schoolActivity) {
            if (MGC.coord2Point(schoolActivity.getCoord()).within(earlyStartArea)) {
                String activityTypeWithStartingTime = schoolActivity.getType() + "_starting_at_" + schoolStartingTime1;
                schoolActivity.setType(activityTypeWithStartingTime);
                return schoolStartingTime1;
            }
            String activityTypeWithStartingTime = schoolActivity.getType() + "_starting_at_" + schoolStartingTime2;
            schoolActivity.setType(activityTypeWithStartingTime);
            return schoolStartingTime2;
        }
    }

}
