package prepare;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.matsim.api.core.v01.Id;
import org.matsim.api.core.v01.Scenario;
import org.matsim.api.core.v01.network.Link;
import org.matsim.api.core.v01.network.Network;
import org.matsim.api.core.v01.network.NetworkWriter;
import org.matsim.application.MATSimAppCommand;
import org.matsim.core.config.Config;
import org.matsim.core.config.ConfigUtils;
import org.matsim.core.network.NetworkUtils;
import org.matsim.core.scenario.ScenarioUtils;
import org.matsim.pt.transitSchedule.api.*;
import org.matsim.pt.utils.CreatePseudoNetwork;
import org.matsim.vehicles.MatsimVehicleWriter;
import org.matsim.vehicles.Vehicle;
import org.matsim.vehicles.VehicleType;
import org.matsim.vehicles.Vehicles;
import picocli.CommandLine;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

@CommandLine.Command(
        name = "merge-transit-schedules",
        description = "Merge two transit schedules together",
        showDefaultValues = true,
        mixinStandardHelpOptions = true
)

public class MergeTransitSchedules implements MATSimAppCommand {

    @CommandLine.Parameters(arity = "1...*", paramLabel = "INPUT", description = "input matsim transit schedules")
    private List<Path> transitSchedules;

    @CommandLine.Option(names = "--vehicles", arity = "1...*", description = "input matsim transit vehicles")
    private List<Path> transitVehicles;

    @CommandLine.Option(names = "--network", description = "path to base network file", required = true)
    private String network;

    @CommandLine.Option(names = "--name", description = "name of the scenario", required = true)
    private String name;

    @CommandLine.Option(names = "--output", description = "output file path")
    private String output;

    private static final Logger log = LogManager.getLogger(MergeTransitSchedules.class);

    public static void main(String[] args) {
        System.exit(new CommandLine(new MergeTransitSchedules()).execute(args));
    }

    @Override
    public Integer call() throws Exception {

        Config emptyConfig = ConfigUtils.createConfig();

        if(transitSchedules.size() != transitVehicles.size()){
            log.info("There are more or less transit schedules than vehicles!");
            return 1;
        }

        var schedules = transitSchedules.stream()
                .map(path -> readTransitSchedule(path, emptyConfig))
                .collect(Collectors.toList());

        log.info("+++++++++Read in " + schedules.size() + " transit schedules.+++++++++");

        var vehicles = transitVehicles.stream()
                .map(path -> readTransitVehicles(path, emptyConfig))
                .collect(Collectors.toList());

        log.info("+++++++++Read in " + vehicles.size() + " transit vehicles.+++++++++");

        var completeSchedule = mergeSchedule(schedules);

        var finalVehicles = mergeVehicles(vehicles);

        emptyConfig.network().setInputFile(network);

        Scenario scenario = mergeScheduleWithNetwork(emptyConfig, NetworkUtils.readNetwork(network, emptyConfig), completeSchedule);

        //write finalSchedule to file
        new TransitScheduleWriter(scenario.getTransitSchedule()).writeFile(output + "/" + name + "-transitSchedule.xml.gz");

        //write finalNetwork to file
        new NetworkWriter(scenario.getNetwork()).write(output + "/" + name + "-network-with-pt.xml.gz");

        //write finalVehicles to file
        new MatsimVehicleWriter(finalVehicles).writeFile(output + "/" + name + "-transitVehicles.xml.gz");

        return 0;
    }

    private static Vehicles mergeVehicles(List<Vehicles> vehicles){

        /**
         * takes the first vehicles in the list and
         * applies the mergeVehicles function below on the vehicles collection
         */

        var newVehicles = vehicles.get(0);
        vehicles.remove(0);

        for (var veh: vehicles){
            mergeVehicles(newVehicles, veh);
        }

        return newVehicles;
    }

    private static TransitSchedule mergeSchedule(List<TransitSchedule> schedules){

        /**
         * takes the first schedule in the list and
         * applies the mergeTransitSchedule function below on the schedule collection
         */

        var newSchedule = schedules.get(0);
        schedules.remove(0);

        int id = 0;

        for (var schedule: schedules){
            mergeSchedule(newSchedule, Integer.toString(id++) ,schedule);
        }

        return newSchedule;
    }

    private static void mergeVehicles(Vehicles baseTransitVehicles, Vehicles transitVehicles) {
        for (VehicleType vehicleType : transitVehicles.getVehicleTypes().values()) {

            if(baseTransitVehicles.getVehicleTypes().containsKey(vehicleType.getId())) continue;

            VehicleType vehicleType2 = baseTransitVehicles.getFactory().createVehicleType(vehicleType.getId());
            vehicleType2.setNetworkMode(vehicleType.getNetworkMode());
            vehicleType2.setPcuEquivalents(vehicleType.getPcuEquivalents());
            vehicleType2.setDescription(vehicleType.getDescription());
            vehicleType2.getCapacity().setSeats(vehicleType.getCapacity().getSeats());

            baseTransitVehicles.addVehicleType(vehicleType2);
        }

        for (Vehicle vehicle : transitVehicles.getVehicles().values()) {
            Vehicle vehicle2 = baseTransitVehicles.getFactory().createVehicle(vehicle.getId(), vehicle.getType());
            baseTransitVehicles.addVehicle(vehicle2);
        }

    }

    /**
     * Merges two schedules into one, by copying all stops, lines and so on from the addSchedule to the baseSchedule.
     *
     */
    private static void mergeSchedule(TransitSchedule baseSchedule, String id, TransitSchedule addSchedule) {

        for (TransitStopFacility stop : addSchedule.getFacilities().values()) {
            Id<TransitStopFacility> newStopId = Id.create(id + "_" + stop.getId(), TransitStopFacility.class);
            TransitStopFacility stop2 = baseSchedule.getFactory().createTransitStopFacility(newStopId, stop.getCoord(), stop.getIsBlockingLane());
            stop2.setLinkId(stop.getLinkId());
            stop2.setName(stop.getName());
            baseSchedule.addStopFacility(stop2);
        }
        for (TransitLine line : addSchedule.getTransitLines().values()) {
            TransitLine line2 = baseSchedule.getFactory().createTransitLine(Id.create(id + "_" + line.getId(), TransitLine.class));

            for (TransitRoute route : line.getRoutes().values()) {

                List<TransitRouteStop> stopsWithNewIDs = new ArrayList<>();
                for (TransitRouteStop routeStop : route.getStops()) {
                    Id<TransitStopFacility> newFacilityId = Id.create(id + "_" + routeStop.getStopFacility().getId(), TransitStopFacility.class);
                    TransitStopFacility stop = baseSchedule.getFacilities().get(newFacilityId);
                    stopsWithNewIDs.add(baseSchedule.getFactory().createTransitRouteStop(stop , routeStop.getArrivalOffset().seconds(), routeStop.getDepartureOffset().seconds()));
                }

                TransitRoute route2 = baseSchedule.getFactory().createTransitRoute(route.getId(), route.getRoute(), stopsWithNewIDs, route.getTransportMode());
                route2.setDescription(route.getDescription());

                for (Departure departure : route.getDepartures().values()) {
                    Departure departure2 = baseSchedule.getFactory().createDeparture(departure.getId(), departure.getDepartureTime());
                    departure2.setVehicleId(departure.getVehicleId());
                    route2.addDeparture(departure2);
                }
                line2.addRoute(route2);
            }
            baseSchedule.addTransitLine(line2);
        }
    }

    private static TransitSchedule readTransitSchedule(Path pathToTransitSchedule, Config config){
        config.transit().setTransitScheduleFile(pathToTransitSchedule.toString());

        return ScenarioUtils.loadScenario(config).getTransitSchedule();
    }

    private static Vehicles readTransitVehicles(Path pathToTransitVehicles, Config config){
        config.transit().setVehiclesFile(pathToTransitVehicles.toString());

        return ScenarioUtils.loadScenario(config).getTransitVehicles();
    }

    private static void increaseLinkFreespeedIfLower(Link link, double newFreespeed) {
        if (link.getFreespeed() < newFreespeed) {
            link.setFreespeed(newFreespeed);
        }
    }

    private static Scenario mergeScheduleWithNetwork(Config config, Network network, TransitSchedule transitSchedule){

        TransitSchedule temp = transitSchedule.getFactory().createTransitSchedule();

        if(transitSchedule.getFacilities().isEmpty()){

            log.info("transit schedule does not contain any facilities");
        } else{

            for(var transitLine: transitSchedule.getTransitLines().values()){

                temp.addTransitLine(transitLine);
            }
        }

        var scenario = new ScenarioUtils.ScenarioBuilder(config)
                .setTransitSchedule(temp)
                .setNetwork(network)
                .build();

        //creates pseudo network for pt
        //link freespeed is set to 100 km per hour, link capacity is set to 100000 as a default value
        new CreatePseudoNetwork(scenario.getTransitSchedule(), scenario.getNetwork(), "pt_").createNetwork();

        // set link speeds and create vehicles according to pt mode
        for (TransitLine line: scenario.getTransitSchedule().getTransitLines().values()) {

            for (TransitRoute route : line.getRoutes().values()) {

                // increase speed if current freespeed is lower.
                List<TransitRouteStop> routeStops = route.getStops();
                if (routeStops.size() < 2) {
                    log.warn("TransitRoute with less than 2 stops found: line {}, route {}", line.getId(), route.getId());
                    continue;
                }

                double lastDepartureOffset = route.getStops().get(0).getDepartureOffset().seconds();
                // min. time spend at a stop, useful especially for stops whose arrival and departure offset is identical,
                // so we need to add time for passengers to board and alight
                double minStopTime = 30.0;

                for (int i = 1; i < routeStops.size(); i++) {
                    TransitRouteStop routeStop = routeStops.get(i);
                    // if there is no departure offset set (or infinity), it is the last stop of the line,
                    // so we don't need to care about the stop duration
                    double stopDuration = routeStop.getDepartureOffset().isDefined() ?
                            routeStop.getDepartureOffset().seconds() - routeStop.getArrivalOffset().seconds() : minStopTime;
                    // ensure arrival at next stop early enough to allow for 30s stop duration -> time for passengers to board / alight
                    // if link freespeed had been set such that the pt veh arrives exactly on time, but departure tiome is identical
                    // with arrival time the pt vehicle would have been always delayed
                    // Math.max to avoid negative values of travelTime
                    double travelTime = Math.max(1, routeStop.getArrivalOffset().seconds() - lastDepartureOffset - 1.0 -
                            (stopDuration >= minStopTime ? 0 : (minStopTime - stopDuration)));
                    Link link = network.getLinks().get(routeStop.getStopFacility().getLinkId());
                    increaseLinkFreespeedIfLower(link, link.getLength() / travelTime);
                    lastDepartureOffset = routeStop.getDepartureOffset().seconds();
                }
            }
        }
        return scenario;
    }
}
