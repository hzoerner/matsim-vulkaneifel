package prepare;

import org.matsim.api.core.v01.Coord;
import org.matsim.api.core.v01.Id;
import org.matsim.api.core.v01.Scenario;
import org.matsim.api.core.v01.TransportMode;
import org.matsim.api.core.v01.network.Link;
import org.matsim.api.core.v01.network.Network;
import org.matsim.api.core.v01.network.NetworkFactory;
import org.matsim.api.core.v01.network.Node;
import org.matsim.application.MATSimAppCommand;
import org.matsim.application.options.CrsOptions;
import org.matsim.application.options.ShpOptions;
import org.matsim.core.config.Config;
import org.matsim.core.config.ConfigUtils;
import org.matsim.core.population.routes.NetworkRoute;
import org.matsim.core.population.routes.RouteUtils;
import org.matsim.core.scenario.ScenarioUtils;
import org.matsim.core.utils.collections.Tuple;
import org.matsim.pt.transitSchedule.TransitRouteImpl;
import org.matsim.pt.transitSchedule.api.*;
import org.matsim.vehicles.*;
import picocli.CommandLine;

import java.util.*;
import java.util.function.Predicate;
import java.util.stream.Collectors;

@CommandLine.Command(
        name = "create-train-line",
        description = "Creates regional train line between Cologne and Trier",
        showDefaultValues = true,
        mixinStandardHelpOptions = true
)

public class CreateRegionalTrainLine implements MATSimAppCommand {

    @CommandLine.Option(names = "--vehicles", description = "input matsim transit vehicles")
    private String vehicles;

    @CommandLine.Option(names = "--schedule", description = "input matsim transit schedule")
    private String schedule;

    @CommandLine.Option(names = "--network", description = "path to base network file", required = true)
    private String network;

    @CommandLine.Option(names = "--name", description = "name of the scenario", required = true)
    private String name;

    @CommandLine.Option(names = "--output", description = "output file path")
    private String output;

    @CommandLine.Mixin
    private ShpOptions shp = new ShpOptions();

    @CommandLine.Mixin
    private CrsOptions crs = new CrsOptions("EPSG:25832");

    public static void main(String[] args) { System.exit(new CommandLine(new CreateRegionalTrainLine()).execute(args));}

    @Override
    public Integer call() throws Exception {

        /*

          script extracts and removes SEV busline form bus schedule and creates new schedule and vehicles,
          which contains only the regional train

          still two stops in the train schedule which do NOT belong in there...
         */

        String sevID = "SEV---1747";

        Id<TransitLine> id = Id.create(sevID, TransitLine.class);

        Config config = ConfigUtils.createConfig();
        config.transit().setTransitScheduleFile(schedule);
        config.transit().setVehiclesFile(vehicles);
        config.network().setInputFile(network);


        var scenario = ScenarioUtils.loadScenario(config);

        var trainSchedule = createTrainSchedule(scenario, id, shp, crs);

        new TransitScheduleWriter(scenario.getTransitSchedule()).writeFile(output + "/" + name + "-transitSchedule-edit.xml.gz");
        new TransitScheduleWriter(trainSchedule).writeFile(output + "/" + name + "-transitSchedule-only-regional-train.xml.gz");
        new MatsimVehicleWriter(scenario.getVehicles()).writeFile(output + "/" + name + "-transitVehicles-only-regional-train.xml.gz");


        return 0;
    }

    private static void removeVehicles(Vehicles vehicles){

        for(var v: vehicles.getVehicles().keySet()) vehicles.removeVehicle(v);
    }

    private static void addFaciltiesFromTransitLine(TransitSchedule transitSchedule, TransitLine transitLine){

        var facilities = transitLine
                .getRoutes()
                .values()
                .stream()
                .findFirst()
                .get()
                .getStops()
                .stream()
                .map(TransitRouteStop::getStopFacility)
                .collect(Collectors.toList());

        for(var f: facilities) transitSchedule.addStopFacility(f);
    }

    private static List<TransitRouteStop> filterStops(List<TransitRouteStop> unfiltered){

         var filtered = unfiltered.stream()
                 .map(transitRouteStop -> Tuple.of(transitRouteStop.getStopFacility().getName(), transitRouteStop))
                 .collect(Collectors.toMap(Tuple::getFirst, Tuple::getSecond, (first,second) -> first));


        return new ArrayList<>(filtered.values());
    }

    private static List<TransitRouteStop> getAllBusLineStops(List<TransitRoute> busRoutes){

        var stops = busRoutes
                .stream()
                .map(TransitRoute::getStops)
                .flatMap(Collection::stream)
                .collect(Collectors.toList());

        //sort Facilities to ensure that order is correct, sorting by y-coordinate is possible because train route is nearly vertical
        var filteredstops = filterStops(stops);
        filteredstops.sort(new TransitStopComparator());

        return filteredstops;
    }

    private static List<Id<Link>> getAllBusRouteLinks(List<TransitRoute> busRoutes, Network network){

        Map<Id<Link>, ? extends Link> linkMap = network.getLinks();

        return busRoutes
                .stream()
                .map(transitRoute -> transitRoute.getRoute().getLinkIds())
                .flatMap(Collection::stream)
                .distinct()
                .map(linkMap::get)
                .sorted(new LinkComparator())
                .map(Link::getId)
                .collect(Collectors.toList());
    }

    private static List<TransitRoute> getBusRoutes(TransitLine transitLine, ShpOptions shp, CrsOptions crs){

        final Predicate<Coord> filter;
        if (shp.getShapeFile() != null) {
            // default input is set to lat lon
            ShpOptions.Index index = shp.createIndex(crs.getInputCRS(), "_");
            filter = index::contains;
        } else filter = (coord) -> true;

        return transitLine.getRoutes().values().stream()
                .filter(transitRoute -> transitRoute.getStops().stream()
                        .map(transitRouteStop -> transitRouteStop.getStopFacility().getCoord())
                        .anyMatch(filter))
                .collect(Collectors.toList());
    }

    private static List<TransitRouteStop> createTransitRouteStop(List<TransitStopFacility> facilities,
                                                                 List<? extends Node> nodes,
                                                                 TransitScheduleFactory transitScheduleFactory,
                                                                 NetworkFactory networkFactory){

        //note: facility.getLinkId gets the link id from the link TO the facility (depending on route direction)
        //assumes that links in list are in the right order
        double stopTime = 120;
        double totalTraveltime = 0;

        if(facilities.size() != nodes.size()) throw new RuntimeException("There are either more or less facilities than nodes...");

        List<TransitRouteStop> stops = new ArrayList<>();

        List<Link> ptLinks = new ArrayList<>();

        for(int i = 0; i < nodes.size() - 1; i ++){

            if(i == 0){

                var stop = transitScheduleFactory
                        .createTransitRouteStopBuilder(facilities.get(0))
                        .arrivalOffset(totalTraveltime)
                        .departureOffset(totalTraveltime += stopTime)
                        .build();

                stops.add(stop);
            }

            //creates pt links

            Node fromNode = nodes.get(i);
            Node toNode = nodes.get(i + 1);

            Id<Link> id = Id.createLinkId("pt_edit_" + i);

            Link link = networkFactory.createLink(id, fromNode, toNode);
            link.setFreespeed(100/3.6);
            link.setNumberOfLanes(1);
            link.setAllowedModes(Set.of(TransportMode.pt));
            link.setCapacity(1000000.0);

            facilities.get(i + 1).setLinkId(id);
            ptLinks.add(link);


            totalTraveltime += link.getLength()/link.getFreespeed();

            //create stops

            var stop = transitScheduleFactory
                    .createTransitRouteStopBuilder(facilities.get(i + 1))
                    .arrivalOffset(totalTraveltime)
                    .departureOffset(totalTraveltime += stopTime)
                    .build();

            stops.add(stop);
        }


        return stops;
    }

    private static List<TransitRouteStop> createReverseTransitRouteStop(List<TransitStopFacility> facilities, List<? extends Node> nodes, TransitScheduleFactory transitScheduleFactory, NetworkFactory networkFactory){

        Collections.reverse(facilities);
        return createTransitRouteStop(facilities, nodes, transitScheduleFactory, networkFactory);
    }

    private static List<TransitRoute> createTrainRoutes(List<TransitRoute> busRoutes, Vehicles vehicles, Network network, TransitScheduleFactory transitScheduleFactory){

        double lineFrequency = 3600.;

        List<Id<Link>> linkIds = getAllBusRouteLinks(busRoutes, network);

        var networkFactory = network.getFactory();
        var vehiclesFactory = vehicles.getFactory();

        var stops = getAllBusLineStops(busRoutes);

        var facilities = stops
                .stream()
                .map(TransitRouteStop::getStopFacility)
                .collect(Collectors.toList());

        var routeNodes = stops
                .stream()
                .map(stop ->  network.getNodes().get(
                        /*if a facilty has more stops of different lines the stop have a suffix like .1 e.g.
                        which is removed which .split
                        suffix does not belong to node id*/
                        Id.createNodeId("pt_" + stop.getStopFacility().getId().toString().split("\\.")[0])))
                .collect(Collectors.toList());

        final NetworkRoute trainNetworkRoute = RouteUtils.createNetworkRoute(linkIds);

        List<Id<Link>> linkIdsReversed = new ArrayList<>(linkIds);
        Collections.reverse(linkIdsReversed);
        final NetworkRoute trainNetworkRouteReverse = RouteUtils.createNetworkRoute(linkIdsReversed);

        List<TransitRouteStop> trainRouteStops =  createTransitRouteStop(facilities, routeNodes, transitScheduleFactory, networkFactory);
        List<TransitRouteStop> trainRouteReverseStops = createReverseTransitRouteStop(facilities, routeNodes, transitScheduleFactory, networkFactory);

        TransitRouteImpl trainRoute = (TransitRouteImpl) transitScheduleFactory.createTransitRoute(
                Id.create("CustomTrain--Route", TransitRoute.class),
                trainNetworkRoute,
                trainRouteStops,
                "rail"
        );

        TransitRouteImpl trainRouteReverse = (TransitRouteImpl) transitScheduleFactory.createTransitRoute(
                Id.create("CustomTrain--RouteReverse", TransitRoute.class),
                trainNetworkRouteReverse,
                trainRouteReverseStops,
                "rail"
        );

        VehicleType regionalBahnVehicleType = createRegionalBahnVehicleType(vehiclesFactory);
        vehicles.addVehicleType(regionalBahnVehicleType);

        for(int i = 4*3600; i < 22*3600; i += lineFrequency){

            //creates departures every hour from 4 am to 10 pm
            var id1 = Id.createVehicleId("CustomTrain--" + i);
            var id2 = Id.createVehicleId("CustomTrainReverseRoute--" + i);

            Departure departure = transitScheduleFactory.createDeparture(Id.create(i, Departure.class), i);
            departure.setVehicleId(id1);
            vehicles.addVehicle(vehiclesFactory.createVehicle(id1, regionalBahnVehicleType));

            Departure departure1 = transitScheduleFactory.createDeparture(Id.create(i, Departure.class), i);
            departure1.setVehicleId(id2);
            vehicles.addVehicle(vehiclesFactory.createVehicle(id2, regionalBahnVehicleType));

            //add departure to route
            trainRoute.addDeparture(departure);
            trainRouteReverse.addDeparture(departure1);
        }

        return List.of(trainRoute, trainRouteReverse);
    }

    private static VehicleType createRegionalBahnVehicleType(VehiclesFactory vehiclesFactory){

        VehicleType reRbVehicleType = vehiclesFactory.createVehicleType( Id.create( "RE_RB_veh_type", VehicleType.class ) );

        VehicleCapacity capacity = reRbVehicleType.getCapacity();
        capacity.setSeats( 500 );
        capacity.setStandingRoom( 600 );
        VehicleUtils.setDoorOperationMode(reRbVehicleType, VehicleType.DoorOperationMode.serial); // first finish boarding, then start alighting
        VehicleUtils.setAccessTime(reRbVehicleType, 1.0 / 10.0); // 1s per boarding agent, distributed on 10 doors
        VehicleUtils.setEgressTime(reRbVehicleType, 1.0 / 10.0);

        return reRbVehicleType;
    }

    private static TransitSchedule createTrainSchedule(Scenario scenario, Id<TransitLine> id, ShpOptions shp, CrsOptions crs){

        TransitSchedule transitSchedule = scenario.getTransitSchedule();
        TransitScheduleFactory factory = transitSchedule.getFactory();

        Vehicles vehicles = scenario.getVehicles();

        //empty vehicle container
        removeVehicles(vehicles);

        //create train line
        var busRoutes = getBusRoutes(transitSchedule.getTransitLines().get(id), shp, crs);
        var trainRoutes = createTrainRoutes(busRoutes, vehicles, scenario.getNetwork(), factory);
        TransitLine trainLine = factory.createTransitLine(Id.create("RB26---edit", TransitLine.class));
        trainLine.setName("RB26");

        for(var trainRoute: trainRoutes) trainLine.addRoute(trainRoute);

        //add transit line to empty schedule
        TransitSchedule trainSchedule = factory.createTransitSchedule();
        trainSchedule.addTransitLine(trainLine);
        addFaciltiesFromTransitLine(trainSchedule, trainLine);

        //remove busline from original bus scheudle
        transitSchedule.removeTransitLine(transitSchedule.getTransitLines().get(id));

        return trainSchedule;
    }

    private static class TransitStopComparator implements Comparator<TransitRouteStop>{

        @Override
        public int compare(TransitRouteStop o1, TransitRouteStop o2) {
            double y1 = o1.getStopFacility().getCoord().getY();
            double y2 = o2.getStopFacility().getCoord().getY();

            return Double.compare(y1,y2);
        }
    }

    private static class LinkComparator implements Comparator<Link>{

        @Override
        public int compare(Link o1, Link o2) {

            double y1 = o1.getCoord().getY();
            double y2 = o2.getCoord().getY();

            return Double.compare(y1,y2);
        }
    }
}