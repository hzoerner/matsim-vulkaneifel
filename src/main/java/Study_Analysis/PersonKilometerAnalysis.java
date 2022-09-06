package Study_Analysis;

import org.matsim.api.core.v01.Coord;
import org.matsim.api.core.v01.Id;
import org.matsim.api.core.v01.TransportMode;
import org.matsim.api.core.v01.network.Link;
import org.matsim.api.core.v01.population.HasPlansAndId;
import org.matsim.api.core.v01.population.Person;
import org.matsim.api.core.v01.population.Population;
import org.matsim.application.MATSimAppCommand;
import org.matsim.application.options.CrsOptions;
import org.matsim.application.options.ShpOptions;
import org.matsim.core.network.NetworkUtils;
import org.matsim.core.population.PopulationUtils;
import org.matsim.core.router.TripStructureUtils;
import picocli.CommandLine;

import java.util.*;
import java.util.function.Predicate;
import java.util.stream.Collectors;

@CommandLine.Command(
        name = "pkm-from-plans",
        description = {"analysis of person kilometeres from plans file"}
)

public class PersonKilometerAnalysis implements MATSimAppCommand {

    @CommandLine.Option(names = "--plans", description = "output plans file", required = true)
    private String plans;

    @CommandLine.Option(names = "--network", description = "network file path", required = true)
    private String network;

    @CommandLine.Mixin
    private ShpOptions shp;

    @CommandLine.Mixin
    CrsOptions crs;

    public static void main(String[] args) { System.exit(new CommandLine(new EmissionCSVWriter()).execute(args));}

    @Override
    public Integer call() throws Exception {

        Population population = PopulationUtils.readPopulation(plans);
        var links = NetworkUtils.readNetwork(network).getLinks();

        List<String> networkModes = List.of(TransportMode.car, TransportMode.ride);

        final Predicate<Coord> filter;
        if (shp.getShapeFile() != null) {
            // default input is set to lat lon
            ShpOptions.Index index = shp.createIndex(crs.getTargetCRS(), "_");
            filter = index::contains;
        } else filter = (coord) -> true;

        // create Map for pkm for each agent
        Map<String, Double> pkmMap = new HashMap<>();
        networkModes.forEach(s -> pkmMap.put(s, 0.0));

        population.getPersons().values().stream()
                .map(HasPlansAndId::getSelectedPlan)
                .map(TripStructureUtils::getTrips)
                .forEach(trips -> {
                    var legs = trips.stream()
                            .map(TripStructureUtils.Trip::getLegsOnly)
                            .flatMap(Collection::stream)
                            .filter(leg -> networkModes.contains(leg.getMode()))
                            .collect(Collectors.toList());

                    legs.forEach(leg -> {
                        String description = leg.getRoute().getRouteDescription();
                        Optional<Double> pkm = Arrays.stream(description.split(" "))
                                .map(string -> Id.create(string, Link.class))
                                .map(links::get)
                                .filter(link -> filter.test(link.getCoord()))
                                .map(Link::getLength)
                                .reduce(Double::sum);

                        pkm.ifPresent(aDouble -> pkmMap.merge(leg.getMode(), aDouble / 1000, Double::sum));
                    });
                });

        for(var key: pkmMap.keySet()){

            String text = "Mode " + key + ": " + pkmMap.get(key) + " pkm";
            System.out.println(text);
        }

        return 0;
    }
}
