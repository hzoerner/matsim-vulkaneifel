package Study_Analysis;

import org.matsim.contrib.emissions.Pollutant;
import org.matsim.core.api.experimental.events.EventsManager;
import org.matsim.core.events.EventsUtils;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class EmissionCSVWriter {

    private static final String emissionsEventFile = "";
    private static final String outputFile = "";

    public static void main(String[] args) {
        EmissionEventHandler handler = processEvents(emissionsEventFile);

    }

    private static EmissionEventHandler processEvents(String eventsFile){

        EventsManager manager = EventsUtils.createEventsManager();
        EmissionEventHandler handler = new EmissionEventHandler();

        manager.addHandler(handler);
        EventsUtils.readEvents(manager, eventsFile);

        return handler;
    }

    private static List<String> convertToCSV(EmissionEventHandler handler){

        List<String> asCSV = new ArrayList<>();

        var coldEmissions = handler.getColdEmissionsPerVehicle();
        var warmEmissions = handler.getWarmEmissionsPerVehicle();
        var pollutants = coldEmissions.values().stream()
                .map(Map::keySet)
                .flatMap(Collection::stream)
                .distinct()
                .collect(Collectors.toList());

        String header = pollutants.stream()
                .map(Enum::name)
                .map(name -> name + ",")
                .reduce(String::concat)
                .get();

        asCSV.add(header);

        coldEmissions.keySet().stream()
                .map(coldEmissions::get)
                .forEach(pollutantDoubleMap -> {
                    for(Pollutant pollutant: pollutants){
                        Double emissionSum = pollutantDoubleMap.get(pollutant);
                        asCSV.add(emissionSum.toString());
                    }
                        }
                );

        return asCSV;
    }
}
