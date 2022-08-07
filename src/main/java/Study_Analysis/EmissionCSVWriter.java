package Study_Analysis;

import org.matsim.api.core.v01.Id;
import org.matsim.application.MATSimAppCommand;
import org.matsim.contrib.emissions.Pollutant;
import org.matsim.core.api.experimental.events.EventsManager;
import org.matsim.core.events.EventsUtils;
import org.matsim.vehicles.Vehicle;
import picocli.CommandLine;
import prepare.CreateRegionalTrainLine;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@CommandLine.Command(
        name = "emission analysis",
        description = {"runs event handler for emission events"}
)

public class EmissionCSVWriter implements MATSimAppCommand {

    @CommandLine.Option(names = "--emissionsEventFile", description = "input emission events file", required = true)
    private String emissionsEventFile;

    @CommandLine.Option(names = "--output", description = "output directory", required = true)
    private String output;

    @CommandLine.Option(names = "--runId", description = "run id for example base case, plan case ", required = true)
    private String runId;

    public static void main(String[] args) { System.exit(new CommandLine(new EmissionCSVWriter()).execute(args));}

    @Override
    public Integer call() throws Exception {
        EmissionEventHandler handler = processEvents(emissionsEventFile);

        List<String> warmEmissionsAsCSV = convertToCSV(handler.getWarmEmissionsPerVehicle());
        List<String> coldEmissionsAsCSV = convertToCSV(handler.getColdEmissionsPerVehicle());

        output = output.endsWith("/") ? output: output + "/";
        String outputFile = output + runId + "_emissions.csv";

        printToFile(warmEmissionsAsCSV, outputFile);
        printToFile(coldEmissionsAsCSV, outputFile);

        return 0;
    }

    private static EmissionEventHandler processEvents(String eventsFile){

        EventsManager manager = EventsUtils.createEventsManager();
        EmissionEventHandler handler = new EmissionEventHandler();

        manager.addHandler(handler);
        EventsUtils.readEvents(manager, eventsFile);

        return handler;
    }

    private static List<String> convertToCSV(Map<Id<Vehicle>, Map<Pollutant, Double>> emissions){

        List<String> asCSV = new ArrayList<>();

        var pollutants = emissions.values().stream()
                .map(Map::keySet)
                .flatMap(Collection::stream)
                .distinct()
                .collect(Collectors.toList());

        String header1 = "vehicleId,";
        String header2 = pollutants.stream()
                .map(Enum::name)
                .map(name -> name + ",")
                .reduce(String::concat)
                .get();

        asCSV.add(header1 + header2);

        emissions.keySet().forEach(vehicleId -> {

            StringBuilder csv = new StringBuilder(vehicleId.toString());
            var vehicleEmissions = emissions.get(vehicleId);

            for(var pollutant: pollutants){
                String emissionAsString = vehicleEmissions.get(pollutant).toString();
                csv.append(emissionAsString).append(",");
            }

            asCSV.add(csv.toString());
        });

        return asCSV;
    }

    private static void printToFile(List<String> emissionsAsCSV, String outputFilePath){

        try {
            PrintWriter pWriter = new PrintWriter(new BufferedWriter(new FileWriter(outputFilePath)));

            emissionsAsCSV.forEach(pWriter::println);
            pWriter.close();
        } catch (IOException e) {
            e.printStackTrace();
        }

    }
}
