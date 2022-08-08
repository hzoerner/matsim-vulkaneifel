package Study_Analysis;

import org.jboss.logging.Logger;
import org.matsim.api.core.v01.Id;
import org.matsim.api.core.v01.Scenario;
import org.matsim.api.core.v01.network.Network;
import org.matsim.application.MATSimAppCommand;
import org.matsim.contrib.emissions.Pollutant;
import org.matsim.contrib.emissions.events.EmissionEventsReader;
import org.matsim.core.api.experimental.events.EventsManager;
import org.matsim.core.config.Config;
import org.matsim.core.config.ConfigUtils;
import org.matsim.core.events.EventsUtils;
import org.matsim.core.scenario.ScenarioUtils;
import org.matsim.vehicles.*;
import picocli.CommandLine;

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
        name = "emissions",
        description = {"runs event handler for emission events"}
)

public class EmissionCSVWriter implements MATSimAppCommand {

    @CommandLine.Option(names = "--emissionsEventFile", description = "input emission events file", required = true)
    private String emissionsEventFile;

    @CommandLine.Option(names = "--output", description = "output directory", required = true)
    private String output;

    @CommandLine.Option(names = "--shapeFilePath", description = "path to dilutionArea", required = true)
    private String shapeFilePath;

    @CommandLine.Option(names = "--config", description = "path to config file", required = true)
    private String config;

    @CommandLine.Option(names = "--runId", description = "run id for example base case, plan case ", required = true)
    private String runId;

    private final Logger logger = Logger.getLogger(EmissionCSVWriter.class);

    public static void main(String[] args) { System.exit(new CommandLine(new EmissionCSVWriter()).execute(args));}

    @Override
    public Integer call() throws Exception {

        Config cfg = ConfigUtils.loadConfig(config);

        logger.info("++++++++++ Start to read and process emission events file ++++++++++");
        EmissionEventHandler handler = processEvents(emissionsEventFile, shapeFilePath, cfg.network().getInputFile());

        Scenario scenario = ScenarioUtils.loadScenario(cfg);

        logger.info("++++++++++ Convert events to csv format ++++++++++");
        List<String> warmEmissionsAsCSV = convertToCSV(handler.getWarmEmissionsPerVehicle(), scenario.getVehicles());
        List<String> coldEmissionsAsCSV = convertToCSV(handler.getColdEmissionsPerVehicle(), scenario.getVehicles());

        output = output.endsWith("\\") ? output: output + "\\";
        String outputWarmEventsFile = output + runId + "_warm_emissions.csv";
        String outputColdEventsFile = output + runId + "_cold_emissions.csv";

        logger.info("++++++++++ Print warm emissions to " + outputWarmEventsFile + " ++++++++++");
        printToFile(warmEmissionsAsCSV, outputWarmEventsFile);
        logger.info("++++++++++ Print cold emissions to " + outputColdEventsFile + " ++++++++++");
        printToFile(coldEmissionsAsCSV, outputWarmEventsFile);

        logger.info("++++++++++ Analysis is done. Continue in R and have fun :) ++++++++++");
        return 0;
    }

    private static EmissionEventHandler processEvents(String eventsFile, String shapeFilePath, String networkFilePath){

        EventsManager manager = EventsUtils.createEventsManager();
        EmissionEventHandler handler = new EmissionEventHandler(shapeFilePath, networkFilePath);

        manager.addHandler(handler);
        new EmissionEventsReader(manager).readFile(eventsFile);


        return handler;
    }

    private static List<String> convertToCSV(Map<Id<Vehicle>, Map<Pollutant, Double>> emissions, Vehicles vehicles){

        Map<Id<Vehicle>, Vehicle> vehicleMap = vehicles.getVehicles();
        List<String> asCSV = new ArrayList<>();

        var pollutants = emissions.values().stream()
                .map(Map::keySet)
                .flatMap(Collection::stream)
                .distinct()
                .collect(Collectors.toList());

        String header1 = "vehicleId,vehicleType,";
        String header2 = pollutants.stream()
                .map(Enum::name)
                .map(name -> name + ",")
                .reduce(String::concat)
                .get();

        asCSV.add(header1 + header2);

        emissions.keySet().forEach(vehicleId -> {

            String type = vehicleMap.get(vehicleId).getType().getId().toString();

            StringBuilder csv = new StringBuilder(vehicleId.toString());
            csv.append(type).append(",");
            var vehicleEmissions = emissions.get(vehicleId);

            for(var pollutant: pollutants){

                if(vehicleEmissions.containsKey(pollutant)){
                    String emissionAsString = vehicleEmissions.get(pollutant).toString();
                    csv.append(emissionAsString).append(",");
                } else {

                    csv.append(",");
                }

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
