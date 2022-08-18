package Study_Analysis;

import org.jboss.logging.Logger;
import org.locationtech.jts.geom.Geometry;
import org.matsim.api.core.v01.Scenario;
import org.matsim.api.core.v01.network.Network;
import org.matsim.application.MATSimAppCommand;
import org.matsim.core.config.Config;
import org.matsim.core.config.ConfigUtils;
import org.matsim.core.events.EventsUtils;
import org.matsim.core.scenario.ScenarioUtils;
import org.matsim.core.utils.gis.ShapeFileReader;
import org.matsim.vehicles.Vehicles;
import org.opengis.feature.simple.SimpleFeature;
import picocli.CommandLine;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

@CommandLine.Command(
        name = "person-kilometers",
        description = {"analysis of person kilometeres in pt"}
)

public class VehicleKilometerAnalysis implements MATSimAppCommand {

    @CommandLine.Option(names = "--events", description = "input events file", required = true)
    private String events;

    @CommandLine.Option(names = "--output", description = "output directory", required = true)
    private String output;

    @CommandLine.Option(names = "--shapeFilePath", description = "path to dilutionArea", required = true)
    private String shapeFilePath;

    @CommandLine.Option(names = "--config", description = "path to config file", required = true)
    private String config;

    @CommandLine.Option(names = "--runId", description = "run id for example base case, plan case ", required = true)
    private String runId;

    private final Logger logger = Logger.getLogger(VehicleKilometerAnalysis.class);

    public static void main(String[] args) { System.exit(new CommandLine(new EmissionCSVWriter()).execute(args));}

    @Override
    public Integer call() throws Exception {

        Config cfg = ConfigUtils.loadConfig(config);

        Scenario scenario = ScenarioUtils.loadScenario(cfg);
        Network network = scenario.getNetwork();
        Vehicles transitVehicles = scenario.getTransitVehicles();

        logger.info("++++++++++ Start events processing ++++++++++++");
        VehicleKilometerHandler handler = processEvents(events, network);
        String header = "vehicleId,drivenDistance_km";

        logger.info("++++++++++ Convert map to csv format ++++++++++++");
        List<String> csv = convertToCSV(handler.getVehicleKm(), header);

        String outputfile = output + runId + "-vehicle_kilometers.csv";
        logger.info("++++++++++ Print to: " + outputfile + " ++++++++++++");
        printToFile(csv, outputfile);

        return null;
    }

    private Geometry loadGeometry(String shapeFilePath){

        return (Geometry) ShapeFileReader.getAllFeatures(shapeFilePath).stream()
                .map(SimpleFeature::getDefaultGeometry)
                .findFirst()
                .get();
    }

    private VehicleKilometerHandler processEvents(String eventsFilePath, Network network){

        var manager = EventsUtils.createEventsManager();
        var handler = new VehicleKilometerHandler(network, loadGeometry(shapeFilePath));
        manager.addHandler(handler);

        EventsUtils.readEvents(manager, eventsFilePath);
        return handler;
    }

    private List<String> convertToCSV(Map<String, Double> vehicleKm, String header){

        List<String> csv = new ArrayList<>();
        csv.add(header);

        vehicleKm.keySet().forEach(vehicleId -> {
            String entry = vehicleId + ",";
            entry += vehicleKm.get(vehicleId);
            csv.add(entry);
        });
        return csv;
    }

    private void printToFile(List<String> vehicleKm, String outputFilePath){

        try {
            PrintWriter pWriter = new PrintWriter(new BufferedWriter(new FileWriter(outputFilePath)));

            vehicleKm.forEach(pWriter::println);
            pWriter.close();
        } catch (IOException e) {
            e.printStackTrace();
        }

    }
}
