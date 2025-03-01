package drtSchoolTransportStudy.run;

import drtSchoolTransportStudy.analysis.SchoolTripsAnalysis;
import drtSchoolTransportStudy.jsprit.PreplannedSchedulesCalculator;
import drtSchoolTransportStudy.run.dummyTraffic.DvrpBenchmarkTravelTimeModuleFixedTT;
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVPrinter;
import org.apache.commons.io.FileUtils;
import org.apache.log4j.Logger;
import org.matsim.api.core.v01.network.Network;
import org.matsim.api.core.v01.population.Population;
import org.matsim.application.MATSimAppCommand;
import org.matsim.contrib.drt.extension.preplanned.optimizer.PreplannedDrtOptimizer;
import org.matsim.contrib.drt.extension.preplanned.run.PreplannedDrtControlerCreator;
import org.matsim.contrib.drt.run.DrtConfigGroup;
import org.matsim.contrib.drt.run.MultiModeDrtConfigGroup;
import org.matsim.contrib.dvrp.fleet.FleetSpecification;
import org.matsim.contrib.dvrp.run.AbstractDvrpModeQSimModule;
import org.matsim.contrib.dvrp.run.DvrpConfigGroup;
import org.matsim.contrib.dvrp.run.DvrpModule;
import org.matsim.core.config.Config;
import org.matsim.core.config.ConfigUtils;
import org.matsim.core.controler.Controler;
import picocli.CommandLine;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

@CommandLine.Command(
        name = "run-jsprit-experiment",
        description = "run jsprit experiment"
)
public class RunOfflineApproachJsprit implements MATSimAppCommand {
    private static final Logger log = Logger.getLogger(RunOfflineApproachJsprit.class);

    @CommandLine.Option(names = "--config", description = "path to config file", required = true)
    private Path configPath;

    @CommandLine.Option(names = "--output", description = "root output folder", required = true)
    private String output;

    @CommandLine.Option(names = "--alpha", description = "travel time alpha", defaultValue = "2")
    private String alpha;

    @CommandLine.Option(names = "--beta", description = "travel time beta", defaultValue = "1200")
    private String beta;

    @CommandLine.Option(names = "--school-starting-time", description = "school starting time", defaultValue = "UNIFORM")
    private CaseStudyTool.SchoolStartingTime schoolStartingTime;

    @CommandLine.Option(names = "--service-scheme", description = "Service scheme", defaultValue = "DOOR_TO_DOOR")
    private CaseStudyTool.ServiceScheme serviceScheme;

    @CommandLine.Option(names = "--fleet-size", description = "fleet size", defaultValue = "100")
    private int fleetSize;

    @CommandLine.Option(names = "--steps", description = "maximum number of runs", defaultValue = "1")
    private int steps;

    @CommandLine.Option(names = "--step-size", description = "number of vehicles reduced for each step", defaultValue = "5")
    private int stepSize;

    @CommandLine.Option(names = "--iters", description = "jsprit iteraions", defaultValue = "1000")
    private int jspritIterations;

    @CommandLine.Option(names = "--multi-thread", defaultValue = "false", description = "enable multi-threading in JSprit to increase computation speed")
    private boolean multiThread;

    @CommandLine.Option(names = "--network-change-events", description = "Path to network change events file", defaultValue = "")
    private String networkChangeEvents;

    @CommandLine.Option(names = "--buffer", description = "Add buffer to the plans by reducing the max travel time", defaultValue = "0.0")
    private double bufferForTraffic;

    private final SchoolTripsAnalysis analysis = new SchoolTripsAnalysis();

    public static void main(String[] args) {
        new RunOfflineApproachJsprit().execute(args);
    }

    @Override
    public Integer call() throws Exception {
        // Create a temporary config file in the same folder, so that multiple runs can be run in the cluster at the same time
        String temporaryConfig = createTemporaryConfig();

        output = output + "-alpha_" + alpha + "-beta_" + beta + "-" + schoolStartingTime.toString() + "-" + serviceScheme.toString();
        CaseStudyTool caseStudyTool = new CaseStudyTool(alpha, beta, schoolStartingTime, serviceScheme);

        if (!Files.exists(Path.of(output))) {
            Files.createDirectory(Path.of(output));
        }
        CSVPrinter tsvWriter = new CSVPrinter(new FileWriter(output + "/result-summary.tsv", true), CSVFormat.TDF);
        tsvWriter.printRecord(SchoolTripsAnalysis.TITLE_ROW_KPI);
        tsvWriter.close();

        // Run finite fleet sizes
        int maxFleetSize = fleetSize + stepSize * (steps - 1);
        while (fleetSize <= maxFleetSize) {
            String outputDirectory = output + "/fleet-size-" + fleetSize;
            runJsprit(temporaryConfig, outputDirectory, caseStudyTool);

            // If the on-time rate is approaching 100%, then we can stop the experiments sequence early
            double onTimeRate = Double.parseDouble(analysis.getOutputKPIRow().get(4));
            int numRequests = Integer.parseInt(analysis.getOutputKPIRow().get(1));
            int numServedRequests = Integer.parseInt(analysis.getOutputKPIRow().get(2));
            if (onTimeRate >= 0.99 && numServedRequests == numRequests) {
                maxFleetSize = Math.min(maxFleetSize, fleetSize + stepSize * 2); // 2 more runs and then finish
            }
            fleetSize += stepSize;
        }

        // Delete the temporary config file for the current run
        Files.delete(Path.of(temporaryConfig));

        return 0;
    }

    private void runJsprit(String configPath, String outputDirectory, CaseStudyTool caseStudyTool) throws IOException {
        Config config = ConfigUtils.loadConfig(configPath, new MultiModeDrtConfigGroup(), new DvrpConfigGroup());
        MultiModeDrtConfigGroup multiModeDrtConfig = MultiModeDrtConfigGroup.get(config);
        DrtConfigGroup drtConfigGroup = multiModeDrtConfig.getModalElements().iterator().next(); // By default, the first drt config group is the one we are using
        config.controler().setOutputDirectory(outputDirectory);
        config.controler().setLastIteration(0);
        modifyConfig(config, caseStudyTool);

        if (!networkChangeEvents.equals("")){
            config.network().setChangeEventsInputFile(networkChangeEvents);
            config.network().setTimeVariantNetwork(true);
            double modifiedAlpha = drtConfigGroup.getMaxTravelTimeAlpha() * (1 - bufferForTraffic);
            double modifiedBeta = drtConfigGroup.getMaxTravelTimeBeta() * (1 - bufferForTraffic);
            drtConfigGroup.setMaxTravelTimeAlpha(modifiedAlpha);
            drtConfigGroup.setMaxTravelTimeBeta(modifiedBeta);
        }

        Controler controler = PreplannedDrtControlerCreator.createControler(config, false);
        controler.addOverridingModule(new DvrpModule(new DvrpBenchmarkTravelTimeModuleFixedTT(0)));

        var options = new PreplannedSchedulesCalculator.Options(false, false, jspritIterations, multiThread, caseStudyTool);
        MultiModeDrtConfigGroup.get(config)
                .getModalElements()
                .forEach(drtConfig -> controler.addOverridingQSimModule(
                        new AbstractDvrpModeQSimModule(drtConfig.getMode()) {
                            @Override
                            protected void configureQSim() {
                                bindModal(PreplannedDrtOptimizer.PreplannedSchedules.class).toProvider(modalProvider(
                                        getter -> new PreplannedSchedulesCalculator(drtConfig,
                                                getter.getModal(FleetSpecification.class),
                                                getter.getModal(Network.class), getter.get(Population.class),
                                                options).calculate())).asEagerSingleton();
                            }
                        }));

        controler.run();

        // Post analysis
        analysis.setIncludeDepartureDelayAnalysis(false);
        analysis.analyze(Path.of(outputDirectory));
        CSVPrinter resultsWriter = new CSVPrinter(new FileWriter(output + "/result-summary.tsv", true), CSVFormat.TDF);
        resultsWriter.printRecord(analysis.getOutputKPIRow());
        resultsWriter.close();

//        String[] input = new String[]{outputDirectory};
//        DrtVehicleStoppingTaskWriter.main(input);  //TODO modification is needed for the plotting script
    }

    private void modifyConfig(Config config, CaseStudyTool caseStudyTool) {
        MultiModeDrtConfigGroup multiModeDrtConfigGroup = MultiModeDrtConfigGroup.get(config);
        for (DrtConfigGroup drtCfg : multiModeDrtConfigGroup.getModalElements()) {
            caseStudyTool.prepareCaseStudy(config, drtCfg);  // Prepare DRT config based on case study
            drtCfg.setVehiclesFile("drt-vehicles-with-depot/" + fleetSize + "-8_seater-drt-vehicles.xml");
            if (drtCfg.getRebalancingParams().isPresent()) {
                log.warn("The rebalancing parameter set is defined for drt mode: "
                        + drtCfg.getMode()
                        + ". It will be ignored. No rebalancing will happen.");
                drtCfg.removeParameterSet(drtCfg.getRebalancingParams().get());
            }
        }
    }

    private String createTemporaryConfig() {
        int taskId = (int) (System.currentTimeMillis() / 1000);
        File originalConfig = new File(configPath.toString());
        String temporaryConfig = configPath.getParent().toString() + "/temporary_" + taskId + ".config.xml";
        File copy = new File(temporaryConfig);
        try {
            FileUtils.copyFile(originalConfig, copy);
        } catch (IOException e) {
            e.printStackTrace();
        }
        return temporaryConfig;
    }
}
