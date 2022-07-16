package run;

import org.matsim.api.core.v01.Scenario;
import org.matsim.contrib.drt.routing.DrtRoute;
import org.matsim.contrib.drt.routing.DrtRouteFactory;
import org.matsim.contrib.drt.run.DrtConfigs;
import org.matsim.contrib.drt.run.MultiModeDrtConfigGroup;
import org.matsim.contrib.drt.run.MultiModeDrtModule;
import org.matsim.contrib.dvrp.run.DvrpConfigGroup;
import org.matsim.contrib.dvrp.run.DvrpModule;
import org.matsim.contrib.dvrp.run.DvrpQSimComponents;
import org.matsim.contrib.dvrp.run.MultiModal;
import org.matsim.core.config.Config;
import org.matsim.core.config.ConfigUtils;
import org.matsim.core.config.groups.PlanCalcScoreConfigGroup;
import org.matsim.core.config.groups.PlansCalcRouteConfigGroup;
import org.matsim.core.config.groups.QSimConfigGroup;
import org.matsim.core.controler.Controler;
import org.matsim.core.scenario.ScenarioUtils;

import java.util.List;

public class RunFleetSizeDetermination {

    private static final String pathToConfig = "./scenario/open-vulkaneifel-scenario/vulkaneifel-drt-test.config.xml";
    private static final String vehicleDirectory = "vulkaneifel-v1.0-25pct/drt-vehicles/";
    private static final String pathToServieArea = "vulkaneifel-v1.0-25pct/dilutionArea/dilutionArea.shp";

    private static final String[] fleetSize = {"40", "60", "80", "100", "120", "140", "160"};

    public static void main(String[] args) {

        Config config = ConfigUtils.loadConfig(pathToConfig, new DvrpConfigGroup(), new MultiModeDrtConfigGroup());
        config.qsim().setSimStarttimeInterpretation(QSimConfigGroup.StarttimeInterpretation.onlyUseStarttime);
        config.controler().setLastIteration(300);
        prepareConfig(config);

        MultiModeDrtConfigGroup multiModeDrtConfig = MultiModeDrtConfigGroup.get(config);

        for(String size: fleetSize){

            config.controler().setRunId("fleet-size-" + fleetSize + "-no-rebalanc");
            config.controler().setOutputDirectory("fleetsize-determination/fleet-size-" + fleetSize + "-no-rebalanc-output/");

            multiModeDrtConfig.getModalElements().forEach(drtConfigGroup -> {
                drtConfigGroup.setDrtServiceAreaShapeFile(pathToServieArea);
                drtConfigGroup.setVehiclesFile(vehicleDirectory + size + "-1_seater-drt-vehicles.xml");
            });

            DrtConfigs.adjustMultiModeDrtConfig(multiModeDrtConfig, config.planCalcScore(), config.plansCalcRoute());
            Scenario scenario = createScenarioWithDrtRouteFactory(config);
            ScenarioUtils.loadScenario(scenario);

            Controler controler = new Controler(scenario);
            controler.addOverridingModule(new DvrpModule());
            controler.addOverridingModule(new MultiModeDrtModule());
            controler.configureQSimComponents(DvrpQSimComponents.activateAllModes(new MultiModal[]{multiModeDrtConfig}));

            controler.run();
        }
    }

    private static void prepareConfig(Config config){

        config.plansCalcRoute().setAccessEgressType(PlansCalcRouteConfigGroup.AccessEgressType.accessEgressModeToLink);

        for (long ii = 600; ii <= 97200; ii += 600) {

            for (String act : List.of("educ_higher", "educ_kiga", "educ_other", "educ_primary", "educ_secondary",
                    "educ_tertiary", "errands", "home", "visit")) {
                config.planCalcScore()
                        .addActivityParams(new PlanCalcScoreConfigGroup.ActivityParams(act + "_" + ii).setTypicalDuration(ii));
            }

            config.planCalcScore().addActivityParams(new PlanCalcScoreConfigGroup.ActivityParams("work_" + ii).setTypicalDuration(ii)
                    .setOpeningTime(6. * 3600.).setClosingTime(20. * 3600.));
            config.planCalcScore().addActivityParams(new PlanCalcScoreConfigGroup.ActivityParams("business_" + ii).setTypicalDuration(ii)
                    .setOpeningTime(6. * 3600.).setClosingTime(20. * 3600.));
            config.planCalcScore().addActivityParams(new PlanCalcScoreConfigGroup.ActivityParams("leisure_" + ii).setTypicalDuration(ii)
                    .setOpeningTime(9. * 3600.).setClosingTime(27. * 3600.));
            config.planCalcScore().addActivityParams(new PlanCalcScoreConfigGroup.ActivityParams("shop_daily_" + ii).setTypicalDuration(ii)
                    .setOpeningTime(8. * 3600.).setClosingTime(20. * 3600.));
            config.planCalcScore().addActivityParams(new PlanCalcScoreConfigGroup.ActivityParams("shop_other_" + ii).setTypicalDuration(ii)
                    .setOpeningTime(8. * 3600.).setClosingTime(20. * 3600.));
        }

    }

    private static Scenario createScenarioWithDrtRouteFactory(Config config) {
        Scenario scenario = ScenarioUtils.createScenario(config);
        scenario.getPopulation().getFactory().getRouteFactories().setRouteFactory(DrtRoute.class, new DrtRouteFactory());
        return scenario;
    }
}
