package run;

import org.apache.log4j.Logger;
import org.matsim.api.core.v01.Scenario;
import org.matsim.contrib.drt.optimizer.rebalancing.RebalancingParams;
import org.matsim.contrib.drt.optimizer.rebalancing.mincostflow.MinCostFlowRebalancingStrategyParams;
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

public class RunRebalancingTuning {

    private static final String pathToConfig = "scenario/open-vulkaneifel-scenario/vulkaneifel-drt-rebalanc.config.xml";
    private static final String pathToServieArea = "vulkaneifel-v1.0-25pct/dilutionArea/dilutionArea.shp";
    private static final String pathToDRTVehicles = "vulkaneifel-v1.0-25pct/drt-vehicles/100-1_seater-drt-vehicles.xml";

    private static final Double[] alphaValues = {0.2, 0.4, 0.6, 0.8};
    private static final Double[] betaValues = {0.0, 0.1, 0.3, 0.7,};

    private static final Logger log = Logger.getLogger(RunRebalancingTuning.class);

    public static void main(String[] args) {

        for(Double beta: betaValues){

            for(Double alpha: alphaValues){

                Config config = ConfigUtils.loadConfig(pathToConfig, new DvrpConfigGroup(), new MultiModeDrtConfigGroup());
                config.qsim().setSimStarttimeInterpretation(QSimConfigGroup.StarttimeInterpretation.onlyUseStarttime);
                prepareConfig(config);

                MultiModeDrtConfigGroup multiModeDrtConfig = MultiModeDrtConfigGroup.get(config);

                String runid = "drt-rebalanc-tuning-alpha-" + alpha.doubleValue() + "-beta-" + beta.doubleValue();
                config.controler().setRunId(runid);
                config.controler().setOutputDirectory("rebalanc-tuning/" + runid);

                multiModeDrtConfig.getModalElements().forEach(drtConfigGroup -> {
                    drtConfigGroup.setDrtServiceAreaShapeFile(pathToServieArea);
                    drtConfigGroup.setVehiclesFile(pathToDRTVehicles);

                    MinCostFlowRebalancingStrategyParams minCostFlowRebalancingStrategyParams = new MinCostFlowRebalancingStrategyParams();
                    minCostFlowRebalancingStrategyParams.setRebalancingTargetCalculatorType(
                            MinCostFlowRebalancingStrategyParams.RebalancingTargetCalculatorType.EstimatedDemand);
                    minCostFlowRebalancingStrategyParams
                            .setZonalDemandEstimatorType(MinCostFlowRebalancingStrategyParams.ZonalDemandEstimatorType.PreviousIterationDemand);
                    minCostFlowRebalancingStrategyParams.setTargetAlpha(alpha);
                    minCostFlowRebalancingStrategyParams.setTargetBeta(beta);
                    minCostFlowRebalancingStrategyParams.setDemandEstimationPeriod(1800);

                    RebalancingParams rebalancingParams = new RebalancingParams();
                    rebalancingParams.addParameterSet(minCostFlowRebalancingStrategyParams);
                    drtConfigGroup.addParameterSet(rebalancingParams);
                });

                DrtConfigs.adjustMultiModeDrtConfig(multiModeDrtConfig, config.planCalcScore(), config.plansCalcRoute());
                Scenario scenario = createScenarioWithDrtRouteFactory(config);
                ScenarioUtils.loadScenario(scenario);

                Controler controler = new Controler(scenario);
                controler.addOverridingModule(new DvrpModule());
                controler.addOverridingModule(new MultiModeDrtModule());
                controler.configureQSimComponents(DvrpQSimComponents.activateAllModes(new MultiModal[]{multiModeDrtConfig}));

                log.info("++++++++ Run Vulkaneifel Scenario with alpha: " + alpha + " and beta: " + beta + " ++++++++");
                controler.run();
            }
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
