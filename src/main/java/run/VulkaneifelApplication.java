package run;

import Study_Analysis.EmissionCSVWriter;
import Study_Analysis.PersonKilometerAnalysis;
import Study_Analysis.VehicleKilometerAnalysis;
import org.matsim.application.MATSimApplication;
import org.matsim.application.analysis.emissions.AirPollutionByVehicleCategory;
import org.matsim.application.prepare.population.CleanPopulation;
import org.matsim.application.prepare.population.DownSamplePopulation;
import org.matsim.application.prepare.population.ResolveGridCoordinates;
import org.matsim.application.prepare.population.TrajectoryToPlans;
import org.matsim.core.config.Config;
import org.matsim.core.config.groups.PlanCalcScoreConfigGroup;
import org.matsim.core.config.groups.PlansCalcRouteConfigGroup;
import picocli.CommandLine;
import prepare.*;

import java.util.List;

@CommandLine.Command(header = ":: Open Vulkaneifel Scenario ::", version="1.0")
@MATSimApplication.Prepare({
        CreateNetwork.class, CreatePTFromGtfs.class,
        CreateRegionalTrainLine.class, RemoveBusLineFromSmallSchedule.class,
        MergeTransitSchedules.class, TrajectoryToPlans.class, CleanPopulation.class, ResolveGridCoordinates.class,
        DownSamplePopulation.class
})
@MATSimApplication.Analysis({
        AirPollutionByVehicleCategory.class, EmissionCSVWriter.class, VehicleKilometerAnalysis.class, PersonKilometerAnalysis.class
})

public class VulkaneifelApplication extends MATSimApplication {

    public static void main(String[] args) {
        MATSimApplication.run(VulkaneifelApplication.class, args);
    }

    @Override
    protected Config prepareConfig(Config config) {

        for (long ii = 600; ii <= 97200; ii += 600) {

            for (String act : List.of("business", "educ_higher", "educ_kiga", "educ_other", "educ_primary", "educ_secondary",
                    "educ_tertiary", "errands", "home", "leasure", "shop_daily", "shop_other", "visit", "work")) {
                config.planCalcScore()
                        .addActivityParams(new PlanCalcScoreConfigGroup.ActivityParams(act + "_" + ii).setTypicalDuration(ii));
            }

            config.planCalcScore().addActivityParams(new PlanCalcScoreConfigGroup.ActivityParams("work_" + ii).setTypicalDuration(ii)
                    .setOpeningTime(6. * 3600.).setClosingTime(20. * 3600.));
            config.planCalcScore().addActivityParams(new PlanCalcScoreConfigGroup.ActivityParams("business_" + ii).setTypicalDuration(ii)
                    .setOpeningTime(6. * 3600.).setClosingTime(20. * 3600.));
            config.planCalcScore().addActivityParams(new PlanCalcScoreConfigGroup.ActivityParams("leisure_" + ii).setTypicalDuration(ii)
                    .setOpeningTime(9. * 3600.).setClosingTime(27. * 3600.));
            config.planCalcScore().addActivityParams(new PlanCalcScoreConfigGroup.ActivityParams("shopping_" + ii).setTypicalDuration(ii)
                    .setOpeningTime(8. * 3600.).setClosingTime(20. * 3600.));
        }

        config.plansCalcRoute().setAccessEgressType(PlansCalcRouteConfigGroup.AccessEgressType.accessEgressModeToLink);
        config.qsim().setUsingTravelTimeCheckInTeleportation(true);
        config.qsim().setUsePersonIdForMissingVehicleId(false);

        return config;
    }
}
