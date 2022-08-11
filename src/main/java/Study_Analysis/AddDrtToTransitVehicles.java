package Study_Analysis;

import org.matsim.api.core.v01.Id;
import org.matsim.api.core.v01.Scenario;
import org.matsim.api.core.v01.TransportMode;
import org.matsim.core.config.Config;
import org.matsim.core.config.ConfigUtils;
import org.matsim.core.scenario.ScenarioUtils;
import org.matsim.vehicles.MatsimVehicleWriter;
import org.matsim.vehicles.Vehicle;
import org.matsim.vehicles.VehicleType;

public class AddDrtToTransitVehicles {

    private static final String ptVehiclesFile = "C:\\Users\\ACER\\IdeaProjects\\matsim-vulkaneifel\\output\\study\\fleet-size-400\\fleet-size-400-plan-case-2.output_transitVehicles.xml.gz";
    private static final int fleetsize = 400;

    public static void main(String[] args) {

        Config config = ConfigUtils.createConfig();
        config.transit().setVehiclesFile(ptVehiclesFile);

        Scenario scenario = ScenarioUtils.loadScenario(config);

        var transitVehicles = scenario.getTransitVehicles();
        var factory = transitVehicles.getFactory();

        VehicleType drtType = factory.createVehicleType( Id.create("drt_veh_type", VehicleType.class ));
        drtType.getCapacity().setSeats(1).setStandingRoom(0);
        drtType.setPcuEquivalents(1.0);
        drtType.setNetworkMode(TransportMode.drt);
        transitVehicles.addVehicleType(drtType);

        for(int drt_id = 0; drt_id < fleetsize; drt_id ++){

            Vehicle drtVehicle = factory.createVehicle(Id.createVehicleId("drt_" + drt_id), drtType);
            transitVehicles.addVehicle(drtVehicle);
        }

        new MatsimVehicleWriter(transitVehicles).writeFile(ptVehiclesFile);
    }
}
