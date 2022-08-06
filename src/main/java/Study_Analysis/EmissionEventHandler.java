package Study_Analysis;

import org.matsim.api.core.v01.Id;
import org.matsim.contrib.emissions.Pollutant;
import org.matsim.contrib.emissions.events.ColdEmissionEvent;
import org.matsim.contrib.emissions.events.ColdEmissionEventHandler;
import org.matsim.contrib.emissions.events.WarmEmissionEvent;
import org.matsim.contrib.emissions.events.WarmEmissionEventHandler;
import org.matsim.vehicles.Vehicle;

import java.util.HashMap;
import java.util.Map;

public class EmissionEventHandler implements ColdEmissionEventHandler, WarmEmissionEventHandler {

    private Map<Id<org.matsim.vehicles.Vehicle>, Map<Pollutant, Double>> coldEmissionsPerVehicle = new HashMap<>();
    private Map<Id<Vehicle>, Map<Pollutant, Double>> warmEmissionsPerVehicle = new HashMap<>();

    @Override
    public void handleEvent(ColdEmissionEvent coldEmissionEvent) {

        var vehicleId = coldEmissionEvent.getVehicleId();

        if(coldEmissionsPerVehicle.containsKey(vehicleId)){

           var existingPolutions = coldEmissionsPerVehicle.get(vehicleId);
           var newPollutions = coldEmissionEvent.getColdEmissions();

           //merge Emissions
           newPollutions.keySet().
                   forEach(pollutant -> existingPolutions.merge(pollutant, newPollutions.get(pollutant), Double::sum));

        } else {
            coldEmissionsPerVehicle.put(vehicleId, coldEmissionEvent.getColdEmissions());
        }
    }

    @Override
    public void handleEvent(WarmEmissionEvent warmEmissionEvent) {

        var vehicleId = warmEmissionEvent.getVehicleId();

        if(warmEmissionsPerVehicle.containsKey(vehicleId)){

            var existingPolutions = coldEmissionsPerVehicle.get(vehicleId);
            var newPollutions = warmEmissionEvent.getWarmEmissions();

            //merge Emissions
            newPollutions.keySet().
                    forEach(pollutant -> existingPolutions.merge(pollutant, newPollutions.get(pollutant), Double::sum));

        } else {
            warmEmissionsPerVehicle.put(vehicleId, warmEmissionEvent.getWarmEmissions());
        }
    }

    public Map<Id<Vehicle>, Map<Pollutant, Double>> getColdEmissionsPerVehicle() {
        return coldEmissionsPerVehicle;
    }

    public Map<Id<Vehicle>, Map<Pollutant, Double>> getWarmEmissionsPerVehicle() {
        return warmEmissionsPerVehicle;
    }
}
