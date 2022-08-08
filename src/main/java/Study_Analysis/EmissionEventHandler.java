package Study_Analysis;

import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.locationtech.jts.geom.Geometry;
import org.matsim.api.core.v01.Id;
import org.matsim.api.core.v01.network.Link;
import org.matsim.api.core.v01.network.Network;
import org.matsim.contrib.emissions.Pollutant;
import org.matsim.contrib.emissions.events.ColdEmissionEvent;
import org.matsim.contrib.emissions.events.ColdEmissionEventHandler;
import org.matsim.contrib.emissions.events.WarmEmissionEvent;
import org.matsim.contrib.emissions.events.WarmEmissionEventHandler;
import org.matsim.core.network.NetworkUtils;
import org.matsim.core.utils.geometry.geotools.MGC;
import org.matsim.core.utils.gis.ShapeFileReader;
import org.matsim.vehicles.Vehicle;

import java.util.HashMap;
import java.util.Map;

public class EmissionEventHandler implements ColdEmissionEventHandler, WarmEmissionEventHandler {

    private Map<Id<Vehicle>, Map<Pollutant, Double>> coldEmissionsPerVehicle;
    private Map<Id<Vehicle>, Map<Pollutant, Double>> warmEmissionsPerVehicle;

    private Geometry dilutionArea;
    private Map<Id<Link>, ? extends Link> links;

    private int counter = 0;

    private final Logger logger = LogManager.getLogger(EmissionEventHandler.class);

    public EmissionEventHandler(String shapeFilePath, String networkFilePath){
        logger.info("Initalized EmissionEventHandler...");

        coldEmissionsPerVehicle = new HashMap<>();
        warmEmissionsPerVehicle = new HashMap<>();

        dilutionArea = loadGeometry(shapeFilePath);
        links = loadNetworkLinks(networkFilePath);

    }

    @Override
    public void handleEvent(ColdEmissionEvent coldEmissionEvent) {

        if(!isLinkInDilutionArea(coldEmissionEvent.getLinkId())) return;

        Id<Vehicle> vehicleId = coldEmissionEvent.getVehicleId();

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

        if(!isLinkInDilutionArea(warmEmissionEvent.getLinkId())) return;

        if(counter++ == 1247){

            System.out.println("STOP");
        }
        var vehicleId = warmEmissionEvent.getVehicleId();

        if(warmEmissionsPerVehicle.containsKey(vehicleId)){

            var existingPolutions = warmEmissionsPerVehicle.get(vehicleId);
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

    private Geometry loadGeometry(String shapeFilePath){

        return ShapeFileReader.getAllFeatures(shapeFilePath).stream()
                .map(simpleFeature -> (Geometry) simpleFeature.getDefaultGeometry())
                .findFirst()
                .get();
    }

    private Map<Id<Link>, ? extends Link> loadNetworkLinks(String networkFilePath){

        return NetworkUtils.readNetwork(networkFilePath).getLinks();
    }

    private boolean isLinkInDilutionArea(Id<Link> linkId){

        return dilutionArea.covers(MGC.coord2Point(links.get(linkId).getCoord()));
    }

    /*
    * TODO
    *  add shape filter
    *  add vehicle type (perhaps in Run class)
    * */
}
