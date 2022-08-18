package Study_Analysis;

import org.locationtech.jts.geom.Geometry;
import org.matsim.api.core.v01.Coord;
import org.matsim.api.core.v01.Id;
import org.matsim.api.core.v01.events.LinkLeaveEvent;
import org.matsim.api.core.v01.events.handler.LinkLeaveEventHandler;
import org.matsim.api.core.v01.network.Link;
import org.matsim.api.core.v01.network.Network;
import org.matsim.core.utils.geometry.geotools.MGC;
import org.matsim.vehicles.Vehicle;

import java.util.HashMap;
import java.util.Map;

public class VehicleKilometerHandler implements LinkLeaveEventHandler {

    private final Map<String, Double> vehicleKm;

    private final Geometry dilutionArea;
    private final Network network;

    public VehicleKilometerHandler(Network network, Geometry dilutionArea){

        this.network = network;
        this.dilutionArea = dilutionArea;

        this.vehicleKm = new HashMap<>();
    }

    //Analysis for mode car
    @Override
    public void handleEvent(LinkLeaveEvent linkLeaveEvent) {

        if(isDrtVehicle(linkLeaveEvent.getVehicleId())) return;

        if(!isInGeometry(linkLeaveEvent.getLinkId())) return;

        double length = network.getLinks().get(linkLeaveEvent.getLinkId()).getLength();
        String vehicleId = linkLeaveEvent.getVehicleId().toString();

        if(vehicleKm.containsKey(vehicleId)){
            vehicleKm.merge(vehicleId, length / 1000, Double::sum);
        } else {
            vehicleKm.put(vehicleId, length / 1000);
        }
    }

    public Map<String, Double> getVehicleKm() {
        return vehicleKm;
    }

    private boolean isDrtVehicle(Id<Vehicle> vehicleId){

        return vehicleId.toString().startsWith("drt");
    }

    private boolean isInGeometry(Id<Link> linkId){

        Coord coord = network.getLinks().get(linkId).getCoord();
        return dilutionArea.covers(MGC.coord2Point(coord));
    }

}
