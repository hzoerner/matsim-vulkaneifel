package Study_Preparation;

import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.locationtech.jts.geom.Geometry;
import org.matsim.api.core.v01.Id;
import org.matsim.api.core.v01.Scenario;
import org.matsim.api.core.v01.TransportMode;
import org.matsim.api.core.v01.network.Link;
import org.matsim.api.core.v01.network.Network;
import org.matsim.api.core.v01.network.NetworkWriter;
import org.matsim.contrib.analysis.vsp.traveltimedistance.NetworkRouteValidator;
import org.matsim.core.config.Config;
import org.matsim.core.config.ConfigUtils;
import org.matsim.core.network.algorithms.MultimodalNetworkCleaner;
import org.matsim.core.scenario.ScenarioUtils;
import org.matsim.core.utils.geometry.geotools.MGC;
import org.matsim.core.utils.gis.ShapeFileReader;

import java.util.List;
import java.util.Set;

public class PrepareNetwork {

    private static final String workingDirectory = "./scenario/open-vulkaneifel-scenario/";
    private static final String pathToConfig = workingDirectory + "vulkaneifel-v1.0-25pct.config.xml";
    private static final String pathToDilutionArea = "vulkaneifel-v1.0-25pct/dilutionArea/dilutionArea.shp";

    private static final String drtNetworkName = ".network_with_drt.xml.gz";

    private static final Logger log = LogManager.getLogger(PrepareNetwork.class);

    public static void main(String[] args) {

        log.info("+++++ Reading config from " + pathToConfig + " +++++");
        Scenario scenario;
        String runId;

        {
            Config config = ConfigUtils.loadConfig(pathToConfig);
            runId = config.controler().getRunId();
            scenario = ScenarioUtils.loadScenario(config);
        }

        Network network = scenario.getNetwork();

        Geometry dilutionArea = ShapeFileReader.getAllFeatures(workingDirectory + pathToDilutionArea).stream()
                .map(simpleFeature -> (Geometry) simpleFeature.getDefaultGeometry())
                .findFirst()
                .get()
                .getEnvelope();

        log.info("+++++ Start to add drt to network car links +++++");

        //list contains links which are covered by shape envelope but are need to generate a connected network
        List<Id<Link>> idList = List.of(Id.createLinkId("607487450011f"), Id.createLinkId("607487450011r"));

        for(Link link: network.getLinks().values()){

            if(idList.stream().anyMatch(linkId -> linkId.equals(link.getId()))){
                log.info("+++++ Add Drt as allowed mode to missing links +++++");

                Set<String> newModes = new java.util.HashSet<>(Set.copyOf(link.getAllowedModes().stream().toList()));
                newModes.add(TransportMode.drt);

                link.setAllowedModes(newModes);
            }

            if(!dilutionArea.covers(MGC.coord2Point(link.getCoord()))) continue;

            if(link.getAllowedModes().contains(TransportMode.car) ){
                Set<String> newModes = new java.util.HashSet<>(Set.copyOf(link.getAllowedModes().stream().toList()));
                newModes.add(TransportMode.drt);

                link.setAllowedModes(newModes);
            }
        }

        new MultimodalNetworkCleaner(network).run(Set.of(TransportMode.drt));

        String drtNetworkFilePath = workingDirectory + runId + drtNetworkName;

        log.info("+++++ Writing new network to " + drtNetworkFilePath + "+++++");
        new NetworkWriter(network).write(drtNetworkFilePath);
    }
}
