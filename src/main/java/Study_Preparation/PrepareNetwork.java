package Study_Preparation;

import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.locationtech.jts.geom.Geometry;
import org.matsim.api.core.v01.Scenario;
import org.matsim.api.core.v01.TransportMode;
import org.matsim.api.core.v01.network.Link;
import org.matsim.api.core.v01.network.Network;
import org.matsim.api.core.v01.network.NetworkWriter;
import org.matsim.core.config.Config;
import org.matsim.core.config.ConfigUtils;
import org.matsim.core.config.ConfigWriter;
import org.matsim.core.scenario.ScenarioUtils;
import org.matsim.core.utils.geometry.geotools.MGC;
import org.matsim.core.utils.gis.ShapeFileReader;

import java.util.Set;

public class PrepareNetwork {

    private static final String workingDirectory = "./scenario/open-vulkaneifel-scenario/";
    private static final String pathToConfig = workingDirectory + "vulkaneifel-v1.0-25pct.config.xml";
    private static final String pathToDilutionArea = "vulkaneifel-v1.0-25pct/dilutionArea/dilutionArea.shp";

    private static final String drtNetworkName = "network_with_drt.xml.gz";

    private static final Logger log = LogManager.getLogger(PrepareNetwork.class);

    public static void main(String[] args) {

        log.info("+++++ Reading config from " + pathToConfig + " +++++");
        Config config = ConfigUtils.loadConfig(pathToConfig);

        Scenario scenario = ScenarioUtils.loadScenario(config);
        Network network = scenario.getNetwork();

        Geometry dilutionArea = ShapeFileReader.getAllFeatures(workingDirectory + pathToDilutionArea).stream()
                .map(simpleFeature -> (Geometry) simpleFeature.getDefaultGeometry())
                .findFirst()
                .get();

        log.info("+++++ Start to add drt to network car links +++++");
        for(Link link: network.getLinks().values()){

            if(!dilutionArea.covers(MGC.coord2Point(link.getCoord()))) continue;

            if(link.getAllowedModes().contains(TransportMode.car)){
                Set<String> newModes = link.getAllowedModes();
                newModes.add(TransportMode.drt);

                link.setAllowedModes(newModes);
            }
        }

        String drtNetworkFilePath = workingDirectory + config.controler().getRunId() + drtNetworkName;

        log.info("+++++ Writing new network to " + drtNetworkFilePath + "+++++");
        new NetworkWriter(network).write(drtNetworkFilePath);

        config.network().setInputFile(drtNetworkFilePath);
        config.controler().setRunId(config.controler().getRunId() + "_with_drt");

        new ConfigWriter(config).write(workingDirectory + "vulkaneifel-v1.0-25pct.config.drt.xml");
    }
}
