package prepare;

import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.Point;
import org.matsim.api.core.v01.TransportMode;
import org.matsim.api.core.v01.network.Link;
import org.matsim.application.MATSimAppCommand;
import org.matsim.contrib.osm.networkReader.LinkProperties;
import org.matsim.contrib.osm.networkReader.OsmTags;
import org.matsim.contrib.osm.networkReader.SupersonicOsmNetworkReader;
import org.matsim.core.network.algorithms.MultimodalNetworkCleaner;
import org.matsim.core.network.io.NetworkWriter;
import org.matsim.core.utils.geometry.CoordinateTransformation;
import org.matsim.core.utils.geometry.geotools.MGC;
import org.matsim.core.utils.geometry.transformations.TransformationFactory;
import org.matsim.core.utils.gis.ShapeFileReader;
import picocli.CommandLine;

import java.util.Arrays;
import java.util.Map;
import java.util.Set;

@CommandLine.Command(
        name="network",
        description = "Create matsim network from osm data",
        showDefaultValues = true
)

public class CreateNetwork implements MATSimAppCommand {

    @CommandLine.Option(names = "--osmnetwork", description = "path to osm data files", required = true)
    private String osmnetwork;

    @CommandLine.Option(names = "--output", description = "path to output directory", required = true)
    private String output;

    @CommandLine.Option(names = "--veryDetailedArea", description = "path to shape that covers very detailed network", required = true)
    private String veryDetailedArea;

    private static final Logger log = LogManager.getLogger(CreateNetwork.class);
    private static final CoordinateTransformation transformation = TransformationFactory.getCoordinateTransformation("EPSG:4326", "EPSG:25832");
    private static final Set<String> carRideBike = Set.of(TransportMode.car, TransportMode.ride, TransportMode.bike);
    private static final Set<String> carRide = Set.of(TransportMode.car, TransportMode.ride);

    public static void main(String[] args) {
        System.exit(new CommandLine(new CreateNetwork()).execute(args));
    }

    @Override
    public Integer call() {

        Geometry veryDetailedAreaBox = getVeryDetailedAreaBox(veryDetailedArea);
        Geometry semiDetailedAreaBox = getSemiDetailedAreaBox(veryDetailedAreaBox);

        log.info("Start to parse network. This might not output anything for a while");
        var network = new SupersonicOsmNetworkReader.Builder()
                .setCoordinateTransformation(transformation)
                .setIncludeLinkAtCoordWithHierarchy((coord, level) -> {
                    if(level <= LinkProperties.LEVEL_PRIMARY) return true;

                    if(level <= LinkProperties.LEVEL_SECONDARY) return semiDetailedAreaBox
                            .covers(MGC.coord2Point(coord));

                    return veryDetailedAreaBox
                            .covers(MGC.coord2Point(coord));

                })
                .setAfterLinkCreated((link, tags, direction) -> setAllowedMode(link, tags))
                .build()
                .read(osmnetwork);

        log.info("Finished parsing network. Start Network cleaner.");
        new MultimodalNetworkCleaner(network).run(Set.of(TransportMode.car));
        new MultimodalNetworkCleaner(network).run(Set.of(TransportMode.ride));
        new MultimodalNetworkCleaner(network).run(Set.of(TransportMode.bike));

        log.info("Finished cleaning network. Write network");
        new NetworkWriter(network).write(this.output + "/vulkaneifel-network.xml.gz");

        log.info("Finished CreateNetwork. Exiting.");
        return 0;
    }

    private static Geometry getGeometries(String path){

        //dilutionArea needs to be a single shape file
        log.info("reading in very detailed area file from " + path);

        if(path == null) {

            log.info("Path to geometry has not been initialized. Returning null...");
            return null;
        }

        return ShapeFileReader.getAllFeatures(path).stream()
                .map(simpleFeature -> (Geometry) simpleFeature.getDefaultGeometry())
                .findFirst()
                .get();
    }

    private Geometry getSemiDetailedAreaBox(Geometry veryDetailedAreaBox){

        Point center = veryDetailedAreaBox.getCentroid();

        Double highestDistance = Arrays.stream(veryDetailedAreaBox.getCoordinates())
                .map(coordinate -> Math.abs(center.getX() - MGC.coordinate2Point(coordinate).getX()))
                .reduce(Math::max)
                .get() * 4;

        var left = center.getX() - highestDistance;
        var right = center.getX() + highestDistance;
        var top = center.getY() + highestDistance;
        var bottom = center.getY() - highestDistance;

        var geometryPolygon = new GeometryFactory().createPolygon(new Coordinate[]{
                new Coordinate(left, top), new Coordinate(right, top), new Coordinate(right, bottom), new Coordinate(left, bottom), new Coordinate(left, top)});

        return new GeometryFactory().createGeometry(geometryPolygon);
    }

    private Geometry getVeryDetailedAreaBox(String pathToVeryDetailedArea) {

        Geometry dilutionArea = getGeometries(pathToVeryDetailedArea);
        
        return dilutionArea != null ? dilutionArea.getEnvelope() : null;
    }

    private void setAllowedMode(Link link, Map<String, String> tags) {

        if (isCarOnly(tags)) {
            link.setAllowedModes(carRide);
        } else {
            link.setAllowedModes(carRideBike);
        }
    }

    private boolean isCarOnly (Map<String, String> tags) {

        var highwayType = tags.get(OsmTags.HIGHWAY);
        return highwayType == null || highwayType.equals(OsmTags.MOTORWAY) || highwayType.equals(OsmTags.MOTORWAY_LINK) || highwayType.equals(OsmTags.TRUNK) || highwayType.equals(OsmTags.TRUNK_LINK);
    }
}
