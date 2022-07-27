package prepare;

import org.matsim.api.core.v01.Id;
import org.matsim.application.MATSimAppCommand;
import org.matsim.core.config.Config;
import org.matsim.core.config.ConfigUtils;
import org.matsim.core.scenario.ScenarioUtils;
import org.matsim.pt.transitSchedule.api.TransitLine;
import org.matsim.pt.transitSchedule.api.TransitSchedule;
import org.matsim.pt.transitSchedule.api.TransitScheduleWriter;
import picocli.CommandLine;

@CommandLine.Command(
        name = "remove-bus-line",
        description = "Remove SEV bus line from small bus schedule",
        showDefaultValues = true,
        mixinStandardHelpOptions = true
)

public class RemoveBusLineFromSmallSchedule implements MATSimAppCommand {

    @CommandLine.Option(names = "--schedule", description = "input matsim transit bus schedule from dilution area", required = true)
    private String schedule;

    @CommandLine.Option(names = "--name", description = "name of the scenario", required = true)
    private String name;

    @CommandLine.Option(names = "--output", description = "output directory", required = true)
    private String output;

    @CommandLine.Option(names = "--lineId", description = "Id of SEV line", required = true)
    private String lineId;

    @Override
    public Integer call() throws Exception {

        Config config = ConfigUtils.createConfig();
        config.transit().setTransitScheduleFile(schedule);

        TransitSchedule transitSchedule = ScenarioUtils.loadScenario(config).getTransitSchedule();

        TransitLine sevLine = transitSchedule.getTransitLines().get(Id.create(lineId, TransitLine.class));

        transitSchedule.removeTransitLine(sevLine);

        new TransitScheduleWriter(transitSchedule).writeFile(output + "/" + name + "-bus-schedule-without-SEV.xml.gz" );

        return 0;
    }
}
