/*
 * *********************************************************************** *
 * project: org.matsim.*
 * *********************************************************************** *
 *                                                                         *
 * copyright       : (C) 2022 by the members listed in the COPYING,        *
 *                   LICENSE and WARRANTY file.                            *
 * email           : info at matsim dot org                                *
 *                                                                         *
 * *********************************************************************** *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *   See also COPYING, LICENSE and WARRANTY file                           *
 *                                                                         *
 * *********************************************************************** *
 */

package drtSchoolTransportStudy.jsprit;

import com.google.common.base.Preconditions;
import com.graphhopper.jsprit.core.problem.Location;
import org.matsim.api.core.v01.Id;
import org.matsim.api.core.v01.IdMap;
import org.matsim.api.core.v01.network.Node;
import org.matsim.application.MATSimAppCommand;
import org.matsim.contrib.drt.run.MultiModeDrtConfigGroup;
import org.matsim.contrib.dvrp.fleet.FleetReader;
import org.matsim.contrib.dvrp.fleet.FleetSpecificationImpl;
import org.matsim.core.config.ConfigUtils;
import org.matsim.core.scenario.ScenarioUtils;
import picocli.CommandLine;

import java.util.Map;

/**
 * @author Michal Maciejewski (michalm)， modified by Chengqi Lu (luchengqi7)
 */
@CommandLine.Command(name = "run-jsprit", description = "run Jsprit scenario")
public class RunJsprit implements MATSimAppCommand {
	private final Map<Id<Node>, Location> locationByNodeId = new IdMap<>(Node.class);

	@CommandLine.Option(names = "--config", description = "path to config file", required = true)
	private String configPath;

	@CommandLine.Option(names = "--infinite-fleet", description = "path to config file", defaultValue = "true")
	private boolean infiniteFleetSize;

	@CommandLine.Option(names = "--write-optimization-progress", description = "path to config file", defaultValue = "false")
	private boolean printProgressStatistics;

	@Override
	public Integer call() throws Exception {
		runJsprit(configPath, infiniteFleetSize, printProgressStatistics);
		return 0;
	}

	public static void main(String[] args) {
		new RunJsprit().execute(args);
	}

	void runJsprit(String matsimConfig, boolean infiniteFleet, boolean printProgressStatistics) {
		var config = ConfigUtils.loadConfig(matsimConfig, new MultiModeDrtConfigGroup());
		var scenario = ScenarioUtils.loadScenario(config);
		var network = scenario.getNetwork();
		var population = scenario.getPopulation();

		Preconditions.checkState(MultiModeDrtConfigGroup.get(config).getModalElements().size() == 1);
		var drtCfg = MultiModeDrtConfigGroup.get(config).getModalElements().iterator().next();

		var fleetSpecification = new FleetSpecificationImpl();
		new FleetReader(fleetSpecification).parse(drtCfg.getVehiclesFileUrl(scenario.getConfig().getContext()));

		new PreplannedSchedulesCalculator(drtCfg, fleetSpecification, network, population,
				new PreplannedSchedulesCalculator.Options(infiniteFleet, printProgressStatistics, 200, true)).calculate();
	}
}
