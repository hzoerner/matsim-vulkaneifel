VERSION := nrw-sued-rlp-saar
SHP := NRW-Sued-Rlp-Saar.shp
OSM := network.osm.pbf
BIGBUS := big-bus-schedule
TRAIN := vulkaneifel-train
BUS := vulkaneifel-bus
BUS_EDIT := $(BUS)-edit
CONFIG := config_vulkaneifel-test.xml
OUTPUT := ../vulkaneifel-1.0

#create network from osm.pbf
Prepare-Runs_Make/temp/vulkaneifel-network.xml.gz:
	java -Djava.io.tmpdir=${TMPDIR} -Xmx80G -jar matsim-vulkaneifel-1.0-SNAPSHOT.jar prepare network\
		--output $(VERSION)/temp/\
		--osmnetwork osm/$(OSM)\
		--veryDetailedArea dilutionArea/dilutionArea.shp\
	
#create big bus schedule
Prepare-Runs_Make/temp/vulkaneifel-pt-big-bus-schedule.xml.gz: Prepare-Runs_Make/temp/vulkaneifel-pt-bus-schedule.xml.gz
	java -Djava.io.tmpdir=${TMPDIR} -Xmx80G -jar matsim-vulkaneifel-1.0-SNAPSHOT.jar prepare pt-from-gtfs\
		gtfs/bus-tram-subway-gtfs-2021-11-14t.zip\
		--network $(VERSION)/temp/vulkaneifel-network.xml.gz\
		--name $(BIGBUS)\
		--date 2021-11-21 2021-11-22\
		--target-crs EPSG:25832\
		--shp shp/$(SHP)\
		--output $(VERSION)/temp

#create small bus schedule
Prepare-Runs_Make/temp/vulkaneifel-pt-bus-schedule.xml.gz: Prepare-Runs_Make/temp/vulkaneifel-network.xml.gz
	java -Djava.io.tmpdir=${TMPDIR} -Xmx80G -jar matsim-vulkaneifel-1.0-SNAPSHOT.jar prepare pt-from-gtfs\
		gtfs/bus-tram-subway-gtfs-2021-11-14t.zip\
		--network $(VERSION)/temp/vulkaneifel-network.xml.gz\
		--name $(BUS)\
		--date 2021-11-21 2021-11-22\
		--target-crs EPSG:25832\
		--shp dilutionArea/dilutionArea.shp\
		--output $(VERSION)/temp

#remove sev line from small schedule
Prepare-Runs_Make/temp/vulkaneifel-pt-bus-schedule-without-SEV-line.xml.gz: Prepare-Runs_Make/temp/vulkaneifel-pt-regional-train-scheduletrain-schedule.xml.gz
	java -Xmx80G -jar matsim-vulkaneifel-1.0-SNAPSHOT.jar prepare remove-bus-line\
		--schedule $(VERSION)/temp/$(BUS)-transitSchedule.xml.gz\
		--name $(BUS_EDIT)\
		--output $(VERSION)/temp\
		--lineId SEV---1747
		
#create regional-train-schedule
Prepare-Runs_Make/temp/vulkaneifel-pt-regional-train-scheduletrain-schedule.xml.gz: Prepare-Runs_Make/temp/vulkaneifel-pt-big-bus-schedule.xml.gz
	java -Djava.io.tmpdir=${TMPDIR} -Xmx80G -jar matsim-vulkaneifel-1.0-SNAPSHOT.jar prepare create-train-line	\
		--network $(VERSION)/temp/$(BIGBUS)-network-with-pt.xml.gz	\
		--schedule $(VERSION)/temp/$(BIGBUS)-transitSchedule.xml.gz	\
		--vehicles $(VERSION)/temp/$(BIGBUS)-transitVehicles.xml.gz	\
		--shp dilutionArea/dilutionArea.shp\
		--shp-crs EPSG:25832\
        --target-crs EPSG:25832\
		--name vulkaneifel\
		--output $(VERSION)/temp

#create train schedule
Prepare-Runs_Make/temp/vulkaneifel-pt-train-schedule.xml.gz: Prepare-Runs_Make/temp/vulkaneifel-pt-bus-schedule-without-SEV-line.xml.gz
	java -Djava.io.tmpdir=${TMPDIR} -Xmx80G -jar matsim-vulkaneifel-1.0-SNAPSHOT.jar prepare pt-from-gtfs\
		gtfs/regio-s-train-gtfs-2021-11-14.zip\
        --network $(VERSION)/temp/vulkaneifel-network.xml.gz\
        --name $(TRAIN)\
        --date 2021-11-21 2021-11-22\
        --target-crs EPSG:25832\
        --shp shp/$(SHP)\
        --output $(VERSION)/temp

#merge bus and train schedule together
Prepare-Runs_Make/input/vulkaneifel-complete-schedule.xml.gz: Prepare-Runs_Make/temp/vulkaneifel-pt-train-schedule.xml.gz
	java -Djava.io.tmpdir=${TMPDIR} -Xmx80G -jar matsim-vulkaneifel-1.0-SNAPSHOT.jar prepare merge-transit-schedules\
        $(VERSION)/temp/$(TRAIN)-transitSchedule.xml.gz\
        $(VERSION)/temp/vulkaneifel-bus-edit-bus-schedule-without-SEV.xml.gz\
		$(VERSION)/temp/vulkaneifel-transitSchedule-only-regional-train.xml.gz\
		--vehicles $(VERSION)/temp/$(BUS)-transitVehicles.xml.gz\
        $(VERSION)/temp/$(TRAIN)-transitVehicles.xml.gz\
		$(VERSION)/temp/vulkaneifel-transitVehicles-only-regional-train.xml.gz\
		--name vulkaneifel\
		--network $(VERSION)/temp/vulkaneifel-network.xml.gz\
		--output $(OUTPUT)\

Prepare-Runs_Make/input/vulkaneifel-plans.xml.gz: Prepare-Runs_Make/input/vulkaneifel-complete-schedule.xml.gz
	java -jar matsim-vulkaneifel-1.0-SNAPSHOT.jar prepare trajectory-to-plans	\
	--name tmp	--sample-size 0.25	\
	--attributes population/personAttributes.xml.gz	\
	--population population/population.xml.gz	\
	--output $(VERSION)/temp	\
	--target-crs EPSG:25832\

#grid2coordinates
	java -Xmx10G -jar matsim-vulkaneifel-1.0-SNAPSHOT.jar prepare resolve-grid-coords	\
	$(VERSION)/temp/tmp-25pct.plans.xml.gz	\
	--grid-resolution 300	\
	--input-crs EPSG:25832	\
	--landuse landuse/landuse.shp	\
	--network $(VERSION)/vulkaneifel-network-with-pt.xml.gz	\
	--output $(VERSION)/temp/tmp-25pct.plans.xml.gz\

#clean population
	java -jar matsim-vulkaneifel-1.0-SNAPSHOT.jar prepare clean-population	\
	--plans $(VERSION)/temp/tmp-25pct.plans.xml.gz	\
	--remove-routes	\
	--remove-unselected-plans	\
	--remove-activity-location	\
	--trips-to-legs	\
	--output $(OUTPUT)/vulkaneifel-25pct-plans.xml.gz\

#downsampling
	java -jar matsim-vulkaneifel-1.0-SNAPSHOT.jar prepare downsample-population	\
	$(OUTPUT)/vulkaneifel-25pct-plans.xml.gz	\
	--sample-size 0.25	\
	--samples 0.1 0.01\

prepare: Prepare-Runs_Make/input/vulkaneifel-plans.xml.gz
	echo "Done, Have fun with your Vulkaneifel-Scenario ;)"

run: prepare
	java -Xmx80G -jar matsim-vulkaneifel-1.0-SNAPSHOT.jar run --config $(VERSION)/$(CONFIG) --config:controler.runId=$(VERSION) --output $(VERSION)/output/
	echo "Test-run finished!"