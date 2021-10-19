WORKING_DIRECTORY := C:/Users/ACER/Desktop/Uni/Bachelorarbeit/MATSim/Erstellung-Vulkaneifel
VERSION := matsim-vulkaneifel-v1.0
OUTPUT_FOLDER := C:/Users/ACER/Desktop/Uni/Bachelorarbeit/MATSim/input
JAR := matsim-vulkaneifel-0.0.1-SNAPSHOT.jar

NETWORK := $(OUTPUT_FOLDER)/$(VERSION).network.xml.gz

# build the application before we can do anything with it
$(JAR):
	java --version
	mvn package

# create network and pt
$(NETWORK): $(JAR)
	java -Xmx5G -jar $(JAR)  prepare network --osmnetwork

	#java -Xmx5G -jar $(JAR) prepare transit-from-gtfs --network $(NETWORK)
	#	--date "2021-10-19" --name $(VERSION) --shp $(WORKING_DIRECTORY)/dilutionArea.shp

network: $(NETWORK)

prepare: network
	@echo "Done"