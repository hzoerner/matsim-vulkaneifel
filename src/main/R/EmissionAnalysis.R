library(tidyverse)
library(sf)

##Emission Analysis for Base Case and both Plan Cases

prepare_trips <- function(tripsFilePath, shapeFilePath){
  
  
  trips.raw = read.csv2(tripsFilePath)
  shape = st_read(shapeFilePath)
  
  trips.start = trips.raw %>%
    st_as_sf(coords = c("start_x", "start_y"), crs = 25832) %>%
    st_filter(shape)
  
  trips.end = trips.raw %>%
    st_as_sf(coords = c("end_x", "end_y"), crs = 25832) %>%
    st_filter(shape)
  
  trips.filtered = semi_join(as.data.frame(trips.start), 
                                       as.data.frame(trips.end), by = "trip_id")
  
  base.case.pkm = trips.filtered %>%
    
    select(person, trip_id, longest_distance_mode, traveled_distance) %>%
    
    rename("mode" = "longest_distance_mode") %>%
    
    mutate(mode = ifelse(mode == "ride", "car", mode)) %>%
    
    group_by(mode) %>%
    
    summarise(pkm = sum(traveled_distance) / 1000) %>%
    
    ungroup() %>%
    
    mutate(mode_ger = ifelse(mode == "car", "MIV",
                             ifelse(mode == "pt", "ÖPNV",
                                    ifelse(mode == "drt", "DRT", mode)))
    )
  
  base.case.pkm
}

merge_with_emissions <- function(trips, warmEmissionFilePath, coldEmissionFilePath){
  
  if(!is.data.frame(trips)) {
    print("TRIPS must be a data frame!!!")
    return(NULL)
  }
  
  if("mode_ger" %in% colnames(trips) == FALSE){
    
    print("TRIPS needs a col named 'mode_ger' with german translation for mode names pt = 'ÖPNV' e.g.")
  }
  
  emissions.raw = read.csv(file = warmEmissionFilePath)
  
  emissions.cold.raw = read.csv(file = coldEmissionFilePath)
  
  emissions.1 = emissions.raw %>%
    
    bind_rows(emissions.cold.raw) %>%
    
    select(vehicleId, CO, CO2_TOTAL, PM, NOx, NO2) %>%
    
    filter(!str_detect(vehicleId, "CustomTrain")) %>%
    
    mutate(mode = ifelse(str_detect(vehicleId, "car"), "MIV", 
                         ifelse(str_detect(vehicleId, "drt"), "DRT", "ÖPNV")))
  
  emissions.sum = emissions.1 %>%
    
    select(-vehicleId) %>%
    
    group_by(mode) %>%
    
    summarise_all(sum) %>%
    
    ungroup() %>%
    
    mutate_if(is.double, function(x){ x / 1000}) %>%
    
    left_join(trips, by = c("mode" = "mode_ger")) %>%
    
    select(- mode.y) %>%
    
    mutate(CO2_per_pkm = CO2_TOTAL / pkm)
  
  emissions.sum
}

SHAPEFILE = "C:/Users/ACER/IdeaProjects/matsim-vulkaneifel/scenario/open-vulkaneifel-scenario/vulkaneifel-v1.0-25pct/dilutionArea/dilutionArea.shp"

WARM_EMISSIONS_BASE_CASE = "C:/Users/ACER/IdeaProjects/matsim-vulkaneifel/output/study/baseCase_warm_emissions.csv"
COLD_EMISSIONS_BASE_CASE = "C:/Users/ACER/IdeaProjects/matsim-vulkaneifel/output/study/baseCase_cold_emissions.csv"
TRIPS_BASE_CASE = "C:/Users/ACER/IdeaProjects/matsim-vulkaneifel/scenario/open-vulkaneifel-scenario/vulkaneifel-v1.0-25pct/165/165.output_trips.csv.gz"

base.case.pkm = prepare_trips(tripsFilePath = TRIPS_BASE_CASE, shapeFilePath = SHAPEFILE)

base.case.sum = merge_with_emissions(trips = base.case.pkm, warmEmissionFilePath = WARM_EMISSIONS_BASE_CASE, coldEmissionFilePath = COLD_EMISSIONS_BASE_CASE)

########### PLAN CASE 1 ################
WARM_EMISSIONS_PLAN_CASE_1 = "C:/Users/ACER/IdeaProjects/matsim-vulkaneifel/output/study/plan-case-1_warm_emissions.csv"
COLD_EMISSIONS_PLAN_CASE_1 = "C:/Users/ACER/IdeaProjects/matsim-vulkaneifel/output/study/plan-case-1_cold_emissions.csv"
TRIPS_PLAN_CASE_1 = "C:/Users/ACER/IdeaProjects/matsim-vulkaneifel/output/study/fleet-size-60/fleet-size-60-plan-case-1.output_trips.csv.gz"

plan.case.1.pkm = prepare_trips(tripsFilePath = TRIPS_PLAN_CASE_1, shapeFilePath = SHAPEFILE)

emissions.1.sum = merge_with_emissions(trips = plan.case.1.pkm, warmEmissionFilePath = WARM_EMISSIONS_PLAN_CASE_1, coldEmissionFilePath = COLD_EMISSIONS_PLAN_CASE_1)

emissions.1.long = emissions.1.sum %>%
  
  select(-ends_with("pkm")) %>%
  
  pivot_longer(cols = -mode, names_to = "emission_type", values_to = "emission") %>%
  
  mutate(emission_type = factor(emission_type, levels = c("CO2_TOTAL", "CO", "NO2", "NOx", "PM")))

## Plot total emission for each emission category
ggplot(filter(emissions.1.long, mode != "MIV"), aes(mode, emission, fill = mode)) +
  
  geom_col() +
  
  facet_wrap(~ emission_type, scales = "free") +
  
  labs(x = "Verkehrsmittel", y = "Gesamtemisssionen in kg") +
  
  theme_bw() +
  
  theme(legend.position = "none")

pt.vs.drt = emissions.1.sum %>%
  
  filter(mode == "DRT") %>%
  
  bind_rows(base.case.sum)

ggplot(pt.vs.drt, aes(mode, CO2_per_pkm)) +
  
  geom_col() +
  
  labs(y = "CO2 / pkm in kg")