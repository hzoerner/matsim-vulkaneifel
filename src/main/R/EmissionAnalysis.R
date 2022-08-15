library(tidyverse)
library(sf)

options(scipen=999)

##Emission Analysis for Base Case and both Plan Cases

prepare_legs <- function(legsFilePath, shapeFilePath, vehicleKilometerFilePath, n){
  
  trips.raw = read.csv2(legsFilePath)
  shape = st_read(shapeFilePath)
  
  count = trips.raw %>%
    count(mode) %>%
    pivot_wider(names_from = mode, values_from = n)
  
  print(1 + count$ride / count$car)
  
  trips.start = trips.raw %>%
    st_as_sf(coords = c("start_x", "start_y"), crs = 25832) %>%
    st_filter(shape) %>%
    group_by(trip_id) %>%
    mutate(leg_id = paste0(trip_id, row_number())) %>%
    ungroup()
  
  trips.end = trips.raw %>%
    st_as_sf(coords = c("end_x", "end_y"), crs = 25832) %>%
    st_filter(shape) %>%
    group_by(trip_id) %>%
    mutate(leg_id = paste0(trip_id, row_number())) %>%
    ungroup()
  
  trips.filtered = semi_join(as.data.frame(trips.start), 
                                       as.data.frame(trips.end), by = "leg_id")
  
  vehicle.km = read.csv(vehicleKilometerFilePath) %>%
    filter(str_detect(vehicleId, "car"))
  
  total = sum(vehicle.km$drivenDistance_km)
  miv.pkm = total * n
  
  base.case.pkm = trips.filtered %>%
    
    select(person, leg_id, mode, distance, vehicle_id) %>%
    
    mutate(mode = ifelse(mode == "ride", "car", mode),
           mode = ifelse(mode == "pt", ifelse(str_detect(vehicle_id, "Custom"), "pt_train", "pt_bus"), mode)) %>%
    
    group_by(mode) %>%
    
    summarise(pkm = sum(distance) / 1000) %>%
    
    ungroup() %>%
    
    mutate(mode_ger = ifelse(mode == "car", "MIV",
                             ifelse(mode == "pt_bus", "ÖPNV_Bus",
                                  ifelse(mode == "pt_train", "ÖPNV_Zug",
                                    ifelse(mode == "drt", "DRT", mode))))
    )
  
  entry = data.frame(
    mode = c("car_2"),
    pkm = c(miv.pkm),
    mode_ger = c("MIV")
  )
  
  bind_rows(base.case.pkm, entry)
}

merge_with_emissions <- function(legs, warmEmissionFilePath, coldEmissionFilePath){
  
  if(!is.data.frame(legs)) {
    print("TRIPS must be a data frame!!!")
    return(NULL)
  }
  
  if("mode_ger" %in% colnames(legs) == FALSE){
    
    print("TRIPS needs a col named 'mode_ger' with german translation for mode names pt = 'ÖPNV' e.g.")
  }
  
  emissions.raw = read.csv(file = warmEmissionFilePath)
  
  emissions.cold.raw = read.csv(file = coldEmissionFilePath)
  
  emissions.1 = emissions.raw %>%
    
    bind_rows(emissions.cold.raw) %>%
    
    select(vehicleId, CO, CO2_TOTAL, PM, NOx, NO2) %>%
    
    mutate(mode = ifelse(str_detect(vehicleId, "car"), "MIV", 
                         ifelse(str_detect(vehicleId, "drt"), "DRT", 
                                ifelse(str_detect(vehicleId, "pt"), "ÖPNV_Bus", "ÖPNV_Zug"))))
  
  emissions.sum = emissions.1 %>%
    
    select(-vehicleId) %>%
    
    group_by(mode) %>%
    
    summarise_all(sum) %>%
    
    ungroup() %>%
    
    mutate_if(is.double, function(x){ x / 1000}) %>%
    
    left_join(legs, by = c("mode" = "mode_ger")) %>%
    
    select(- mode.y) %>%
    
    mutate(CO2_per_pkm = CO2_TOTAL / pkm)
  
  emissions.sum
}

SHAPEFILE = "C:/Users/ACER/IdeaProjects/matsim-vulkaneifel/scenario/open-vulkaneifel-scenario/vulkaneifel-v1.0-25pct/dilutionArea/dilutionArea.shp"

WARM_EMISSIONS_BASE_CASE = "C:/Users/ACER/IdeaProjects/matsim-vulkaneifel/output/study/baseCase_warm_emissions.csv"
COLD_EMISSIONS_BASE_CASE = "C:/Users/ACER/IdeaProjects/matsim-vulkaneifel/output/study/baseCase_cold_emissions.csv"
LEGS_BASE_CASE = "C:/Users/ACER/IdeaProjects/matsim-vulkaneifel/scenario/open-vulkaneifel-scenario/vulkaneifel-v1.0-25pct/165/165.output_legs.csv.gz"
VEHICLE_KM_BASE_CASE = "C:/Users/ACER/IdeaProjects/matsim-vulkaneifel/output/study/baseCase-vehicle_kilometers.csv"

base.case.pkm = prepare_legs(legsFilePath = LEGS_BASE_CASE, shapeFilePath = SHAPEFILE, vehicleKilometerFilePath = VEHICLE_KM_BASE_CASE, n = 1.25)

base.case.sum = merge_with_emissions(legs = base.case.pkm, warmEmissionFilePath = WARM_EMISSIONS_BASE_CASE, coldEmissionFilePath = COLD_EMISSIONS_BASE_CASE)

base.case.sum.long = base.case.sum %>%
  
  select(-CO2_per_pkm) %>%
  
  pivot_longer(cols = -c(mode, pkm), names_to = "emission_type", values_to = "emission") %>%
  
  mutate(emission_type = factor(emission_type, levels = c("CO2_TOTAL", "CO", "NO2", "NOx", "PM")),
         emission_pkm_g = emission * 1000 / pkm)

base.case.sum.long %>%
  
  select(-pkm) %>%
  
  mutate(
    'Gesamtemissionen in Kilogramm pro Tag' = round(emission, 4),
    'Emissionen in Gramm je Personenkilometer' = round(emission_pkm_g, 4),
    ) %>%
  
  select(-c(emission, emission_pkm_g)) %>%
  
  write.csv2(file = "C:/Users/ACER/Desktop/Uni/Bachelorarbeit/Daten/Emissionen_Nullfall.csv", row.names = F)

plt.base.case.emissions = ggplot(base.case.sum.long, aes(x = mode, y = emission_pkm_g, fill = mode)) +

  geom_col() +
  
  facet_wrap(~ emission_type, scales = "free") +
  
  labs(x = "Verkehrsmittel", y = "Gesamtemisssionen in Gramm pro Kilometer") +
  
  theme_bw() +
  
  theme(legend.position = "none")
  
########### PLAN CASE 1 ################

WARM_EMISSIONS_PLAN_CASE_1 = "C:/Users/ACER/IdeaProjects/matsim-vulkaneifel/output/study/plan-case-1_warm_emissions.csv"
COLD_EMISSIONS_PLAN_CASE_1 = "C:/Users/ACER/IdeaProjects/matsim-vulkaneifel/output/study/plan-case-1_cold_emissions.csv"
LEGS_PLAN_CASE_1 = "C:/Users/ACER/IdeaProjects/matsim-vulkaneifel/output/study/fleet-size-60/fleet-size-60-plan-case-1.output_legs.csv.gz"

plan.case.1.pkm = prepare_legs(legsFilePath = LEGS_PLAN_CASE_1, shapeFilePath = SHAPEFILE)

emissions.1.sum = merge_with_emissions(legs = plan.case.1.pkm, warmEmissionFilePath = WARM_EMISSIONS_PLAN_CASE_1, coldEmissionFilePath = COLD_EMISSIONS_PLAN_CASE_1)

emissions.1.long = emissions.1.sum %>%
  
  select(-CO2_per_pkm) %>%
  
  pivot_longer(cols = -c(mode, pkm), names_to = "emission_type", values_to = "emission") %>%
  
  mutate(emission_type = factor(emission_type, levels = c("CO2_TOTAL", "CO", "NO2", "NOx", "PM")),
         emission_pkm_g = emission * 1000 / pkm)

## Plot total emission for each emission category
ggplot(filter(emissions.1.long, mode != "MIV"), aes(mode, emission, fill = mode)) +
  
  geom_col() +
  
  facet_wrap(~ emission_type, scales = "free") +
  
  labs(x = "Verkehrsmittel", y = "Gesamtemisssionen in kg") +
  
  theme_bw() +
  
  theme(legend.position = "none")

ggsave(filename = "C:/Users/ACER/Desktop/Uni/Bachelorarbeit/Grafiken/Planfall_1_Gesamtemissionen.jpg")

pt.vs.drt = emissions.1.sum %>%
  
  filter(mode == "DRT") %>%
  
  bind_rows(base.case.sum)

ggplot(pt.vs.drt, aes(mode, CO2_per_pkm)) +
  
  geom_col() +
  
  labs(y = "CO2 / pkm in kg")

pt.vs.drt.ext = pt.vs.drt %>%
  
  select(-CO2_per_pkm) %>%
  
  pivot_longer(cols = -c(mode, pkm), names_to = "emission_type", values_to = "emission") %>%
  
  mutate(emission_type = factor(emission_type, levels = c("CO2_TOTAL", "CO", "NO2", "NOx", "PM")),
         emission_pkm_g = emission * 1000 / pkm)

pt.vs.drt.wide = pt.vs.drt.ext %>%
  
  select(-pkm) %>%
  
  pivot_wider(names_from = "mode", values_from = c(emission, emission_pkm_g)) %>%
  
  mutate(emission_diff = emission_DRT / emission_ÖPNV)

## Plot emission per pkm for each emission category
ggplot(pt.vs.drt.ext, aes(mode, emission_pkm_g, fill = mode)) +
  
  geom_col() +
  
  facet_wrap(~ emission_type, scales = "free") +
  
  labs(x = "Verkehrsmittel", y = "CO2 / pkm in kg") +
  
  theme_bw() +
  
  theme(legend.position = "none")

########### PLAN CASE 2 ################
WARM_EMISSIONS_PLAN_CASE_2 = "C:/Users/ACER/IdeaProjects/matsim-vulkaneifel/output/study/plan-case-2_warm_emissions.csv"
COLD_EMISSIONS_PLAN_CASE_2 = "C:/Users/ACER/IdeaProjects/matsim-vulkaneifel/output/study/plan-case-2_cold_emissions.csv"
LEGS_PLAN_CASE_2 = "C:/Users/ACER/IdeaProjects/matsim-vulkaneifel/output/study/fleet-size-400/fleet-size-400-plan-case-2.output_legs.csv.gz"

sumEmissionsByType <- function(emissions, case = c("Nullfall", "Planfall 1", "Planfall 2")){
  
  if(!is.data.frame(emissions)) {
    warning("Argument emissions must be a data frame")
    return(NULL)}
  
  emissions %>%
    
    group_by(emission_type) %>%
    
    summarise(emission = sum(emission)) %>%
    
    ungroup() %>%
    
    mutate(case = case)
}

compareTotalEmissions <- function(emissions0, emissions1, emissions2){
  
  sum0 = sumEmissionsByType(emissions = emissions0, case = "Nullfall")
  sum1 = sumEmissionsByType(emissions = emissions1, case = "Planfall 1")
  sum2 = sumEmissionsByType(emissions = emissions2, case = "Planfall 2")
  
  bind_rows(sum0, sum1, sum2,) %>%
    
    select(case, emission_type, emission)
}

plan.case.2.pkm = prepare_legs(legsFilePath = LEGS_PLAN_CASE_2, shapeFilePath = SHAPEFILE)

emissions.2.sum = merge_with_emissions(legs = plan.case.2.pkm, warmEmissionFilePath = WARM_EMISSIONS_PLAN_CASE_2, coldEmissionFilePath = COLD_EMISSIONS_PLAN_CASE_2)

emissions.2.long = emissions.2.sum %>%
  
  select(-CO2_per_pkm) %>%
  
  pivot_longer(cols = -c(mode, pkm), names_to = "emission_type", values_to = "emission") %>%
  
  mutate(emission_type = factor(emission_type, levels = c("CO2_TOTAL", "CO", "NO2", "NOx", "PM")),
         emission_pkm_g = emission * 1000 / pkm)

## Plot total emission for each emission category
ggplot(filter(emissions.2.long, mode != "MIV"), aes(mode, emission, fill = mode)) +
  
  geom_col() +
  
  facet_wrap(~ emission_type, scales = "free") +
  
  labs(x = "Verkehrsmittel", y = "Gesamtemisssionen in kg") +
  
  theme_bw() +
  
  theme(legend.position = "none")

ggsave(filename = "C:/Users/ACER/Desktop/Uni/Bachelorarbeit/Grafiken/Planfall_2_Gesamtemissionen.jpg")

pt.vs.drt.2 = emissions.2.sum %>%
  
  filter(mode == "DRT") %>%
  
  bind_rows(base.case.sum)

ggplot(pt.vs.drt.2, aes(mode, CO2_per_pkm)) +
  
  geom_col() +
  
  labs(y = "CO2 / pkm in kg")

pt.vs.drt.ext.2 = pt.vs.drt.2 %>%
  
  select(-CO2_per_pkm) %>%
  
  pivot_longer(cols = -c(mode, pkm), names_to = "emission_type", values_to = "emission") %>%
  
  mutate(emission_type = factor(emission_type, levels = c("CO2_TOTAL", "CO", "NO2", "NOx", "PM")),
         emission_pkm_g = emission * 1000 / pkm)

pt.vs.drt.wide = pt.vs.drt.ext.2 %>%
  
  select(-pkm) %>%
  
  pivot_wider(names_from = "mode", values_from = c(emission, emission_pkm_g)) %>%
  
  mutate(emission_diff = emission_DRT / emission_ÖPNV)

ggplot(filter(pt.vs.drt.ext.2, mode != "MIV"), aes(mode, emission_pkm_g, fill = mode)) +
  
  geom_col() +
  
  facet_wrap(~ emission_type, scales = "free") +
  
  labs(x = "Verkehrsmittel", y = "Emissionen in Gramm je Personenkilometer") +
  
  theme_bw() +
  
  theme(legend.position = "none")

##Summary of total emissions drt 2 vs base case

total.emission.vs = compareTotalEmissions(emissions0 = base.case.sum.long, emissions1 = filter(emissions.1.long ,mode != "ÖPNV"), emissions2 = filter(emissions.2.long, mode != "ÖPNV"))
write.csv2(total.emission.vs, file = "C:/Users/ACER/Desktop/Uni/Bachelorarbeit/Daten/Gesamtemissionen_Überblick.csv")

ggplot(total.emission.vs, aes(case, emission, fill = case)) +
  
  geom_col() +
  
  facet_wrap(~ emission_type, scales = "free") +
  
  labs(x = "Verkehrsmittel", y = "Gesamtemissionen") +
  
  theme_bw() +
  
  theme(legend.position = "none")
