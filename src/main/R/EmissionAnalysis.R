library(tidyverse)

##Emission Analysis for Base Case and both Plan Cases

EMISSIONS_BASE_CASE = "C:/Users/ACER/IdeaProjects/matsim-vulkaneifel/output/study/baseCase_warm_emissions.csv"
base.case.raw = read.csv(file = EMISSIONS_BASE_CASE)

base.case.1 = base.case.raw %>%
  
  select(vehicleId, CO, CO2_TOTAL, PM, NOx, NO2) %>%
  
  filter(!str_detect(vehicleId, "CustomTrain")) %>%
  
  mutate(mode = ifelse(str_detect(vehicleId, "car"), "MIV", "ÖPNV"))

base.case.sum = base.case.1 %>%
  
  select(-vehicleId) %>%
  
  group_by(mode) %>%
  
  summarise_all(sum) %>%
  
  ungroup() %>%
  
  mutate_if(is.double, function(x){ x / 1000})

rm(base.case.raw)

EMISSIONS_PLAN_CASE_1 = "C:/Users/ACER/IdeaProjects/matsim-vulkaneifel/output/study/plan-case-1_warm_emissions.csv"
plan.case.1.raw = read.csv(file = EMISSIONS_PLAN_CASE_1)

plan.case.1.1 = plan.case.1.raw %>%
  
  select(vehicleId, CO, CO2_TOTAL, PM, NOx, NO2) %>%
  
  filter(!str_detect(vehicleId, "CustomTrain")) %>%
  
  mutate(mode = ifelse(str_detect(vehicleId, "car"), "MIV",
                       ifelse(str_detect(vehicleId, "pt"), "ÖPNV", "DRT")))

plan.case.1.sum = plan.case.1.1 %>%
  
  select(-vehicleId) %>%
  
  group_by(mode) %>%
  
  summarise_all(sum) %>%
  
  ungroup() %>%
  
  mutate_if(is.double, function(x){ x / 1000}) %>%
  
  pivot_longer(cols = -mode, names_to = "emission_type", values_to = "emission") %>%
  
  mutate(emission_type = factor(emission_type, levels = c("CO2_TOTAL", "CO", "NO2", "NOx", "PM")))

## Plot total emission for each emission category
ggplot(filter(plan.case.1.sum, mode != "MIV"), aes(mode, emission, fill = mode)) +
  
  geom_col() +
  
  facet_wrap(~ emission_type, scales = "free") +
  
  labs(x = "Verkehrsmittel", y = "Gesamtemisssionen in kg") +
  
  theme_bw() +
  
  theme(legend.position = "none")

rm(plan.case.1.raw)
