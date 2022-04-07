library(tidyverse)
library(readr)

filepath = "C:/Users/ACER/IdeaProjects/matsim-vulkaneifel/scenario/open-vulkaneifel-scenario/calibration/trips_filtered.csv"

trips.raw = read_csv2(filepath)

trips.inProgress = trips.raw %>%
  
  select(-...6) %>%
  
  mutate(tripID = row_number(),
         distanceBin = ifelse(traveled_distance < 1000, "< 1 km",
                              ifelse(traveled_distance < 2000, "1 bis 2 km",
                                     ifelse(traveled_distance < 5000, "2 bis 5 km",
                                            ifelse(traveled_distance < 10000, "5 bis 10 km", "> 10 km")))),
         distanceBin = factor(distanceBin, levels = c("< 1 km", "1 bis 2 km", "2 bis 5 km", "5 bis 10 km", "> 10 km"))) %>%
  
  transmute(tripID, personID, mainmode = mainMode, traveltime, distance = traveled_distance, distanceBin, legList) %>%
  
  filter(!is.na(mainmode))

trips.inProgress %>% group_by(mainmode)  %>%
  
  summarize(share = length(mainmode)/nrow(trips.inProgress)) %>%
  
  ggplot(aes(mainmode, share, fill = mainmode)) +
  
  geom_col(color = "black") +
  
  geom_text(aes(label = round(share, 2)), vjust = -0.5) +
  
  coord_cartesian(ylim = c(0,0.7)) +
  
  scale_y_continuous(breaks = seq(0,0.7, 0.1)) +
  
  theme_bw()

trips.inProgress %>% group_by(mainmode, distanceBin)  %>%
  
  summarize(share = length(mainmode)/nrow(trips.inProgress)) %>%
  
  ggplot(aes(distanceBin, share, fill = mainmode)) +
  
  geom_col(color = "black", position = position_dodge()) +
  
  coord_cartesian(ylim = c(0,0.4)) +
  
  scale_y_continuous(breaks = seq(0,0.7, 0.05)) +
  
  labs(fill = "Verkehrsmittel:", y = "Anteil am Modal Split", x = "Distanzgruppe") +
  
  theme_bw() +
  
  theme(legend.position = "bottom")
