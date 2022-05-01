library(tidyverse)
library(readr)
library(plotly)
library(sf)
library(readxl)

# Script for analysing mid modal share and distance share and calculating difference
# between mid and matsim output trips data


####            PART 1: Prepare MiD-Data                                    ####


##function to save plots
save_plot_as_jpg <- function(plot, name){
  
  ggsave(
    filename = paste0("../Grafiken/",
                      Sys.Date(),"_",name, ".jpg"
    ),
    plot = plot,
    device = "jpg"
  )
  
  plot
}



## import raw data
mid.raw = read_excel("C:/Users/ACER/Desktop/Uni/Bachelorarbeit/Daten/Kalibrierung/modal_split_mid.xlsx", col_names = T)


#reduce excel to relevant data
mid = mid.raw[151:nrow(mid.raw), ]
colnames.mid = mid[5, ]

##Prepare Colnames
colnames.as.col = vector(mode = "character")

for(col in 1:ncol(colnames.mid)){
  
  name = colnames.mid[1, col]
  
  colnames.as.col[ length( colnames.as.col ) + 1] = ifelse(is.na(name), paste0("name", col), name)
}

colnames.as.col = unlist(colnames.as.col)
print(colnames.as.col)

colnames(mid) = colnames.as.col

miv.modes = c("Pkw (Mitfahrer)", "Pkw (Fahrer)", "Motorrad/Moped/Mofa", "Lkw")
pt.modes = c("ÖPNV", "ÖPFV", "Taxi")


##removing pattern
mid.tidy.1 = mid %>% 
  filter(!is.na(Fahrrad) & !is.na(name1)) %>%
  mutate_at(c("Basis gewichtet", "Basis ungewichtet"), str_replace_all, pattern = "[.]", replacement = "") %>%
  mutate_at(colnames.as.col, str_replace_all, pattern = "-", replacement = "0.00") %>%
  mutate_at(colnames.as.col[2:9], as.numeric) %>%
  rename("Type" = "name1") %>%
  mutate(ÖV = `ÖPNV (inkl. Taxi, anderes)` + ÖPFV,
          Total_1 = `MIV (Mitfahrer)` + `MIV (Fahrer)` + ÖV + `zu Fuß` + Fahrrad) %>%
  select(-c(ÖPFV, `ÖPNV (inkl. Taxi, anderes)`, `keine Angabe`)) %>%
  mutate(n_walk = `Basis gewichtet` * `zu Fuß`,
         n_bike = `Basis gewichtet` * Fahrrad,
         n_car = `Basis gewichtet` * `MIV (Fahrer)`,
         n_ride = `Basis gewichtet` * `MIV (Mitfahrer)`,
         n_pt = `Basis gewichtet` * ÖV,
         n_total = n_walk + n_bike + n_car + n_ride + n_pt,
         share_walk = n_walk / n_total,
         share_bike = n_bike / n_total,
         share_car = n_car / n_total,
         share_ride = n_ride / n_total,
         share_pt = n_pt / n_total)


##transpose data frame, because its easier than pivot_wider
temp = mid.tidy.1 %>% 
  select(starts_with("n_")) %>%
  t() %>%
  as.data.frame()


#set types as col names
colnames(temp) = mid.tidy.1$Type
rownames(temp)


#join distance groups
mid.distance.share.abs = temp %>%
  mutate("< 1 km" = `unter 0,5 km` + `0,5 bis unter 1 km`,
         "1 bis 5 km" = `1 bis unter 2 km` + `2 bis unter 5 km`,
         "10 bis 50 km" = `10 bis unter 20 km` + `20 bis unter 50 km`,
         "mainmode" = rownames(temp)
  ) %>%
  rename("> 100 km" = "100 km und mehr",
         "50 bis 100 km" = "50 bis unter 100 km",
         "5 bis 10 km" = "5 bis unter 10 km") %>%
  select(-c(`unter 0,5 km`, `0,5 bis unter 1 km`, `1 bis unter 2 km`, `2 bis unter 5 km`, `10 bis unter 20 km`, `20 bis unter 50 km`)) %>%
  pivot_longer(cols = -mainmode, names_to = "Distanzgruppe") %>%
  pivot_wider(names_from = "mainmode")


##calculate share in percentage, remove absolute values and round
mid.distance.share = mid.distance.share.abs %>%
  mutate(Distanzgruppe = factor(Distanzgruppe, levels = c("< 1 km", "1 bis 5 km", "5 bis 10 km", "10 bis 50 km", "50 bis 100 km", "> 100 km")),
         Fahrrad = n_bike / n_total,
         Fuß = n_walk / n_total,
         "MIV (Fahrer)" = n_car / n_total,
         "MIV (Mitfahrer)" = n_ride / n_total,
         ÖV = n_pt / n_total) %>%
  arrange(Distanzgruppe) %>%
  select(-starts_with("n_")) %>%
  mutate_if(is.double, funs(round(.,3)))


#calculate modal share over all distance groups
mid.modal.share = mid.tidy.1 %>%
  select(starts_with("n_")) %>%
  pivot_longer(cols = starts_with("n_"), names_to = "mode", values_to = "trips") %>%
  group_by(mode) %>%
  summarise(sum_trips = sum(trips)) %>%
  ungroup() %>%
  pivot_wider(names_from = "mode", values_from = "sum_trips") %>%
  mutate(Fahrrad = n_bike / n_total,
         Fuß = n_walk / n_total,
         "MIV (Fahrer)" = n_car / n_total,
         "MIV (Mitfahrer)" = n_ride / n_total,
         ÖV = n_pt / n_total) %>%
  mutate_all(round, 4) %>%
  mutate(Distanzgruppe = "Gesamt") %>%
  select(-starts_with("n_"))

#bind data frames to mid frame with all mode share informations
mid.modal.share = bind_rows(mid.modal.share, mid.distance.share)

rm(temp, colnames.mid, col, colnames.as.col, mid, mid.tidy.1, mid.distance.share, mid.raw, name, miv.modes, pt.modes)


##create data frame in long format for plotting
mid_plotting_abs = mid.distance.share.abs %>%
  rename(
    n_Fuß = n_walk,
    n_Fahrrad = n_bike,
    "n_MIV (Fahrer)" = "n_car",
    "n_MIV (Mitfahrer)" = "n_ride",
    n_ÖV = n_pt,
    n_Gesamt = n_total) %>%
  pivot_longer(cols = -Distanzgruppe, names_to = "Verkehrsmittel", names_prefix = "n_", values_to = "n_trips") %>%
  mutate(Distanzgruppe = factor(Distanzgruppe, levels = c("< 1 km", "1 bis 5 km", "5 bis 10 km", "10 bis 50 km", "50 bis 100 km", "> 100 km", "Gesamt")),
         Verkehrsmittel = factor(Verkehrsmittel, levels = c("Fuß", "Fahrrad", "ÖV", "MIV (Fahrer)", "MIV (Mitfahrer)", "Gesamt")),
         Type = "MiD") %>%
  arrange(Distanzgruppe)


##same for share values
mid_plotting = mid.modal.share %>%
  pivot_longer(cols = -Distanzgruppe, names_to = "Verkehrsmittel", values_to = "share") %>%
  mutate(Distanzgruppe = factor(Distanzgruppe, levels = c("< 1 km", "1 bis 5 km", "5 bis 10 km", "10 bis 50 km", "50 bis 100 km", "> 100 km", "Gesamt")),
         Verkehrsmittel = factor(Verkehrsmittel, levels = c("Fuß", "Fahrrad", "ÖV", "MIV (Fahrer)", "MIV (Mitfahrer)")),
         Type = "MiD") %>%
  arrange(Distanzgruppe)


##finally some plots
plt.distance.share = ggplot(filter(mid_plotting, Distanzgruppe != "Gesamt"), 
                            aes(Distanzgruppe, share, fill = Verkehrsmittel)) +
  
  geom_col(position = "dodge", col = "black") +
  
  scale_y_continuous(breaks = seq(0,0.7,0.1)) +
  
  labs(y = "Modal Split - Mobilität in Deutschland", fill = "") +
  
  theme_bw() +
  
  theme(legend.position = "bottom")

plt.mid.modal.share = mid.modal.share %>% 
  
  filter(Distanzgruppe == "Gesamt") %>%
  
  select(-Distanzgruppe) %>%
  
  pivot_longer(cols = everything(), names_to = "Verkehrsmittel", values_to = "Anteil") %>%
  
  mutate(Verkehrsmittel = factor(Verkehrsmittel, levels = c("Fuß", "Fahrrad", "ÖV", "MIV (Fahrer)", "MIV (Mitfahrer)", "Gesamt"))) %>%
  
  ggplot(aes(Verkehrsmittel, Anteil, fill = Verkehrsmittel)) +
  
  geom_col(position = "dodge", col = "black") +
  
  geom_text(aes(label = round(Anteil, 2)), vjust = -0.5) +
  
  coord_cartesian(ylim = c(0,0.7)) +
  
  scale_y_continuous(breaks = seq(0,0.8,0.1)) +
  
  labs(y = "Modal Split - Mobilität in Deutschland", fill = "") +
  
  theme_bw() +
  
  theme(legend.position = "bottom")

save_plot_as_jpg(plt.mid.modal.share, "MiD_Modal_Share")
save_plot_as_jpg(plt.distance.share, "MiD_Distance_Share")




####            PART 2: Prepare MATSim-Data                                 ####



##Parameters for file paths
sampleSize = "25"
runId = "075"

##define distance bins
levels = c("< 1 km", "1 bis 5 km", "5 bis 10 km", "10 bis 50 km", "50 bis 100 km","> 100 km")
breaks = c(0, 1000, 5000, 10000, 50000, 100000, Inf)


## import data
TRIPS = paste0("C:/Users/ACER/IdeaProjects/matsim-vulkaneifel/scenario/open-vulkaneifel-scenario/calibration/", sampleSize, "pct/", runId,".output_trips.csv.gz")
SHP = "C:/Users/ACER/IdeaProjects/matsim-vulkaneifel/scenario/open-vulkaneifel-scenario/vulkaneifel-0.1-25pct/dilutionArea/dilutionArea.shp"
PERSONS =paste0("C:/Users/ACER/IdeaProjects/matsim-vulkaneifel/scenario/open-vulkaneifel-scenario/calibration/", sampleSize, "pct/", runId,".output_persons.csv.gz")

trips_raw = read_delim(TRIPS, delim = ";", trim_ws = T, col_types = cols(person = col_character()))
shape = st_read(SHP)

persons <- read_delim(PERSONS, delim = ";", trim_ws = T, col_types = cols(person = col_character(),good_type = col_integer())) %>%
  st_as_sf(coords = c("first_act_x", "first_act_y"), crs = 25832) %>%
  st_filter(shape)


##clean and prepare trips
trips_inProgress = trips_raw %>%
  semi_join(persons) %>%
  mutate(distanceBin = cut(traveled_distance, breaks = breaks, labels = levels),
         distanceBin = factor(distanceBin, levels = c("< 1 km", "1 bis 5 km", "5 bis 10 km", "10 bis 50 km", "50 bis 100 km", "> 100 km")),
         mainmode = ifelse(longest_distance_mode == "car", "MIV (Fahrer)",
                           ifelse(longest_distance_mode == "ride", "MIV (Mitfahrer)",
                                  ifelse(longest_distance_mode == "pt", "ÖV",
                                         ifelse(longest_distance_mode == "bike", "Fahrrad",
                                                ifelse(longest_distance_mode == "walk", "Fuß", "unknown"))))),
         mainmode_fct = factor(mainmode, levels = c("Fuß", "Fahrrad", "ÖV", "MIV (Fahrer)", "MIV (Mitfahrer)"))
         ) %>%
  transmute(trip_id, person, Verkehrsmittel = mainmode_fct, traveltime = trav_time, distance = traveled_distance, Distanzgruppe = distanceBin) %>%
  filter(!is.na(Verkehrsmittel)) %>%
  filter(!is.na(Distanzgruppe))

rm(trips.raw, persons)

##Modal Split ##

modal_share = trips_inProgress %>%
  group_by(Verkehrsmittel) %>%
  summarize(share = n() / nrow(trips.inProgress)) %>%
  ungroup()

##Plot modal share
plt.modal.share = ggplot(modal_share, aes(Verkehrsmittel, share, fill = Verkehrsmittel)) +
  
  geom_col(color = "black") +
  
  geom_text(aes(label = round(share, 2)), vjust = -0.5) +
  
  coord_cartesian(ylim = c(0,0.7)) +
  
  labs(y = "Modal Split - Open Vulkaneifel Scenario") +
  
  scale_y_continuous(breaks = seq(0,0.7, 0.1)) +
  
  theme_bw() +
  
  theme(legend.position = "bottom")

plt.modal.share
#save_plot_as_jpg(plt.modal.share, "MATSim_modal_share")

## Distance Share
sim_distance_share_abs = trips_inProgress %>%
  group_by(Verkehrsmittel, Distanzgruppe) %>%
  summarize(n = n()) %>% ungroup() %>%
  group_by(Distanzgruppe) %>%
  mutate(total = sum(n)) %>%  ungroup() %>%
  group_by(Verkehrsmittel, Distanzgruppe)


## compare trips number per distance group
{
sim.sum = sim.distance.share.abs %>%
  
  group_by(Distanzgruppe) %>%
  
  summarise(n = sum(n))

mid.sum = mid_plotting_abs %>%
  
  group_by(Distanzgruppe) %>%
  
  summarise(n = sum(n_trips))

total.sim = sum(sim.sum$n)
total.mid = sum(mid.sum$n)
q = total.sim / total.mid

mid.sum = mutate(mid.sum, n = q * n, Type = "MiD")
sim.sum = mutate(sim.sum, Type = "Sim")

n.diff = bind_rows(sim.sum, mid.sum) %>%
  
  pivot_wider(names_from = "Type", values_from = c("n")) %>%
  
  replace_na(list(
    Sim = 0
  )) %>%
  
  mutate(Differenz = Sim - MiD)

plt.diff = ggplot(n.diff, aes(Distanzgruppe, Differenz, fill = Distanzgruppe)) +
  
  geom_col(color = "black", width = 0.7) +
  
  scale_y_continuous(breaks = seq(-5000,5000,1000)) +
  
  coord_flip() +
  
  labs(y = "Sim - MiD") +
  
  theme_bw() +
  
  theme(legend.position = "bottom")

save_plot_as_jpg(plt.diff, "trips_diff")

rm(n.diff, sim.sum, mid.sum, plt.diff)
}

distance_share = sim_distance_share_abs %>%
  summarise(share = n / total) %>% ungroup() %>%
  mutate(Type = "Sim")


##Plot distance share

plt.sim.distance.share = ggplot(distance.share, aes(Distanzgruppe, share, fill = Verkehrsmittel)) +
  
  geom_col(color = "black", position = position_dodge()) +
  
  scale_y_continuous(breaks = seq(0,1, 0.1)) +
  
  labs(fill = "Verkehrsmittel:", y = "Anteil am Modal Split", x = "Distanzgruppe") +
  
  theme_bw() +
  
  theme(legend.position = "bottom")

plt.sim.distance.share
save_plot_as_jpg(plt.sim.distance.share, "MATSim_distance_share")

rm(trips.inProgress, plt.sim.distance.share)



####            PART 3: Analysis differences                                 ####



##calculate difference between mid and sim distance share
distance_share_abs_vs = filter(mid_plotting_abs, Distanzgruppe != "Gesamt") %>%
  bind_rows(sim_distance_share_abs) %>%
  pivot_wider(names_from = "Type", values_from = c("share")) %>%
  replace_na(list(  Sim = 0  )) %>%
  mutate(Differenz = Sim - MiD)

distance_share_vs = filter(mid_plotting, Distanzgruppe != "Gesamt") %>%
  bind_rows(distance_share) %>%
  pivot_wider(names_from = "Type", values_from = c("share")) %>%
  replace_na(list(
    Sim = 0
  )) %>%
  mutate(Differenz = Sim - MiD)

score = sum( distance_share_vs$Differenz^2 )

#write_csv(distance_share_vs, file = paste0("C:/Users/ACER/Desktop/Uni/Bachelorarbeit/Daten/Kalibrierung/",
#                                           sampleSize, "pct/", sampleSize, "_run_", runId, "_score_",
#                                           round(score, 2), "_distance_share.csv"))


## plot difference
plt.diff = ggplot(distance_share_vs, aes(Distanzgruppe, Differenz, fill = Verkehrsmittel)) +
  
  geom_col(color = "black", position = position_dodge()) +
  
  geom_hline(aes(yintercept = 0.02), linetype = "dashed") +
  
  geom_hline(aes(yintercept = -0.02), linetype = "dashed") +
  
  scale_y_continuous(breaks = seq(-0.5,0.5,0.1)) + 
  
  labs(y = "Differenz: Simulation - MiD") +
  
  coord_flip() +
  
  theme_bw() +
  
  theme(legend.position = "bottom")

plt.diff
#save_plot_as_jpg(plt.diff, "distance_share_diff")

#ggplotly(plt.diff)
