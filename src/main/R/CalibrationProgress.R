library(tidyverse)
library(readr)
library(plotly)
library(sf)
library(readxl)




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


####            PART 2: Import and Cleaning                                 ####


##Parameters for file paths
sampleSize = "10"
runId = "197"

##define distance bins
levels = c("< 1 km", "1 bis 5 km", "5 bis 10 km", "10 bis 50 km", "50 bis 100 km","> 100 km")
breaks = c(0, 1000, 5000, 10000, 50000, 100000, Inf)

TRIPS = paste0("C:/Users/ACER/IdeaProjects/matsim-vulkaneifel/scenario/open-vulkaneifel-scenario/calibration/", sampleSize, "pct/")
SHP = "C:/Users/ACER/IdeaProjects/matsim-vulkaneifel/scenario/open-vulkaneifel-scenario/vulkaneifel-0.1-25pct/dilutionArea/dilutionArea.shp"
PERSONS =paste0("C:/Users/ACER/IdeaProjects/matsim-vulkaneifel/scenario/open-vulkaneifel-scenario/calibration/", sampleSize, "pct/", runId,".output_persons.csv.gz")

shape = st_read(SHP)

persons <- read_delim(PERSONS, delim = ";", trim_ws = T, col_types = cols(person = col_character(),good_type = col_integer())) %>%
  st_as_sf(coords = c("first_act_x", "first_act_y"), crs = 25832) %>%
  st_filter(shape)

TRIPS.FILES = list.files(TRIPS, pattern = "output_trips.csv.gz")

progress_frame = data.frame(
  runId = c(),
  Verkehrsmittel = c(),
  Differenz = c()
)

for(file in TRIPS.FILES){
  
  print(paste0("Reading in trips file: ", file))
  
  runId = as.numeric(str_split(file, pattern = "[.]")[[1]][1])
  
  trips_raw = read_delim(paste0(TRIPS, file), delim = ";", trim_ws = T, col_types = cols(person = col_character()))
  
  
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
  
  #calculate distance share in simulation
  sim_distance_share_abs = trips_inProgress %>%
    group_by(Verkehrsmittel, Distanzgruppe) %>%
    summarize(n = n()) %>% ungroup() %>%
    group_by(Distanzgruppe) %>%
    mutate(total = sum(n)) %>%  ungroup() %>%
    group_by(Verkehrsmittel, Distanzgruppe)
  
  distance_share = sim_distance_share_abs %>%
    summarise(share = n / total) %>% ungroup() %>%
    mutate(Type = "Sim")
  
  
  distance_share_vs = filter(mid_plotting, Distanzgruppe != "Gesamt") %>%
    bind_rows(distance_share) %>%
    pivot_wider(names_from = "Type", values_from = c("share")) %>%
    replace_na(list(
      Sim = 0
    )) %>%
    mutate(Differenz = Sim - MiD)
  
  difference_temp = distance_share_vs %>%
    mutate(runId = runId)
  
  progress_frame = bind_rows(progress_frame, difference_temp)
}

rm(persons, sim_distance_share_abs, distance_share, distance_share_vs, difference_temp, trips_inProgress, trips_raw)

####            PART 3: Plotting                                            ####

n = progress_frame %>%
  distinct(runId) %>%
  mutate(n = 1:n())

distance.bins = unique(progress_frame$Distanzgruppe)
breaks = n$n

plt.list = list()

for(distance.group in distance.bins){
  
  filtered_progress = filter(progress_frame, Distanzgruppe == distance.group) %>%
    left_join(n, by ="runId")
  
  labels = unique(filtered_progress$runId)
  
  plt = ggplot(filtered_progress, aes(n, Differenz, color = Verkehrsmittel)) +
    
    geom_line() +
    
    geom_point() +
    
    geom_hline(aes(yintercept = -0.02), linetype = "dashed") +
    
    geom_hline(aes(yintercept = 0.02), linetype = "dashed") +
    
    scale_x_continuous(breaks = breaks, labels = labels) +
    
    scale_y_continuous(breaks = seq(-1,1,0.05)) +
    
    labs(title = paste0("Differenz zwischen MiD und MATSim in Distanzgruppe: ", distance.group)) +
    
    theme_bw()
  
  plt.list[[distance.group]] = plt
}
