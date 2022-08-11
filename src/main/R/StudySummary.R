library(tidyverse)
library(ssh)

############## DOWNLOAD FILES FROM REMOTE REPOSITORY ####################

LOCAL_DIR = "C:/Users/ACER/IdeaProjects/matsim-vulkaneifel/output/study/"
REMOTE_DIR = "zoerner@cluster-i.math.tu-berlin.de"

fleetsize_1 = seq(20,150,10)
fleetsize_2 = seq(100, 800, 50)

downloadFromCluster <- function(password, plan_case = c(1,2), fleetsize){
  
  FILES_DIR = "/net/ils/zoerner/matsim-vulkaneifel/study/plan-case-"
  
  files = character()
  
  if(!is.vector(fleetsize)){
    warning("Argument fleetsize needs to be an vector!")
    return(NULL)
  }
  
  #create ssh connection to math cluster
  connection = ssh_connect(host = REMOTE_DIR, passwd = password)
  
  for(size in fleetsize){
    
    RUN_DIR = paste0(FILES_DIR, plan_case, "/fleet-size-", size, "/")
    
    files = capture.output(
      ssh_exec_wait(connection, 
                    command = paste0("ls ", RUN_DIR))
    )
    
    files = files[str_ends(files, ".csv")]
    print(files)
    
    for(file in files){
      print(paste0(RUN_DIR, file))
      scp_download(connection, files = paste0(RUN_DIR, file), to = paste0(LOCAL_DIR,"plan-case-", plan_case, "/"))
    }
  }
  
  ssh_disconnect(connection)
  
  return(files)
}

pw = ""
files_1 = downloadFromCluster(password = pw, plan_case = 1, fleetsize = fleetsize_1)
files_2 = downloadFromCluster(password = pw, plan_case = 2, fleetsize = fleetsize_2)

############## ANALYZE WAITING TIME AND VEHICLE DISTANCES ####################

summarizeOutput <- function(fleetsize, plan_case = c(1,2), LOCAL_DIR){
  
  waiting.time.data = data.frame(
    "fleetsize" = numeric(),
    "wait_p95" = numeric()
  )
  
  for(size in fleetsize){
    
    LOCAL_CUSTOMERSTATS_PATH = paste0(LOCAL_DIR, "plan-case-", plan_case, "/", "fleet-size-", size, "-plan-case-", plan_case, ".drt_customer_stats_drt.csv"  )
    print(LOCAL_CUSTOMERSTATS_PATH)
    customer.stats = read.csv2(file = LOCAL_CUSTOMERSTATS_PATH, dec = ".")
    
    if(length(customer.stats$iteration[customer.stats$iteration == 500]) != 0){
      
      filtered = customer.stats %>% 
        filter(iteration == 500) %>%
        select(wait_p95, wait_average, wait_median)
      
      newEntry = data.frame(
        "fleetsize" = size,
        "wait_p95" = filtered$wait_p95
      )
      
      waiting.time.data = bind_rows(waiting.time.data, newEntry)
      rm(newEntry)
    }
  }
  
  return(waiting.time.data %>%
           mutate(wait_p95_min = wait_p95 / 60))
}

fleetsize_2 = fleetsize_2[!fleetsize_2 %in% c("450", "750")]
fleetsize_1 = fleetsize_1[fleetsize_1 != 160]

waiting.time.data.1 = summarizeOutput(fleetsize = fleetsize_1, plan_case = 1, LOCAL_DIR = LOCAL_DIR)
waiting.time.data.2 = summarizeOutput(fleetsize = fleetsize_2, plan_case = 2, LOCAL_DIR = LOCAL_DIR)

size = 2

ggplot(waiting.time.data.1, aes(fleetsize, wait_p95_min)) +
  
  geom_line() +
  
  geom_point(size = size) +
  
  geom_hline(aes(yintercept = 15), color = "darkred", size = size) +
  
  coord_cartesian(ylim = c(0, 50)) +
  
  scale_x_continuous(breaks = seq(20, 160, 10)) +
  
  labs(x = "Flottengröße",
       y = "95-Percentil der Wartezeit") +
  
  theme_bw()

## Analysis of Vehicle Distance Distribution
DRT_TRIPS_FILE = "C:/Users/ACER/IdeaProjects/matsim-vulkaneifel/output/study/plan-case-1/fleet-size-60-plan-case-1.500.vehicleDistanceStats_drt.csv"

drt.trips.raw = read.csv2(file = DRT_TRIPS_FILE, dec = ".")
breaks = c(100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600, 650, Inf)
levels = c("100 bis 150 km", "150 bis 200 km", "200 bis 250 km", "250 bis 300 km", "300 bis 350 km", "350 bis 400 km", "400 bis 450 km", "450 bis 500 km", "500 bis 550 km", "550 bis 600 km", "600 bis 650 km", "> 650 km")

#convert meters to kilmoters and edit colnames
drt.trips.1 = drt.trips.raw %>%
  mutate_if(is.double, function(x){x/1000}) %>%
  select(vehicleId, ends_with("_m")) %>%
  mutate(emptyDistanceShare = emptyDistance_m / drivenDistance_m,
         distanceGroup = cut(x = drivenDistance_m, breaks = breaks, labels = levels),
         distanceGroup = factor(distanceGroup)
         )

colnames(drt.trips.1) = str_replace(colnames(drt.trips.1), "_m", "_km")

drt.trips.sum = drt.trips.1 %>%
  group_by(distanceGroup) %>%
  summarise(n = n())

ggplot(drt.trips.sum, aes(x = distanceGroup, y = n)) +
  
  geom_col(fill = "darkblue") +
  
  geom_text(aes(label = n), nudge_y = .5) +
  
  labs(x = "Tägliche Distanz",
       y = "Anzahl") +
  
  theme_bw()

drt2 = read.csv2("C:/Users/ACER/IdeaProjects/matsim-vulkaneifel/output/study/fleet-size-400/fleet-size-400-plan-case-2.500.vehicleDistanceStats_drt.csv", dec = ".")

drt2.1 = drt2 %>% 
  
  mutate_if(is.double, function(x){ x /1000 }) %>%
  
  mutate(emptyDistance_share = emptyDistance_m / drivenDistance_m,
         distanceGroup = cut(x = drivenDistance_m, breaks = breaks, labels = levels),
         distanceGroup = factor(distanceGroup)) %>%
  
  arrange(emptyDistance_share)

drt2.sum = drt2.1 %>%
  group_by(distanceGroup) %>%
  summarise(n = n())

ggplot(drt2.sum, aes(x = distanceGroup, y = n)) +
  
  geom_col(fill = "darkblue") +
  
  geom_text(aes(label = n), nudge_y = 3) +
  
  labs(x = "Tägliche Distanz",
       y = "Anzahl") +
  
  theme_bw() +
  
  theme(axis.text.x = element_text(angle = 90))

ggplot(waiting.time.data.2, aes(fleetsize, wait_p95_min)) +
  
  geom_line() +
  
  geom_point(size = size) +
  
  geom_hline(aes(yintercept = 15), color = "darkred", size = size) +
  
  coord_cartesian(ylim = c(0, 50)) +
  
  labs(x = "Flottengröße",
       y = "95-Percentil der Wartezeit") +
  
  theme_bw()
