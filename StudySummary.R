library(tidyverse)
library(ssh)
library(readr)

############## DOWNLOAD FILES FROM REMOTE REPOSITORY ####################

LOCAL_DIR = "C:/Users/ACER/IdeaProjects/matsim-vulkaneifel/output/study/plan-case-"
REMOTE_DIR = "zoerner@cluster-i.math.tu-berlin.de"

FILES_DIR = "/net/ils/zoerner/matsim-vulkaneifel/study/plan-case-"

fleetsize_1 = seq(20,160,10)

#create ssh connection to math cluster
connection = ssh_connect(host = REMOTE_DIR, passwd = "")

for(size in fleetsize_1){
  
  RUN_DIR = paste0(FILES_DIR, "1", "/fleet-size-", size, "/")
  
  files = capture.output(
    ssh_exec_wait(connection, 
                  command = paste0("ls ", RUN_DIR))
  )
  
  files = files[str_ends(files, ".csv")]
  print(files)
  
  for(file in files){
    print(paste0(RUN_DIR, file))
    scp_download(connection, files = paste0(RUN_DIR, file), to = paste0(LOCAL_DIR, "1/"))
  }
}

ssh_disconnect(connection)

############## ANALYZE WAITING TIME AND VEHICLE DISTANCES ####################

waiting.time.data = data.frame(
  "fleetsize" = numeric(),
  "wait_p95" = numeric()
)

for(size in fleetsize_1){
  
  LOCAL_CUSTOMERSTATS_PATH = paste0(LOCAL_DIR, "1", "/", "fleet-size-", size, "-plan-case-1.drt_customer_stats_drt.csv"  )
  customer.stats = read.csv2(file = LOCAL_CUSTOMERSTATS_PATH, dec = ".")
  
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

waiting.time.data.1 = waiting.time.data %>%
  
  mutate(wait_p95_min = wait_p95 / 60)

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
