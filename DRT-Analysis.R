library(tidyverse)
library(readr)
library(ssh)

FILES_DIR = "C:/Users/ACER/IdeaProjects/matsim-vulkaneifel/output/fleetsize-determination/"
REMOTE_DIR = "zoerner@cluster-i.math.tu-berlin.de"


fleetSizes = seq(40, 100, 20)

#create ssh connection to math cluster
connection = ssh_connect(host = REMOTE_DIR, passwd = "")

for(size in fleetSizes){
  
  files = capture.output(
    ssh_exec_wait(connection, 
                  command = paste0(
                    "ls /net/ils/zoerner/matsim-vulkaneifel/fleetsize-determination/fleet-size-", size, "-no-rebalanc-output/"))
  )
  
  files = files[str_ends(files, ".csv")]
  print(files)
  DIR2 = paste0("/net/ils/zoerner/matsim-vulkaneifel/fleetsize-determination/fleet-size-", size, "-no-rebalanc-output/")
  
  for(file in files){
    scp_download(connection, files = paste0(DIR2, file), to = FILES_DIR)
  }
}

ssh_disconnect(connection)

waiting.time.data = data.frame(
  "fleetsize" = numeric(),
  "wait_p95" = numeric()
)

for(size in fleetSizes){
  FILE = paste0("fleet-size-", size, "-no-rebalanc.drt_customer_stats_drt.csv")
  PATH = paste0(FILES_DIR, FILE)
  
  customer.stats = read_csv2(PATH)
  
  filtered = customer.stats %>% 
    filter(iteration == 300) %>%
    select(wait_p95)
  
  newEntry = data.frame(
    "fleetsize" = c(size),
    "wait_p95" = filtered$wait_p95
  )
  
  waiting.time.data = bind_rows(waiting.time.data, newEntry)
}

waiting.time.data.1 = mutate(waiting.time.data, wait_p95_min = wait_p95 / 60)

ggplot(waiting.time.data.1, aes(fleetsize, wait_p95_min)) +
  
  geom_line() +
  
  geom_hline(aes(yintercept = 15)) +
  
  labs(y = "95-Prozent-Percentil der Wartezeit",
       x = "Flottengröße") +
  
  theme_bw()