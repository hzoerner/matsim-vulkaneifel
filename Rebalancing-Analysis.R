library(tidyverse)
library(readr)
library(ssh)

FILES_DIR = "C:/Users/ACER/IdeaProjects/matsim-vulkaneifel/output/rebalanc-tuning/"
REMOTE_DIR = "zoerner@cluster-i.math.tu-berlin.de"

alphas = c("0", "0.2", "0.4", "0.6", "0.8")
betas = c("0", "0.1", "0.3", "0.7")

#create ssh connection to math cluster
connection = ssh_connect(host = REMOTE_DIR, passwd = "")

for(alpha in alphas){
  
  for(beta in betas){
    
    files = capture.output(
      ssh_exec_wait(connection, 
                    command = paste0(
                      "ls /net/ils/zoerner/matsim-vulkaneifel/rebalanc-tuning/drt-rebalanc-tuning-alpha-", alpha, "-beta-", beta, "/"))
    )
    
    files = files[str_ends(files, ".csv")]
    print(files)
    DIR2 = paste0("/net/ils/zoerner/matsim-vulkaneifel/rebalanc-tuning/drt-rebalanc-tuning-alpha-", alpha, "-beta-", beta, "/")
    
    for(file in files){
      scp_download(connection, files = paste0(DIR2, file), to = FILES_DIR)
    }
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