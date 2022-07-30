library(tidyverse)
library(readr)
library(ssh)

FILES_DIR = "C:/Users/ACER/IdeaProjects/matsim-vulkaneifel/output/rebalanc-tuning/"
REMOTE_DIR = "zoerner@cluster-i.math.tu-berlin.de"

alphas = c("0.2", "0.4", "0.6", "0.8")
betas = c("0.0", "0.1", "0.3", "0.7")

#create ssh connection to math cluster
connection = ssh_connect(host = REMOTE_DIR, passwd = "")


############## DOWNLOAD FILES FROM REMOTE REPOSITORY ####################
for(beta in betas){
  
  for(alpha in alphas){
    
    REMOTE_DIR_FILEPATH = paste0("/net/ils/zoerner/matsim-vulkaneifel/rebalanc-tuning/drt-rebalanc-tuning-alpha-", alpha, "-beta-", beta, "-V2/")
    
    files = capture.output(
      ssh_exec_wait(connection, 
                    command = paste0("ls ", REMOTE_DIR_FILEPATH))
    )
    
    files = files[str_ends(files, ".csv")]
    print(files)
    
    for(file in files){
      print(paste0(REMOTE_DIR_FILEPATH, file))
      scp_download(connection, files = paste0(REMOTE_DIR_FILEPATH, file), to = FILES_DIR)
    }
  }
}

ssh_disconnect(connection)

############## SUMMARY OF OUTPUT DATA FILES ####################

waiting.time.data.rebalanc = data.frame(
  "alpha" = numeric(),
  "beta" = numeric(),
  "wait_p95" = numeric(),
  "wait_avg" = numeric(),
  "wait_med" = numeric()
)

for(beta in betas){
  
  for(alpha in alphas){
    
    FILE = paste0("drt-rebalanc-tuning-alpha-", alpha, "-beta-", beta, "-V2.drt_customer_stats_drt.csv")
    PATH = paste0(FILES_DIR, FILE)
    
    customer.stats = read.csv2(PATH, dec = ".")
    
    filtered = customer.stats %>% 
      filter(iteration == 300) %>%
      select(wait_p95, wait_average, wait_median)
    
    newEntry = data.frame(
      "alpha" = c(alpha),
      "beta" = c(beta),
      "wait_p95" = filtered$wait_p95,
      "wait_avg" = filtered$wait_average,
      "wait_med" = filtered$wait_median
    ) %>% mutate(
      alpha = as.numeric(alpha),
      beta = as.numeric(beta)
    )
    
    waiting.time.data.rebalanc = bind_rows(waiting.time.data.rebalanc, newEntry)
  }
}

#add run with 0,0 values

{
  
  FILE = paste0("drt-rebalanc-tuning-alpha-", 0, "-beta-", 0, ".drt_customer_stats_drt.csv")
  PATH = paste0(FILES_DIR, FILE)
  
  customer.stats = read.csv2(PATH, dec = ".")
  
  filtered = customer.stats %>% 
    filter(iteration == 300) %>%
    select(wait_p95, wait_average, wait_median)
  
  newEntry = data.frame(
    "alpha" = c(0.0),
    "beta" = c(0.0),
    "wait_p95" = filtered$wait_p95,
    "wait_avg" = filtered$wait_average,
    "wait_med" = filtered$wait_median
  ) %>% mutate(
    alpha = as.numeric(alpha),
    beta = as.numeric(beta)
  )
  
  waiting.time.data.rebalanc = bind_rows(waiting.time.data.rebalanc, newEntry)
  
}

waiting.time.data.rebalanc.1 = mutate(waiting.time.data.rebalanc, wait_p95_min = wait_p95 / 60)

ggplot(waiting.time.data.rebalanc.1, aes(alpha, beta, fill = wait_p95_min, size = wait_p95_min)) +
  
  geom_point() +

  labs(y = "Beta",
       x = "Alpha") +
  
  theme_bw()
