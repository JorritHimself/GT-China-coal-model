##### clear env ####
rm(list = ls())
### Libraries
library(readr)
library(stringr)
library(readxl)
library(openxlsx)
library(tidyverse)
library(here)
# To properly render the Chinese characters:
Sys.setlocale(category = "LC_ALL", locale = "Chinese")
# Data source and save paths
scriptpath <-  here("0 r data prep scripts")
build.input.path <-  here("1 input")
build.output.path <-  here("2 build")
solutions.input.path <-  here("4 solutionfiles")
solutions.out.path.all <- here("5 solution processed/all")
solutions.out.path.imp <- here("5 solution processed/imports")
# To properly render Chinese characters:
Sys.setlocale(category = "LC_ALL", locale = "Chinese")


############# list of all solution file names
solution.list <- list.files(solutions.input.path, full.names = FALSE)

# Empty df for appending
df.supplier.summ.all.appended <- data.frame("location"=character(),
                                            "coal_group"=character(),
                                            "volume_Mt"=double(),
                                            "solution"=character())
### loop
for (filename in solution.list){
  ############# Import solution file
  solution.file.name <- filename
  # file names now
  output.file.name.all <- str_replace(solution.file.name, ".txt", " all.xlsx")
  output.file.all <- file.path(solutions.out.path.all, output.file.name.all)
  output.file.name.imp <- str_replace(solution.file.name, ".txt", " imp.xlsx")
  output.file.imp <- file.path(solutions.out.path.imp, output.file.name.imp)
  
  # get solution, read with csv
  solutions.file <- file.path(solutions.input.path, solution.file.name)
  df.solutions <- read.csv(solutions.file, sep = "'", header=FALSE, stringsAsFactors=FALSE)
  #suppliers
  df.suppliers <- df.solutions %>% 
    select(V1, V2, V4, V6, V7) %>% 
    rename(var_type = V1,
           orig_node_id = V2,
           dest_node_id = V4,
           coal_type = V6,
           volume_Mt = V7)
  # clean up char vars
  df.suppliers$var_type <- substr(df.suppliers$var_type,1,nchar(df.suppliers$var_type)-2)
  df.suppliers$var_type <- str_trim(df.suppliers$var_type, side = c("both"))
  df.suppliers$orig_node_id <- str_trim(df.suppliers$orig_node_id, side = c("both"))
  df.suppliers$dest_node_id <- str_trim(df.suppliers$dest_node_id, side = c("both"))
  df.suppliers$coal_type <- str_trim(df.suppliers$coal_type, side = c("both"))
  # fix volumes
  df.suppliers$volume_Mt <- substr(df.suppliers$volume_Mt, 4, 99)
  df.suppliers$volume_Mt <- as.numeric(df.suppliers$volume_Mt)
  df.suppliers$orig_node_type <- substr(df.suppliers$orig_node_id, 1, 4)
  df.suppliers$dest_node_type <- substr(df.suppliers$dest_node_id, 1, 4)  
  # keep mines only (supplies)
  df.suppliers <- df.suppliers %>% 
    filter(var_type=="flow_by_coaltype") %>% 
    filter(orig_node_type=="mine")
  
  # Hook up mine info
  node.info.file <- file.path(build.output.path, "node to node id helper latest.xlsx")
  df.node.info <- read_excel(node.info.file) 
  mine.loc.info.file <- file.path(build.input.path, "helper files/mine name to location.xlsx")
  df.mine.loc.info <- read_excel(mine.loc.info.file) %>% select(node_name, country, location) %>% distinct()
  # Hook up mine names full
  df.suppliers <- left_join(df.suppliers, df.node.info, by = c("orig_node_id" = "node_id"))
  # Hook up mine locations
  df.suppliers <- left_join(df.suppliers, df.mine.loc.info, by = "node_name") %>% rename(orig_node_name = node_name)
  # Hook up destination names full
  df.suppliers <- left_join(df.suppliers, df.node.info, by = c("dest_node_id" = "node_id")) %>% rename(dest_node_name = node_name)
  
  # Russian imports split into overland and seaborne
  df.suppliers$location <- ifelse(df.suppliers$location=="Russia" & df.suppliers$dest_node_type=="ovld", "Russia - overland", df.suppliers$location)
  df.suppliers$location <- ifelse(df.suppliers$location=="Russia" & df.suppliers$dest_node_type=="navo", "Russia - seaborne", df.suppliers$location)
  
  ### Summarize all
  df.suppliers$coal_group <- substr(df.suppliers$coal_type, 1, 7)  
  df.supplier.summ.all <- df.suppliers %>% 
    group_by(location, coal_group) %>% 
    summarise(volume_Mt = sum(volume_Mt))
  df.supplier.summ.imp <- df.suppliers %>% 
    filter(country!="China") %>% 
    group_by(location, coal_group) %>% 
    summarise(volume_Mt = sum(volume_Mt))
  ############################    Save to file   ##############################################
  # Save with date to prevent overwriting
  write.xlsx(df.supplier.summ.all, output.file.all)
  write.xlsx(df.supplier.summ.imp, output.file.imp)
  
  # Append to overview file
  df.supplier.summ.all$solution <- str_replace(solution.file.name, ".txt", "")
  df.supplier.summ.all.appended <- dplyr::bind_rows(df.supplier.summ.all.appended, df.supplier.summ.all)
}
# Save overview
output.file.name.appended <- file.path(solutions.out.path.all, "all appended.xlsx")
write.xlsx(df.supplier.summ.all.appended, output.file.name.appended)
