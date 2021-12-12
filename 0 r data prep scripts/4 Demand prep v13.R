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
# To properly render Chinese characters:
Sys.setlocale(category = "LC_ALL", locale = "Chinese")

############# Coal-fired power consumption
# get prov power cons data
eldc.file <- file.path(build.input.path, "energy demand files/Worksheet demand coal fired power.xlsx")
eldc.df <- read.xlsx(eldc.file, sheet = "coal TWh hist forecast")
# Reshape long
eldc.df <- eldc.df %>%
  filter(!is.na(node_name)) %>% 
  pivot_longer(names_to = "year", values_to = "elec_demand_TWh", power_dem_TWh_2015:power_dem_TWh_2030)
# proper years
eldc.df$year = str_replace(eldc.df$year, "power_dem_TWh_", "")
eldc.df$year <- as.numeric(eldc.df$year)
# Calculate elec demand in GJ
eldc.df$elec_demand_GJ <- eldc.df$elec_demand_TWh*3.6e6
# Keep useful vars
eldc.df <- eldc.df %>% 
  select(node_name, year, elec_demand_GJ)

############# Steel consumption
# get steel prod data
steeldc.file <- file.path(build.input.path, "energy demand files/Worksheet demand steel.xlsx")
steeldc.df <- read.xlsx(steeldc.file, sheet = "steel primary worksheet") %>% 
  filter(!is.na(node_name))
steeldc.df$pri_steel_prod_Mt_2021 <- as.numeric(steeldc.df$pri_steel_prod_Mt_2021)
steeldc.df$pri_steel_prod_Mt_2022 <- as.numeric(steeldc.df$pri_steel_prod_Mt_2022)
steeldc.df$pri_steel_prod_Mt_2025 <- as.numeric(steeldc.df$pri_steel_prod_Mt_2025)
# Reshape long 
steeldc.df <- steeldc.df %>% 
  pivot_longer(names_to = "year", values_to = "steel_demand_Mt", pri_steel_prod_Mt_2015:pri_steel_prod_Mt_2030)
# proper years
steeldc.df$year = str_replace(steeldc.df$year, "pri_steel_prod_Mt_", "")
steeldc.df$year <- as.numeric(steeldc.df$year)
# Keep useful vars
steeldc.df <- steeldc.df %>% 
  select(node_name, year, steel_demand_Mt)

############# heating consumption
# get heating cons data
heating.file <- file.path(build.input.path, "energy demand files/Worksheet demand heating.xlsx")
heating.df <- read.xlsx(heating.file, sheet = "city lvl hist forec Mt")
# Reshape long 
heating.df <- heating.df %>% 
  filter(!is.na(node_name)) %>% 
  pivot_longer(names_to = "year", values_to = "heating_dem_Mt", city_heating_dm_Mt_2015:city_heating_dm_Mt_2030)
# proper years
heating.df$year = str_replace(heating.df$year, "city_heating_dm_Mt_", "")
heating.df$year <- as.numeric(heating.df$year)
# Keep useful vars
heating.df <- heating.df %>% 
  select(node_name, year, heating_dem_Mt)

############# chemicals consumption
# get chemicals cons data
chemicals.file <- file.path(build.input.path, "energy demand files/Worksheet demand chemicals.xlsx")
chemicals.df <- read.xlsx(chemicals.file, sheet = "city lvl hist forec Mt")
# Reshape long 
chemicals.df <- chemicals.df %>% 
  filter(!is.na(node_name)) %>% 
  pivot_longer(names_to = "year", values_to = "chemical_dem_Mt", city_chemical_dm_Mt_2015:city_chemical_dm_Mt_2030)
# proper years
chemicals.df$year = str_replace(chemicals.df$year, "city_chemical_dm_Mt_", "")
chemicals.df$year <- as.numeric(chemicals.df$year)
# Keep useful vars
chemicals.df <- chemicals.df %>% 
  select(node_name, year, chemical_dem_Mt)

############# buildings materials consumption
# get cement cons data
cement.file <- file.path(build.input.path, "energy demand files/Worksheet demand cement.xlsx")
cement.df <- read.xlsx(cement.file, sheet = "city lvl hist forec Mt")
# Reshape long 
cement.df <- cement.df %>% 
  filter(!is.na(node_name)) %>% 
  pivot_longer(names_to = "year", values_to = "cement_dem_Mt", city_cement_dm_Mt_2015:city_cement_dm_Mt_2030)
# proper years
cement.df$year = str_replace(cement.df$year, "city_cement_dm_Mt_", "")
cement.df$year <- as.numeric(cement.df$year)
# Keep useful vars
cement.df <- cement.df %>% 
  select(node_name, year, cement_dem_Mt)

############# other consumption
# get other cons data
other.file <- file.path(build.input.path, "energy demand files/Worksheet demand others.xlsx")
other.df <- read.xlsx(other.file, sheet = "city lvl hist forec Mt")
# Reshape long 
other.df <- other.df %>% 
  filter(!is.na(node_name)) %>% 
  pivot_longer(names_to = "year", values_to = "other_dem_Mt", city_others_dm_Mt_2015:city_others_dm_Mt_2030)
# proper years
other.df$year = str_replace(other.df$year, "city_others_dm_Mt_", "")
other.df$year <- as.numeric(other.df$year)
# Keep useful vars
other.df <- other.df %>% 
  select(node_name, year, other_dem_Mt)

############# Get all nodes
node.id.file <- file.path(build.output.path, "node to node id helper latest.xlsx")
nodes.all.df <- read_excel(node.id.file) 
# Expand to years
years <- as.data.frame(list(year = 2015:2030))
nodes.all.df <- merge(nodes.all.df, years) %>% 
  arrange(node_name, node_id, year)
# Stick in all demand
nodes.all.df <- left_join(nodes.all.df, eldc.df, by = c("node_name", "year"))
nodes.all.df <- left_join(nodes.all.df, steeldc.df, by = c("node_name", "year"))
nodes.all.df <- left_join(nodes.all.df, heating.df, by = c("node_name", "year"))
nodes.all.df <- left_join(nodes.all.df, chemicals.df, by = c("node_name", "year"))
nodes.all.df <- left_join(nodes.all.df, cement.df, by = c("node_name", "year"))
nodes.all.df <- left_join(nodes.all.df, other.df, by = c("node_name", "year"))

# Blanks as zeroes
nodes.all.df$elec_demand_GJ <- ifelse(is.na(nodes.all.df$elec_demand_GJ), 0, nodes.all.df$elec_demand_GJ)
nodes.all.df$steel_demand_Mt <- ifelse(is.na(nodes.all.df$steel_demand_Mt), 0, nodes.all.df$steel_demand_Mt)
nodes.all.df$heating_dem_Mt <- ifelse(is.na(nodes.all.df$heating_dem_Mt), 0, nodes.all.df$heating_dem_Mt)
nodes.all.df$chemical_dem_Mt <- ifelse(is.na(nodes.all.df$chemical_dem_Mt), 0, nodes.all.df$chemical_dem_Mt)
nodes.all.df$cement_dem_Mt <- ifelse(is.na(nodes.all.df$cement_dem_Mt), 0, nodes.all.df$cement_dem_Mt)
nodes.all.df$other_dem_Mt <- ifelse(is.na(nodes.all.df$other_dem_Mt), 0, nodes.all.df$other_dem_Mt)

# Other demand as a single variable, Mt
nodes.all.df$other_dem_Mt_tot = nodes.all.df$heating_dem_Mt + nodes.all.df$chemical_dem_Mt + nodes.all.df$cement_dem_Mt + nodes.all.df$other_dem_Mt
# Other demand as a single variable, GJ
nodes.all.df$other_dem_kcal <- nodes.all.df$other_dem_Mt_tot*1e3*1e6*5000
nodes.all.df$other_dem_GJ <- nodes.all.df$other_dem_kcal*4.184e-6


################ Feasibility correction for power production: make sure production capacity per year and province is not less than demand
elec.capa.file <- file.path(build.output.path, "electric capacities latest.xlsx")
elec.capa.df <- read_excel(elec.capa.file) 
# Elec capa by year and province
elec.capa.summ.df <- elec.capa.df %>% 
  filter(orig_node_type=="pwun") %>% 
  group_by(dest_node_name, year) %>% 
  summarise(cap_PJ_tot = sum(cap_PJ))
# Compare demand with capacity
elec.corr.df <- nodes.all.df %>% 
  filter(substr(node_name,1,4)=="eldc") %>% 
  select(node_name, year, elec_demand_GJ)
elec.corr.df$elec_demand_PJ <- elec.corr.df$elec_demand_GJ*1e-6
elec.corr.df <- elec.corr.df %>% select(-elec_demand_GJ)
elec.corr.df <- left_join(elec.corr.df, elec.capa.summ.df, by = c("node_name"="dest_node_name", "year" = "year"))
# Demand as a factor of prod capacity
elec.corr.df$cap.factor = elec.corr.df$elec_demand_PJ/elec.corr.df$cap_PJ_tot
# Electricity production correction factor: 
# Where power plants are running at 98% or more of capacity, including at more than 100%, multiply prod capacity by:
# (Actual demand/actual capacity)*1.05: the 5% extra is just to create some extra slack in the system
elec.corr.df$corr.factor <- ifelse(elec.corr.df$cap.factor>0.98, elec.corr.df$cap.factor*1.05, 1)
# For when there are no power plants at all (just Beijing 2029 and 2030 really)
elec.corr.df$corr.factor <- ifelse(is.na(elec.corr.df$corr.factor),1,elec.corr.df$corr.factor)
elec.corr.df <- elec.corr.df %>% select(node_name, year, corr.factor)
# Merge with original electrical capa data
elec.capa.df <- left_join(elec.capa.df, elec.corr.df, by = c("dest_node_name" = "node_name", "year" = "year"))
elec.capa.df$cap_PJ_corrected <- elec.capa.df$cap_PJ*elec.capa.df$corr.factor

# Drop variables that get read in with dtype error in python: we dont use these anyway
elec.capa.df <- elec.capa.df %>% 
  select(-cap_MW) %>% 
  select(-cap_GW)

################ Feasibility correction for steel: make sure production capacity per year and province is not less than demand
steel.capa.file <- file.path(build.output.path, "steel prod capacities latest.xlsx")
steel.capa.df <- read_excel(steel.capa.file) 
steel.loc.file <- file.path(build.input.path, "Global Energy Monitor data/Global Steel Plant Tracker Feb 2021 China excerpt -- CENSORED.xlsx")
steel.loc.df <- read_excel(steel.loc.file, sheet = "Steel Plant Data") %>% 
  select(`Plant ID`, prov_steel_demand_node_name) %>% 
  rename(plant_id = `Plant ID`,
         prov_node_name = prov_steel_demand_node_name)
# plant id because names have & and I cant be bothered at this point to figure out how to import these properly
steel.capa.df$plant_id <- substr(steel.capa.df$node_name, start = 6, stop = 13)
# Join
steel.capa.df <- left_join(steel.capa.df, steel.loc.df, by = "plant_id")
# Steel capa by year and province
steel.capa.summ.df <- steel.capa.df %>% 
  group_by(prov_node_name, year) %>% 
  summarise(steel_cap_Mt_tot = sum(steel_cap_Mt))
# Compare demand with capacity
steel.corr.df <- nodes.all.df %>% 
  filter(substr(node_name,1,4)=="stdc") %>% 
  select(node_name, year, steel_demand_Mt)
steel.corr.df <- left_join(steel.corr.df, steel.capa.summ.df, by = c("node_name"="prov_node_name", "year" = "year"))

# Demand as a factor of prod capacity
steel.corr.df$cap.factor = steel.corr.df$steel_demand_Mt/steel.corr.df$steel_cap_Mt_tot
# Steel production correction factor: 
# Where steel mills are running at 92% or more of capacity, including at more than 100%, multiply prod capacity by:
# (Actual demand/actual capacity)*1.05: the 5% extra is just to create some extra slack in the system
steel.corr.df$corr.factor <- ifelse(steel.corr.df$cap.factor>0.92, steel.corr.df$cap.factor*1.05, 1)
steel.corr.df <- steel.corr.df %>% select(node_name, year, corr.factor)
# Merge with original steel capa data
steel.capa.df <- left_join(steel.capa.df, steel.corr.df, by = c("prov_node_name" = "node_name", "year" = "year"))
steel.capa.df$steel_cap_Mt_corrected <- steel.capa.df$steel_cap_Mt*steel.capa.df$corr.factor
# Keep vars
steel.capa.df <- steel.capa.df %>% 
  select(node_name, year, steel_cap_Mt, node_id, steel_cap_Mt_corrected)

################ Units and rounding
nodes.all.df$elec_demand_PJ <- nodes.all.df$elec_demand_GJ*1e-6
nodes.all.df$elec_demand_PJ <- round(nodes.all.df$elec_demand_PJ , 1)
nodes.all.df$steel_demand_Mt <- round(nodes.all.df$steel_demand_Mt, 3)
steel.capa.df$steel_cap_Mt_corrected <- round(steel.capa.df$steel_cap_Mt_corrected, 2)
elec.capa.df$cap_PJ_corrected <- round(elec.capa.df$cap_PJ_corrected, 2)
nodes.all.df$other_demand_PJ <- nodes.all.df$other_dem_GJ*1e-6
nodes.all.df$other_demand_PJ <- round(nodes.all.df$other_demand_PJ , 1)

# Keep useful vars
nodes.all.df <- nodes.all.df %>% 
  select(node_name, node_id, year, elec_demand_PJ, steel_demand_Mt, other_demand_PJ)

############################    Save to file   ##############################################
# Save latest version as a copy to use
out.file.name <- "demand all latest.xlsx"
outfile <- file.path(build.output.path, out.file.name)
write.xlsx(nodes.all.df, outfile)

# Save lastest version as a csv
out.file.name <- "demand all latest.csv"
outfile <- file.path(build.output.path, out.file.name)
con<-file(outfile, encoding="UTF-8")
write.csv(nodes.all.df, file=con, row.names=FALSE)

# Save latest version of elec capacity data 
out.file.name <- "electric capacities latest.xlsx"
outfile <- file.path(build.output.path, out.file.name)
write.xlsx(elec.capa.df, outfile)

# Save latest version of elec capacity data as a csv
out.file.name <- "electric capacities latest.csv"
outfile <- file.path(build.output.path, out.file.name)
con<-file(outfile, encoding="UTF-8")
write.csv(elec.capa.df, file=con, row.names=FALSE)

# Save latest version of steel capacity data 
out.file.name <- "steel prod capacities latest.xlsx"
outfile <- file.path(build.output.path, out.file.name)
write.xlsx(steel.capa.df, outfile)

# Save latest version of steel capacity data as a csv
out.file.name <- "steel prod capacities latest.csv"
outfile <- file.path(build.output.path, out.file.name)
con<-file(outfile, encoding="UTF-8")
write.csv(steel.capa.df, file=con, row.names=FALSE)
