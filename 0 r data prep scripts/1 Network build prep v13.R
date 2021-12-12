### clear env
rm(list = ls())
### Libraries
library(readr)
library(stringr)
library(readxl)
library(openxlsx)
library(tidyverse) 
library(here) 
# Data source and save paths
scriptpath <-  here("0 r data prep scripts")
build.input.path <-  here("1 input")
build.output.path <-  here("2 build")
# To properly render Chinese characters:
Sys.setlocale(category = "LC_ALL", locale = "Chinese")

#######     Variables, cost assumptions

### Transport cost list
truck.cost.rmb.p.tkm <- 0.375
rail.cost.rmb.p.tkm <- 0.131
navi.ocean.cost.rmb.p.tkm <- 0.02
navi.river.cost.rmb.p.tkm <- 0.08
### Handling costs list
mine.to.truck.handlingcost.rmb.p.t <- 20
mine.to.rail.handlingcost.rmb.p.t <- 16.3

truck.to.rail.handlingcost.rmb.p.t <- 16.3
rail.to.truck.handlingcost.rmb.p.t <- 0 # Because handling cost charged when loaded to train

truck.to.port.handlingcost.rmb.p.t <- 12.5
rail.to.port.handlingcost.rmb.p.t <- 12.5
ship.to.port.handlingcost.rmb.p.t <- 12.5
port.to.truck.handlingcost.rmb.p.t <- 12.5
port.to.rail.handlingcost.rmb.p.t <- 16.3
port.to.ship.handlingcost.rmb.p.t <- 12.5
port.to.pwpl.handlingcost.rmb.p.t <- 0 # Because handling cost already charged when offloaded from ship to port

truck.to.pwpl.handlingcost.rmb.p.t <- 0
truck.to.ctct.handlingcost.rmb.p.t <- 0
rail.to.pwpl.handlingcost.rmb.p.t <- 0

minemouth.handlingcost.rmb.p.t <- 15

### range of years df to expand links with constant capacities
years <- as.data.frame(list(year = 2015:2030))

############# Start of network build scripts
# This takes individual types of connections and puts them in a standard format
# The long list of different types of connections is then appended
# Where needed, this also creates a bi-directional network, by appending the same list of rows, with origin and destination swapped

############# railways: railway station to railway station
# get railway data
railway.links.file <- file.path(build.input.path, "rail network/rail links list.xlsx")
railway.type.file <- file.path(build.input.path, "rail network/rail line capacities.xlsx")
railway.links.df <- read_excel(railway.links.file) 
railway.type.df <- read_excel(railway.type.file) 
# keep coal lines only
railway.type.df <- railway.type.df %>% 
  select(line_id, category, 
         cap_Mt_2015, cap_Mt_2016,cap_Mt_2017,cap_Mt_2018,cap_Mt_2019,cap_Mt_2020,
         cap_Mt_2021,cap_Mt_2022,cap_Mt_2023,cap_Mt_2024,cap_Mt_2025,
         cap_Mt_2026,cap_Mt_2027,cap_Mt_2028,cap_Mt_2029,cap_Mt_2030)
railway.links.df <- railway.links.df %>% 
  select(orig_node_name, dest_node_name, distance_km, line_id)
# Mengji = 36-JiZhang + 70-Zhangtang
special.rail.lines.df <- railway.links.df %>% 
  filter(line_id=="10-DaQin"|line_id=="45-HaoJi"|line_id=="36-JiZhang"|line_id=="70-Zhangtang"|line_id=="53-ShuoHuang"|line_id=="35-WaRi"|line_id=="45-HaoJi")
special.rail.lines.df$line_id[special.rail.lines.df$line_id=="36-JiZhang"] <- "MengJi"
special.rail.lines.df$line_id[special.rail.lines.df$line_id=="70-Zhangtang"] <- "MengJi"
special.rail.lines.df <- special.rail.lines.df %>% 
  rename(origin = orig_node_name,
         destination = dest_node_name) %>% 
  select(-distance_km)
railway.links.df <- left_join(railway.links.df, railway.type.df, by = "line_id") %>% 
  filter(category!="presumed no coal") %>% 
  select(-category)
# reshape to long
railway.links.df <- railway.links.df %>% 
  pivot_longer(names_to = "year", values_to = "cap_Mt", cap_Mt_2015:cap_Mt_2030)
# proper years
railway.links.df$year = str_replace(railway.links.df$year, "cap_Mt_", "")
railway.links.df$year <- as.numeric(railway.links.df$year)
# Summarize duplicate tracks: i.e., sum the capacity where multiple lines go parallel over one stretch
railway.links.df <- railway.links.df %>%
  group_by(orig_node_name, dest_node_name, year) %>% 
  summarise(distance_km = dplyr::first(distance_km),
            line_id = dplyr::first(line_id),
            cap_Mt = sum(cap_Mt))
# node types
railway.links.df$orig_node_type <- substr(railway.links.df$orig_node_name, 1, 4)
railway.links.df$dest_node_type <- substr(railway.links.df$dest_node_name, 1, 4)
# additional infos
railway.links.df$dual_dir <- "yes"
railway.links.df$conversion_eff <- 1
railway.links.df$handling.cost.rmb <- 0
railway.links.df$transp.cost.rmb.ptkm <- rail.cost.rmb.p.tkm

############# Roads : city to city 
# get railway data
road.links.file <- file.path(build.input.path, "network connector files/city distances 12 nearest geod and road latest.csv")
city.info.file <- file.path(build.input.path, "helper files/cities names worksheet.xlsx")
road.links.df <- read.csv(road.links.file, fileEncoding = "UTF-16LE") 
city.info.df <- read.xlsx(city.info.file) 
# Select vars
road.links.df <- road.links.df %>% 
  select(orig_city_chn, dest_city_chn, distance_road_km)
# Keep 5 nearest cities
road.links.df <- road.links.df %>%
  group_by(orig_city_chn) %>% 
  arrange(distance_road_km) %>% 
  mutate(count = seq(n())) %>% 
  filter(count<=5) %>% 
  select(-count)
# Hook up node names
road.links.df <- left_join(road.links.df, city.info.df, by = c("orig_city_chn" = "name_city_chn"))
road.links.df <- road.links.df %>% 
  select(orig_city_chn, dest_city_chn, distance_road_km, node_name) %>% 
  rename(orig_node_name = node_name)
road.links.df <- left_join(road.links.df, city.info.df, by = c("dest_city_chn" = "name_city_chn"))
road.links.df <- road.links.df %>% 
  select(orig_city_chn, dest_city_chn, distance_road_km, orig_node_name, node_name) %>% 
  rename(dest_node_name = node_name) %>% 
  ungroup() %>% 
  select(orig_node_name, dest_node_name, distance_road_km)
# years
road.links.df <- merge(road.links.df, years) %>% 
  arrange(orig_node_name, dest_node_name, year)
# Add capacity (keep it missing drop it when defining constraints, considered near infinite)
# road.links.df$cap_Mt <- ""
# node types
road.links.df$orig_node_type <- substr(road.links.df$orig_node_name, 1, 4)
road.links.df$dest_node_type <- substr(road.links.df$dest_node_name, 1, 4)
# additional infos
road.links.df$dual_dir <- "yes"
road.links.df$conversion_eff <- 1
road.links.df$handling.cost.rmb <- 0
road.links.df$transp.cost.rmb.ptkm <- truck.cost.rmb.p.tkm
# rename distance var
road.links.df <- road.links.df %>% rename(distance_km = distance_road_km)

############# Navigation network
# get navigation links
navi.links.file <- file.path(build.input.path, "network connector files/navi links list.xlsx")
navi.links.df <- read.xlsx(navi.links.file) 
# years
navi.links.df <- merge(navi.links.df, years) %>% 
  arrange(orig_node_name, dest_node_name, year)
# Add capacity (missing as this is considered near infinite)
# navi.links.df$cap_Mt <- ""
# node types
navi.links.df$orig_node_type <- substr(navi.links.df$orig_node_name, 1, 4)
navi.links.df$dest_node_type <- substr(navi.links.df$dest_node_name, 1, 4)
# additional infos
navi.links.df$dual_dir <- "yes"
navi.links.df$conversion_eff <- 1
# handling costs 
navi.links.df$handling.cost.rmb <- 0
navi.links.df$handling.cost.rmb <- ifelse(navi.links.df$orig_node_type=="port", port.to.ship.handlingcost.rmb.p.t, navi.links.df$handling.cost.rmb)
navi.links.df$handling.cost.rmb <- ifelse(navi.links.df$dest_node_type=="port", ship.to.port.handlingcost.rmb.p.t, navi.links.df$handling.cost.rmb)
# transport costs
navi.links.df$transp.cost.rmb.ptkm <- navi.river.cost.rmb.p.tkm <- 0.08
navi.links.df$transp.cost.rmb.ptkm <- ifelse(navi.links.df$orig_node_type=="navo", navi.ocean.cost.rmb.p.tkm, navi.links.df$transp.cost.rmb.ptkm)
navi.links.df$transp.cost.rmb.ptkm <- ifelse(navi.links.df$dest_node_type=="navo", navi.ocean.cost.rmb.p.tkm, navi.links.df$transp.cost.rmb.ptkm)

############# Port to cities (subset 3 nearest here)
port.to.city.link.file <- file.path(build.input.path, "network connector files/port to city distances 3 nearest geod and road.xlsx")
port.to.city.links.df <- read.xlsx(port.to.city.link.file) 
# Fix for Hong Kong & Macau distances
port.to.city.links.df$distance_road_km <- ifelse(is.na(port.to.city.links.df$distance_road_km), port.to.city.links.df$distance_geo_km*1.5, port.to.city.links.df$distance_road_km)
# keep actual links only
port.to.city.links.df <- port.to.city.links.df %>% 
  select(orig_node_name, dest_node_name, distance_road_km) %>% 
  rename(distance_km = distance_road_km)
# years
port.to.city.links.df <- merge(port.to.city.links.df, years) %>% 
  arrange(orig_node_name, dest_node_name, year)
# Add capacity (missing as this is considered near infinite)
# port.to.city.links.df$cap_Mt <- ""
# node types
port.to.city.links.df$orig_node_type <- substr(port.to.city.links.df$orig_node_name, 1, 4)
port.to.city.links.df$dest_node_type <- substr(port.to.city.links.df$dest_node_name, 1, 4)
# additional infos
port.to.city.links.df$dual_dir <- "yes"
port.to.city.links.df$conversion_eff <- 1
# handling costs 
port.to.city.links.df$handling.cost.rmb <- truck.to.port.handlingcost.rmb.p.t
# transport costs
port.to.city.links.df$transp.cost.rmb.ptkm <- truck.cost.rmb.p.tkm

############# Port to rail links
# get port consolidated info file
port.info.file <- file.path(build.input.path, "port data/port loc and capa consolidated sources.xlsx")
port.to.rail.links.df <- read.xlsx(port.info.file) 
# keep actual links only
port.to.rail.links.df <-  port.to.rail.links.df %>% 
  select(node_name, train_link) %>% 
  filter(!is.na(train_link)) %>% 
  filter(train_link!="none") %>% 
  rename(dest_node_name = node_name, 
         orig_node_name = train_link) %>% 
  select(orig_node_name, dest_node_name)
# years
port.to.rail.links.df <- merge(port.to.rail.links.df, years) %>% 
  arrange(orig_node_name, dest_node_name, year)
# Add capacity (missing as this is considered near infinite)
# port.to.rail.links.df$cap_Mt <- ""
# node types
port.to.rail.links.df$orig_node_type <- substr(port.to.rail.links.df$orig_node_name, 1, 4)
port.to.rail.links.df$dest_node_type <- substr(port.to.rail.links.df$dest_node_name, 1, 4)
# additional infos
port.to.rail.links.df$dual_dir <- "yes"
port.to.rail.links.df$conversion_eff <- 1
# handling costs 
port.to.rail.links.df$handling.cost.rmb <- rail.to.port.handlingcost.rmb.p.t
# transport costs
port.to.rail.links.df$distance_km <- 0 # direct links
port.to.rail.links.df$transp.cost.rmb.ptkm <- 0 # direct links

############# Links between ports and power or steel plants
# Both direct connections (for powerplants located in ports, or powerplants with their own ports)
# And geolocated driving distances to other plants withing 50km driving. Other connectiosn are via the city-to-city road network
port.info.file <- file.path(build.input.path, "network connector files/stpt and pwpl to port within 50 km driving dist.xlsx")
port.to.plants.links.df <- read.xlsx(port.info.file) 
port.to.plants.links.df$direct_link <- ifelse(is.na(port.to.plants.links.df$direct_link), "No", "Yes")
# Select useful variables
port.to.plants.links.df <- port.to.plants.links.df %>% 
  select(orig_node_name, dest_node_name, distance_road_km, direct_link) %>% 
  rename(distance_km = distance_road_km)
# years
port.to.plants.links.df <- merge(port.to.plants.links.df, years) %>% 
  arrange(orig_node_name, dest_node_name, year)
# Add capacity (missing as this is considered near infinite)
# port.to.pwpl.direct.links.df$cap_Mt <- ""
# node types
port.to.plants.links.df$orig_node_type <- substr(port.to.plants.links.df$orig_node_name, 1, 4)
port.to.plants.links.df$dest_node_type <- substr(port.to.plants.links.df$dest_node_name, 1, 4)
# additional infos
port.to.plants.links.df$dual_dir <- "no"
port.to.plants.links.df$conversion_eff <- 1
# For non-direct links
# handling costs 
port.to.plants.links.df$handling.cost.rmb <- port.to.truck.handlingcost.rmb.p.t
# transport costs
port.to.plants.links.df$transp.cost.rmb.ptkm <- truck.cost.rmb.p.tkm
# For direct links
# handling costs 
port.to.plants.links.df$handling.cost.rmb <- ifelse(port.to.plants.links.df$direct_link=="Yes", 0, port.to.plants.links.df$handling.cost.rmb)
# transport costs
port.to.plants.links.df$transp.cost.rmb.ptkm <- ifelse(port.to.plants.links.df$direct_link=="Yes", 0, port.to.plants.links.df$transp.cost.rmb.ptkm)

############# Rail to power plant links: only one direction
# get network connector file
pwpl.rail.link.file <- file.path(build.input.path, "network connector files/pwpl to rwst distances geod and road.xlsx")
pwpl.to.rail.links.df <- read.xlsx(pwpl.rail.link.file) 
# Fix for Hong Kong & Macau distances
pwpl.to.rail.links.df$distance_road_km <- ifelse(is.na(pwpl.to.rail.links.df$distance_road_km), pwpl.to.rail.links.df$distance_geo_km*1.5, pwpl.to.rail.links.df$distance_road_km)
# keep usuful vars only
pwpl.to.rail.links.df <-  pwpl.to.rail.links.df %>% 
  select(orig_node_name, dest_node_name, distance_road_km)%>% 
  rename(distance_km = distance_road_km)
# years
pwpl.to.rail.links.df <- merge(pwpl.to.rail.links.df, years) %>% 
  arrange(orig_node_name, dest_node_name, year)
# Add capacity (missing as this is considered near infinite)
# pwpl.to.rail.links.df$cap_Mt <- ""
# node types
pwpl.to.rail.links.df$orig_node_type <- substr(pwpl.to.rail.links.df$orig_node_name, 1, 4)
pwpl.to.rail.links.df$dest_node_type <- substr(pwpl.to.rail.links.df$dest_node_name, 1, 4)
# additional infos
pwpl.to.rail.links.df$dual_dir <- "no"
pwpl.to.rail.links.df$conversion_eff <- 1
###Transport costs. Assumptions:
# If the railway station is within 5km, we assume a direct connection: transport costs by rail, plus unloading of rail cars
# If the railway station is further than 5km, we assume a trucking connection: transport costs by truck, plus unloading of rail cars into trucks, plus unloading of trucks at the powerplant
# handling costs 
pwpl.to.rail.links.df$handling.cost.rmb <- ifelse(pwpl.to.rail.links.df$distance_km<5, rail.to.pwpl.handlingcost.rmb.p.t, rail.to.truck.handlingcost.rmb.p.t+truck.to.pwpl.handlingcost.rmb.p.t)
# transport costs
pwpl.to.rail.links.df$transp.cost.rmb.ptkm <- ifelse(pwpl.to.rail.links.df$distance_km<5, rail.cost.rmb.p.tkm, truck.cost.rmb.p.tkm)

############# Rail to steel plant links: only one direction
# get network connector file
stpt.rail.link.file <- file.path(build.input.path, "network connector files/stpt to rwst within 25 km geod and road.xlsx")
stpt.to.rail.links.df <- read.xlsx(stpt.rail.link.file) 
# keep usuful vars only
stpt.to.rail.links.df <-  stpt.to.rail.links.df %>% 
  select(orig_node_name, dest_node_name, distance_road_km, Rail_access)%>% 
  rename(distance_km = distance_road_km)
# years
stpt.to.rail.links.df <- merge(stpt.to.rail.links.df, years) %>% 
  arrange(orig_node_name, dest_node_name, year)
# node types
stpt.to.rail.links.df$orig_node_type <- substr(stpt.to.rail.links.df$orig_node_name, 1, 4)
stpt.to.rail.links.df$dest_node_type <- substr(stpt.to.rail.links.df$dest_node_name, 1, 4)
# additional infos
stpt.to.rail.links.df$dual_dir <- "no"
stpt.to.rail.links.df$conversion_eff <- 1
###Transport costs. Assumptions:
# If direct rail link was identified: transport costs by rail, plus unloading of rail cars
# If no rail connection: transport costs by truck, plus unloading of rail cars into trucks, plus unloading of trucks at the powerplant
# handling costs 
stpt.to.rail.links.df$handling.cost.rmb <- ifelse(stpt.to.rail.links.df$Rail_access=="Yes", rail.to.pwpl.handlingcost.rmb.p.t, rail.to.truck.handlingcost.rmb.p.t+truck.to.pwpl.handlingcost.rmb.p.t)
# transport costs
stpt.to.rail.links.df$transp.cost.rmb.ptkm <- ifelse(stpt.to.rail.links.df$Rail_access=="Yes", rail.cost.rmb.p.tkm, truck.cost.rmb.p.tkm)
stpt.to.rail.links.df <- stpt.to.rail.links.df %>% select(-Rail_access)

############# City to power plant links (i.e., trucking routes): only one direction
# get network connector file
pwpl.city.link.file <- file.path(build.input.path, "network connector files/pwpl to ctct distances 5 nearest geod and road.xlsx")
pwpl.to.city.links.df <- read.xlsx(pwpl.city.link.file) 
# Fix for Hong Kong & Macau distances
pwpl.to.city.links.df$distance_road_km <- ifelse(is.na(pwpl.to.city.links.df$distance_road_km), pwpl.to.city.links.df$distance_geo_km*1.5, pwpl.to.city.links.df$distance_road_km)
# keep usuful vars only
pwpl.to.city.links.df <-  pwpl.to.city.links.df %>% 
  select(orig_node_name, dest_node_name, distance_road_km) %>% 
  rename(distance_km = distance_road_km)
# Select 3 nearest
pwpl.to.city.links.df <- pwpl.to.city.links.df %>% 
  group_by(dest_node_name) %>% 
  arrange(distance_km) %>% 
  mutate(count = seq(n())) %>% 
  filter(count<=3) %>% 
  select(-count)
# years
pwpl.to.city.links.df <- merge(pwpl.to.city.links.df, years) %>% 
  arrange(orig_node_name, dest_node_name, year)
# Add capacity (missing as this is considered near infinite)
# pwpl.to.city.links.df$cap_Mt <- ""
# node types
pwpl.to.city.links.df$orig_node_type <- substr(pwpl.to.city.links.df$orig_node_name, 1, 4)
pwpl.to.city.links.df$dest_node_type <- substr(pwpl.to.city.links.df$dest_node_name, 1, 4)
# additional infos
pwpl.to.city.links.df$dual_dir <- "no"
pwpl.to.city.links.df$conversion_eff <- 1
# handling costs 
pwpl.to.city.links.df$handling.cost.rmb <- truck.to.pwpl.handlingcost.rmb.p.t
# transport costs
pwpl.to.city.links.df$transp.cost.rmb.ptkm <- truck.cost.rmb.p.tkm

############# City to steel plant links (i.e., trucking routes): only one direction
# get network connector file
stpt.city.link.file <- file.path(build.input.path, "network connector files/stpt to ctct distances 3 nearest geod and road.xlsx")
stpt.to.city.links.df <- read.xlsx(stpt.city.link.file) 
# keep usuful vars only
stpt.to.city.links.df <-  stpt.to.city.links.df %>% 
  select(orig_node_name, dest_node_name, distance_road_km) %>% 
  rename(distance_km = distance_road_km)
# Select 3 nearest
stpt.to.city.links.df <- stpt.to.city.links.df %>% 
  group_by(dest_node_name) %>% 
  arrange(distance_km) %>% 
  mutate(count = seq(n())) %>% 
  filter(count<=3) %>% 
  select(-count)
# years
stpt.to.city.links.df <- merge(stpt.to.city.links.df, years) %>% 
  arrange(orig_node_name, dest_node_name, year)
# node types
stpt.to.city.links.df$orig_node_type <- substr(stpt.to.city.links.df$orig_node_name, 1, 4)
stpt.to.city.links.df$dest_node_type <- substr(stpt.to.city.links.df$dest_node_name, 1, 4)
# additional infos
stpt.to.city.links.df$dual_dir <- "no"
stpt.to.city.links.df$conversion_eff <- 1
# handling costs 
stpt.to.city.links.df$handling.cost.rmb <- truck.to.pwpl.handlingcost.rmb.p.t
# transport costs
stpt.to.city.links.df$transp.cost.rmb.ptkm <- truck.cost.rmb.p.tkm

############# Plant to units: only one direction
# get power plant file
pwpl.info.file <- file.path(build.input.path, "Global Energy Monitor data/Global Coal Plant Tracker July 2020 China plants only -- CENSORED.xlsx")
pwpl.to.unit.links.df <- read.xlsx(pwpl.info.file, sheet = "Coal units") 
# keep usuful vars only
pwpl.to.unit.links.df <-  pwpl.to.unit.links.df %>% 
  select(pwpl_node_name, pwun_node_name, oper_cap_2015:oper_cap_2030) %>% 
  rename(orig_node_name = pwpl_node_name,
         dest_node_name = pwun_node_name)
# reshape to long
pwpl.to.unit.links.df <- pwpl.to.unit.links.df %>% 
  pivot_longer(names_to = "year", values_to = "cap_MW", oper_cap_2015:oper_cap_2030)
# proper years
pwpl.to.unit.links.df$year = str_replace(pwpl.to.unit.links.df$year, "oper_cap_", "")
pwpl.to.unit.links.df$year <- as.numeric(pwpl.to.unit.links.df$year)
# Keep links only for years where the plant was active
pwpl.to.unit.links.df <- pwpl.to.unit.links.df %>% 
  filter(cap_MW!=0) %>% 
  select(-cap_MW)
# node types
pwpl.to.unit.links.df$orig_node_type <- substr(pwpl.to.unit.links.df$orig_node_name, 1, 4)
pwpl.to.unit.links.df$dest_node_type <- substr(pwpl.to.unit.links.df$dest_node_name, 1, 4)
# Add capacity (missing as this is considered near infinite)
# pwpl.to.unit.links.df$cap_Mt <- "" # conversion occurs between powerplant unit and electricty demand center
# additional infos
pwpl.to.unit.links.df$dual_dir <- "no"
pwpl.to.unit.links.df$conversion_eff <- 1 # conversion occurs between powerplant unit and electricty demand center
# handling costs 
pwpl.to.unit.links.df$handling.cost.rmb <- 0
# transport costs
pwpl.to.unit.links.df$distance_km <- 0 # direct links
pwpl.to.unit.links.df$transp.cost.rmb.ptkm <- 0

############# Railway station to city center: bidirectional, all railways within 15km of a city center
rwst.city.link.file <- file.path(build.input.path, "network connector files/rwst to ctct within 15 km.xlsx")
rwst.to.city.links.df <- read.xlsx(rwst.city.link.file) 
# keep usuful vars only
rwst.to.city.links.df <-  rwst.to.city.links.df %>% 
  select(rwst_node_name, ctct_node_name, distance_geo_km) %>% 
  rename(orig_node_name = rwst_node_name,
         dest_node_name = ctct_node_name)
# years
rwst.to.city.links.df <- merge(rwst.to.city.links.df, years) %>% 
  arrange(orig_node_name, dest_node_name, year)
# node types
rwst.to.city.links.df$orig_node_type <- substr(rwst.to.city.links.df$orig_node_name, 1, 4)
rwst.to.city.links.df$dest_node_type <- substr(rwst.to.city.links.df$dest_node_name, 1, 4)
# Add capacity (missing as this is considered near infinite)
# rwst.to.city.links.df$cap_Mt <- ""
# additional infos
rwst.to.city.links.df$dual_dir <- "yes"
rwst.to.city.links.df$conversion_eff <- 1 
# handling costs 
rwst.to.city.links.df$handling.cost.rmb <- truck.to.ctct.handlingcost.rmb.p.t
# transport costs
rwst.to.city.links.df$distance_km <- 0 # direct links
rwst.to.city.links.df$transp.cost.rmb.ptkm <- truck.cost.rmb.p.tkm

############# Mongolian rail and road links plus Russian overland ports of entry to Chinese rail and road links
# Note: this is all direct from mine to either railways station or city center in China
import.links.file <- file.path(build.input.path, "network connector files/international mongolia russia interconnectors cities.xlsx")
import.links.df <- read.xlsx(import.links.file) 
# keep usuful vars only
import.links.df <-  import.links.df %>% 
  select(orig_node_name, dest_node_name, distance, capa_Mt_2015:capa_Mt_2030) %>% 
  rename(distance_km = distance)
# reshape to long
import.links.df <- import.links.df %>% 
  pivot_longer(names_to = "year", values_to = "cap_Mt", capa_Mt_2015:capa_Mt_2030)
# proper years
import.links.df$year = str_replace(import.links.df$year, "capa_Mt_", "")
import.links.df$year <- as.numeric(import.links.df$year)
# node types
import.links.df$orig_node_type <- substr(import.links.df$orig_node_name, 1, 4)
import.links.df$dest_node_type <- substr(import.links.df$dest_node_name, 1, 4)
# Add capacity (if cpaaicty is 9999 set to missing as this is considered near infinite)
# import.links.df$cap_Mt <- ifelse(import.links.df$cap_Mt==9999, "", import.links.df$cap_Mt)
# additional infos
import.links.df$dual_dir <- "yes"
import.links.df$conversion_eff <- 1 
# handling costs 
import.links.df$handling.cost.rmb <- ifelse(import.links.df$dest_node_type=="ctct", mine.to.truck.handlingcost.rmb.p.t, 0)
import.links.df$handling.cost.rmb <- ifelse(import.links.df$dest_node_type=="rwst" & import.links.df$distance_km<=1, mine.to.truck.handlingcost.rmb.p.t, import.links.df$handling.cost.rmb)
import.links.df$handling.cost.rmb <- ifelse(import.links.df$dest_node_type=="rwst", mine.to.truck.handlingcost.rmb.p.t+truck.to.rail.handlingcost.rmb.p.t, import.links.df$handling.cost.rmb)
# transport costs
import.links.df$transp.cost.rmb.ptkm <- truck.cost.rmb.p.tkm

############# Overseas mines to navi network + Russian mine to overland ports of entry
### Thermal coals
FOB.thermal.links.file <- file.path(build.input.path, "WoodMac data/Seaborne Export Thermal - non energy adjusted Worksheet -- CENSORED.xlsx")
FOB.thermal.df <- read.xlsx(FOB.thermal.links.file, sheet = "worksheet navo mines connectors") 
FOB.thermal.overland.df <- read.xlsx(FOB.thermal.links.file, sheet = "worksheet ovld mines connectors") 
### Met coals excl Australia
FOB.met.links.file <- file.path(build.input.path, "WoodMac data/Seaborne Export Met Worksheet -- CENSORED.xlsx")
FOB.met.df <- read.xlsx(FOB.met.links.file, sheet = "worksheet navo mines connectors") 
FOB.met.overland.df <- read.xlsx(FOB.met.links.file, sheet = "worksheet ovld mines connectors") 
### Met coals Australia
FOB.met.aus.links.file <- file.path(build.input.path, "WoodMac data/Australia met coal worksheet -- CENSORED.xlsx")
FOB.met.aus.df <- read.xlsx(FOB.met.aus.links.file, sheet = "Met coal operating") 
### All FOB and Russian overland links
FOB.links.df <- dplyr::bind_rows(FOB.thermal.df,
                                 FOB.thermal.overland.df,
                                 FOB.met.df,
                                 FOB.met.overland.df,
                                 FOB.met.aus.df)
# keep usuful vars only
FOB.links.df <-  FOB.links.df %>% 
  select(orig_node_name, dest_node_name)
# years
FOB.links.df <- merge(FOB.links.df, years) %>% 
  arrange(orig_node_name, dest_node_name, year)
# node types
FOB.links.df$orig_node_type <- substr(FOB.links.df$orig_node_name, 1, 4)
FOB.links.df$dest_node_type <- substr(FOB.links.df$dest_node_name, 1, 4)
# additional infos
FOB.links.df$dual_dir <- "no"
FOB.links.df$conversion_eff <- 1 
# handling costs 
FOB.links.df$handling.cost.rmb <- 0 # is already on board
# transport costs
FOB.links.df$distance_km <- 0 # direct links
FOB.links.df$transp.cost.rmb.ptkm <- 0 # direct links, not transport 

############# mine to basin links 
mine.links.file <- file.path(build.input.path, "helper files/basin and mine names.xlsx")
mine.to.basin.links.df <- read.xlsx(mine.links.file, sheet = "china mines") 
# keep usuful vars only
mine.to.basin.links.df <-  mine.to.basin.links.df %>% 
  select(mine_node_name, basi_node_name) %>% 
  rename(orig_node_name = mine_node_name,
         dest_node_name = basi_node_name)
# years
mine.to.basin.links.df <- merge(mine.to.basin.links.df, years) %>% 
  arrange(orig_node_name, dest_node_name, year)
# Add capacity (missing as this is considered near infinite)
# mine.to.basin.links.df$cap_Mt <- ""
# node types
mine.to.basin.links.df$orig_node_type <- substr(mine.to.basin.links.df$orig_node_name, 1, 4)
mine.to.basin.links.df$dest_node_type <- substr(mine.to.basin.links.df$dest_node_name, 1, 4)
# additional infos
mine.to.basin.links.df$distance_km <- 0
mine.to.basin.links.df$dual_dir <- "no"
mine.to.basin.links.df$conversion_eff <- 1 
# handling costs 
mine.to.basin.links.df$handling.cost.rmb <- 0
# transport costs
mine.to.basin.links.df$transp.cost.rmb.ptkm <- 0

############# basin to city links 
basin.city.links.file <- file.path(build.input.path, "network connector files/basin to city helper file.xlsx")
basin.to.city.links.df <- read.xlsx(basin.city.links.file) 
# keep useful vars only
basin.to.city.links.df <-  basin.to.city.links.df %>% 
  rename(orig_node_name = basin,
         dest_node_name = city)
# years
basin.to.city.links.df <- merge(basin.to.city.links.df, years) %>% 
  arrange(orig_node_name, dest_node_name, year)
# node types
basin.to.city.links.df$orig_node_type <- substr(basin.to.city.links.df$orig_node_name, 1, 4)
basin.to.city.links.df$dest_node_type <- substr(basin.to.city.links.df$dest_node_name, 1, 4)
# additional infos
basin.to.city.links.df$dual_dir <- "no"
basin.to.city.links.df$conversion_eff <- 1 
# handling costs 
basin.to.city.links.df$handling.cost.rmb <- mine.to.truck.handlingcost.rmb.p.t
# transport costs
basin.to.city.links.df$distance_km <- 0 # direct links
basin.to.city.links.df$transp.cost.rmb.ptkm <- truck.cost.rmb.p.tkm

############# basin to rwst links 
basin.rwst.links.file <- file.path(build.input.path, "network connector files/basin to rwst helper file.xlsx")
basin.to.rwst.links.df <- read.xlsx(basin.rwst.links.file) 
# keep useful vars only
basin.to.rwst.links.df <-  basin.to.rwst.links.df %>% 
  rename(orig_node_name = basin,
         dest_node_name = rwst_node_name)
# years
basin.to.rwst.links.df <- merge(basin.to.rwst.links.df, years) %>% 
  arrange(orig_node_name, dest_node_name, year)
# Add capacity (missing as this is considered near infinite)
# basin.to.rwst.links.df$cap_Mt <- ""
# node types
basin.to.rwst.links.df$orig_node_type <- substr(basin.to.rwst.links.df$orig_node_name, 1, 4)
basin.to.rwst.links.df$dest_node_type <- substr(basin.to.rwst.links.df$dest_node_name, 1, 4)
# additional infos
basin.to.rwst.links.df$distance_km <- 0
basin.to.rwst.links.df$dual_dir <- "no"
basin.to.rwst.links.df$conversion_eff <- 1 
# handling costs 
basin.to.rwst.links.df$handling.cost.rmb <- mine.to.rail.handlingcost.rmb.p.t
# transport costs
basin.to.rwst.links.df$transp.cost.rmb.ptkm <- rail.cost.rmb.p.tkm

############# minemouth plants 
minemouth.links.file <- file.path(build.input.path, "network connector files/mine mouth plant connectors.xlsx")
minemouth.links.df <- read.xlsx(minemouth.links.file) 
# keep useful vars only
minemouth.links.df <-  minemouth.links.df %>%
  select(basi_node_name, pwpl_node_name) %>% 
  rename(orig_node_name = basi_node_name,
         dest_node_name = pwpl_node_name)
# years
minemouth.links.df <- merge(minemouth.links.df, years) %>% 
  arrange(orig_node_name, dest_node_name, year)
# Add capacity (missing as this is considered near infinite)
# minemouth.links.df$cap_Mt <- ""
# node types
minemouth.links.df$orig_node_type <- substr(minemouth.links.df$orig_node_name, 1, 4)
minemouth.links.df$dest_node_type <- substr(minemouth.links.df$dest_node_name, 1, 4)
# additional infos
minemouth.links.df$distance_km <- 0
minemouth.links.df$dual_dir <- "no"
minemouth.links.df$conversion_eff <- 1 
# handling costs 
minemouth.links.df$handling.cost.rmb <- minemouth.handlingcost.rmb.p.t
# transport costs
minemouth.links.df$transp.cost.rmb.ptkm <- 0

############# power plants units to provincial level electricity demand centers
# get power plant file
pwpl.info.file <- file.path(build.input.path, "Global Energy Monitor data/Global Coal Plant Tracker July 2020 China plants only -- CENSORED.xlsx")
unit.to.eldc.links.df <- read.xlsx(pwpl.info.file, sheet = "Coal units") 
# keep useful vars only
unit.to.eldc.links.df <-  unit.to.eldc.links.df %>% 
  select(pwun_node_name, prov_elec_demand_node_name, oper_cap_2015:oper_cap_2030, `Conversion.efficiency.(factor)`) %>% 
  rename(orig_node_name = pwun_node_name,
         dest_node_name = prov_elec_demand_node_name,
         conversion_eff = `Conversion.efficiency.(factor)`)
# reshape to long
unit.to.eldc.links.df <- unit.to.eldc.links.df %>% 
  pivot_longer(names_to = "year", values_to = "cap_MW", oper_cap_2015:oper_cap_2030)
# proper years
unit.to.eldc.links.df$year = str_replace(unit.to.eldc.links.df$year, "oper_cap_", "")
unit.to.eldc.links.df$year <- as.numeric(unit.to.eldc.links.df$year)
# Keep links only for years where the unit was active
unit.to.eldc.links.df <- unit.to.eldc.links.df %>% 
  filter(cap_MW!=0)
# node types
unit.to.eldc.links.df$orig_node_type <- substr(unit.to.eldc.links.df$orig_node_name, 1, 4)
unit.to.eldc.links.df$dest_node_type <- substr(unit.to.eldc.links.df$dest_node_name, 1, 4)
# Add capacity (missing as this is considered near infinite)
# unit.to.eldc.links.df$cap_Mt <- ""
# additional infos
unit.to.eldc.links.df$dual_dir <- "no"
# handling costs 
unit.to.eldc.links.df$handling.cost.rmb <- 0
# transport costs
unit.to.eldc.links.df$distance_km <- 0 # direct links
unit.to.eldc.links.df$transp.cost.rmb.ptkm <- 0

############# Steel plants to provincial level steel demand centers
# get steel plant file
stpt.info.file <- file.path(build.input.path, "Global Energy Monitor data/Global Steel Plant Tracker Feb 2021 China excerpt -- CENSORED.xlsx")
stpt.to.steeldc.links.df <- read.xlsx(stpt.info.file, sheet = "Steel Plant Data") 
# keep useful vars only
# Note capacity not needed here: this is limited in a separately created sheet for steel plant capacities
stpt.to.steeldc.links.df <-  stpt.to.steeldc.links.df %>% 
  select(stpt_node_name, prov_steel_demand_node_name) %>% 
  rename(orig_node_name = stpt_node_name,
         dest_node_name = prov_steel_demand_node_name)
# years
stpt.to.steeldc.links.df <- merge(stpt.to.steeldc.links.df, years) %>% 
  arrange(orig_node_name, dest_node_name, year)
# node types
stpt.to.steeldc.links.df$orig_node_type <- substr(stpt.to.steeldc.links.df$orig_node_name, 1, 4)
stpt.to.steeldc.links.df$dest_node_type <- substr(stpt.to.steeldc.links.df$dest_node_name, 1, 4)
# additional infos
stpt.to.steeldc.links.df$dual_dir <- "no"
stpt.to.steeldc.links.df$conversion_eff <- 1 
# handling costs 
stpt.to.steeldc.links.df$handling.cost.rmb <- 0
# transport costs
stpt.to.steeldc.links.df$distance_km <- 0 # direct links
stpt.to.steeldc.links.df$transp.cost.rmb.ptkm <- 0

############# UHV lines
# get power plant file
uhv.info.file <- file.path(build.input.path, "network connector files/UHV overview.xlsx")
uhv.links.df <- read.xlsx(uhv.info.file) 
# keep useful vars only
uhv.links.df <-  uhv.links.df %>% 
  select(orig_node_name, dest_node_name, cap_GW_2015:cap_GW_2030, conv_eff, `Transmission.cost,.2018.RMB.per.GJ`) %>% 
  rename(conversion_eff = conv_eff,
         transm_cost_rmb_GJ= `Transmission.cost,.2018.RMB.per.GJ`)
# reshape to long
uhv.links.df <- uhv.links.df %>% 
  pivot_longer(names_to = "year", values_to = "cap_GW", cap_GW_2015:cap_GW_2030)
# proper years
uhv.links.df$year = str_replace(uhv.links.df$year, "cap_GW_", "")
uhv.links.df$year <- as.numeric(uhv.links.df$year)
# Keep links only for years where the line is operational
uhv.links.df <- uhv.links.df %>% 
  filter(cap_GW!=0)
# node types
uhv.links.df$orig_node_type <- substr(uhv.links.df$orig_node_name, 1, 4)
uhv.links.df$dest_node_type <- substr(uhv.links.df$dest_node_name, 1, 4)
# Add capacity (missing as this is considered near infinite)
# uhv.links.df$cap_Mt <- "" # these links will be limited in energy, not Mt throughput
# additional infos
uhv.links.df$dual_dir <- "yes"
# handling costs 
uhv.links.df$handling.cost.rmb <- 0
# transport costs
uhv.links.df$distance_km <- 0 # direct links
uhv.links.df$transp.cost.rmb.ptkm <- 0

##################### Append all the different types of links, then create dual directionality
all.links.df <- dplyr::bind_rows(mine.to.basin.links.df,
                                 basin.to.city.links.df,
                                 basin.to.rwst.links.df,
                                 minemouth.links.df,
                                 railway.links.df,
                                 road.links.df,
                                 navi.links.df,
                                 rwst.to.city.links.df,
                                 port.to.plants.links.df,
                                 port.to.rail.links.df,
                                 port.to.city.links.df,
                                 pwpl.to.rail.links.df,
                                 stpt.to.rail.links.df,
                                 pwpl.to.city.links.df,
                                 stpt.to.city.links.df,
                                 pwpl.to.unit.links.df,
                                 unit.to.eldc.links.df,
                                 stpt.to.steeldc.links.df,
                                 import.links.df,
                                 FOB.links.df,
                                 uhv.links.df)

# Set 9999 capacity to missing (from international road connections)
all.links.df$cap_Mt[all.links.df$cap_Mt==9999] <-""
all.links.df$cap_Mt <- as.numeric(all.links.df$cap_Mt)

### Make it dual direction
vice.versa.df <- all.links.df %>% 
  filter(dual_dir=="yes")
vice.versa.df <- vice.versa.df %>% 
  rename(old_node_name = orig_node_name,
         old_node_type = orig_node_type) %>% 
  rename(orig_node_name = dest_node_name,
         orig_node_type = dest_node_type) %>% 
  rename(dest_node_name = old_node_name,
         dest_node_type = old_node_type)
### Add together
all.links.vv.df <- dplyr::bind_rows(all.links.df, vice.versa.df)
### Fix loops back to mines
all.links.vv.df <- all.links.vv.df %>% 
  filter(dest_node_type!="mine")

############# Node to node ID sheet
node.df <- all.links.vv.df %>% 
  select(orig_node_name, dest_node_name, orig_node_type, dest_node_type) %>% 
  distinct()
nodes.orig.df <- node.df %>% 
  select(orig_node_name, orig_node_type) %>% 
  rename(node_name = orig_node_name,
         node_type = orig_node_type)
nodes.dest.df <- node.df %>% 
  select(dest_node_name, dest_node_type) %>% 
  rename(node_name = dest_node_name,
         node_type = dest_node_type)
all.nodes.df <-  dplyr::bind_rows(nodes.orig.df,nodes.dest.df)
all.nodes.df <- all.nodes.df %>% 
  distinct()
all.nodes.df <- all.nodes.df %>% 
  dplyr::mutate(id_number = dplyr::row_number())
all.nodes.df$node_id <- paste(all.nodes.df$node_type, "idno", all.nodes.df$id_number, sep = "_")
all.nodes.df <- all.nodes.df %>% 
  select(node_name, node_id)

### Hook up with all edges vv sheet
all.links.vv.df <- left_join(all.links.vv.df, all.nodes.df, by = c("orig_node_name" = "node_name")) %>% 
  rename(orig_node_id = node_id)
all.links.vv.df <- left_join(all.links.vv.df, all.nodes.df, by = c("dest_node_name" = "node_name")) %>% 
  rename(dest_node_id = node_id)


################  Specific train line handling and trasnport costs
# Merge special rail lines
all.links.vv.df <- all.links.vv.df %>% 
  select(-railway_line) %>% 
  select(-line_id)
all.links.vv.df <- left_join(all.links.vv.df, special.rail.lines.df, by = c("orig_node_name"="origin"))%>% 
  rename(orig_line_id = line_id) %>% 
  select(-destination)
all.links.vv.df <- left_join(all.links.vv.df, special.rail.lines.df, by = c("dest_node_name"="destination")) %>% 
  rename(dest_line_id = line_id) %>% 
  select(-origin)

# Special per km rates
all.links.vv.df$orig_line_id <- ifelse(is.na(all.links.vv.df$orig_line_id), "foo", all.links.vv.df$orig_line_id)
all.links.vv.df$dest_line_id <- ifelse(is.na(all.links.vv.df$dest_line_id), "foo", all.links.vv.df$dest_line_id)
all.links.vv.df$transp.cost.rmb.ptkm <- ifelse(all.links.vv.df$orig_line_id=="10-DaQin" & all.links.vv.df$dest_line_id=="10-DaQin", 0.1331, all.links.vv.df$transp.cost.rmb.ptkm)
all.links.vv.df$transp.cost.rmb.ptkm <- ifelse(all.links.vv.df$orig_line_id=="MengJi" & all.links.vv.df$dest_line_id=="MengJi", 0.184, all.links.vv.df$transp.cost.rmb.ptkm)
all.links.vv.df$transp.cost.rmb.ptkm <- ifelse(all.links.vv.df$orig_line_id=="53-ShuoHuang" & all.links.vv.df$dest_line_id=="53-ShuoHuang", 0.12, all.links.vv.df$transp.cost.rmb.ptkm)
all.links.vv.df$transp.cost.rmb.ptkm <- ifelse(all.links.vv.df$orig_line_id=="35-WaRi" & all.links.vv.df$dest_line_id=="35-WaRi", 0.184, all.links.vv.df$transp.cost.rmb.ptkm)
all.links.vv.df$transp.cost.rmb.ptkm <- ifelse(all.links.vv.df$orig_line_id=="45-HaoJi" & all.links.vv.df$dest_line_id=="45-HaoJi", 0.184, all.links.vv.df$transp.cost.rmb.ptkm)
# Special handling charges
all.links.vv.df$handling.cost.rmb <- ifelse(all.links.vv.df$orig_node_type!="rwst" & all.links.vv.df$dest_line_id=="10-DaQin", 16.3, all.links.vv.df$handling.cost.rmb)
all.links.vv.df$handling.cost.rmb <- ifelse(all.links.vv.df$orig_node_type!="rwst" & all.links.vv.df$dest_line_id=="MengJi", 0, all.links.vv.df$handling.cost.rmb)
all.links.vv.df$handling.cost.rmb <- ifelse(all.links.vv.df$orig_node_type!="rwst" & all.links.vv.df$dest_line_id=="53-ShuoHuang", 20, all.links.vv.df$handling.cost.rmb)
all.links.vv.df$handling.cost.rmb <- ifelse(all.links.vv.df$orig_node_type!="rwst" & all.links.vv.df$dest_line_id=="35-WaRi", 0, all.links.vv.df$handling.cost.rmb)
all.links.vv.df$handling.cost.rmb <- ifelse(all.links.vv.df$orig_node_type!="rwst" & all.links.vv.df$dest_line_id=="45-HaoJi", 0, all.links.vv.df$handling.cost.rmb)

### Calc transport cost
all.links.vv.df$distance_km <- ifelse(is.na(all.links.vv.df$distance_km), 0, all.links.vv.df$distance_km)
all.links.vv.df$transp.cost.tot <- all.links.vv.df$handling.cost.rmb + all.links.vv.df$distance_km*all.links.vv.df$transp.cost.rmb.ptkm

### Add transmission cost for all links, may be useful to calculate
all.links.vv.df$transm_cost_rmb_GJ[is.na(all.links.vv.df$transm_cost_rmb_GJ)] <- 0
all.links.vv.df$transm_cost_rmb_PJ <- all.links.vv.df$transm_cost_rmb_GJ*1e-6

### Include RMB costs conversion
exch.rate.file <- file.path(build.input.path, "helper files/exchange rates.xlsx")
exch.rate.df <- read.xlsx(exch.rate.file, sheet = "Sheet1") 
all.links.vv.df <- left_join(all.links.vv.df, exch.rate.df, by = "year")
all.links.vv.df <- all.links.vv.df %>% rename(transp_cost_tot_rmb = transp.cost.tot)
all.links.vv.df$transp_cost_tot_usd <- all.links.vv.df$transp_cost_tot_rmb/all.links.vv.df$exch_rate_rmb_usd
all.links.vv.df$transm_cost_usd_GJ <- all.links.vv.df$transm_cost_rmb_GJ/all.links.vv.df$exch_rate_rmb_usd
all.links.vv.df$transm_cost_usd_PJ <- all.links.vv.df$transm_cost_rmb_PJ/all.links.vv.df$exch_rate_rmb_usd

### Order and select vars again
all.links.vv.df <- all.links.vv.df %>% 
  select(orig_node_name, dest_node_name, orig_node_type, dest_node_type, orig_node_id, dest_node_id,
         year, distance_km, transp_cost_tot_usd, transm_cost_usd_GJ, cap_Mt, cap_MW, cap_GW, conversion_eff)

#################  DROP DUPLICATE SEGMENST HERE (e.g., from city-to-city vice versa)
all.links.vv.df  <- all.links.vv.df %>% 
  group_by(orig_node_id, dest_node_id, year) %>% 
  mutate(count = seq(n())) %>% 
  filter(count==1) %>% 
  select(-count)

############# Electric energy capacity sheet
elec.edge.capa.df <- all.links.vv.df %>% 
  filter(orig_node_type=="eldc" & dest_node_type=="eldc" | orig_node_type=="pwun" & dest_node_type=="eldc")
elec.edge.capa.df$cap_GJ <- 0
elec.edge.capa.df$cap_GJ <- ifelse(elec.edge.capa.df$orig_node_type=="pwun", elec.edge.capa.df$cap_MW*8760*0.85*3.6, elec.edge.capa.df$cap_GJ) # MW * max hours * 3.6 GJ/MWh
elec.edge.capa.df$cap_GJ <- ifelse(elec.edge.capa.df$orig_node_type=="eldc", elec.edge.capa.df$cap_GW*1000*8760*1*3.6, elec.edge.capa.df$cap_GJ) # GW *1000 MW/GW * max hours * 3.6 GJ/MWh

############# Steel plant capacity sheet
# get steel plant file
stpt.info.file <- file.path(build.input.path, "Global Energy Monitor data/Global Steel Plant Tracker Feb 2021 China excerpt -- CENSORED.xlsx")
stpt.capa.df <- read.xlsx(stpt.info.file, sheet = "Steel Plant Data") 
# keep useful vars only
# Note capacity not needed here: this is limited in a separately created sheet for steel plant capacities
stpt.capa.df <-  stpt.capa.df %>% 
  select(stpt_node_name, oper_cap_2015:oper_cap_2030) %>% 
  rename(orig_node_name = stpt_node_name)
# reshape to long
stpt.capa.df <- stpt.capa.df %>% 
  pivot_longer(names_to = "year", values_to = "steel_cap_Mt", oper_cap_2015:oper_cap_2030)
# proper years
stpt.capa.df$year = str_replace(stpt.capa.df$year, "oper_cap_", "")
stpt.capa.df$year <- as.numeric(stpt.capa.df$year)
# Hook up node id's
stpt.capa.df <- stpt.capa.df %>% rename(node_name = orig_node_name)
stpt.capa.df <- left_join(stpt.capa.df, all.nodes.df, by = "node_name")
# For python dictionary split
stpt.capa.df$cap_steel_Mt <- stpt.capa.df$steel_cap_Mt

############# Port capacity sheet
# get port consolidated info file
port.info.file <- file.path(build.input.path, "port data/port loc and capa consolidated sources.xlsx")
port.capa.df <- read.xlsx(port.info.file) 
# keep useful vars only
port.capa.df <-  port.capa.df %>% 
  select(node_name, port_capa_2015:port_capa_2030)
# reshape to long
port.capa.df <- port.capa.df %>% 
  pivot_longer(names_to = "year", values_to = "cap_Mt", port_capa_2015:port_capa_2030)
# proper years
port.capa.df$year = str_replace(port.capa.df$year, "port_capa_", "")
port.capa.df$year <- as.numeric(port.capa.df$year)
# Hook up node id's
port.capa.df <- left_join(port.capa.df, all.nodes.df, by = "node_name")
# For python dictionary split
port.capa.df$port_cap_Mt = port.capa.df$cap_Mt

###################### Units and rounding
all.links.vv.df$conversion_eff <- round(all.links.vv.df$conversion_eff, 3)
all.links.vv.df$transp_cost_tot_usd <- round(all.links.vv.df$transp_cost_tot_usd, 2)
all.links.vv.df$transm_cost_usd_GJ <- round(all.links.vv.df$transm_cost_usd_GJ,1)
elec.edge.capa.df$cap_PJ <- elec.edge.capa.df$cap_GJ*1e-6
elec.edge.capa.df$cap_PJ <- round(elec.edge.capa.df$cap_PJ,1)
stpt.capa.df$steel_cap_Mt <- round(stpt.capa.df$steel_cap_Mt, 1)
port.capa.df$port_cap_Mt <- round(port.capa.df$port_cap_Mt, 1)

# ############################    Save to file   ##############################################
# Save network data latest
out.file.name <- "all edges vv plus costs and capa latest.xlsx"
outfile <- file.path(build.output.path, out.file.name)
write.xlsx(all.links.vv.df, outfile)
# Save port capacities
out.file.name <- "port capacities latest.xlsx"
outfile <- file.path(build.output.path, out.file.name)
write.xlsx(port.capa.df, outfile)
# Save electric capacities
out.file.name <- "electric capacities latest.xlsx"
outfile <- file.path(build.output.path, out.file.name)
write.xlsx(elec.edge.capa.df, outfile)
# Save steelplant capacities
out.file.name <- "steel prod capacities latest.xlsx"
outfile <- file.path(build.output.path, out.file.name)
write.xlsx(stpt.capa.df, outfile)
# Save node to node id file latest
out.file.name <- "node to node id helper latest.xlsx"
outfile <- file.path(build.output.path, out.file.name)
write.xlsx(all.nodes.df, outfile)
### csv versions
# edges
out.file.name <- "all edges vv plus costs and capa latest.csv"
outfile <- file.path(build.output.path, out.file.name)
con<-file(outfile, encoding="UTF-8")
write.csv(all.links.vv.df, file=con, row.names=FALSE)
# port capas
out.file.name <- "port capacities latest.csv"
outfile <- file.path(build.output.path, out.file.name)
con<-file(outfile, encoding="UTF-8")
write.csv(port.capa.df, file=con, row.names=FALSE)
# elec capas
out.file.name <- "electric capacities latest.csv"
outfile <- file.path(build.output.path, out.file.name)
con<-file(outfile, encoding="UTF-8")
write.csv(elec.edge.capa.df, file=con, row.names=FALSE)
# steelplant capas
out.file.name <- "steel prod capacities latest.csv"
outfile <- file.path(build.output.path, out.file.name)
con<-file(outfile, encoding="UTF-8")
write.csv(stpt.capa.df, file=con, row.names=FALSE)
# node to node id file
out.file.name <- "node to node id helper latest.csv"
outfile <- file.path(build.output.path, out.file.name)
con<-file(outfile, encoding="UTF-8")
write.csv(all.nodes.df, file=con, row.names=FALSE)