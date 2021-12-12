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

### range of years df to expand links with constant capacities
years <- as.data.frame(list(year = 2015:2030))

########### Production capacity
############# Import coal mine data
prod.capa.file <- file.path(build.input.path, "WoodMac data/selected china coal mine prod data woodmac -- CENSORED.csv")
prod.capa.df <- read.csv(prod.capa.file)  # has tab as separator
# Select useful columns
prod.capa.df <- prod.capa.df %>% 
  filter(Year>=2010) %>% 
  filter(Year<=2030)
# Keep operating and possible capacity
prod.capa.df <- prod.capa.df %>% 
  filter(Operating.Status=="Operating" | Operating.Status=="Possible")
# Keep useful vars
prod.capa.summ.df <- prod.capa.df %>% 
  select(Mine.Name, Product.Type, Product.Subtype, Product.Market, Market.Type, Operating.Status, Year, Metric.Value..Real.)
# Sum by mine name: different values for coastal and non-coastal markets should be added together to say something about total production capacity
prod.capa.summ.df <- prod.capa.summ.df %>%
  group_by(Mine.Name, Product.Type, Product.Subtype, Operating.Status, Year) %>% 
  summarise(capa_Mt = sum(Metric.Value..Real.))
# Wide on operating and possible
prod.capa.summ.df <- prod.capa.summ.df %>%
  spread(Operating.Status, capa_Mt)
# Set missing to zero
prod.capa.summ.df$Operating[is.na(prod.capa.summ.df$Operating)] <- 0
prod.capa.summ.df$Possible[is.na(prod.capa.summ.df$Possible)] <- 0
prod.capa.summ.df$capa_total_Mt <- prod.capa.summ.df$Operating + prod.capa.summ.df$Possible
# Total output of all products per year 
prod.capa.summ.df <- prod.capa.summ.df %>%
  group_by(Mine.Name, Year) %>% 
  mutate(capa_Mt_minelevel = sum(capa_total_Mt))

### Before joiningg: Standard mine naming. 
### Needed because WoodMac gets angry when you tell them they have inconsistent capitalization and trailing spaces in their coal quality vs coal mine data
prod.capa.summ.df$Mine.Name <- as.character(prod.capa.summ.df$Mine.Name)
prod.capa.summ.df$Mine.Name[prod.capa.summ.df$Mine.Name=="Ningxia KSOE Surface "] <- "Ningxia KSOE Surface"
prod.capa.summ.df$Mine.Name[prod.capa.summ.df$Mine.Name=="Ordos Generic Surface "] <- "Ordos Generic Surface"
prod.capa.summ.df$Mine.Name[prod.capa.summ.df$Mine.Name=="Ordos KSOE Surface "] <- "Ordos KSOE Surface"
prod.capa.summ.df$Mine.Name[prod.capa.summ.df$Mine.Name=="Shanxi (North) KSOE Surface "] <- "Shanxi (North) KSOE Surface"
prod.capa.summ.df$Mine.Name[prod.capa.summ.df$Mine.Name=="Xinjiang KSOE Surface "] <- "Xinjiang KSOE Surface"
prod.capa.summ.df$Mine.Name[prod.capa.summ.df$Mine.Name=="Hebei KSOE export"] <- "Hebei KSOE Export"
prod.capa.summ.df$Mine.Name[prod.capa.summ.df$Mine.Name=="Ordos KSOE export"] <- "Ordos KSOE Export"
prod.capa.summ.df$Mine.Name[prod.capa.summ.df$Mine.Name=="Shaanxi KSOE export"] <- "Shaanxi KSOE Export"
prod.capa.summ.df$Mine.Name[prod.capa.summ.df$Mine.Name=="Shandong KSOE export"] <- "Shandong KSOE Export"
prod.capa.summ.df$Mine.Name[prod.capa.summ.df$Mine.Name=="Shanxi (East) KSOE export"] <- "Shanxi (East) KSOE Export"
prod.capa.summ.df$Mine.Name[prod.capa.summ.df$Mine.Name=="Shanxi (North) KSOE export"] <- "Shanxi (North) KSOE Export"
prod.capa.summ.df$Mine.Name[prod.capa.summ.df$Mine.Name=="Shanxi (East) other mines"] <- "Shanxi (East) Other Mines"
prod.capa.summ.df$Mine.Name[prod.capa.summ.df$Mine.Name=="Shanxi (West) other mines"] <- "Shanxi (West) Other Mines"
prod.capa.summ.df$Mine.Name[prod.capa.summ.df$Mine.Name=="Shanxi (North) other mines"] <- "Shanxi (North) Other Mines"
prod.capa.summ.df$Mine.Name[prod.capa.summ.df$Mine.Name=="Shanxi (North) other mines surface"] <- "Shanxi (North) Other Mines Surface"

# Keep this variable name as the max annual output by product, this is what is referenced in rest of script
# Total here meaning total output for different markets (onshore or coastal), not total over different products
prod.capa.summ.df$max_capa_Mt <- prod.capa.summ.df$capa_total_Mt

################ Cost and prices
cost.prices.file <- file.path(build.input.path, "WoodMac data/selected china coal mine cost data woodmac -- CENSORED.csv")
cost.prices.df <- read.csv(cost.prices.file)  
# Select useful years
cost.prices.df <- cost.prices.df %>% 
  filter(Year>=2010) %>% 
  filter(Year<=2030)
# df for summarizing
cost.prices.summ.df <- cost.prices.df %>% 
  select(Mine.Name, Company, Product.Type, Product.Subtype, Year, Product.Market, Market.Type, Metric, Metric.Value..Nominal., Unit)
#Concatenate metric and unit before making it wide
cost.prices.summ.df <- cost.prices.summ.df %>% 
  unite(quality, c("Metric", "Unit"))
# Make it wide
cost.prices.summ.df <- cost.prices.summ.df %>% 
  spread(quality, Metric.Value..Nominal.)
# Getting distinct rows: mine names duplicated because of mulitple owners or because of multiple markets served (e.g., coastal and non-coastal markets)
# Also some zero value, for mines which do not have any production.
# Bulk of mines with multiple owners or markets has exact same costs repeated over different lines, some have small differences.
# To deal with this all in one go, we set zero values to 999, and select the row with the lowest mining value
# Tis will not be the zero value, most usually the same value as other in other lines, and the lower of two values for which there is little explanation in other cases
cost.prices.summ.df$`Mining_US$/t` <- ifelse(cost.prices.summ.df$`Mining_US$/t`==0,999,cost.prices.summ.df$`Mining_US$/t`)
cost.prices.summ.df <- cost.prices.summ.df %>%
  group_by(Mine.Name, Product.Type, Product.Subtype, Year) %>%
  arrange(`Mining_US$/t`) %>% 
  mutate(count = seq(n())) %>% 
  filter(count==1) %>% 
  select(-count)
# Fix costs that are missing; set to zero for use in calculation later on.
# These are missing because we dropped all zero value lines.
cost.prices.summ.df$`C1 cash cost_US$/t` <- ifelse(is.na(cost.prices.summ.df$`C1 cash cost_US$/t`), 0, cost.prices.summ.df$`C1 cash cost_US$/t`)
cost.prices.summ.df$`Coal preparation_US$/t` <- ifelse(is.na(cost.prices.summ.df$`Coal preparation_US$/t`), 0, cost.prices.summ.df$`Coal preparation_US$/t`)
cost.prices.summ.df$`Mining_US$/t` <- ifelse(is.na(cost.prices.summ.df$`Mining_US$/t`), 0, cost.prices.summ.df$`Mining_US$/t`)
cost.prices.summ.df$`Overheads_US$/t` <- ifelse(is.na(cost.prices.summ.df$`Overheads_US$/t`), 0, cost.prices.summ.df$`Overheads_US$/t`)
cost.prices.summ.df$`Port_US$/t` <- ifelse(is.na(cost.prices.summ.df$`Port_US$/t`), 0, cost.prices.summ.df$`Port_US$/t`)
cost.prices.summ.df$`Product Transport_US$/t` <- ifelse(is.na(cost.prices.summ.df$`Product Transport_US$/t`), 0, cost.prices.summ.df$`Product Transport_US$/t`)
cost.prices.summ.df$`Royalty and Levies_US$/t` <- ifelse(is.na(cost.prices.summ.df$`Royalty and Levies_US$/t`), 0, cost.prices.summ.df$`Royalty and Levies_US$/t`)

########### Costs and Prices
# Use a generic handling fee. This would be part of coal_prep costs as reported by WoodMac
# Use 16.5 rmb, or 2.35 USD
cost.prices.summ.df$generic_handling_cost <- 2.35
cost.prices.summ.df$generic_handling_cost <- ifelse(cost.prices.summ.df$`Coal preparation_US$/t`<2.35, cost.prices.summ.df$`Coal preparation_US$/t`, cost.prices.summ.df$generic_handling_cost)
# Price ex transport and handling costs
cost.prices.summ.df$price_ex_transp <- cost.prices.summ.df$`Price_US$/t`- cost.prices.summ.df$`Port_US$/t`- cost.prices.summ.df$`Product Transport_US$/t`- cost.prices.summ.df$generic_handling_cost
# Costs ex royalties, transport and handling costs
cost.prices.summ.df$c1_cash_cost_ex_transp <- cost.prices.summ.df$`Mining_US$/t`+cost.prices.summ.df$`Overheads_US$/t`+cost.prices.summ.df$`Coal preparation_US$/t`- cost.prices.summ.df$generic_handling_cost
# Costs ex transport and handling costs
cost.prices.summ.df$total_cash_cost_ex_transp <- cost.prices.summ.df$c1_cash_cost_ex_transp + cost.prices.summ.df$`Royalty and Levies_US$/t`

### Housekeeping: standard mine naming
cost.prices.summ.df$Mine.Name <- as.character(cost.prices.summ.df$Mine.Name)
cost.prices.summ.df$Mine.Name[cost.prices.summ.df$Mine.Name=="Ningxia KSOE Surface "] <- "Ningxia KSOE Surface"
cost.prices.summ.df$Mine.Name[cost.prices.summ.df$Mine.Name=="Ordos Generic Surface "] <- "Ordos Generic Surface"
cost.prices.summ.df$Mine.Name[cost.prices.summ.df$Mine.Name=="Ordos KSOE Surface "] <- "Ordos KSOE Surface"
cost.prices.summ.df$Mine.Name[cost.prices.summ.df$Mine.Name=="Shanxi (North) KSOE Surface "] <- "Shanxi (North) KSOE Surface"
cost.prices.summ.df$Mine.Name[cost.prices.summ.df$Mine.Name=="Xinjiang KSOE Surface "] <- "Xinjiang KSOE Surface"
cost.prices.summ.df$Mine.Name[cost.prices.summ.df$Mine.Name=="Hebei KSOE export"] <- "Hebei KSOE Export"
cost.prices.summ.df$Mine.Name[cost.prices.summ.df$Mine.Name=="Ordos KSOE export"] <- "Ordos KSOE Export"
cost.prices.summ.df$Mine.Name[cost.prices.summ.df$Mine.Name=="Shaanxi KSOE export"] <- "Shaanxi KSOE Export"
cost.prices.summ.df$Mine.Name[cost.prices.summ.df$Mine.Name=="Shandong KSOE export"] <- "Shandong KSOE Export"
cost.prices.summ.df$Mine.Name[cost.prices.summ.df$Mine.Name=="Shanxi (East) KSOE export"] <- "Shanxi (East) KSOE Export"
cost.prices.summ.df$Mine.Name[cost.prices.summ.df$Mine.Name=="Shanxi (North) KSOE export"] <- "Shanxi (North) KSOE Export"
cost.prices.summ.df$Mine.Name[cost.prices.summ.df$Mine.Name=="Shanxi (East) other mines"] <- "Shanxi (East) Other Mines"
cost.prices.summ.df$Mine.Name[cost.prices.summ.df$Mine.Name=="Shanxi (West) other mines"] <- "Shanxi (West) Other Mines"
cost.prices.summ.df$Mine.Name[cost.prices.summ.df$Mine.Name=="Shanxi (North) other mines"] <- "Shanxi (North) Other Mines"
cost.prices.summ.df$Mine.Name[cost.prices.summ.df$Mine.Name=="Shanxi (North) other mines surface"] <- "Shanxi (North) Other Mines Surface"

################# Hook up coal brands
### Import coal quality data
coal.quality.file <- file.path(build.input.path, "WoodMac data/selected coal quality data by mine and type -- CENSORED.csv")
coal.quality.df <- read.csv(coal.quality.file)  # has tab as separator
hcc.quality.df <- coal.quality.df %>% 
  select(Mine.Name, Product.Type, Product.Subtype, Coal.Brand, Quality.Metric, Value) %>% 
  filter(Product.Subtype=="HCC") %>% 
  filter(Quality.Metric=="CSR") %>% 
  group_by(Mine.Name) %>%
  dplyr::mutate(row_number = dplyr::row_number())%>% 
  filter(row_number==1) 
# Define HCC price premia
hcc.quality.df$hcc.premium <- 0
hcc.quality.df$hcc.premium <- ifelse(hcc.quality.df$Value>=55, 7, hcc.quality.df$hcc.premium)
hcc.quality.df$hcc.premium <- ifelse(hcc.quality.df$Value>=60, 15, hcc.quality.df$hcc.premium)
hcc.quality.df$hcc.premium <- ifelse(hcc.quality.df$Value>=65, 32, hcc.quality.df$hcc.premium)
hcc.quality.df$hcc.premium <- ifelse(hcc.quality.df$Value>=70, 39, hcc.quality.df$hcc.premium)
hcc.quality.df <- hcc.quality.df%>% 
  select(Mine.Name, Product.Subtype, hcc.premium)
# Select useful columns
coal.quality.df <- coal.quality.df %>% 
  select(Mine.Name, Product.Type, Product.Subtype, Coal.Brand) %>% 
  distinct()
coal.quality.df$Coal.Brand <- as.character(coal.quality.df$Coal.Brand)
coal.quality.df$Coal.Brand[coal.quality.df$Mine.Name=="Shandong KSOE Export" & coal.quality.df$Product.Subtype=="HCC"] <- "Shandong Export HCC"
coal.quality.df$Coal.Brand[coal.quality.df$Mine.Name=="West Tsankhi (New Tavan Tolgoi)" & coal.quality.df$Product.Subtype=="Thermal"] <- "West Tsankhi Thermal"
# Manual fix for Shanxi Luan PCI
coal.quality.df$Coal.Brand[coal.quality.df$Mine.Name=="Shanxi (East) KSOE_Shanxi Luan" & coal.quality.df$Coal.Brand=="FOB PCI" & coal.quality.df$Product.Subtype=="SCC"] <- "FOB SCC"
coal.quality.df$Coal.Brand[coal.quality.df$Mine.Name=="Shanxi (East) KSOE_Shanxi Luan" & coal.quality.df$Coal.Brand=="FOT PCI" & coal.quality.df$Product.Subtype=="SCC"] <- "FOT SCC"
# Hook it up
prod.capa.price.qual.df <- left_join(prod.capa.summ.df, coal.quality.df, by = c("Mine.Name", "Product.Type", "Product.Subtype"))
prod.capa.price.qual.df <- left_join(prod.capa.price.qual.df, cost.prices.summ.df, by = c("Mine.Name", "Product.Type", "Product.Subtype", "Year"))
prod.capa.price.qual.df <- prod.capa.price.qual.df %>% 
  select(Mine.Name, Product.Type, Product.Subtype, Coal.Brand, Year, max_capa_Mt, 
         total_cash_cost_ex_transp, c1_cash_cost_ex_transp, price_ex_transp, `Price_US$/t`, `C1 cash cost_US$/t`, 
         `Mining_US$/t`, `Coal preparation_US$/t`, `Overheads_US$/t`, `Port_US$/t`,  `Product Transport_US$/t`, `Royalty and Levies_US$/t`) %>% 
  rename(mine_node_name = Mine.Name, 
         prod_type = Product.Type, 
         prod_subtype = Product.Subtype, 
         coal_brand = Coal.Brand, 
         year = Year, 
         prod_capa_Mt = max_capa_Mt, 
         price_ex_transp = price_ex_transp, 
         price_usd_pt = `Price_US$/t`, 
         c1cashcost_usd_pt = `C1 cash cost_US$/t`, 
         mining_cost_usd_pt = `Mining_US$/t`, 
         coal_prep_cost_usd_pt = `Coal preparation_US$/t`,
         overhead_cost_usd_pt = `Overheads_US$/t`, 
         port_cost_usd_pt = `Port_US$/t`,  
         transp_cost_usd_pt = `Product Transport_US$/t`,
         royalties_usd_pt = `Royalty and Levies_US$/t`)
# put back in coal quality df
hcc.quality.df <- hcc.quality.df %>% 
  rename(mine_node_name = Mine.Name, 
         prod_subtype = Product.Subtype)
prod.capa.price.qual.df <- left_join(prod.capa.price.qual.df, hcc.quality.df, by=c("mine_node_name", "prod_subtype"))
prod.capa.price.qual.df$hcc.premium <- ifelse(is.na(prod.capa.price.qual.df$hcc.premium), 0, prod.capa.price.qual.df$hcc.premium)
# Apply HCC price premium to domestic and Mongolian HCC coals
prod.capa.price.qual.df$total_cash_cost_ex_transp_ex_premium <- prod.capa.price.qual.df$total_cash_cost_ex_transp
prod.capa.price.qual.df$c1_cash_cost_ex_transp_ex_premium <- prod.capa.price.qual.df$c1_cash_cost_ex_transp
prod.capa.price.qual.df$c1cashcost_usd_pt_ex_premium <- prod.capa.price.qual.df$c1cashcost_usd_pt
prod.capa.price.qual.df$total_cash_cost_ex_transp <- prod.capa.price.qual.df$total_cash_cost_ex_transp - prod.capa.price.qual.df$hcc.premium
prod.capa.price.qual.df$c1_cash_cost_ex_transp <- prod.capa.price.qual.df$c1_cash_cost_ex_transp - prod.capa.price.qual.df$hcc.premium
prod.capa.price.qual.df$c1cashcost_usd_pt <- prod.capa.price.qual.df$c1cashcost_usd_pt - prod.capa.price.qual.df$hcc.premium
# Rename mine name to node name
prod.capa.price.qual.df$mine_node_name <- str_c("mine ", prod.capa.price.qual.df$mine_node_name, sep = "", collapse = NULL)

###################### node ids
node.id.file <- file.path(build.output.path, "node to node id helper latest.xlsx")
node.id.df <- read_excel(node.id.file) 
# rename to node_name
prod.capa.price.qual.df <- prod.capa.price.qual.df %>% rename(node_name = mine_node_name)
prod.capa.price.qual.df <- left_join(prod.capa.price.qual.df, node.id.df, by = "node_name")
#### Manual removal of Mongolian mines
prod.capa.price.qual.df <- prod.capa.price.qual.df %>% 
  filter(node_name!= "mine Aduun Chuluun") %>% 
  filter(node_name!= "mine Baganuur") %>% 
  filter(node_name!= "mine Sharyn Gol") %>% 
  filter(node_name!= "mine Aduun Chuluun") %>% 
  filter(node_name!= "mine Shivee Ovoo") %>% 
  filter(node_name!= "mine Shivee Ovoo Expansion")

#### Manual correction of coal qualities
prod.capa.price.qual.df$coal_brand[prod.capa.price.qual.df$node_name=="mine Hebei KSOE export" & prod.capa.price.qual.df$prod_subtype== "HCC"] <- "International HCC"
prod.capa.price.qual.df$coal_brand[prod.capa.price.qual.df$node_name=="mine Ordos KSOE export" & prod.capa.price.qual.df$prod_subtype== "Bituminous"] <- "International 6000"
prod.capa.price.qual.df$coal_brand[prod.capa.price.qual.df$node_name=="mine Shaanxi KSOE export" & prod.capa.price.qual.df$prod_subtype== "Bituminous"] <- "International 6000"
prod.capa.price.qual.df$coal_brand[prod.capa.price.qual.df$node_name=="mine Shandong KSOE export" & prod.capa.price.qual.df$prod_subtype== "HCC"] <- "Shandong Export HCC"
prod.capa.price.qual.df$coal_brand[prod.capa.price.qual.df$node_name=="mine Shandong KSOE export" & prod.capa.price.qual.df$prod_subtype== "Bituminous"] <- "International 5500"
prod.capa.price.qual.df$coal_brand[prod.capa.price.qual.df$node_name=="mine Shanxi (East) KSOE export" & prod.capa.price.qual.df$prod_subtype== "Anthracite"] <- "International 6500"
# Presumed non-export market, so FOT and not FOB
prod.capa.price.qual.df$coal_brand[prod.capa.price.qual.df$node_name=="mine Shanxi (East) other mines" & prod.capa.price.qual.df$prod_subtype== "HCC"] <- "FOT HCC"
prod.capa.price.qual.df$coal_brand[prod.capa.price.qual.df$node_name=="mine Shanxi (East) other mines" & prod.capa.price.qual.df$prod_subtype== "PCI"] <- "FOT PCI"
prod.capa.price.qual.df$coal_brand[prod.capa.price.qual.df$node_name=="mine Shanxi (East) other mines" & prod.capa.price.qual.df$prod_subtype== "SCC"] <- "FOT SCC"
prod.capa.price.qual.df$coal_brand[prod.capa.price.qual.df$node_name=="mine Shanxi (East) other mines" & prod.capa.price.qual.df$prod_subtype== "Anthracite"] <- "FOT 6500"
prod.capa.price.qual.df$coal_brand[prod.capa.price.qual.df$node_name=="mine Shanxi (East) other mines" & prod.capa.price.qual.df$prod_subtype== "Bituminous"] <- "FOT 5500"
prod.capa.price.qual.df$coal_brand[prod.capa.price.qual.df$node_name=="mine Shanxi (North) KSOE export" & prod.capa.price.qual.df$prod_subtype== "Bituminous"] <- "International 5500"
prod.capa.price.qual.df$coal_brand[prod.capa.price.qual.df$node_name=="mine Shanxi (North) other mines" & prod.capa.price.qual.df$prod_subtype== "SCC"] <- "FOT SCC"
prod.capa.price.qual.df$coal_brand[prod.capa.price.qual.df$node_name=="mine Shanxi (North) other mines" & prod.capa.price.qual.df$prod_subtype== "Bituminous"] <- "FOT 5000"
prod.capa.price.qual.df$coal_brand[prod.capa.price.qual.df$node_name=="mine Shanxi (North) other mines surface" & prod.capa.price.qual.df$prod_subtype== "Bituminous"] <- "FOT 5000"
prod.capa.price.qual.df$coal_brand[prod.capa.price.qual.df$node_name=="mine Shanxi (West) other mines" & prod.capa.price.qual.df$prod_subtype== "HCC"] <- "FOT HCC"
prod.capa.price.qual.df$coal_brand[prod.capa.price.qual.df$node_name=="mine Shanxi (West) other mines" & prod.capa.price.qual.df$prod_subtype== "SCC"] <- "FOT SCC"
prod.capa.price.qual.df$coal_brand[prod.capa.price.qual.df$node_name=="mine Shanxi (West) other mines" & prod.capa.price.qual.df$prod_subtype== "PCI"] <- "FOT PCI"
prod.capa.price.qual.df$coal_brand[prod.capa.price.qual.df$node_name=="mine Shanxi (West) other mines" & prod.capa.price.qual.df$prod_subtype== "Bituminous"] <- "FOT 5500"
prod.capa.price.qual.df$coal_brand[prod.capa.price.qual.df$node_name=="mine Shanxi (West) other mines" & prod.capa.price.qual.df$prod_subtype== "Anthracite"] <- "FOT 6500"
prod.capa.price.qual.df$coal_brand[prod.capa.price.qual.df$node_name=="mine Xinjiang KSOE Surface " & prod.capa.price.qual.df$prod_subtype== "HCC"] <- "Xinjiang HCC"
prod.capa.price.qual.df$coal_brand[prod.capa.price.qual.df$node_name=="mine Xinjiang KSOE Surface " & prod.capa.price.qual.df$prod_subtype== "SCC"] <- "Xinjiang SCC"
prod.capa.price.qual.df$coal_brand[prod.capa.price.qual.df$node_name=="mine Xinjiang KSOE Surface " & prod.capa.price.qual.df$prod_subtype== "Bituminous"] <- "Xinjiang 5000"
# Other name fixes
prod.capa.price.qual.df$coal_brand[prod.capa.price.qual.df$node_name=="mine Ningxia KSOE Surface" & prod.capa.price.qual.df$prod_subtype== "HCC"] <- "Ningxia HCC"
prod.capa.price.qual.df$coal_brand[prod.capa.price.qual.df$node_name=="mine Ningxia KSOE Surface" & prod.capa.price.qual.df$prod_subtype== "PCI"] <- "Ningxia PCI"
prod.capa.price.qual.df$coal_brand[prod.capa.price.qual.df$node_name=="mine Ningxia KSOE Surface" & prod.capa.price.qual.df$prod_subtype== "Bituminous"] <- "Ningxia 5100"
prod.capa.price.qual.df$coal_brand[prod.capa.price.qual.df$node_name=="mine Ningxia KSOE Surface" & prod.capa.price.qual.df$prod_subtype== "Anthracite"] <- "Ningxia Anthracite"
prod.capa.price.qual.df$coal_brand[prod.capa.price.qual.df$node_name=="mine Ordos Generic Surface" & prod.capa.price.qual.df$prod_subtype== "Bituminous"] <- "Ordos 5500"
prod.capa.price.qual.df$coal_brand[prod.capa.price.qual.df$node_name=="mine Ordos KSOE Surface" & prod.capa.price.qual.df$prod_subtype== "Bituminous"] <- "Ordos 5500"
prod.capa.price.qual.df$coal_brand[prod.capa.price.qual.df$node_name=="mine Shanxi (North) KSOE Surface" & prod.capa.price.qual.df$prod_subtype== "Bituminous"] <- "FOT 5000"

###################### coal group
coal.grp.file <- file.path(build.output.path, "coal qualities summary latest.xlsx")
coal.grp.df <- read_excel(coal.grp.file)%>% 
  select(coal_brand, coal_group)
# rename to node_name
prod.capa.price.qual.df <- left_join(prod.capa.price.qual.df, coal.grp.df, by = "coal_brand")

# var ordering and selection
prod.capa.price.qual.df <- prod.capa.price.qual.df %>% 
  select(node_name, node_id, prod_type, prod_subtype, coal_brand, coal_group, year,
         prod_capa_Mt, total_cash_cost_ex_transp, c1_cash_cost_ex_transp, price_ex_transp, 
         price_usd_pt, c1cashcost_usd_pt, mining_cost_usd_pt, coal_prep_cost_usd_pt,
         overhead_cost_usd_pt, port_cost_usd_pt, transp_cost_usd_pt, royalties_usd_pt)

################# prep international seaborne suppliers' info. Much repetition of above here. Dont care.
### All thermal coals
intern.therm.file  <- file.path(build.input.path, "WoodMac data/Seaborne Export Thermal - non energy adjusted Worksheet -- CENSORED.xlsx")
intern.therm.df <- read.xlsx(intern.therm.file, sheet = "worksheet navo mines connectors") 
# Remove Chinese and Mongolian mines: these are already in our dataset
intern.therm.df <- intern.therm.df %>% 
  filter(Region!="China") %>% 
  filter(Region!="Mongolia")
# product type
intern.therm.df$prod_type <- "Thermal"
# years
intern.therm.df <- merge(intern.therm.df, years) %>% 
  arrange(Mine, year)
### Expansions REQ
# # Committed
# intern.therm.df$Mt <- ifelse(intern.therm.df$Mine=="Wambo" & intern.therm.df$year>=2021, 9.5, intern.therm.df$Mt)
# intern.therm.df$Mt <- ifelse(intern.therm.df$Mine=="Stratford" & intern.therm.df$year>=2021, 1.6, intern.therm.df$Mt)
# intern.therm.df$Mt <- ifelse(intern.therm.df$Mine=="Mt Owen Complex" & intern.therm.df$year>=2021, 6.5, intern.therm.df$Mt) # Expansion to continue operations beyond 2022
# # feasible
# intern.therm.df$Mt <- ifelse(intern.therm.df$Mine=="Airly" & intern.therm.df$year>=2022, 2, intern.therm.df$Mt)
# intern.therm.df$Mt <- ifelse(intern.therm.df$Mine=="Baralaba" & intern.therm.df$year>=2023, 3.3, intern.therm.df$Mt)
# intern.therm.df$Mt <- ifelse(intern.therm.df$Mine=="Bulga" & intern.therm.df$year>=2024, 14.7, intern.therm.df$Mt)
# intern.therm.df$Mt <- ifelse(intern.therm.df$Mine=="Dendrobium" & intern.therm.df$year>=2024, 4.9, intern.therm.df$Mt)
# intern.therm.df$Mt <- ifelse(intern.therm.df$Mine=="Isaac Plains" & intern.therm.df$year>=2027, 1.3, intern.therm.df$Mt)
# intern.therm.df$Mt <- ifelse(intern.therm.df$Mine=="Mandalong" & intern.therm.df$year>=2021, 1.3, intern.therm.df$Mt)
# intern.therm.df$Mt <- ifelse(intern.therm.df$Mine=="Mt Pleasant" & intern.therm.df$year>=2027, 12.2, intern.therm.df$Mt)
# intern.therm.df$Mt <- ifelse(intern.therm.df$Mine=="Narrabri" & intern.therm.df$year>=2027, 13.5, intern.therm.df$Mt)
# intern.therm.df$Mt <- ifelse(intern.therm.df$Mine=="New Acland" & intern.therm.df$year>=2023, 8.3, intern.therm.df$Mt)
# intern.therm.df$Mt <- ifelse(intern.therm.df$Mine=="Rolleston" & intern.therm.df$year>=2026, 17.72, intern.therm.df$Mt)

### All Met coals
intern.met.file  <- file.path(build.input.path, "WoodMac data/Seaborne Export Met Worksheet -- CENSORED.xlsx")
intern.met.df <- read.xlsx(intern.met.file, sheet = "worksheet navo mines connectors") 
# Remove Chinese and Mongolian mines: these are already in our dataset
# Also remove Australian mines as these are in a separate sheet with more info
intern.met.df <- intern.met.df %>% 
  filter(Region!="China") %>% 
  filter(Region!="Mongolia") %>% 
  filter(Region!="Australia")
# product type
intern.met.df$prod_type <- "Metallurgical"
# Fix: if price less than $125 its not going to be HCC. Assume its PCI not HCC
intern.met.df$Met.type <- ifelse(intern.met.df$price<125, "PCI", intern.met.df$Met.type) 
# Fix: if price less than $125 and its from Indonesia, assume its SCC
intern.met.df$Met.type <- ifelse(intern.met.df$ctry_code=="IDN" & intern.met.df$price<125, "SCC", intern.met.df$Met.type) 
# Fix: if price less than $95, assume its SCC
intern.met.df$Met.type <- ifelse(intern.met.df$price<95, "SCC", intern.met.df$Met.type) 
# years
intern.met.df <- merge(intern.met.df, years)

#### all intern mines df
all.intern.coals.df <- dplyr::bind_rows(intern.therm.df, intern.met.df)

### Standard naming conventions
all.intern.coals.df <- all.intern.coals.df %>% 
  rename(node_name = orig_node_name,
         prod_capa_Mt = Mt, 
         price_usd_pt = price, 
         c1cashcost_usd_pt = C1.Cash.Cost, 
         mining_cost_usd_pt = Mining, 
         coal_prep_cost_usd_pt = Coal.Preparation,
         overhead_cost_usd_pt =Overheads, 
         port_cost_usd_pt= Port, 
         transp_cost_usd_pt = Transport, 
         royalties_usd_pt = `Royalty.&.Levies`,
         total_cash_cost = Total.Cash.Cost)

### Australian met coals
aus.met.file  <- file.path(build.input.path, "WoodMac data/Australia met coal worksheet -- CENSORED.xlsx")
aus.met.df <- read.xlsx(aus.met.file, sheet = "Met coal operating") 
# Exclude everthing but operating here; probable/feaisble mines are dealt with separately
aus.met.df <- aus.met.df %>% 
  filter(Operating.Status!="Suspended")
# Summ to mine name and subtype
aus.met.df <- aus.met.df %>%
  group_by(Mine.Name,Product.Subtype) %>% 
  mutate(`Production.(Mt)`=sum(`Production.(Mt)`)) %>% 
  dplyr::mutate(row_number = dplyr::row_number()) %>% 
  filter(row_number==1)
# years
aus.met.df <- merge(aus.met.df, years) %>% 
  arrange(Mine.Name, year) %>% 
  select(-Year)
### Expansions WM
# Probably/ highly probably WM, set to zero for years prior to operation:
aus.met.df$`Production.(Mt)` <- ifelse(aus.met.df$Mine.Name=="United Wambo" & aus.met.df$year<2022, 0, aus.met.df$`Production.(Mt)`)
aus.met.df$`Production.(Mt)` <- ifelse(aus.met.df$Mine.Name=="Fairhill" & aus.met.df$year<2024, 0, aus.met.df$`Production.(Mt)`)
aus.met.df$`Production.(Mt)` <- ifelse(aus.met.df$Mine.Name=="Tahmoor South" & aus.met.df$year<2022, 0, aus.met.df$`Production.(Mt)`)
aus.met.df$`Production.(Mt)` <- ifelse(aus.met.df$Mine.Name=="Wilton" & aus.met.df$year<2024, 0, aus.met.df$`Production.(Mt)`)
# # Expansions REQ
# aus.met.df$`Production.(Mt)` <- ifelse(aus.met.df$Mine.Name=="Mt Owen Complex" & aus.met.df$year>=2022, 1.9, aus.met.df$`Production.(Mt)`)
# aus.met.df$`Production.(Mt)` <- ifelse(aus.met.df$Mine.Name=="Stratford" & aus.met.df$year>=2022, 1.9, aus.met.df$`Production.(Mt)`)
# aus.met.df$`Production.(Mt)` <- ifelse(aus.met.df$Mine.Name=="Baralaba" & aus.met.df$year>=2023, 3, aus.met.df$`Production.(Mt)`)
# aus.met.df$`Production.(Mt)` <- ifelse(aus.met.df$Mine.Name=="Bulga" & aus.met.df$year>=2024, 1, aus.met.df$`Production.(Mt)`)
# aus.met.df$`Production.(Mt)` <- ifelse(aus.met.df$Mine.Name=="Isaac Plains" & aus.met.df$year>=2023, 3.32, aus.met.df$`Production.(Mt)`)
# aus.met.df$`Production.(Mt)` <- ifelse(aus.met.df$Mine.Name=="Ashton (Underground)" & aus.met.df$year>=2027, 2.4, aus.met.df$`Production.(Mt)`)
# aus.met.df$`Production.(Mt)` <- ifelse(aus.met.df$Mine.Name=="Dendrobium" & aus.met.df$year>=2027, 4.2, aus.met.df$`Production.(Mt)`)
# aus.met.df$`Production.(Mt)` <- ifelse(aus.met.df$Mine.Name=="Grosvenor" & aus.met.df$year>=2027, 8.5, aus.met.df$`Production.(Mt)`)
# aus.met.df$`Production.(Mt)` <- ifelse(aus.met.df$Mine.Name=="Narrabri" & aus.met.df$year>=2027, 2, aus.met.df$`Production.(Mt)`)

### Standard naming conventions
aus.met.df <- aus.met.df %>% 
  rename(Mine = Mine.Name,
         node_name = orig_node_name,
         prod_capa_Mt = `Production.(Mt)`, 
         Region = Country,
         prod_type = Product.Type,
         Met.type = Product.Subtype,
         price_usd_pt = `Mine.Price.($US/t)`, 
         coal_prep_cost_usd_pt = `Coal.Preparation.($US/t)`,
         mining_cost_usd_pt = `Mining.($US/t)`, 
         overhead_cost_usd_pt =`Overheads.($US/t)`, 
         port_cost_usd_pt= `Port.($US/t)`, 
         transp_cost_usd_pt = `Product.Transport.($US/t)`,          
         royalties_usd_pt = `Royalty.and.Levies.($US/t)`,
         c1cashcost_usd_pt = `C1.Cash.Cost.($US/t)`, 
         total_cash_cost = `Total.Cash.Cost.($US/t)`,
         Operating.Margin = `Margin.($US/t)`)

# Append all international coal info
all.intern.coals.df <- dplyr::bind_rows(all.intern.coals.df, aus.met.df)

### Get other vars as in domestic info df
# Hook up mine info
node.id.helper.file <- file.path(build.output.path, "node to node id helper latest.xlsx")
node.id.helper.df <- read_excel(node.id.helper.file) 
all.intern.coals.df <- left_join(all.intern.coals.df, node.id.helper.df, by = "node_name")

# add product subtype: use CV first to get thermal coal CV bin
# Cv to use is Specific Energy minus 260
# This is for the conversion of SE gar_kcal/kg to SE nar_kcal/kg, acc to WM
all.intern.coals.df$cv_helper <- all.intern.coals.df$Specific.Energy-260
# Coal CV bins
all.intern.coals.df$CV_bin <- "2250"
all.intern.coals.df$CV_bin[all.intern.coals.df$cv_helper >= 2375] <- "2500"
all.intern.coals.df$CV_bin[all.intern.coals.df$cv_helper >= 2625] <- "2750"
all.intern.coals.df$CV_bin[all.intern.coals.df$cv_helper >= 2875] <- "3000"
all.intern.coals.df$CV_bin[all.intern.coals.df$cv_helper >= 3125] <- "3250"
all.intern.coals.df$CV_bin[all.intern.coals.df$cv_helper >= 3375] <- "3500"
all.intern.coals.df$CV_bin[all.intern.coals.df$cv_helper >= 3625] <- "3750"
all.intern.coals.df$CV_bin[all.intern.coals.df$cv_helper >= 3875] <- "4000"
all.intern.coals.df$CV_bin[all.intern.coals.df$cv_helper >= 4125] <- "4250"
all.intern.coals.df$CV_bin[all.intern.coals.df$cv_helper >= 4375] <- "4500"
all.intern.coals.df$CV_bin[all.intern.coals.df$cv_helper >= 4625] <- "4750"
all.intern.coals.df$CV_bin[all.intern.coals.df$cv_helper >= 4875] <- "5000"
all.intern.coals.df$CV_bin[all.intern.coals.df$cv_helper >= 5125] <- "5250"
all.intern.coals.df$CV_bin[all.intern.coals.df$cv_helper >= 5375] <- "5500"
all.intern.coals.df$CV_bin[all.intern.coals.df$cv_helper >= 5625] <- "5750"
all.intern.coals.df$CV_bin[all.intern.coals.df$cv_helper >= 5875] <- "6000"
all.intern.coals.df$CV_bin[all.intern.coals.df$cv_helper >= 6125] <- "6250"
all.intern.coals.df$CV_bin[all.intern.coals.df$cv_helper >= 6375] <- "6500"
all.intern.coals.df$CV_bin[all.intern.coals.df$cv_helper >= 6625] <- "6750"
all.intern.coals.df$CV_bin[all.intern.coals.df$cv_helper >= 6875] <- "7000"

# Renaming coal group for thermal coals
all.intern.coals.df$coal_group <- paste("Thermal_", all.intern.coals.df$CV_bin, sep = "")
# Fix for met type missing: will otherwise have issues with == statement in following line
all.intern.coals.df$Met.type <- ifelse(all.intern.coals.df$prod_type=="Thermal", "Thermal", all.intern.coals.df$Met.type)
# Coking coal groups
all.intern.coals.df$coal_group <- ifelse(all.intern.coals.df$Met.type=="HCC", "HCC", all.intern.coals.df$coal_group)
all.intern.coals.df$coal_group <- ifelse(all.intern.coals.df$Met.type=="SCC", "SCC", all.intern.coals.df$coal_group)
all.intern.coals.df$coal_group <- ifelse(all.intern.coals.df$Met.type=="PCI", "PCI", all.intern.coals.df$coal_group)
all.intern.coals.df$prod_subtype <- all.intern.coals.df$coal_group
# Reset CV_bin for coking coals and make it numeric
all.intern.coals.df$CV_bin <- ifelse(all.intern.coals.df$prod_type=="Metallurgical", "0", all.intern.coals.df$CV_bin)
all.intern.coals.df$CV_bin <- as.numeric(all.intern.coals.df$CV_bin)

########### Costs and Prices for the international sheet
# Ex transport is here ex international transport: we do not add these handling or prep costs again in the network build file
# Price ex transport and handling costs
all.intern.coals.df$price_ex_transp <- all.intern.coals.df$price_usd_pt
# Costs ex royalties, transport and handling costs
all.intern.coals.df$c1_cash_cost_ex_transp <- all.intern.coals.df$c1cashcost_usd_pt
# Costs ex transport and handling costs
all.intern.coals.df$total_cash_cost_ex_transp <- all.intern.coals.df$c1cashcost_usd_pt + all.intern.coals.df$royalties_usd_pt
# Hcc premium fixes
all.intern.coals.df$hccpremium <- ifelse(is.na(all.intern.coals.df$hccpremium), 0, all.intern.coals.df$hccpremium)
all.intern.coals.df$c1_cash_cost_ex_transp_ex_premium <- all.intern.coals.df$c1_cash_cost_ex_transp
all.intern.coals.df$total_cash_cost_ex_transp_ex_premium <- all.intern.coals.df$total_cash_cost_ex_transp
all.intern.coals.df$c1_cash_cost_ex_transp <- all.intern.coals.df$c1_cash_cost_ex_transp - all.intern.coals.df$hccpremium
all.intern.coals.df$total_cash_cost_ex_transp <- all.intern.coals.df$total_cash_cost_ex_transp - all.intern.coals.df$hccpremium

################# Production capacity correction factors: used to represent that China will not be able to buy all of the cheapest coals
################# Production capacity is multiplied with the share of Chinese imports in each supplier exports
all.intern.coals.df$capa_corr_factor <- 1
# Thermal coals
all.intern.coals.df$capa_corr_factor <- ifelse(all.intern.coals.df$prod_type=="Thermal" & all.intern.coals.df$ctry_code=="ROW", 0.014, all.intern.coals.df$capa_corr_factor)
all.intern.coals.df$capa_corr_factor <- ifelse(all.intern.coals.df$prod_type=="Thermal" & all.intern.coals.df$Region=="Australia", 0.241, all.intern.coals.df$capa_corr_factor)
all.intern.coals.df$capa_corr_factor <- ifelse(all.intern.coals.df$prod_type=="Thermal" & all.intern.coals.df$Region=="Indonesia", 0.293, all.intern.coals.df$capa_corr_factor)
all.intern.coals.df$capa_corr_factor <- ifelse(all.intern.coals.df$prod_type=="Thermal" & all.intern.coals.df$Region=="Russia", 0.143, all.intern.coals.df$capa_corr_factor)
all.intern.coals.df$capa_corr_factor <- ifelse(all.intern.coals.df$prod_type=="Thermal" & all.intern.coals.df$Region=="Canada", 0.558, all.intern.coals.df$capa_corr_factor)
all.intern.coals.df$capa_corr_factor <- ifelse(all.intern.coals.df$prod_type=="Thermal" & all.intern.coals.df$Region=="Philippines", 0.763, all.intern.coals.df$capa_corr_factor)
# Met coals
all.intern.coals.df$capa_corr_factor <- ifelse(all.intern.coals.df$prod_type=="Metallurgical" & all.intern.coals.df$ctry_code=="ROW", 0.031, all.intern.coals.df$capa_corr_factor)
all.intern.coals.df$capa_corr_factor <- ifelse(all.intern.coals.df$prod_type=="Metallurgical" & all.intern.coals.df$Region=="Australia", 0.168, all.intern.coals.df$capa_corr_factor)
all.intern.coals.df$capa_corr_factor <- ifelse(all.intern.coals.df$prod_type=="Metallurgical" & all.intern.coals.df$Region=="Indonesia", 0.081, all.intern.coals.df$capa_corr_factor)
all.intern.coals.df$capa_corr_factor <- ifelse(all.intern.coals.df$prod_type=="Metallurgical" & all.intern.coals.df$Region=="Russia", 0.196, all.intern.coals.df$capa_corr_factor)
all.intern.coals.df$capa_corr_factor <- ifelse(all.intern.coals.df$prod_type=="Metallurgical" & all.intern.coals.df$Region=="Canada", 0.099, all.intern.coals.df$capa_corr_factor)
all.intern.coals.df$capa_corr_factor <- ifelse(all.intern.coals.df$prod_type=="Metallurgical" & all.intern.coals.df$Region=="Philippines", 0, all.intern.coals.df$capa_corr_factor)
# SCC counted as thermal
all.intern.coals.df$capa_corr_factor <- ifelse(all.intern.coals.df$prod_subtype=="SCC" & all.intern.coals.df$ctry_code=="ROW", 0.014, all.intern.coals.df$capa_corr_factor)
all.intern.coals.df$capa_corr_factor <- ifelse(all.intern.coals.df$prod_subtype=="SCC" & all.intern.coals.df$Region=="Australia", 0.241, all.intern.coals.df$capa_corr_factor)
all.intern.coals.df$capa_corr_factor <- ifelse(all.intern.coals.df$prod_subtype=="SCC" & all.intern.coals.df$Region=="Indonesia", 0.293, all.intern.coals.df$capa_corr_factor)
all.intern.coals.df$capa_corr_factor <- ifelse(all.intern.coals.df$prod_subtype=="SCC" & all.intern.coals.df$Region=="Russia", 0.143, all.intern.coals.df$capa_corr_factor)
all.intern.coals.df$capa_corr_factor <- ifelse(all.intern.coals.df$prod_subtype=="SCC" & all.intern.coals.df$Region=="Canada", 0.558, all.intern.coals.df$capa_corr_factor)
all.intern.coals.df$capa_corr_factor <- ifelse(all.intern.coals.df$prod_subtype=="SCC" & all.intern.coals.df$Region=="Philippines", 0.763, all.intern.coals.df$capa_corr_factor)
# Apply the correction factor
all.intern.coals.df$prod_capa_Mt <- all.intern.coals.df$prod_capa_Mt*all.intern.coals.df$capa_corr_factor

# var ordering and selection
# coal brand is not important here. It is coal group that we use in the python code
all.intern.coals.df <- all.intern.coals.df %>% 
  select(node_name, node_id, prod_type, prod_subtype, coal_group, year,
         prod_capa_Mt, total_cash_cost_ex_transp, c1_cash_cost_ex_transp, price_ex_transp, 
         price_usd_pt, c1cashcost_usd_pt, mining_cost_usd_pt, coal_prep_cost_usd_pt,
         overhead_cost_usd_pt, port_cost_usd_pt, transp_cost_usd_pt, royalties_usd_pt)

################# Append domestic and international seaborne suppliers' info
prod.capa.price.qual.df <- dplyr::bind_rows(prod.capa.price.qual.df, all.intern.coals.df)

################# Apply cost (price) correction factor for calorific value of thermal coals
################# $8 premium per 1000 kcal, vs a benchmark of 5500 kcal coal
# Already got rid of CV values before so just do this explicitly for each group
prod.capa.price.qual.df$CV_cost_premium <- 0
prod.capa.price.qual.df$CV_cost_premium <- ifelse(prod.capa.price.qual.df$coal_group=="Thermal_2500", 24, prod.capa.price.qual.df$CV_cost_premium)
prod.capa.price.qual.df$CV_cost_premium <- ifelse(prod.capa.price.qual.df$coal_group=="Thermal_2750", 22, prod.capa.price.qual.df$CV_cost_premium)
prod.capa.price.qual.df$CV_cost_premium <- ifelse(prod.capa.price.qual.df$coal_group=="Thermal_3000", 20, prod.capa.price.qual.df$CV_cost_premium)
prod.capa.price.qual.df$CV_cost_premium <- ifelse(prod.capa.price.qual.df$coal_group=="Thermal_3250", 18, prod.capa.price.qual.df$CV_cost_premium)
prod.capa.price.qual.df$CV_cost_premium <- ifelse(prod.capa.price.qual.df$coal_group=="Thermal_3500", 16, prod.capa.price.qual.df$CV_cost_premium)
prod.capa.price.qual.df$CV_cost_premium <- ifelse(prod.capa.price.qual.df$coal_group=="Thermal_3750", 14, prod.capa.price.qual.df$CV_cost_premium)
prod.capa.price.qual.df$CV_cost_premium <- ifelse(prod.capa.price.qual.df$coal_group=="Thermal_4000", 12, prod.capa.price.qual.df$CV_cost_premium)
prod.capa.price.qual.df$CV_cost_premium <- ifelse(prod.capa.price.qual.df$coal_group=="Thermal_4250", 10, prod.capa.price.qual.df$CV_cost_premium)
prod.capa.price.qual.df$CV_cost_premium <- ifelse(prod.capa.price.qual.df$coal_group=="Thermal_4500", 8, prod.capa.price.qual.df$CV_cost_premium)
prod.capa.price.qual.df$CV_cost_premium <- ifelse(prod.capa.price.qual.df$coal_group=="Thermal_4750", 6, prod.capa.price.qual.df$CV_cost_premium)
prod.capa.price.qual.df$CV_cost_premium <- ifelse(prod.capa.price.qual.df$coal_group=="Thermal_5000", 4, prod.capa.price.qual.df$CV_cost_premium)
prod.capa.price.qual.df$CV_cost_premium <- ifelse(prod.capa.price.qual.df$coal_group=="Thermal_5250", 2, prod.capa.price.qual.df$CV_cost_premium)
prod.capa.price.qual.df$CV_cost_premium <- ifelse(prod.capa.price.qual.df$coal_group=="Thermal_5500", 0, prod.capa.price.qual.df$CV_cost_premium)
prod.capa.price.qual.df$CV_cost_premium <- ifelse(prod.capa.price.qual.df$coal_group=="Thermal_5750", -2, prod.capa.price.qual.df$CV_cost_premium)
prod.capa.price.qual.df$CV_cost_premium <- ifelse(prod.capa.price.qual.df$coal_group=="Thermal_6000", -4, prod.capa.price.qual.df$CV_cost_premium)
prod.capa.price.qual.df$CV_cost_premium <- ifelse(prod.capa.price.qual.df$coal_group=="Thermal_6250", -6, prod.capa.price.qual.df$CV_cost_premium)
prod.capa.price.qual.df$CV_cost_premium <- ifelse(prod.capa.price.qual.df$coal_group=="Thermal_6500", -8, prod.capa.price.qual.df$CV_cost_premium)
prod.capa.price.qual.df$CV_cost_premium <- ifelse(prod.capa.price.qual.df$coal_group=="Thermal_6750", -10, prod.capa.price.qual.df$CV_cost_premium)
prod.capa.price.qual.df$CV_cost_premium <- ifelse(prod.capa.price.qual.df$coal_group=="Thermal_7000", -12, prod.capa.price.qual.df$CV_cost_premium)
# Apply the correction factors
prod.capa.price.qual.df$total_cash_cost_ex_transp <- prod.capa.price.qual.df$total_cash_cost_ex_transp + prod.capa.price.qual.df$CV_cost_premium
prod.capa.price.qual.df$c1_cash_cost_ex_transp <- prod.capa.price.qual.df$c1_cash_cost_ex_transp + prod.capa.price.qual.df$CV_cost_premium

################## Units and rounding
prod.capa.price.qual.df$price_ex_transp <- round(prod.capa.price.qual.df$price_ex_transp, 2)
prod.capa.price.qual.df$prod_capa_Mt <- round(prod.capa.price.qual.df$prod_capa_Mt, 3)

### Fix repeated values for different FOT vs FOB coals etc
unique.check.df <- prod.capa.price.qual.df %>% 
  group_by(node_name, coal_group, year) %>% 
  mutate(count = n()) %>% 
  filter(count==1)
repi.check.df <- prod.capa.price.qual.df %>% 
  group_by(node_name, coal_group, year) %>% 
  mutate(count = n()) %>% 
  filter(count!=1)
repi.check.df <- repi.check.df %>% 
  distinct(node_name, coal_group, year, price_ex_transp, .keep_all = TRUE) 
repi.check.df <- repi.check.df %>% 
  group_by(node_name, coal_group, year) %>% 
  summarise(node_id = first(node_id),
            prod_type = first(prod_type),
            prod_subtype = first(prod_subtype),
            coal_brand = last(coal_brand),
            prod_capa_Mt= sum (prod_capa_Mt),
            price_ex_transp = first(price_ex_transp),
            price_usd_pt = first(price_usd_pt),
            c1_cash_cost_ex_transp = first(c1_cash_cost_ex_transp),
            total_cash_cost_ex_transp = first(total_cash_cost_ex_transp),
            c1cashcost_usd_pt = first(c1cashcost_usd_pt),
            mining_cost_usd_pt = first(mining_cost_usd_pt),
            coal_prep_cost_usd_pt = first(coal_prep_cost_usd_pt),
            overhead_cost_usd_pt = first(overhead_cost_usd_pt),
            port_cost_usd_pt = first(port_cost_usd_pt),
            transp_cost_usd_pt = first(transp_cost_usd_pt),
            royalties_usd_pt = first(royalties_usd_pt),
            count = first(count)
            )
repi2.check.df <- repi.check.df %>% 
  group_by(node_name, coal_group, year) %>% 
  mutate(count = n()) %>% 
  filter(count!=1)

# Final df prod capa price and quality with no repeated values
prod.capa.price.qual.df <- dplyr::bind_rows(unique.check.df, repi.check.df) %>% 
  select(-count)
###################### Units and rounding
prod.capa.price.qual.df$prod_capa_Mt <- round(prod.capa.price.qual.df$prod_capa_Mt, 2)
prod.capa.price.qual.df$total_cash_cost_ex_transp <- round(prod.capa.price.qual.df$total_cash_cost_ex_transp, 1)
prod.capa.price.qual.df$c1_cash_cost_ex_transp <- round(prod.capa.price.qual.df$c1_cash_cost_ex_transp, 1)
prod.capa.price.qual.df$price_ex_transp <- round(prod.capa.price.qual.df$price_ex_transp, 1)
### Drop years <2015 (used only for running capa calc)

###################### Fixes
# Set costs and prices to 999 for those missing
# They are only missing when no production capacity is reported, but we'll need to keep these in to help keep the full network)
prod.capa.price.qual.df$total_cash_cost_ex_transp <- ifelse(is.na(prod.capa.price.qual.df$total_cash_cost_ex_transp), 999, prod.capa.price.qual.df$total_cash_cost_ex_transp)
prod.capa.price.qual.df$c1_cash_cost_ex_transp <- ifelse(is.na(prod.capa.price.qual.df$c1_cash_cost_ex_transp), 999, prod.capa.price.qual.df$c1_cash_cost_ex_transp)

############################    Save to file   ##############################################
# Save latest version as a copy
out.file.name <- "prod capa cost price brand by mine latest.xlsx"
outfile <- file.path(build.output.path, out.file.name)
write.xlsx(prod.capa.price.qual.df, outfile)

# Save latest version as a csv
out.file.name <- "prod capa cost price brand by mine latest.csv"
outfile <- file.path(build.output.path, out.file.name)
con<-file(outfile, encoding="UTF-8")
write.csv(prod.capa.price.qual.df, file=con, row.names=FALSE)