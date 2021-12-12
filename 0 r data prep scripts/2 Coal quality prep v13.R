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

############# Import coal quality data
# get coal quality data
coal.quality.file <- file.path(build.input.path, "WoodMac data/selected coal quality data by mine and type -- CENSORED.csv")
coal.quality.df <- read.csv(coal.quality.file)
# Select useful columns
coal.quality.df <- coal.quality.df %>% 
  select(Mine.Name, Product.Type, Product.Subtype, Coal.Brand, Quality.Metric, Value, Unit) %>% 
  distinct()
coal.quality.df$Coal.Brand <- as.character(coal.quality.df$Coal.Brand)
coal.quality.df$Coal.Brand[coal.quality.df$Mine.Name=="Shandong KSOE Export" & coal.quality.df$Product.Subtype=="HCC"] <- "Shandong Export HCC"
coal.quality.df$Coal.Brand[coal.quality.df$Mine.Name=="West Tsankhi (New Tavan Tolgoi)" & coal.quality.df$Product.Subtype=="Thermal"] <- "West Tsankhi Thermal"
# Drop mine and get distinct values again
coal.quality.df <- coal.quality.df %>% 
  select(Product.Type, Product.Subtype, Coal.Brand, Quality.Metric, Value, Unit) %>% 
  distinct()
#Concetanate metric and unit before making it wide
coal.quality.df <- coal.quality.df %>% 
  unite(quality, c("Quality.Metric", "Unit"))
# Take first instance of a quality metric
coal.quality.df <- coal.quality.df %>% 
  group_by(Coal.Brand, quality) %>% 
  mutate(count = seq(n())) %>% 
  filter(count==1) %>% 
  select(-count)
# Make it wide
coal.quality.df <- coal.quality.df %>% 
  spread(quality, Value)
# FIX: FOB 5500 thermal value
coal.quality.df$`SE nar_kcal/kg`[coal.quality.df$Coal.Brand=="FOB 5500"] <- 5500
# FIX: net energy content for Mongolian coals
coal.quality.df$`SE nar_kcal/kg` <- ifelse(coal.quality.df$`SE nar_kcal/kg`==0 & coal.quality.df$Product.Type=="Thermal",
                                           coal.quality.df$`SE gar_kcal/kg`*0.95,
                                           coal.quality.df$`SE nar_kcal/kg`)
# Coking coal qualities
coal.quality.df$coking_coal_t_HCC <- ifelse(coal.quality.df$Product.Subtype=="HCC", 1, 0)
coal.quality.df$coking_coal_t_SCC <- ifelse(coal.quality.df$Product.Subtype=="SCC", 1, 0)
coal.quality.df$coking_coal_t_PCI <- ifelse(coal.quality.df$Product.Subtype=="PCI", 1, 0)

# Rename vars 
coal.quality.summ.df <- coal.quality.df %>%
  rename(product_type = Product.Type,
         product_subtype = Product.Subtype,
         coal_brand = Coal.Brand)
# Coal grouping 
coal.quality.summ.df$coal_group <- as.character(coal.quality.summ.df$product_subtype)
# Keep useful vars plus some
coal.quality.summ.df <- coal.quality.summ.df %>% 
  select(product_type, product_subtype, coal_brand, coal_group,
         `SE gad_kcal/kg`, `SE gar_kcal/kg`, `SE nar_kcal/kg`, 
         coking_coal_t_HCC, 
         coking_coal_t_SCC,
         coking_coal_t_PCI,
         CSN_Number, CSR_Number, `TS_%`, `N_%`)
# Coal CV bins
coal.quality.summ.df$CV_bin <- "2250"
coal.quality.summ.df$CV_bin[coal.quality.summ.df$`SE nar_kcal/kg` >= 2375] <- "2500"
coal.quality.summ.df$CV_bin[coal.quality.summ.df$`SE nar_kcal/kg` >= 2625] <- "2750"
coal.quality.summ.df$CV_bin[coal.quality.summ.df$`SE nar_kcal/kg` >= 2875] <- "3000"
coal.quality.summ.df$CV_bin[coal.quality.summ.df$`SE nar_kcal/kg` >= 3125] <- "3250"
coal.quality.summ.df$CV_bin[coal.quality.summ.df$`SE nar_kcal/kg` >= 3375] <- "3500"
coal.quality.summ.df$CV_bin[coal.quality.summ.df$`SE nar_kcal/kg` >= 3625] <- "3750"
coal.quality.summ.df$CV_bin[coal.quality.summ.df$`SE nar_kcal/kg` >= 3875] <- "4000"
coal.quality.summ.df$CV_bin[coal.quality.summ.df$`SE nar_kcal/kg` >= 4125] <- "4250"
coal.quality.summ.df$CV_bin[coal.quality.summ.df$`SE nar_kcal/kg` >= 4375] <- "4500"
coal.quality.summ.df$CV_bin[coal.quality.summ.df$`SE nar_kcal/kg` >= 4625] <- "4750"
coal.quality.summ.df$CV_bin[coal.quality.summ.df$`SE nar_kcal/kg` >= 4875] <- "5000"
coal.quality.summ.df$CV_bin[coal.quality.summ.df$`SE nar_kcal/kg` >= 5125] <- "5250"
coal.quality.summ.df$CV_bin[coal.quality.summ.df$`SE nar_kcal/kg` >= 5375] <- "5500"
coal.quality.summ.df$CV_bin[coal.quality.summ.df$`SE nar_kcal/kg` >= 5625] <- "5750"
coal.quality.summ.df$CV_bin[coal.quality.summ.df$`SE nar_kcal/kg` >= 5875] <- "6000"
coal.quality.summ.df$CV_bin[coal.quality.summ.df$`SE nar_kcal/kg` >= 6125] <- "6250"
coal.quality.summ.df$CV_bin[coal.quality.summ.df$`SE nar_kcal/kg` >= 6375] <- "6500"
coal.quality.summ.df$CV_bin[coal.quality.summ.df$`SE nar_kcal/kg` >= 6625] <- "6750"
coal.quality.summ.df$CV_bin[coal.quality.summ.df$`SE nar_kcal/kg` >= 6875] <- "7000"

# Renaming coal group for thermal coals
coal.quality.summ.df$coal_group_helper <- paste("Thermal_", coal.quality.summ.df$CV_bin, sep = "")
coal.quality.summ.df$coal_group <- ifelse(coal.quality.summ.df$product_type=="Thermal", coal.quality.summ.df$coal_group_helper, coal.quality.summ.df$coal_group)
# Reset CV_bin for coking coals and make it numberic
coal.quality.summ.df$CV_bin <- ifelse(coal.quality.summ.df$product_type=="Metallurgical", "0", coal.quality.summ.df$CV_bin)
coal.quality.summ.df$CV_bin <- as.numeric(coal.quality.summ.df$CV_bin)

# var ordering
coal.quality.summ.df <- coal.quality.summ.df %>% 
  select(product_type, product_subtype, coal_brand, CV_bin, coal_group,
         `SE gad_kcal/kg`, `SE gar_kcal/kg`, `SE nar_kcal/kg`, 
         coking_coal_t_HCC, 
         coking_coal_t_SCC,
         coking_coal_t_PCI,
         CSN_Number, CSR_Number, `TS_%`, `N_%`) %>% 
  rename(CV_gad_kcal_kg = `SE gad_kcal/kg`, 
         CV_gar_kcal_kg = `SE gar_kcal/kg`, 
         CV_nar_kcal_kg = `SE nar_kcal/kg`) 
       
# Calculate energy content of thermal coals as GJ/Mt, based on CV bin energy content
coal.quality.summ.df$CV_Kcal_p_Mt <- coal.quality.summ.df$CV_bin*1e3*1e6
coal.quality.summ.df$CV_GJ_p_Mt <- coal.quality.summ.df$CV_Kcal_p_Mt*4.184e-6
coal.quality.summ.df$CV_GJ_p_Mt_therm <- ifelse(coal.quality.summ.df$product_type=="Thermal", coal.quality.summ.df$CV_GJ_p_Mt, 0)
###################### Units and rounding
coal.quality.summ.df$CV_PJ_p_Mt_therm <- coal.quality.summ.df$CV_GJ_p_Mt_therm*1e-6
coal.quality.summ.df$CV_PJ_p_Mt_therm <- round(coal.quality.summ.df$CV_PJ_p_Mt_therm, 2)

####### final selection of vars and unique values
coal.quality.summ.df <- coal.quality.summ.df %>% 
  ungroup() %>% 
  select(coal_brand, coal_group, coking_coal_t_HCC, coking_coal_t_SCC, coking_coal_t_PCI, CV_PJ_p_Mt_therm) %>% 
  distinct() %>% 
  arrange(coal_group)

# ############################    Save to file   ##############################################
# Save latest version
out.file.name <- "coal qualities summary latest.xlsx"
outfile <- file.path(build.output.path, out.file.name)
write.xlsx(coal.quality.summ.df, outfile) # to build path

# Save latest version as a csv
out.file.name <- "coal qualities summary latest.csv"
outfile <- file.path(build.output.path, out.file.name)
con<-file(outfile, encoding="UTF-8")
write.csv(coal.quality.summ.df, file=con, row.names=FALSE)