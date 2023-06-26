
# Libraries
library(dplyr)
library(tidyverse)
#library(lubridate)


#Setting working directory
setwd("")



# Rental Data
# Asking Rent
# All rentals
RentAskRentAll = read.csv("medianAskingRent_All.csv", stringsAsFactors = FALSE) %>%
  filter(., 
         areaType == "neighborhood"
         ) %>%
  gather(., 
         key = "yearMonth", 
         value = "RentAskRentAll", 
         X2010.01:X2022.12) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d")) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, Borough, time, RentAskRentAll)

#Studio rentals
RentAskRentStudio = read.csv("medianAskingRent_Studio.csv", stringsAsFactors = FALSE) %>%
  filter(., 
         areaType == "neighborhood"
  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "RentAskRentStudio", 
         X2010.01:X2022.12) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d")) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, Borough, time, RentAskRentStudio)

#One Bedroom rentals
RentAskRentOneBd = read.csv("medianAskingRent_OneBd.csv", stringsAsFactors = FALSE) %>%
  filter(., 
         areaType == "neighborhood"
  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "RentAskRentOneBd", 
         X2010.01:X2022.12) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d")) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, Borough, time, RentAskRentOneBd)

#Two Bedroom rentals
RentAskRentTwoBd = read.csv("medianAskingRent_TwoBd.csv", stringsAsFactors = FALSE) %>%
  filter(., 
         areaType == "neighborhood"
  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "RentAskRentTwoBd", 
         X2010.01:X2022.12) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d")) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, Borough, time, RentAskRentTwoBd)

#Three or more Bedroom Rentals
RentAskRentThreePlusBd = read.csv("medianAskingRent_ThreePlusBd.csv", stringsAsFactors = FALSE) %>%
  filter(., 
         areaType == "neighborhood"
  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "RentAskRentThreePlusBd", 
         X2010.01:X2022.12) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d")) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, Borough, time, RentAskRentThreePlusBd)

#Rental Inventory
# All rentals
RentInventoryAll = read.csv("rentalInventory_All.csv", stringsAsFactors = FALSE) %>%
  filter(., 
         areaType == "neighborhood"
  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "RentInventoryAll", 
         X2010.01:X2022.12) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d")) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, Borough, time, RentInventoryAll)

#Studio rentals
RentInventoryStudio = read.csv("rentalInventory_Studio.csv", stringsAsFactors = FALSE) %>%
  filter(., 
         areaType == "neighborhood"
  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "RentInventoryStudio", 
         X2010.01:X2022.12) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d")) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, Borough, time, RentInventoryStudio)

#One Bedroom rentals
RentInventoryOneBd = read.csv("rentalInventory_OneBd.csv", stringsAsFactors = FALSE) %>%
  filter(., 
         areaType == "neighborhood"
  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "RentInventoryOneBd", 
         X2010.01:X2022.12) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d")) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, Borough, time, RentInventoryOneBd)

#Two Bedroom rentals
RentInventoryTwoBd = read.csv("rentalInventory_TwoBd.csv", stringsAsFactors = FALSE) %>%
  filter(., 
         areaType == "neighborhood"
  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "RentInventoryTwoBd", 
         X2010.01:X2022.12) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d")) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, Borough, time, RentInventoryTwoBd)

#Three or more Bedroom Rentals
RentInventoryThreePlusBd = read.csv("rentalInventory_ThreePlusBd.csv", stringsAsFactors = FALSE) %>%
  filter(., 
         areaType == "neighborhood"
  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "RentInventoryThreePlusBd", 
         X2010.01:X2022.12) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d")) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, Borough, time, RentInventoryThreePlusBd)

#####
#Sales Data
# Asking price
# All Sales
SaleAskPriceAll = read.csv("medianAskingPrice_All.csv", stringsAsFactors = FALSE) %>%
  filter(., 
         areaType == "neighborhood"
  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "SaleAskPriceAll", 
         X2010.01:X2022.12) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d")) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, Borough, time, SaleAskPriceAll)

#Condo Sales
SaleAskPriceCondo = read.csv("medianAskingPrice_Condo.csv", stringsAsFactors = FALSE) %>%
  filter(., 
         areaType == "neighborhood"
  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "SaleAskPriceCondo", 
         X2010.01:X2022.12) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d")) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, Borough, time, SaleAskPriceCondo)

#Coop Sales
SaleAskPriceCoop = read.csv("medianAskingPrice_Coop.csv", stringsAsFactors = FALSE) %>%
  filter(., 
         areaType == "neighborhood"
  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "SaleAskPriceCoop", 
         X2010.01:X2022.12) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d")) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, Borough, time, SaleAskPriceCoop)

#Single Family Sales
SaleAskPriceSF = read.csv("medianAskingPrice_Sfr.csv", stringsAsFactors = FALSE) %>%
  filter(., 
         areaType == "neighborhood"
  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "SaleAskPriceSF", 
         X2010.01:X2022.12) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d")) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, Borough, time, SaleAskPriceSF)

# Sale price
# All Sales
SalePriceAll = read.csv("medianSalesPrice_All.csv", stringsAsFactors = FALSE) %>%
  filter(., 
         areaType == "neighborhood"
  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "SalePriceAll", 
         X2010.01:X2022.12) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d")) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, Borough, time, SalePriceAll)

#Condo Sales
SalePriceCondo = read.csv("medianSalesPrice_Condo.csv", stringsAsFactors = FALSE) %>%
  filter(., 
         areaType == "neighborhood"
  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "SalePriceCondo", 
         X2010.01:X2022.12) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d")) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, Borough, time, SalePriceCondo)

#Coop Sales
SalePriceCoop = read.csv("medianSalesPrice_Coop.csv", stringsAsFactors = FALSE) %>%
  filter(., 
         areaType == "neighborhood"
  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "SalePriceCoop", 
         X2010.01:X2022.12) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d")) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, Borough, time, SalePriceCoop)

#Single Family Sales
SalePriceSF = read.csv("medianSalesPrice_Sfr.csv", stringsAsFactors = FALSE) %>%
  filter(., 
         areaType == "neighborhood"
  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "SalePriceSF", 
         X2010.01:X2022.12) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d")) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, Borough, time, SalePriceSF)

#Sales Inventory
# All Inventory
SaleInventoryAll = read.csv("totalInventory_All.csv", stringsAsFactors = FALSE) %>%
  filter(., 
         areaType == "neighborhood"
  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "SaleInventoryAll", 
         X2010.01:X2022.12) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d")) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, Borough, time, SaleInventoryAll)

#Condo Inventory
SaleInventoryCondo = read.csv("totalInventory_Condo.csv", stringsAsFactors = FALSE) %>%
  filter(., 
         areaType == "neighborhood"
  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "SaleInventoryCondo", 
         X2010.01:X2022.12) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d")) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, Borough, time, SaleInventoryCondo)

#Coop Inventory
SaleInventoryCoop = read.csv("totalInventory_Coop.csv", stringsAsFactors = FALSE) %>%
  filter(., 
         areaType == "neighborhood"
  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "SaleInventoryCoop", 
         X2010.01:X2022.12) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d")) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, Borough, time, SaleInventoryCoop)

#Single Family Inventory
SaleInventorySF = read.csv("totalInventory_Sfr.csv", stringsAsFactors = FALSE) %>%
  filter(., 
         areaType == "neighborhood"
  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "SaleInventorySF", 
         X2010.01:X2022.12) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d")) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, Borough, time, SaleInventorySF)

# Sales Volume
# All Sales
SaleVolumeAll = read.csv("recordedSalesVolume_All.csv", stringsAsFactors = FALSE) %>%
  filter(., 
         areaType == "neighborhood"
  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "SaleVolumeAll", 
         X2010.01:X2022.12) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d")) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, Borough, time, SaleVolumeAll)

#Condo Sales
SaleVolumeCondo = read.csv("recordedSalesVolume_Condo.csv", stringsAsFactors = FALSE) %>%
  filter(., 
         areaType == "neighborhood"
  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "SaleVolumeCondo", 
         X2010.01:X2022.12) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d")) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, Borough, time, SaleVolumeCondo)

#Coop Sales
SaleVolumeCoop = read.csv("recordedSalesVolume_Coop.csv", stringsAsFactors = FALSE) %>%
  filter(., 
         areaType == "neighborhood"
  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "SaleVolumeCoop", 
         X2010.01:X2022.12) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d")) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, Borough, time, SaleVolumeCoop)

#Single Family Sales
SaleVolumeSF = read.csv("recordedSalesVolume_Sfr.csv", stringsAsFactors = FALSE) %>%
  filter(., 
         areaType == "neighborhood"
  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "SaleVolumeSF", 
         X2010.01:X2022.12) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d")) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, Borough, time, SaleVolumeSF)

#Days on Market
#All
DOMAll = read.csv("daysOnMarket_All.csv", stringsAsFactors = FALSE) %>%
  filter(., 
         areaType == "neighborhood"
  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "DOMAll", 
         X2010.01:X2022.12) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d")) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, Borough, time, DOMAll)

#Condo 
DOMCondo = read.csv("daysOnMarket_Condo.csv", stringsAsFactors = FALSE) %>%
  filter(., 
         areaType == "neighborhood"
  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "DOMCondo", 
         X2010.01:X2022.12) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d")) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, Borough, time, DOMCondo)

#Coop 
DOMCoop = read.csv("daysOnMarket_Coop.csv", stringsAsFactors = FALSE) %>%
  filter(., 
         areaType == "neighborhood"
  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "DOMCoop", 
         X2010.01:X2022.12) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d")) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, Borough, time, DOMCoop)

#Single Family 
DOMSF = read.csv("daysOnMarket_Sfr.csv", stringsAsFactors = FALSE) %>%
  filter(., 
         areaType == "neighborhood"
  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "DOMSF", 
         X2010.01:X2022.12) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d")) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, Borough, time, DOMSF)

#Joining the datasets
StreetEasy_df = DOMAll %>%
   left_join(., DOMCondo, by = c("Borough","areaName","time")) %>%
   left_join(., DOMCoop, by = c("Borough","areaName","time")) %>%
   left_join(., DOMSF, by = c("Borough","areaName","time")) %>%
   left_join(., RentAskRentAll, by = c("Borough","areaName","time")) %>%
   left_join(., RentAskRentStudio, by = c("Borough","areaName","time")) %>%
   left_join(., RentAskRentOneBd, by = c("Borough","areaName","time")) %>%
   left_join(., RentAskRentTwoBd, by = c("Borough","areaName","time")) %>%
   left_join(., RentAskRentThreePlusBd, by = c("Borough","areaName","time")) %>%
   left_join(., RentInventoryAll, by = c("Borough","areaName","time")) %>%
   left_join(., RentInventoryStudio, by = c("Borough","areaName","time")) %>%
   left_join(., RentInventoryOneBd, by = c("Borough","areaName","time")) %>%
   left_join(., RentInventoryTwoBd, by = c("Borough","areaName","time")) %>%
   left_join(., RentInventoryThreePlusBd, by = c("Borough","areaName","time")) %>%
   left_join(., SaleAskPriceAll, by = c("Borough","areaName","time")) %>%
   left_join(., SaleAskPriceCondo, by = c("Borough","areaName","time")) %>%
   left_join(., SaleAskPriceCoop, by = c("Borough","areaName","time")) %>%
   left_join(., SaleAskPriceSF, by = c("Borough","areaName","time")) %>%
   left_join(., SaleInventoryAll, by = c("Borough","areaName","time")) %>%
   left_join(., SaleInventoryCondo, by = c("Borough","areaName","time")) %>%
   left_join(., SaleInventoryCoop, by = c("Borough","areaName","time")) %>%
   left_join(., SaleInventorySF, by = c("Borough","areaName","time")) %>%
   left_join(., SalePriceAll, by = c("Borough","areaName","time")) %>%
   left_join(., SalePriceCondo, by = c("Borough","areaName","time")) %>%
   left_join(., SalePriceCoop, by = c("Borough","areaName","time")) %>%
   left_join(., SalePriceSF, by = c("Borough","areaName","time")) %>%
   left_join(., SaleVolumeAll, by = c("Borough","areaName","time")) %>%
   left_join(., SaleVolumeCondo, by = c("Borough","areaName","time")) %>%
   left_join(., SaleVolumeCoop, by = c("Borough","areaName","time")) %>%
   left_join(., SaleVolumeSF, by = c("Borough","areaName","time")) 


StreetEasy_df$areaName <- gsub("Central Harlem", "Harlem", StreetEasy_df$areaName)
StreetEasy_df$areaName <- gsub("Soho", "SoHo", StreetEasy_df$areaName)

#Merging StreetEasy_df with nyc_hoods_clean sf dataframe
master_df <- merge(nyc_hoods_clean, StreetEasy_df, by = c("areaName", "Borough"))

master_df <- master_df %>%
  st_as_sf()

