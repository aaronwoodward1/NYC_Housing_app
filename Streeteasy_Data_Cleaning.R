###############################################################################
#                                                                             #
#                           STREETEASY RENTAL DATA                            #
#                                                                             #
###############################################################################


# Asking rent
rent_ask_all = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/medianAskingRent_All.csv", stringsAsFactors = FALSE) %>%
  #  filter(., 
  #         areaType == "neighborhood" | areaType == "borough"
  #  ) %>% 
  gather(., 
         key = "yearMonth", 
         value = "value", 
         X2010.01:X2023.09) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d"),
         metric = "Median Asking Rent",
         type = "All Rentals" ) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

rent_ask_studio = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/medianAskingRent_Studio.csv", stringsAsFactors = FALSE) %>%
  #  filter(., 
  #         areaType == "neighborhood" | areaType == "borough"
  #  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "value", 
         X2010.01:X2023.09) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d"),
         metric = "Median Asking Rent",
         type = "Studio") %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

rent_ask_onebd = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/medianAskingRent_OneBd.csv", stringsAsFactors = FALSE) %>%
  #  filter(., 
  #         areaType == "neighborhood" | areaType == "borough"
  #  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "value", 
         X2010.01:X2023.09) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d"),
         metric = "Median Asking Rent",
         type = "1 Bedroom") %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

rent_ask_twobd = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/medianAskingRent_TwoBd.csv", stringsAsFactors = FALSE) %>%
  #  filter(., 
  #         areaType == "neighborhood" | areaType == "borough"
  #  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "value", 
         X2010.01:X2023.09) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d"),
         metric = "Median Asking Rent",
         type = "2 Bedrooms") %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

rent_ask_threeplusbd = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/medianAskingRent_ThreePlusBd.csv", stringsAsFactors = FALSE) %>%
  #  filter(., 
  #         areaType == "neighborhood" | areaType == "borough"
  #  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "value", 
         X2010.01:X2023.09) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d"),
         metric = "Median Asking Rent",
         type = "3+ Bedrooms") %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

#Rental Inventory
rent_inventory_all = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/rentalInventory_All.csv", stringsAsFactors = FALSE) %>%
  #  filter(., 
  #         areaType == "neighborhood" | areaType == "borough"
  #  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "value", 
         X2010.01:X2023.09) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d"),
         metric = "Rental Inventory",
         type = "All Rentals" ) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

rent_inventory_studio = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/rentalInventory_Studio.csv", stringsAsFactors = FALSE) %>%
  #  filter(., 
  #         areaType == "neighborhood" | areaType == "borough"
  #  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "value", 
         X2010.01:X2023.09) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d"),
         metric = "Rental Inventory",
         type = "Studio" ) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

rent_inventory_onebd = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/rentalInventory_OneBd.csv", stringsAsFactors = FALSE) %>%
  #  filter(., 
  #         areaType == "neighborhood" | areaType == "borough"
  #  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "value", 
         X2010.01:X2023.09) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d"),
         metric = "Rental Inventory",
         type = "1 Bedroom" ) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

rent_inventory_twobd = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/rentalInventory_TwoBd.csv", stringsAsFactors = FALSE) %>%
  #  filter(., 
  #         areaType == "neighborhood" | areaType == "borough"
  #  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "value", 
         X2010.01:X2023.09) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d"),
         metric = "Rental Inventory",
         type = "2 Bedrooms" ) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

rent_inventory_threeplusbd = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/rentalInventory_ThreePlusBd.csv", stringsAsFactors = FALSE) %>%
  #  filter(., 
  #         areaType == "neighborhood" | areaType == "borough"
  #  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "value", 
         X2010.01:X2023.09) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d"),
         metric = "Rental Inventory",
         type = "3+ Bedrooms" ) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

# Creating a larger dataframe "rent_df" by combining all of these smaller dataframes via row bind (rbind) function.
rent_df <- rbind(rent_ask_all, 
                 rent_ask_studio,
                 rent_ask_onebd,
                 rent_ask_twobd,
                 rent_ask_threeplusbd,
                 rent_inventory_all,
                 rent_inventory_studio,
                 rent_inventory_onebd,
                 rent_inventory_twobd,
                 rent_inventory_threeplusbd)
dplyr::glimpse(rent_df)


#rent_df <- prettyNum(rent_df$value, big.mark = ",", scientific = FALSE)
#thousands_sep <- function(x) {formatC(x, format="d", big.mark=",")}
#rent_df <- thousands_sep(rent_df$value)
#rent_df <- formatC(rent_df$value, format="f", big.mark = ",", digits=0)

# Renaming some neighborhoods for merging with the neighborhood sf dataframe
rent_df$areaName <- gsub("Columbia St Waterfront District", "Columbia Street Waterfront District", rent_df$areaName)
rent_df$areaName <- gsub("Central Harlem", "Harlem", rent_df$areaName)
rent_df$areaName <- gsub("Soho", "SoHo", rent_df$areaName)


#ADDING THE SPATIAL GEOMETRIES
rent_sf <- merge(nyc_hoods_clean, rent_df, by = c("areaName", "Borough"))

rent_sf <- rent_sf %>%
  st_as_sf()

rent_sf <- rent_sf[order(rent_sf$time, decreasing = TRUE),]

# Ordering the rental types by factor for the dropdown selection for interactive map and historical plot
rental <- factor(rent_sf$type, levels = c("All Rentals", "Studio", "1 Bedroom", "2 Bedrooms", "3+ Bedrooms"))


# Creating a couple dataframes and calculating household income share spent on rent for Affordability bubble chart

nyc_median_hhinc = 74694

nyc_rent_afford_df <- filter(rent_df, areaType == 'neighborhood' & time == max(rent_df$time) & type == 'All Rentals'
                             & metric == "Median Asking Rent") #%>%

nyc_rent_inv_df <- filter(rent_df, areaType == 'neighborhood' & time == max(rent_df$time) & type == 'All Rentals'
                          & metric == "Rental Inventory")

nyc_rent_inv_df <- rename(nyc_rent_inv_df, "Rent_inventory" = "value")

nyc_rent_afford_df <- rename(nyc_rent_afford_df, "Median_ask_rent" = "value")


#group_by(., areaName, Borough) %>%
#mutate(., )

nyc_rent_afford_df <- mutate(nyc_rent_afford_df, Share_hhinc_to_rent = Median_ask_rent/(nyc_median_hhinc/12))

nyc_rent_inv_df <- as.data.frame(
  select(nyc_rent_inv_df, areaName, Borough, Rent_inventory)
)

#Merging Neighborhood affordability df with Inventory dataframe
nyc_rent_afford_df <- left_join(nyc_rent_afford_df, nyc_rent_inv_df, by=c("areaName","Borough"))


###############################################################################
#                                                                             #
#                           STREETEASY SALES DATA                             #
#                                                                             #
###############################################################################

#Days on Market
days_on_mkt_all = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/daysOnMarket_All.csv", stringsAsFactors = FALSE) %>%
  #  filter(., 
  #         areaType == "neighborhood" | areaType == "borough"
  #  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "value", 
         X2010.01:X2023.09) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d"),
         metric = "Days on market",
         type = "All properties" ) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

days_on_mkt_condo = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/daysOnMarket_Condo.csv", stringsAsFactors = FALSE) %>%
  #  filter(., 
  #         areaType == "neighborhood" | areaType == "borough"
  #  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "value", 
         X2010.01:X2023.09) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d"),
         metric = "Days on market",
         type = "Condo" ) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

days_on_mkt_coop = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/daysOnMarket_Coop.csv", stringsAsFactors = FALSE) %>%
  #  filter(., 
  #         areaType == "neighborhood" | areaType == "borough"
  #  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "value", 
         X2010.01:X2023.09) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d"),
         metric = "Days on market",
         type = "Co-Op" ) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

days_on_mkt_sf = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/daysOnMarket_Sfr.csv", stringsAsFactors = FALSE) %>%
  #  filter(., 
  #         areaType == "neighborhood" | areaType == "borough"
  #  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "value", 
         X2010.01:X2023.09) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d"),
         metric = "Days on market",
         type = "Single-Family" ) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

# Median Asking Price
median_ask_price_all = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/medianAskingPrice_All.csv", stringsAsFactors = FALSE) %>%
  #  filter(., 
  #         areaType == "neighborhood" | areaType == "borough"
  #  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "value", 
         X2010.01:X2023.09) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d"),
         metric = "Median Asking Price",
         type = "All properties" ) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

median_ask_price_condo = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/medianAskingPrice_Condo.csv", stringsAsFactors = FALSE) %>%
  #  filter(., 
  #         areaType == "neighborhood" | areaType == "borough"
  #  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "value", 
         X2010.01:X2023.09) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d"),
         metric = "Median Asking Price",
         type = "Condo" ) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

median_ask_price_coop = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/medianAskingPrice_Coop.csv", stringsAsFactors = FALSE) %>%
  #  filter(., 
  #         areaType == "neighborhood" | areaType == "borough"
  #  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "value", 
         X2010.01:X2023.09) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d"),
         metric = "Median Asking Price",
         type = "Co-Op" ) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

median_ask_price_sf = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/medianAskingPrice_Sfr.csv", stringsAsFactors = FALSE) %>%
  #  filter(., 
  #         areaType == "neighborhood" | areaType == "borough"
  #  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "value", 
         X2010.01:X2023.09) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d"),
         metric = "Median Asking Price",
         type = "Single-Family" ) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

median_sales_price_all = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/medianSalesPrice_All.csv", stringsAsFactors = FALSE) %>%
  #  filter(., 
  #         areaType == "neighborhood" | areaType == "borough"
  #  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "value", 
         X2010.01:X2023.09) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d"),
         metric = "Median Sales Price",
         type = "All properties" ) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

median_sales_price_condo = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/medianSalesPrice_Condo.csv", stringsAsFactors = FALSE) %>%
  #  filter(., 
  #         areaType == "neighborhood" | areaType == "borough"
  #  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "value", 
         X2010.01:X2023.09) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d"),
         metric = "Median Sales Price",
         type = "Condo" ) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

median_sales_price_coop = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/medianSalesPrice_Coop.csv", stringsAsFactors = FALSE) %>%
  #  filter(., 
  #         areaType == "neighborhood" | areaType == "borough"
  #  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "value", 
         X2010.01:X2023.09) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d"),
         metric = "Median Sales Price",
         type = "Co-Op" ) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

median_sales_price_sf = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/medianSalesPrice_Sfr.csv", stringsAsFactors = FALSE) %>%
  #  filter(., 
  #         areaType == "neighborhood" | areaType == "borough"
  #  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "value", 
         X2010.01:X2023.09) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d"),
         metric = "Median Sales Price",
         type = "Single-Family" ) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

# Share of Price Cut
price_cut_share_all = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/priceCutShare_All.csv", stringsAsFactors = FALSE) %>%
  #  filter(., 
  #         areaType == "neighborhood" | areaType == "borough"
  #  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "value", 
         X2010.01:X2023.09) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d"),
         metric = "Share of Price Cut",
         type = "All properties" ) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

price_cut_share_condo = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/priceCutShare_Condo.csv", stringsAsFactors = FALSE) %>%
  #  filter(., 
  #         areaType == "neighborhood" | areaType == "borough"
  #  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "value", 
         X2010.01:X2023.09) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d"),
         metric = "Share of Price Cut",
         type = "Condo" ) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

price_cut_share_coop = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/priceCutShare_Coop.csv", stringsAsFactors = FALSE) %>%
  #  filter(., 
  #         areaType == "neighborhood" | areaType == "borough"
  #  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "value", 
         X2010.01:X2023.09) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d"),
         metric = "Share of Price Cut",
         type = "Co-Op" ) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

price_cut_share_sf = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/priceCutShare_Sfr.csv", stringsAsFactors = FALSE) %>%
  #  filter(., 
  #         areaType == "neighborhood" | areaType == "borough"
  #  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "value", 
         X2010.01:X2023.09) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d"),
         metric = "Share of Price Cut",
         type = "Single-Family" ) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

recorded_sales_vol_all = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/recordedSalesVolume_All.csv", stringsAsFactors = FALSE) %>%
  #  filter(., 
  #         areaType == "neighborhood" | areaType == "borough"
  #  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "value", 
         X2010.01:X2023.09) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d"),
         metric = "Sales Volume",
         type = "All properties" ) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

recorded_sales_vol_condo = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/recordedSalesVolume_Condo.csv", stringsAsFactors = FALSE) %>%
  #  filter(., 
  #         areaType == "neighborhood" | areaType == "borough"
  #  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "value", 
         X2010.01:X2023.09) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d"),
         metric = "Sales Volume",
         type = "Condo" ) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

recorded_sales_vol_coop = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/recordedSalesVolume_Coop.csv", stringsAsFactors = FALSE) %>%
  #  filter(., 
  #         areaType == "neighborhood" | areaType == "borough"
  #  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "value", 
         X2010.01:X2023.09) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d"),
         metric = "Sales Volume",
         type = "Co-Op" ) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

recorded_sales_vol_sf = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/recordedSalesVolume_Sfr.csv", stringsAsFactors = FALSE) %>%
  #  filter(., 
  #         areaType == "neighborhood" | areaType == "borough"
  #  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "value", 
         X2010.01:X2023.09) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d"),
         metric = "Sales Volume",
         type = "Single-Family" ) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

# Sale to List Ratio
sale_list_ratio_all = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/saleListRatio_All.csv", stringsAsFactors = FALSE) %>%
  #  filter(., 
  #         areaType == "neighborhood" | areaType == "borough"
  #  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "value", 
         X2010.01:X2023.09) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d"),
         metric = "Sale to List Ratio",
         type = "All properties" ) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

sale_list_ratio_condo = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/saleListRatio_Condo.csv", stringsAsFactors = FALSE) %>%
  #  filter(., 
  #         areaType == "neighborhood" | areaType == "borough"
  #  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "value", 
         X2010.01:X2023.09) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d"),
         metric = "Sale to List Ratio",
         type = "Condo" ) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

sale_list_ratio_coop = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/saleListRatio_Coop.csv", stringsAsFactors = FALSE) %>%
  #  filter(., 
  #         areaType == "neighborhood" | areaType == "borough"
  #  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "value", 
         X2010.01:X2023.09) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d"),
         metric = "Sale to List Ratio",
         type = "Co-Op" ) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

sale_list_ratio_sf = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/saleListRatio_Sfr.csv", stringsAsFactors = FALSE) %>%
  #  filter(., 
  #         areaType == "neighborhood" | areaType == "borough"
  #  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "value", 
         X2010.01:X2023.09) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d"),
         metric = "Sale to List Ratio",
         type = "Single-Family" ) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

# Total Inventory

total_inv_all = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/totalInventory_All.csv", stringsAsFactors = FALSE) %>%
  #  filter(., 
  #         areaType == "neighborhood" | areaType == "borough"
  #  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "value", 
         X2010.01:X2023.09) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d"),
         metric = "Total Inventory",
         type = "All properties" ) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

total_inv_condo = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/totalInventory_Condo.csv", stringsAsFactors = FALSE) %>%
  #  filter(., 
  #         areaType == "neighborhood" | areaType == "borough"
  #  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "value", 
         X2010.01:X2023.09) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d"),
         metric = "Total Inventory",
         type = "Condo" ) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

total_inv_coop = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/totalInventory_Coop.csv", stringsAsFactors = FALSE) %>%
  #  filter(., 
  #         areaType == "neighborhood" | areaType == "borough"
  #  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "value", 
         X2010.01:X2023.09) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d"),
         metric = "Total Inventory",
         type = "Co-Op" ) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

total_inv_sf = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/totalInventory_Sfr.csv", stringsAsFactors = FALSE) %>%
  #  filter(., 
  #         areaType == "neighborhood" | areaType == "borough"
  #  ) %>%
  gather(., 
         key = "yearMonth", 
         value = "value", 
         X2010.01:X2023.09) %>%
  mutate(., 
         time = as.Date(paste(yearMonth, ".01", sep = ""), format = "X%Y.%m.%d"),
         metric = "Total Inventory",
         type = "Single-Family" ) %>%
  filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

# Creating a larger dataframe "sales_df" by combining all of these smaller dataframes via row bind (rbind) function.
sales_df <- rbind(days_on_mkt_all, 
                 days_on_mkt_condo,
                 days_on_mkt_coop,
                 days_on_mkt_sf,
                 median_ask_price_all,
                 median_ask_price_condo,
                 median_ask_price_coop,
                 median_ask_price_sf,
                 median_sales_price_all,
                 median_sales_price_condo,
                 median_sales_price_coop,
                 median_sales_price_sf,
                 sale_list_ratio_all,
                 sale_list_ratio_condo,
                 sale_list_ratio_coop,
                 sale_list_ratio_sf,
                 price_cut_share_all,
                 price_cut_share_condo,
                 price_cut_share_coop,
                 price_cut_share_sf,
                 recorded_sales_vol_all,
                 recorded_sales_vol_condo,
                 recorded_sales_vol_coop,
                 recorded_sales_vol_sf,
                 total_inv_all,
                 total_inv_condo,
                 total_inv_coop,
                 total_inv_sf)
dplyr::glimpse(sales_df)

#Renaming some neighborhoods for merging with the neighborhood sf dataframe
sales_df$areaName <- gsub("Columbia St Waterfront District", "Columbia Street Waterfront District", sales_df$areaName)
sales_df$areaName <- gsub("Central Harlem", "Harlem", sales_df$areaName)
sales_df$areaName <- gsub("Soho", "SoHo", sales_df$areaName)

#ADDING THE SPATIAL GEOMETRIES
sales_sf <- merge(nyc_hoods_clean, sales_df, by = c("areaName", "Borough"))

sales_sf <- sales_sf %>%
  st_as_sf()

sales_sf <- sales_sf[order(sales_sf$time, decreasing = TRUE),]

# Ordering the sale property types by factor for the dropdown selection for interactive map and historical plot
properties <- factor(sales_sf$type, levels = c("All properties", "Condo", "Co-Op", "Single-Family"))
