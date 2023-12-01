library(dplyr)
library(tidyverse)
library(leaflet)
library(sp)
#library(ggmap)
#library(maptools)
library(broom)
library(httr)
#library(rgdal)
library(plotly)
#library(ggplot2)
library(sf)
library(s2)
library(geojsonsf)
library(lubridate)
library(scales)
library(timetk)
library(tidyverse)
library(trelliscopejs)
library(modeltime)
library(tidymodels)
library(shinyWidgets)

###############################################################################
#                                                                             #
#                           NYC NEIGHBORHOOD SHAPEFILE DATA                   #
#                                                                             #
###############################################################################
#Setting working directory
#setwd("/Users/aaronwoodward/Desktop/Datasets/Cities/NYC/Streeteasy_data/Master_report")
#"https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/ZillowNeighborhoods-NY.shp"

#WORKING WITH NEIGHBORHOOD SHAPEFILE
#Reading the shapefile into R.
nyc_hoods_sf <- st_read(dsn  = "Zillow_neighborhoods/ZillowNeighborhoods-NY.shp")

#Assigning the coordinate reference system or CRS
nyc_hoods_sf <- st_transform(nyc_hoods_sf, 4326)

#Filtering the NYC neighborhoods
nyc_hoods_sf <- nyc_hoods_sf %>% 
  filter(City == "New York")

#Creating a Borough column
nyc_hoods_sf <- nyc_hoods_sf %>%
  mutate(Borough = case_when(County == 'New York' ~ 'Manhattan',
                             County == 'Bronx' ~ 'Bronx',
                             County == 'Kings' ~ 'Brooklyn',
                             County == 'Queens' ~ 'Queens',
                             County == 'Richmond' ~ 'Staten Island'))

#Subneighborhood Aggregation

###MANHATTAN###

#Midtown East
midtowneast_df <-  subset(nyc_hoods_sf, Name == "Turtle Bay" | Name == "Tudor City" | Name == "Sutton Place")

midtowneast_df <- midtowneast_df %>% 
  summarize(geometry = st_union(geometry))

midtowneast_df <- data.frame("State" = "NY", 
                             "County" = "New York",
                             "City" = "New York", 
                             "Name" = "Midtown East",
                             "RegionID" = "1XXXX", 
                             "geometry" = midtowneast_df$geometry, 
                             "Borough" = "Manhattan") 

midtowneast_df <- midtowneast_df %>% 
  st_as_sf()

#Midtown West
midtownwest_df <-  subset(nyc_hoods_sf, Name == "Columbus Circle" | Name == "Clinton")

midtownwest_df <- midtownwest_df %>% 
  summarize(geometry = st_union(geometry))

midtownwest_df <- data.frame("State" = "NY", 
                             "County" = "New York",
                             "City" = "New York", 
                             "Name" = "Midtown West",
                             "RegionID" = "1XXXX", 
                             "geometry" = midtownwest_df$geometry, 
                             "Borough" = "Manhattan") 

midtownwest_df <- midtownwest_df %>% 
  st_as_sf()

#Midtown South
midtownsouth_df <-  subset(nyc_hoods_sf, Name == "Garment District" | RegionID == "274627")

midtownsouth_df <- midtownsouth_df %>% 
  summarize(geometry = st_union(geometry))

midtownsouth_df <- data.frame("State" = "NY", 
                              "County" = "New York",
                              "City" = "New York", 
                              "Name" = "Midtown South",
                              "RegionID" = "1XXXX", 
                              "geometry" = midtownsouth_df$geometry, 
                              "Borough" = "Manhattan") 

midtownsouth_df <- midtownsouth_df %>% 
  st_as_sf()

#Greenwich Village
greenwichvillage_df <-  subset(nyc_hoods_sf, Name == "Greenwich Village" | Name == "NoHo")

greenwichvillage_df <- greenwichvillage_df %>% 
  summarize(geometry = st_union(geometry))

greenwichvillage_df <- data.frame("State" = "NY", 
                                  "County" = "New York",
                                  "City" = "New York", 
                                  "Name" = "Greenwich Village",
                                  "RegionID" = "1XXXX", 
                                  "geometry" = greenwichvillage_df$geometry, 
                                  "Borough" = "Manhattan") 

greenwichvillage_df <- greenwichvillage_df %>% 
  st_as_sf()

#Upper East Side
uppereastside_df <-  subset(nyc_hoods_sf, Name == "Upper East Side" | Name == "Carnegie Hill")

uppereastside_df <- uppereastside_df %>% 
  summarize(geometry = st_union(geometry))

uppereastside_df <- data.frame("State" = "NY", 
                               "County" = "New York",
                               "City" = "New York", 
                               "Name" = "Upper East Side",
                               "RegionID" = "1XXXX", 
                               "geometry" = uppereastside_df$geometry, 
                               "Borough" = "Manhattan") 

uppereastside_df <- uppereastside_df %>% 
  st_as_sf()

#West Harlem
westharlem_df <-  subset(nyc_hoods_sf, Name == "Manhattanville" | Name == "Hamilton Heights")

westharlem_df <- westharlem_df %>% 
  summarize(geometry = st_union(geometry))

westharlem_df <- data.frame("State" = "NY", 
                            "County" = "New York",
                            "City" = "New York", 
                            "Name" = "West Harlem",
                            "RegionID" = "1XXXX", 
                            "geometry" = westharlem_df$geometry, 
                            "Borough" = "Manhattan") 

westharlem_df <- westharlem_df %>% 
  st_as_sf()

###BRONX###

#East Tremont
easttremont_df <-  subset(nyc_hoods_sf, Name == "East Tremont" | Name == "West Farms")

easttremont_df <- easttremont_df %>% 
  summarize(geometry = st_union(geometry))

easttremont_df <- data.frame("State" = "NY", 
                             "County" = "Bronx",
                             "City" = "New York", 
                             "Name" = "East Tremont",
                             "RegionID" = "1XXXX", 
                             "geometry" = easttremont_df$geometry, 
                             "Borough" = "Bronx") 

easttremont_df <- easttremont_df %>% 
  st_as_sf()

###BROOKLYN###

#DUMBO/Vinegar Hill
dumbo_df <-  subset(nyc_hoods_sf, Name == "DUMBO" | Name == "Vinegar Hill")

dumbo_df <- dumbo_df %>% 
  summarize(geometry = st_union(geometry))

dumbo_df <- data.frame("State" = "NY", 
                       "County" = "Kings",
                       "City" = "New York", 
                       "Name" = "DUMBO",
                       "RegionID" = "1XXXX", 
                       "geometry" = dumbo_df$geometry, 
                       "Borough" = "Brooklyn") 

dumbo_df <- dumbo_df %>% 
  st_as_sf()

#Bay Ridge/Fort Hamilton
bayridge_df <-  subset(nyc_hoods_sf, Name == "Bay Ridge" | Name == "Fort Hamilton")

bayridge_df <- bayridge_df %>% 
  summarize(geometry = st_union(geometry))

bayridge_df <- data.frame("State" = "NY", 
                          "County" = "Kings",
                          "City" = "New York", 
                          "Name" = "Bay Ridge",
                          "RegionID" = "1XXXX", 
                          "geometry" = bayridge_df$geometry, 
                          "Borough" = "Brooklyn") 

bayridge_df <- bayridge_df %>% 
  st_as_sf()

#Bergen Beach
bergenbeach_df <-  subset(nyc_hoods_sf, Name == "Bergen Beach" | Name == "Georgetown")

bergenbeach_df<- bergenbeach_df %>% 
  summarize(geometry = st_union(geometry))

bergenbeach_df <- data.frame("State" = "NY", 
                             "County" = "Kings",
                             "City" = "New York", 
                             "Name" = "Bergen Beach",
                             "RegionID" = "1XXXX", 
                             "geometry" = bergenbeach_df$geometry, 
                             "Borough" = "Brooklyn") 

bergenbeach_df <- bergenbeach_df %>% 
  st_as_sf()

#Sheepshead Bay
sheepsheadbay_df <-  subset(nyc_hoods_sf, Name == "Sheepshead Bay" | Name == "Homecrest")

# The geometries are invalid to do the st_union. To combine the geometries, we're going to 
# to coerce or "make valid" the combined geometry using the 'st_make_valid' argument 
# inside the 'st_union' argument.
sheepsheadbay_df <- sheepsheadbay_df %>%
  summarize(geometry = st_union(st_make_valid(geometry)))

sheepsheadbay_df <- data.frame("State" = "NY", 
                               "County" = "Kings",
                               "City" = "New York", 
                               "Name" = "Sheepshead Bay",
                               "RegionID" = "1XXXX", 
                               "geometry" = sheepsheadbay_df$geometry, 
                               "Borough" = "Brooklyn") 

sheepsheadbay_df <- sheepsheadbay_df %>% 
  st_as_sf()

#East Flatbush
eastflatbush_df <-  subset(nyc_hoods_sf, Name == "East Flatbush" | Name == "Wingate")

eastflatbush_df <- eastflatbush_df %>% 
  summarize(geometry = st_union(geometry))

eastflatbush_df <- data.frame("State" = "NY", 
                              "County" = "Kings",
                              "City" = "New York", 
                              "Name" = "East Flatbush",
                              "RegionID" = "1XXXX", 
                              "geometry" = eastflatbush_df$geometry, 
                              "Borough" = "Brooklyn") 

eastflatbush_df <- eastflatbush_df %>% 
  st_as_sf()

###QUEENS###

#Astoria
astoria_df <-  subset(nyc_hoods_sf, Name == "Astoria" | Name == "Astoria Heights")

astoria_df <- astoria_df %>% 
  summarize(geometry = st_union(geometry))

astoria_df <- data.frame("State" = "NY", 
                         "County" = "Queens",
                         "City" = "New York", 
                         "Name" = "Astoria",
                         "RegionID" = "1XXXX", 
                         "geometry" = astoria_df$geometry, 
                         "Borough" = "Queens") 

astoria_df <- astoria_df %>% 
  st_as_sf()

#Rockaways
rockaways_df <-  subset(nyc_hoods_sf, Name == "Fort Tilden" | Name == "Breezy Point" | Name == "Roxbury" | 
                          Name == "Jacob Riis Park" | Name == "Neponsit" | Name == "Belle Harbor" | 
                          Name == "Rockaway Park" | Name == "Rockaway Beach" | Name == "Arverne" |
                          Name == "Far Rockaway")

rockaways_df <- rockaways_df %>% 
  summarize(geometry = st_union(geometry))

rockaways_df <- data.frame("State" = "NY", 
                           "County" = "Queens",
                           "City" = "New York", 
                           "Name" = "Rockaways",
                           "RegionID" = "1XXXX", 
                           "geometry" = rockaways_df$geometry, 
                           "Borough" = "Queens") 

rockaways_df <- rockaways_df %>% 
  st_as_sf()

#Forest Hills
foresthills_df <-  subset(nyc_hoods_sf, Name == "Forest Hills" | Name == "Forest Hills Gardens")

foresthills_df <- foresthills_df %>% 
  summarize(geometry = st_union(geometry))

foresthills_df <- data.frame("State" = "NY", 
                             "County" = "Queens",
                             "City" = "New York", 
                             "Name" = "Forest Hills",
                             "RegionID" = "1XXXX", 
                             "geometry" = foresthills_df$geometry, 
                             "Borough" = "Queens") 

foresthills_df <- foresthills_df %>% 
  st_as_sf()

#Flushing
flushing_df <-  subset(nyc_hoods_sf, Name == "Flushing" | RegionID == "196538")

flushing_df <- flushing_df %>% 
  summarize(geometry = st_union(geometry))

flushing_df <- data.frame("State" = "NY", 
                          "County" = "Queens",
                          "City" = "New York", 
                          "Name" = "Flushing",
                          "RegionID" = "1XXXX", 
                          "geometry" = flushing_df$geometry, 
                          "Borough" = "Queens") 

flushing_df <- flushing_df %>% 
  st_as_sf()

#Hollis
hollis_df <-  subset(nyc_hoods_sf, Name == "Hollis" | Name == "Holliswood")

hollis_df <- hollis_df %>% 
  summarize(geometry = st_union(geometry))

hollis_df <- data.frame("State" = "NY", 
                        "County" = "Queens",
                        "City" = "New York", 
                        "Name" = "Hollis",
                        "RegionID" = "1XXXX", 
                        "geometry" = hollis_df$geometry, 
                        "Borough" = "Queens") 

hollis_df <- hollis_df %>% 
  st_as_sf()

#Whitestone
whitestone_df <-  subset(nyc_hoods_sf, Name == "Whitestone" | Name == "Malba")

whitestone_df <- whitestone_df %>% 
  summarize(geometry = st_union(geometry))

whitestone_df <- data.frame("State" = "NY", 
                            "County" = "Queens",
                            "City" = "New York", 
                            "Name" = "Whitestone",
                            "RegionID" = "1XXXX", 
                            "geometry" = whitestone_df$geometry, 
                            "Borough" = "Queens") 

whitestone_df <- whitestone_df %>% 
  st_as_sf()

nyc_hoods_clean <- subset(nyc_hoods_sf, !(Name %in% c('Turtle Bay','Sutton Place','Tudor City',
                                                      'Columbus Circle','Clinton','Garment District',
                                                      'Greenwich Village','NoHo','Upper East Side',
                                                      'Carnegie Hill','Manhattanville','Hamilton Heights',
                                                      'East Tremont','West Farms','DUMBO','Vinegar Hill',
                                                      'Bay Ridge','Fort Hamilton','Bergen Beach','Georgetown',
                                                      'East Flatbush','Wingate','Astoria','Astoria Heights',
                                                      'Fort Tilden','Breezy Point','Roxbury','Jacob Riis Park','Neponsit',
                                                      'Belle Harbor','Rockaway Park','Rockaway Beach','Arverne',
                                                      'Far Rockaway','Forest Hills','Forest Hills Gardens','Flushing',
                                                      'Hollis','Holliswood','Whitestone','Malba','Murray Hill',
                                                      'Sheepshead Bay','Homecrest')))

#Appending the aggregated neighborhoods
nyc_hoods_clean <- rbind(nyc_hoods_clean,astoria_df)
nyc_hoods_clean <- rbind(nyc_hoods_clean,bayridge_df)
nyc_hoods_clean <- rbind(nyc_hoods_clean,bergenbeach_df)
nyc_hoods_clean <- rbind(nyc_hoods_clean,dumbo_df)
nyc_hoods_clean <- rbind(nyc_hoods_clean,eastflatbush_df)
nyc_hoods_clean <- rbind(nyc_hoods_clean,easttremont_df)
nyc_hoods_clean <- rbind(nyc_hoods_clean,flushing_df)
nyc_hoods_clean <- rbind(nyc_hoods_clean,foresthills_df)
nyc_hoods_clean <- rbind(nyc_hoods_clean,greenwichvillage_df)
nyc_hoods_clean <- rbind(nyc_hoods_clean,hollis_df)
nyc_hoods_clean <- rbind(nyc_hoods_clean,midtowneast_df)
nyc_hoods_clean <- rbind(nyc_hoods_clean,midtownsouth_df)
nyc_hoods_clean <- rbind(nyc_hoods_clean,midtownwest_df)
nyc_hoods_clean <- rbind(nyc_hoods_clean,sheepsheadbay_df)
nyc_hoods_clean <- rbind(nyc_hoods_clean,rockaways_df)
nyc_hoods_clean <- rbind(nyc_hoods_clean,uppereastside_df)
nyc_hoods_clean <- rbind(nyc_hoods_clean,westharlem_df)
nyc_hoods_clean <- rbind(nyc_hoods_clean,whitestone_df)


#Removing Greenwich Village row that has a LINESTRING geometry
nyc_hoods_clean <- nyc_hoods_clean[-c(134),]

#Removing Staten Island neighborhoods because StreetEasy doesn't have housing data for Staten Island neighborhoods
nyc_hoods_clean <- subset(nyc_hoods_clean, Borough!="Staten Island")

#Re-naming a couple of neighborhoods to make merging the Streeteasy data easier.
nyc_hoods_clean$Name <- gsub("Battery Park","Battery Park City",nyc_hoods_clean$Name)
nyc_hoods_clean$Name <- gsub("Floral park","Floral Park",nyc_hoods_clean$Name)
nyc_hoods_clean$Name <- gsub("Flatiron District","Flatiron",nyc_hoods_clean$Name)
nyc_hoods_clean$Name <- gsub("Gramercy","Gramercy Park",nyc_hoods_clean$Name)
nyc_hoods_clean$Name <- gsub("Stuyvesant Town","Stuyvesant Town/PCV",nyc_hoods_clean$Name)
nyc_hoods_clean$Name <- gsub("Bedford Stuyvesant","Bedford-Stuyvesant",nyc_hoods_clean$Name)
nyc_hoods_clean$Name <- gsub("Douglaston-Little Neck","Douglaston",nyc_hoods_clean$Name)
nyc_hoods_clean$Name <- gsub("Downtown","Downtown Brooklyn",nyc_hoods_clean$Name)
nyc_hoods_clean$Name <- gsub("Soho", "SoHo", nyc_hoods_clean$Name)
nyc_hoods_clean$Name <- gsub("Hunters Point", "Long Island City", nyc_hoods_clean$Name)

nyc_hoods_clean <- subset(nyc_hoods_clean, select = c("Name", "RegionID", "Borough", "geometry"))

nyc_hoods_clean <- nyc_hoods_clean %>%
  rename("areaName" = "Name")


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
  #  #filter(., 
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
  filter(., time >= as.Date("2014-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

days_on_mkt_condo = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/daysOnMarket_Condo.csv", stringsAsFactors = FALSE) %>%
  #  #filter(., 
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
  filter(., time >= as.Date("2014-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

days_on_mkt_coop = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/daysOnMarket_Coop.csv", stringsAsFactors = FALSE) %>%
  #  #filter(., 
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
  filter(., time >= as.Date("2014-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

days_on_mkt_sf = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/daysOnMarket_Sfr.csv", stringsAsFactors = FALSE) %>%
  #  #filter(., 
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
  #filter(., time >= as.Date("2018-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

# Median Asking Price
median_ask_price_all = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/medianAskingPrice_All.csv", stringsAsFactors = FALSE) %>%
  #  #filter(., 
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
  filter(., time >= as.Date("2014-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

median_ask_price_condo = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/medianAskingPrice_Condo.csv", stringsAsFactors = FALSE) %>%
  #  #filter(., 
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
  filter(., time >= as.Date("2014-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

median_ask_price_coop = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/medianAskingPrice_Coop.csv", stringsAsFactors = FALSE) %>%
  #  #filter(., 
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
  filter(., time >= as.Date("2014-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

median_ask_price_sf = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/medianAskingPrice_Sfr.csv", stringsAsFactors = FALSE) %>%
  #  #filter(., 
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
  filter(., time >= as.Date("2014-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

median_sales_price_all = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/medianSalesPrice_All.csv", stringsAsFactors = FALSE) %>%
  #  #filter(., 
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
  filter(., time >= as.Date("2014-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

median_sales_price_condo = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/medianSalesPrice_Condo.csv", stringsAsFactors = FALSE) %>%
  #  #filter(., 
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
  filter(., time >= as.Date("2014-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

median_sales_price_coop = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/medianSalesPrice_Coop.csv", stringsAsFactors = FALSE) %>%
  #  #filter(., 
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
  filter(., time >= as.Date("2014-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

median_sales_price_sf = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/medianSalesPrice_Sfr.csv", stringsAsFactors = FALSE) %>%
  #  #filter(., 
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
  filter(., time >= as.Date("2014-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

# Share of Price Cut
price_cut_share_all = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/priceCutShare_All.csv", stringsAsFactors = FALSE) %>%
  #  #filter(., 
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
  filter(., time >= as.Date("2014-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

price_cut_share_condo = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/priceCutShare_Condo.csv", stringsAsFactors = FALSE) %>%
  #  #filter(., 
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
  filter(., time >= as.Date("2014-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

price_cut_share_coop = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/priceCutShare_Coop.csv", stringsAsFactors = FALSE) %>%
  #  #filter(., 
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
  filter(., time >= as.Date("2014-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

price_cut_share_sf = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/priceCutShare_Sfr.csv", stringsAsFactors = FALSE) %>%
  #  #filter(., 
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
  filter(., time >= as.Date("2014-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

recorded_sales_vol_all = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/recordedSalesVolume_All.csv", stringsAsFactors = FALSE) %>%
  #  #filter(., 
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
  filter(., time >= as.Date("2014-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

recorded_sales_vol_condo = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/recordedSalesVolume_Condo.csv", stringsAsFactors = FALSE) %>%
  #  #filter(., 
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
  filter(., time >= as.Date("2014-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

recorded_sales_vol_coop = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/recordedSalesVolume_Coop.csv", stringsAsFactors = FALSE) %>%
  #  #filter(., 
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
  filter(., time >= as.Date("2014-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

recorded_sales_vol_sf = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/recordedSalesVolume_Sfr.csv", stringsAsFactors = FALSE) %>%
  #  #filter(., 
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
  filter(., time >= as.Date("2014-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

# Sale to List Ratio
sale_list_ratio_all = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/saleListRatio_All.csv", stringsAsFactors = FALSE) %>%
  #  #filter(., 
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
  filter(., time >= as.Date("2014-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

sale_list_ratio_condo = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/saleListRatio_Condo.csv", stringsAsFactors = FALSE) %>%
  #  #filter(., 
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
  filter(., time >= as.Date("2014-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

sale_list_ratio_coop = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/saleListRatio_Coop.csv", stringsAsFactors = FALSE) %>%
  #  #filter(., 
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
  filter(., time >= as.Date("2014-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

sale_list_ratio_sf = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/saleListRatio_Sfr.csv", stringsAsFactors = FALSE) %>%
  #  #filter(., 
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
  filter(., time >= as.Date("2014-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

# Total Inventory

total_inv_all = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/totalInventory_All.csv", stringsAsFactors = FALSE) %>%
  #  #filter(., 
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
  filter(., time >= as.Date("2014-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

total_inv_condo = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/totalInventory_Condo.csv", stringsAsFactors = FALSE) %>%
  #  #filter(., 
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
  filter(., time >= as.Date("2014-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

total_inv_coop = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/totalInventory_Coop.csv", stringsAsFactors = FALSE) %>%
  #  #filter(., 
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
  filter(., time >= as.Date("2014-01-01")) %>% 
  select(., areaName, areaType, Borough, time, type, metric, value)

total_inv_sf = read.csv("https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/Data/totalInventory_Sfr.csv", stringsAsFactors = FALSE) %>%
  #  #filter(., 
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
  filter(., time >= as.Date("2014-01-01")) %>% 
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

# Mortgage payment calculation

mort_rate = 0.0785 #Annual rate for 30-year fixed mortgage

mortgage_df <- mutate(sales_df, 
                      down_payment = sales_df$value*0.2,
                      monthly_mort_payment = (sales_df$value*.80)*(mort_rate/12)/ (1-((1 + mort_rate/12)^(-360)))) %>%
  filter(., time == "2023-09-01", !is.na(value)) 

# Cleaning time series data
sales_ts <- median_ask_price_all %>% 
  select(areaName, Borough, metric, type, time, value) %>%
  group_by(areaName) %>%
  filter(., sum(is.na(value))<36) %>% #Only keeping areas that have less than 36 months of missing data
  mutate(value = ts_impute_vec(value, period = 1, lambda = NULL)) # Imputing values for the missing data


###############################################################################
#                                                                             #
#                           SHINY APP                                         #
#                                                                             #
###############################################################################


ui <- fluidPage(
  
  titlePanel(div('StreetEasy New York City Housing Sales and Rental Dashboard', style = 'color: white')),
  
  setBackgroundImage(src = "https://raw.githubusercontent.com/aaronwoodward1/Housing/main/NYC/NYC_brownstone.png", shinydashboard = FALSE),
  
  tags$head(tags$style('body {color:#0c18b3}')),
  
  # Application title
  #titlePanel(div('StreetEasy New York City Housing Sales and Rental Dashboard', style = 'color: white')),
  
 tabsetPanel(
    tabPanel("Sales Data",
            sidebarLayout(
               sidebarPanel(width = 3, h4("For the Interactive Map"),
                  selectInput("sales_type","Type of Property:",
                              choices = levels(factor(properties)),
                              selected = "All properties"),
                  selectInput("sales_metric", "Select Metric", 
                              choices = unique(sales_sf$metric),
                              selected = "Median Asking Price"),
                  selectInput("date", "Select Date:",
                              choices = unique(sales_sf$time),
                              selected = "2023-09-01"),
               
                  h4("For the Historical Plot"),
                  selectizeInput(
                    inputId = "sales_areaName",
                    label = "Select an area",
                    #choices = unique(rent_df$areaName),
                    choices = NULL,
                    selected = "NYC",
                    multiple = TRUE),
                  #options = list(maxOptions = 10)),
                  selectInput("sales_type_plot","Type of Property:", 
                              choices = levels(factor(properties)),
                              selected = "All properties"),
                  selectInput("sales_metric_plot", "Select Metric:",
                              choices = unique(sales_df$metric),
                              selected = "Median Asking Price"),
                  
                  h4("For Listing (Asking) Price Forecast"),
                  selectInput("forecast_area", "Select Area:",
                              choices = unique(sales_ts$areaName),
                              selected = "NYC"),
                  
                  h4("For Mortgage Calculation"),
                  selectInput("mort_area", "Select Area:",
                              choices = unique(mortgage_df$areaName),
                              selected = "NYC"),
                  selectInput("mort_property", "Type of Property:",
                              choices = levels(factor(properties)),
                              selected = "All properties")
                  
                  ),
               
                mainPanel(width = 9,
                         fluidRow(column(width = 12, style = "background-color:white;", h4("Interactive Map", align='left'),leafletOutput(outputId = "sales_map", width = 1000, height=350))),
                         fluidRow(column(width = 12, style = "background-color:white;", h4("Historical Sales Trends", align='left'), plotlyOutput("sales_history_plot"))),
                         fluidRow(column(width = 12, style = "background-color:white;", plotlyOutput("forecast"))),
                         fluidRow(column(width = 12, style = "background-color:white;", h4("Estimated Monthly Mortgage Payment", align='left'),
                                         h5("Based on median listing (asking) price, benchmark mortgage rate in New York, and down payment of 20%."), 
                                         uiOutput("mortgage")))
               )
            )
        ),
              
  tabPanel("Rental Data",
    sidebarLayout(
      sidebarPanel(width = 3, h4("For the Interactive Map"),
        selectInput("rent_type","Type of Rental:", 
                    #choices = sort(c(ordered_types,unique(rent_df$type))),
                    choices = levels(factor(rental)),
                    selected = "All Rentals"),

        selectInput("rent_metric", "Select Metric:",
                    choices = unique(rent_sf$metric),
                    selected = "Median Asking Rent"),
      selectInput("rent_date", "Select Date:",
                 choices = unique(rent_sf$time),
                 selected = "2023-09-01"),
    
      h4("For the Historical Plot"),
      selectizeInput(
        inputId = "rent_areaName",
        label = "Select an area",
        #choices = unique(rent_df$areaName),
        choices = NULL,
        selected = "NYC",
        multiple = TRUE),
        #options = list(maxOptions = 10)),
      selectInput("rent_type_plot","Type of Rental:", 
                  choices = levels(factor(rental)),
                  selected = "All Rentals"),
      selectInput("rent_metric_plot", "Select Metric:",
                  choices = unique(rent_df$metric),
                  selected = "Median Asking Rent")
      
      ),
      
      mainPanel(width = 9,
        fluidRow(column(width = 12, style = "background-color:white;", h4("Interactive Map", align='left'),leafletOutput(outputId = "rent_map", width = 1000, height=350))),
        fluidRow(column(width = 12, style = "background-color:white;", h4("Historical Asking Rent and Rental Inventory Trends", align='left'), plotlyOutput("rent_history_plot"))),
        fluidRow(column(width = 12, style = "background-color:white;", h4("Affordability of New York City Neighborhoods", align='left'),h5("Affordability is based on the median household income in New York City of $74,694 in 2022. Bubble sizes reflect median asking rent as of September 2023.", align='left'), plotlyOutput("rent_afford_plot"))) #h6("Affordability is based on the median household income in New York City of $74,694 in 2022", align='left'), 
        )
      )
    )
  )
)

  

server <- function(input, output, session){
  
  # RENTAL PLOTS
  rent_map_data <- reactive({
    filter(rent_sf, type == input$rent_type & metric == input$rent_metric & time == input$rent_date)
  })

  rent_plot_data <- reactive({
    as.data.frame(
      subset(rent_df, rent_df$areaName %in% input$rent_areaName & rent_df$type %in% input$rent_type_plot & rent_df$metric %in% input$rent_metric_plot)
    )
  }) #This code works!!!
  
  # Combine the selected variables into a new dataframe
  
  output$rent_map <- renderLeaflet({
    pal <- colorNumeric("plasma", NULL)
    
    rent_map_data() %>%
    leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(-73.9, 40.7, zoom = 10) %>%
    addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.65,
                fillColor = ~pal(value),
                label = ~paste0(areaName, ": ", formatC(value, big.mark = ","))) %>%
    addLegend(pal = pal, values = ~rent_map_data()$value, opacity = 0.65, position = "bottomleft",
              title = NULL)
  
    })

 updateSelectizeInput(session = session, 'rent_areaName', choices = unique(rent_df$areaName), selected = "NYC", server = TRUE)
 
 output$rent_history_plot <- renderPlotly({
   plot_ly(rent_plot_data(), x = ~time, y = ~value, type = 'scatter', mode = 'lines', color = ~areaName, connectgaps = TRUE) %>%
     layout(
       xaxis = list(title = ' '),
       yaxis = list(title = input$rent_metric_plot, tickformat = ",")
     ) 
  })
  
 z <- nyc_rent_afford_df$Median_ask_rent
 
 output$rent_afford_plot <- renderPlotly({
   p <- nyc_rent_afford_df %>%
     plot_ly(#nyc_rent_afford_df, 
                x=~Share_hhinc_to_rent, 
                y=~Rent_inventory, 
                type = "scatter", 
                mode = "markers",
                text = ~I(areaName),
                color = ~areaName, 
                sizes = c(10, 50),
                #size = ~Median_ask_rent,
                size = z,
                marker = list(opacity = 0.5, sizemode = "diameter"),
                #hoverinfo = 'text',
                hovertext = ~I(formatC(z, big.mark = ",",format = "d")),
                hovertemplate = paste0(
                  "Neighborhood: %{text}</b><br>",
                  "Percent of household income spent on rent: %{x:.1%}<br>",
                  "Total available rental units (Rental inventory): %{y:,.0f}<br>",
                  'Median asking rent: $%{hovertext}'
                )) %>%
                #hovertemplate = paste0(
                #    "<br><br>Neighborhood: ", areaName,"</b><br>",
                #    "Percent of household income spent on rent: ", percent(Share_hhinc_to_rent),"<br>",
                #    #"%{yaxis.title.text}: %{y:0,0f}<br>",
                #    'Median asking rent: $%{z:,.0f}'
                #    )) %>%
      layout(
             xaxis = list(title = 'Percentage of household income spent on rent', tickformat = ".0%"),
             yaxis = list(title = 'Total available rental units', tickformat = "," ))
  })
      
   # SALES PLOTS
   
   sales_map_data <- reactive({
     filter(sales_sf, type == input$sales_type & metric == input$sales_metric & time == input$date)
   })
   
   sales_plot_data <- reactive({
     as.data.frame(
       subset(sales_df, sales_df$areaName %in% input$sales_areaName & sales_df$type %in% input$sales_type_plot & sales_df$metric %in% input$sales_metric_plot)
     )
   }) #This code works!!!
   
   # Combine the selected variables into a new dataframe
   
   output$sales_map <- renderLeaflet({
     pal <- colorNumeric("plasma", NULL)
     
     sales_map_data() %>%
       leaflet() %>%
       addProviderTiles("CartoDB.Positron") %>%
       setView(-73.9, 40.7, zoom = 10) %>%
       addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.65,
                   fillColor = ~pal(value),
                   label = ~paste0(areaName, ": ", prettyNum(value, big.mark = ","))) %>%
                   # label = ~paste0(areaName, ": ", "%{value:$,.0f"})) %>%
       addLegend(pal = pal, values = ~sales_map_data()$value, opacity = 0.65, position = "bottomleft",
                   title = NULL) #%>%
       # hovertemplate = paste("%{sales_map_data()$areaName}: %{sales_map_data()$value:$,.0f}")
     
   })
   
   updateSelectizeInput(session = session, 'sales_areaName', choices = unique(sales_df$areaName), selected = "NYC", server = TRUE)
   
   output$sales_history_plot <- renderPlotly({
     plot_ly(sales_plot_data(), x = ~time, y = ~value, type = 'scatter', mode = 'lines', color = ~areaName, connectgaps = TRUE) %>%
       layout(
         xaxis = list(title = ' '),
         yaxis = list(title = input$sales_metric_plot, tickformat = ",")
       ) 
   })
   
   # Mortgage Calculation
   mortgage_data <- reactive({
     as.data.frame(filter(mortgage_df, areaName == input$mort_area, type == input$mort_property))
   })
   
   # output$mortgage <- renderText({
   #   #mortgage_data() %>%
   #   paste0("The average monthly 30-year mortgage payment for a ",~mortgage_data()$type ," in ", ~mortgage_data()$areaName, " is ", "$",
   #          formatC(~mortgage_data()$monthly_mort_payment, format="f", digits=2, big.mark=","))
   #          }))
   
  
   #payment <- mortgage_data()$monthly_mort_payment
   
   # output$mortgage <- renderText({
   #   #mortgage_data <- mortgage_df %>%
   #    # filter(., areaName == input$areaName, type == input$mort_property, time == '2023-09-01')
   #   paste0('The average monthly 30-year mortgage payment for a ',input$mort_property,' in ', input$mort_area, ' is $',#,
   #        formatC(mortgage_data()$monthly_mort_payment, format="f", digits=2, big.mark=","), ' as of September 2023.')
   #   
   # })
   
   output$mortgage <- renderUI({
     HTML(paste0("<b>",
        "Area: ", input$mort_area,
        "<br>",
        "Type of property: ", input$mort_property,
        "<br>",
        "Recent median listing (asking) price: $", formatC(mortgage_data()$value[2], format="f", digits=0, big.mark=","),
        "<br>",
        "Estimated down payment: $", formatC(mortgage_data()$down_payment[2], format="f", digits=2, big.mark=","),
        "<br>",
        "30-year benchmark mortgage interest rate: ", 7.85,"%",
        "<br>",
        "Estimated monthly payment: $", formatC(mortgage_data()$monthly_mort_payment[2], format="f", digits=2, big.mark=","),
        "</b>"
     ))
   })
   
   library(lubridate)
   
   # Forecast plot
   
   # Reactive dataframe for forecast
   forecast_data <- reactive({
     as.data.frame(filter(sales_ts, areaName == input$forecast_area))
   })
  
   # Plotting forecast
   output$forecast <- renderPlotly({
     # Train-Test split
     splits <- time_series_split(forecast_data(), 
                                 assess = "48 months",
                                 cumulative = TRUE
     )
     
     # Forecast: Using Prophet time series model
     
     model_prophet <- prophet_reg(
       seasonality_yearly = TRUE
     ) %>%
       set_engine("prophet") %>%
       fit(value ~ time, training(splits))
     
     # Model table
     model_table <- modeltime_table(model_prophet)
     
     # Calibrate
     calib_tbl <- model_table %>%
       modeltime_calibrate(testing(splits))
     
     future_forecast_tbl <- calib_tbl %>%
       modeltime_refit(forecast_data()) %>%
       modeltime_forecast(
         h = "12 months",
         actual_data = forecast_data()
       )
     
     future_forecast_tbl %>%
       plot_modeltime_forecast(., .title = paste0("12-month listing price forecast for ", input$forecast_area), .facet_scales = "free_y", .legend_show = FALSE)
     #title = paste0("12-month listing price forecast for ", input$forecast_area)
   })
   
   
}

shinyApp(ui, server)
