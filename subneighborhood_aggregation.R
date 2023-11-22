###############################################################################
#                                                                             #
#                           NYC NEIGHBORHOOD SHAPEFILE DATA                   #
#                                                                             #
###############################################################################
#Setting working directory
setwd("/Users/aaronwoodward/Desktop/Datasets/Cities/NYC/Streeteasy_data/Master_report")


#WORKING WITH NEIGHBORHOOD SHAPEFILE
#Reading the shapefile into R.
nyc_hoods_sf <- st_read(dsn  = "Zillow_neighborhoods/ZillowNeighborhoods-NY.shp")
#nyc_hoods_sf <- st_read(dsn  = "ZillowNeighborhoods-NY.shp")

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
