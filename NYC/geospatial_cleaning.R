#library(tigris)
library(dplyr)
library(leaflet)
library(sp)
library(ggmap)
library(maptools)
library(broom)
library(httr)
library(rgdal)
library(plotly)
library(ggplot2)
#library(terra)
library(sf)
library(s2)
#library(sp)


#setwd("~/Desktop/Datasets/Cities/NYC/Streeteasy_data/Master_Report/Zillow_neighborhoods")
#ggmap_api_secret = "AIzaSyBalbdxv9yyknVhv5Mbmz66XUhY2cUEnlE"
#ggmap::register_google(key = ggmap_api_secret, write = TRUE)

#get_googlemap(center = "New York, NY") %>% ggmap()

#get_googlemap(center = "New York, NY", zoom = 12, maptype = "satellite") %>% ggmap()


nyc_hoods_sf <- st_read(dsn  = "Zillow_neighborhoods/ZillowNeighborhoods-NY.shp")
nyc_hoods_sf <- st_read(dsn  = "ZillowNeighborhoods-NY.shp")

nyc_hoods_sf <- st_transform(nyc_hoods_sf, 4326)
nyc_hoods_sf <- nyc_hoods_sf %>% 
  filter(City == "New York")

nyc_hoods_sf <- nyc_hoods_sf %>%
  mutate(Borough = case_when(County == 'New York' ~ 'Manhattan',
                             County == 'Bronx' ~ 'Bronx',
                             County == 'Kings' ~ 'Brooklyn',
                             County == 'Queens' ~ 'Queens',
                             County == 'Richmond' ~ 'Staten Island'))



#Plotly viz via Mapbox token  - code works
mapbox_token = "pk.eyJ1IjoiYXdvb2R3YXJkMjEiLCJhIjoiY2xmZzRxejdtMHRwbTQwcGRmY3hjYmI1bSJ9.dVCc2iRCdBAAcFWKOH_gdQ"


#Reading the NYC Neighborhood shapefile
#nyc_hoods_sf <- st_read(dsn  = "ZillowNeighborhoods-NY.shp")
#nyc_hoods_sf <- st_transform(nyc_hoods_sf, 4326)
#nyc_hoods_sf <- nyc_hoods_sf %>% 
#  filter(City == "New York")




#mapboxToken <- paste(readLines("../.mapbox_token"), collapse="")    # You need your own token
Sys.setenv("MAPBOX_TOKEN" = mapbox_token) # for Orca

fig <- plot_mapbox(nyc_hoods_sf, split=~Name) 
fig <- fig %>%
  layout(
    mapbox = list(
      zoom = 6,
      style = "carto-positron"
    )
  ) 
fig <- fig %>%
  config(mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"))

fig

#fig <- plot_geo(nyc_hoods_sf, style = "carto-positron")
#fig

#fig <- plot_mapbox(nyc_hoods_sf, split=~Name)

fig <- plot_ly(nyc_hoods_sf, type = "scattergeo")
fig <-fig %>% 
  layout(
    mapbox = list(
      style = "carto-positron"
      )
)


fig

#Dissolving/Merging smaller neighborhoods into 1 larger neighborhood

###THIS CODE WORKS!!!###
#Subneighborhood Aggregation

###MANHATTAN###

#Midtown East
midtowneast <-  subset(nyc_hoods_sf, Name == "Turtle Bay" | Name == "Tudor City" | Name == "Sutton Place")

midtowneast <- midtowneast %>% 
  summarize(geometry = st_union(geometry))

midtowneast_df <- data.frame("State" = "NY", 
                             "County" = "New York",
                             "City" = "New York", 
                             "Name" = "Midtown East",
                             "RegionID" = "1XXXX", 
                             "geometry" = midtowneast$geometry, 
                             "Borough" = "Manhattan") 

midtowneast_df <- midtowneast_df %>% 
  st_as_sf()

 # Plotting
Sys.setenv("MAPBOX_TOKEN" = mapbox_token) # for Orca

fig <- plot_mapbox(midtowneast_df, split=~Name) 
fig <- fig %>%
  layout(
    mapbox = list(
      zoom = 6,
      style = "carto-positron"
    )
  ) 
fig <- fig %>%
  config(mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"))

fig



#Midtown West
midtownwest <-  subset(nyc_hoods_sf, Name == "Columbus Circle" | Name == "Clinton")

midtownwest <- midtownwest %>% 
  summarize(geometry = st_union(geometry))

midtownwest_df <- data.frame("State" = "NY", 
                             "County" = "New York",
                             "City" = "New York", 
                             "Name" = "Midtown West",
                             "RegionID" = "1XXXX", 
                             "geometry" = midtownwest$geometry, 
                             "Borough" = "Manhattan") 

midtownwest_df <- midtownwest_df %>% 
  st_as_sf()

# Plotting
Sys.setenv("MAPBOX_TOKEN" = mapbox_token) # for Orca

fig <- plot_mapbox(midtownwest_df, split=~Name) 
fig <- fig %>%
  layout(
    mapbox = list(
      zoom = 6,
      style = "carto-positron"
    )
  ) 
fig <- fig %>%
  config(mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"))

fig


#Midtown South
midtownsouth <-  subset(nyc_hoods_sf, Name == "Garment District" | RegionID == "274627")

midtownsouth <- midtownsouth %>% 
  summarize(geometry = st_union(geometry))

midtownsouth_df <- data.frame("State" = "NY", 
                             "County" = "New York",
                             "City" = "New York", 
                             "Name" = "Midtown South",
                             "RegionID" = "1XXXX", 
                             "geometry" = midtownsouth$geometry, 
                             "Borough" = "Manhattan") 

midtownsouth_df <- midtownsouth_df %>% 
  st_as_sf()

# Plotting
Sys.setenv("MAPBOX_TOKEN" = mapbox_token) # for Orca

fig <- plot_mapbox(midtownsouth_df, split=~Name) 
fig <- fig %>%
  layout(
    mapbox = list(
      zoom = 6,
      style = "carto-positron"
    )
  ) 
fig <- fig %>%
  config(mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"))

fig


#Greenwich Village
greenwichvillage <-  subset(nyc_hoods_sf, Name == "Greenwich Village" | Name == "NoHo")

greenwichvillage <- greenwichvillage %>% 
  summarize(geometry = st_union(geometry))

greenwichvillage_df <- data.frame("State" = "NY", 
                              "County" = "New York",
                              "City" = "New York", 
                              "Name" = "Greenwich Village",
                              "RegionID" = "1XXXX", 
                              "geometry" = greenwichvillage$geometry, 
                              "Borough" = "Manhattan") 

greenwichvillage_df <- greenwichvillage_df %>% 
  st_as_sf()

# Plotting
Sys.setenv("MAPBOX_TOKEN" = mapbox_token) # for Orca

fig <- plot_mapbox(greenwichvillage_df, split=~Name) 
fig <- fig %>%
  layout(
    mapbox = list(
      zoom = 6,
      style = "carto-positron"
    )
  ) 
fig <- fig %>%
  config(mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"))

fig


#Upper East Side
uppereastside <-  subset(nyc_hoods_sf, Name == "Upper East Side" | Name == "Carnegie Hill")

uppereastside <- uppereastside %>% 
  summarize(geometry = st_union(geometry))

uppereastside_df <- data.frame("State" = "NY", 
                                  "County" = "New York",
                                  "City" = "New York", 
                                  "Name" = "Upper East Side",
                                  "RegionID" = "1XXXX", 
                                  "geometry" = uppereastside$geometry, 
                                  "Borough" = "Manhattan") 

uppereastside_df <- uppereastside_df %>% 
  st_as_sf()

# Plotting
Sys.setenv("MAPBOX_TOKEN" = mapbox_token) # for Orca

fig <- plot_mapbox(uppereastside_df, split=~Name) 
fig <- fig %>%
  layout(
    mapbox = list(
      zoom = 6,
      style = "carto-positron"
    )
  ) 
fig <- fig %>%
  config(mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"))

fig


#West Harlem
westharlem <-  subset(nyc_hoods_sf, Name == "Manhattanville" | Name == "Hamilton Heights")

westharlem <- westharlem %>% 
  summarize(geometry = st_union(geometry))

westharlem_df <- data.frame("State" = "NY", 
                               "County" = "New York",
                               "City" = "New York", 
                               "Name" = "West Harlem",
                               "RegionID" = "1XXXX", 
                               "geometry" = westharlem$geometry, 
                               "Borough" = "Manhattan") 

westharlem_df <- westharlem_df %>% 
  st_as_sf()

# Plotting
Sys.setenv("MAPBOX_TOKEN" = mapbox_token) # for Orca

fig <- plot_mapbox(westharlem_df, split=~Name) 
fig <- fig %>%
  layout(
    mapbox = list(
      zoom = 6,
      style = "carto-positron"
    )
  ) 
fig <- fig %>%
  config(mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"))

fig


###BRONX###

#East Tremont
easttremont <-  subset(nyc_hoods_sf, Name == "East Tremont" | Name == "West Farms")

easttremont <- easttremont %>% 
  summarize(geometry = st_union(geometry))

easttremont_df <- data.frame("State" = "NY", 
                            "County" = "Bronx",
                            "City" = "New York", 
                            "Name" = "East Tremont",
                            "RegionID" = "1XXXX", 
                            "geometry" = easttremont$geometry, 
                            "Borough" = "Bronx") 

easttremont_df <- easttremont_df %>% 
  st_as_sf()

# Plotting
Sys.setenv("MAPBOX_TOKEN" = mapbox_token) # for Orca

fig <- plot_mapbox(easttremont_df, split=~Name) 
fig <- fig %>%
  layout(
    mapbox = list(
      zoom = 6,
      style = "carto-positron"
    )
  ) 
fig <- fig %>%
  config(mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"))

fig



#South Bronx -> Crotona Park East | Morrisania
#Westchester Heights -> Westchester Village

###Brooklyn###

#DUMBO/Vinegar Hill
dumbo <-  subset(nyc_hoods_sf, Name == "DUMBO" | Name == "Vinegar Hill")

dumbo <- dumbo %>% 
  summarize(geometry = st_union(geometry))

dumbo_df <- data.frame("State" = "NY", 
                             "County" = "Kings",
                             "City" = "New York", 
                             "Name" = "DUMBO",
                             "RegionID" = "1XXXX", 
                             "geometry" = dumbo$geometry, 
                             "Borough" = "Brooklyn") 

dumbo_df <- dumbo_df %>% 
  st_as_sf()

# Plotting
Sys.setenv("MAPBOX_TOKEN" = mapbox_token) # for Orca

fig <- plot_mapbox(dumbo_df, split=~Name) 
fig <- fig %>%
  layout(
    mapbox = list(
      zoom = 6,
      style = "carto-positron"
    )
  ) 
fig <- fig %>%
  config(mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"))

fig


#Bay Ridge/Fort Hamilton
bayridge <-  subset(nyc_hoods_sf, Name == "Bay Ridge" | Name == "Fort Hamilton")

bayridge <- bayridge %>% 
  summarize(geometry = st_union(geometry))

bayridge_df <- data.frame("State" = "NY", 
                       "County" = "Kings",
                       "City" = "New York", 
                       "Name" = "Bay Ridge",
                       "RegionID" = "1XXXX", 
                       "geometry" = bayridge$geometry, 
                       "Borough" = "Brooklyn") 

bayridge_df <- bayridge_df %>% 
  st_as_sf()

# Plotting
Sys.setenv("MAPBOX_TOKEN" = mapbox_token) # for Orca

fig <- plot_mapbox(bayridge_df, split=~Name) 
fig <- fig %>%
  layout(
    mapbox = list(
      zoom = 6,
      style = "carto-positron"
    )
  ) 
fig <- fig %>%
  config(mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"))

fig

#Bergen Beach
bergenbeach <-  subset(nyc_hoods_sf, Name == "Bergen Beach" | Name == "Georgetown")

bergenbeach_union <- bergenbeach %>% 
  summarize(geometry = st_union(geometry))

bergenbeach_df <- data.frame("State" = "NY", 
                          "County" = "Kings",
                          "City" = "New York", 
                          "Name" = "Bergen Beach",
                          "RegionID" = "1XXXX", 
                          "geometry" = bergenbeach$geometry, 
                          "Borough" = "Brooklyn") 

bergenbeach_df <- bergenbeach_df %>% 
  st_as_sf()

# Plotting
Sys.setenv("MAPBOX_TOKEN" = mapbox_token) # for Orca

fig <- plot_mapbox(bergenbeach_df, split=~Name) 
fig <- fig %>%
  layout(
    mapbox = list(
      zoom = 6,
      style = "carto-positron"
    )
  ) 
fig <- fig %>%
  config(mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"))

fig


#Sheepshead Bay
sheepsheadbay <-  subset(nyc_hoods_sf, Name == "Sheepshead Bay" | Name == "Homecrest")

sheepsheadbay_union <- sheepsheadbay %>%
#  s2_rebuild() %>%
  summarize(geometry = st_union(geometry)) 

sheepsheadbay_df <- data.frame("State" = "NY", 
                             "County" = "Kings",
                             "City" = "New York", 
                             "Name" = "Sheepshead Bay",
                             "RegionID" = "1XXXX", 
                             "geometry" = sheepsheadbay_union$geometry, 
                             "Borough" = "Brooklyn") 

sheepsheadbay_df <- sheepsheadbay_df %>% 
  st_as_sf()

# Plotting
Sys.setenv("MAPBOX_TOKEN" = mapbox_token) # for Orca

fig <- plot_mapbox(sheepsheadbay_df, split=~Name) 
fig <- fig %>%
  layout(
    mapbox = list(
      zoom = 6,
      style = "carto-positron"
    )
  ) 
fig <- fig %>%
  config(mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"))

fig


#East Flatbush
eastflatbush <-  subset(nyc_hoods_sf, Name == "East Flatbush" | Name == "Wingate")

eastflatbush <- eastflatbush %>% 
  summarize(geometry = st_union(geometry))

eastflatbush_df <- data.frame("State" = "NY", 
                             "County" = "Kings",
                             "City" = "New York", 
                             "Name" = "East Flatbush",
                             "RegionID" = "1XXXX", 
                             "geometry" = eastflatbush$geometry, 
                             "Borough" = "Brooklyn") 

eastflatbush_df <- eastflatbush_df %>% 
  st_as_sf()

# Plotting
Sys.setenv("MAPBOX_TOKEN" = mapbox_token) # for Orca

fig <- plot_mapbox(eastflatbush_df, split=~Name) 
fig <- fig %>%
  layout(
    mapbox = list(
      zoom = 6,
      style = "carto-positron"
    )
  ) 
fig <- fig %>%
  config(mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"))

fig


###QUEENS###

#Astoria
astoria <-  subset(nyc_hoods_sf, Name == "Astoria" | Name == "Astoria Heights")

astoria <- astoria %>% 
  summarize(geometry = st_union(geometry))

astoria_df <- data.frame("State" = "NY", 
                              "County" = "Queens",
                              "City" = "New York", 
                              "Name" = "Astoria",
                              "RegionID" = "1XXXX", 
                              "geometry" = astoria$geometry, 
                              "Borough" = "Queens") 

astoria_df <- astoria_df %>% 
  st_as_sf()

# Plotting
Sys.setenv("MAPBOX_TOKEN" = mapbox_token) # for Orca

fig <- plot_mapbox(astoria_df, split=~Name) 
fig <- fig %>%
  layout(
    mapbox = list(
      zoom = 6,
      style = "carto-positron"
    )
  ) 
fig <- fig %>%
  config(mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"))

fig


#Rockaways
rockaways <-  subset(nyc_hoods_sf, Name == "Fort Tilden" | Name == "Breezy Point" | Name == "Roxbury" | 
                       Name == "Jacob Riis Park" | Name == "Neponsit" | Name == "Belle Harbor" | 
                       Name == "Rockaway Park" | Name == "Rockaway Beach" | Name == "Arverne" |
                       Name == "Far Rockaway")

rockaways <- rockaways %>% 
  summarize(geometry = st_union(geometry))

rockaways_df <- data.frame("State" = "NY", 
                         "County" = "Queens",
                         "City" = "New York", 
                         "Name" = "Rockaways",
                         "RegionID" = "1XXXX", 
                         "geometry" = rockaways$geometry, 
                         "Borough" = "Queens") 

rockaways_df <- rockaways_df %>% 
  st_as_sf()

# Plotting
Sys.setenv("MAPBOX_TOKEN" = mapbox_token) # for Orca

fig <- plot_mapbox(rockaways_df, split=~Name) 
fig <- fig %>%
  layout(
    mapbox = list(
      zoom = 6,
      style = "carto-positron"
    )
  ) 
fig <- fig %>%
  config(mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"))

fig


#Forest Hills
foresthills <-  subset(nyc_hoods_sf, Name == "Forest Hills" | Name == "Forest Hills Gardens")

foresthills <- foresthills %>% 
  summarize(geometry = st_union(geometry))

foresthills_df <- data.frame("State" = "NY", 
                           "County" = "Queens",
                           "City" = "New York", 
                           "Name" = "Forest Hills",
                           "RegionID" = "1XXXX", 
                           "geometry" = foresthills$geometry, 
                           "Borough" = "Queens") 

foresthills_df <- foresthills_df %>% 
  st_as_sf()

# Plotting
Sys.setenv("MAPBOX_TOKEN" = mapbox_token) # for Orca

fig <- plot_mapbox(foresthills_df, split=~Name) 
fig <- fig %>%
  layout(
    mapbox = list(
      zoom = 6,
      style = "carto-positron"
    )
  ) 
fig <- fig %>%
  config(mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"))

fig


#Flushing
flushing <-  subset(nyc_hoods_sf, Name == "Flushing" | RegionID == "196538")

flushing <- flushing %>% 
  summarize(geometry = st_union(geometry))

flushing_df <- data.frame("State" = "NY", 
                             "County" = "Queens",
                             "City" = "New York", 
                             "Name" = "Flushing",
                             "RegionID" = "1XXXX", 
                             "geometry" = flushing$geometry, 
                             "Borough" = "Queens") 

flushing_df <- flushing_df %>% 
  st_as_sf()

# Plotting
Sys.setenv("MAPBOX_TOKEN" = mapbox_token) # for Orca

fig <- plot_mapbox(flushing_df, split=~Name) 
fig <- fig %>%
  layout(
    mapbox = list(
      zoom = 6,
      style = "carto-positron"
    )
  ) 
fig <- fig %>%
  config(mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"))

fig


#Hollis
hollis <-  subset(nyc_hoods_sf, Name == "Hollis" | Name == "Holliswood")

hollis <- hollis %>% 
  summarize(geometry = st_union(geometry))

hollis_df <- data.frame("State" = "NY", 
                          "County" = "Queens",
                          "City" = "New York", 
                          "Name" = "Hollis",
                          "RegionID" = "1XXXX", 
                          "geometry" = hollis$geometry, 
                          "Borough" = "Queens") 

hollis_df <- hollis_df %>% 
  st_as_sf()

# Plotting
Sys.setenv("MAPBOX_TOKEN" = mapbox_token) # for Orca

fig <- plot_mapbox(hollis_df, split=~Name) 
fig <- fig %>%
  layout(
    mapbox = list(
      zoom = 6,
      style = "carto-positron"
    )
  ) 
#fig <- fig %>%
#  config(mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"))

fig


#Whitestone
whitestone <-  subset(nyc_hoods_sf, Name == "Whitestone" | Name == "Malba")

whitestone <- whitestone %>% 
  summarize(geometry = st_union(geometry))

whitestone_df <- data.frame("State" = "NY", 
                        "County" = "Queens",
                        "City" = "New York", 
                        "Name" = "Whitestone",
                        "RegionID" = "1XXXX", 
                        "geometry" = whitestone$geometry, 
                        "Borough" = "Queens") 

whitestone_df <- whitestone_df %>% 
  st_as_sf()

# Plotting
Sys.setenv("MAPBOX_TOKEN" = mapbox_token) # for Orca

fig <- plot_mapbox(whitestone_df, split=~Name) 
fig <- fig %>%
  layout(
    mapbox = list(
      zoom = 6,
      style = "carto-positron"
    )
  ) 
#fig <- fig %>%
#  config(mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"))

fig


#Name change: Rochdale -> South Jamaica - Rochdale


###Staten Island###
#Name change: Port Ivory -> Howland Hook

#New Brighton
newbrighton <-  subset(nyc_hoods_sf, Name == "newbrighton" | Name == "Malba")

newbrighton <- newbrighton %>% 
  summarize(geometry = st_union(geometry))

newbrighton_df <- data.frame("State" = "NY", 
                            "County" = "Queens",
                            "City" = "New York", 
                            "Name" = "newbrighton",
                            "RegionID" = "1XXXX", 
                            "geometry" = newbrighton$geometry, 
                            "Borough" = "Queens") 

newbrighton_df <- newbrighton_df %>% 
  st_as_sf()

# Plotting
Sys.setenv("MAPBOX_TOKEN" = mapbox_token) # for Orca

fig <- plot_mapbox(newbrighton_df, split=~Name) 
fig <- fig %>%
  layout(
    mapbox = list(
      zoom = 6,
      style = "carto-positron"
    )
  ) 
#fig <- fig %>%
#  config(mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"))

fig


####Removing the neighborhoods that were used in the neighborhood aggregation

#nyc_hoods_clean <- subset(nyc_hoods_sf, Name != 'Turtle Bay'| Name != 'Sutton Place')

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
                                                      'Hollis','Holliswood','Whitestone','Malba','Murray Hill'
                                                      )))
                          #!(RegionID %in% c('196538','274627')))


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
#nyc_hoods_clean <- rbind(nyc_hoods_clean,newbrighton_df)
nyc_hoods_clean <- rbind(nyc_hoods_clean,rockaways_df)
nyc_hoods_clean <- rbind(nyc_hoods_clean,uppereastside_df)
nyc_hoods_clean <- rbind(nyc_hoods_clean,westharlem_df)
nyc_hoods_clean <- rbind(nyc_hoods_clean,whitestone_df)

nyc_hoods_clean <- sf::st_cast(nyc_hoods_clean, "MULTIPOLYGON")

#Removing Staten Island neighborhoods because StreetEasy doesn't have housing data for Staten Island neighborhoods

nyc_hoods_clean <- subset(nyc_hoods_clean, Borough!="Staten Island")

#nyc_hoods_clean <- subset(nyc_hoods_clean, (Borough!="Manhattan" & Name != "Brooklyn Heights") 




#Re-naming a couple of neighborhoods to make merging the Streeteasy data easier.
nyc_hoods_clean$Name <- gsub("Battery Park","Battery Park City",nyc_hoods_clean$Name)
nyc_hoods_clean$Name <- gsub("Floral park","Floral Park",nyc_hoods_clean$Name)
nyc_hoods_clean$Name <- gsub("Flatiron District","Flatiron",nyc_hoods_clean$Name)
nyc_hoods_clean$Name <- gsub("Gramercy","Gramercy Park",nyc_hoods_clean$Name)
nyc_hoods_clean$Name <- gsub("Stuyvesant Town","Stuyvesant Town/PCV",nyc_hoods_clean$Name)
nyc_hoods_clean$Name <- gsub("Bedford Stuyvesant","Bedford-Stuyvesant",nyc_hoods_clean$Name)
nyc_hoods_clean$Name <- gsub("Douglaston-Little Neck","Douglaston",nyc_hoods_clean$Name)
nyc_hoods_clean$Name <- gsub("Downtown","Downtown Brooklyn",nyc_hoods_clean$Name)


#Running the following code to check whether the neighborhoods have properly aggregated
Sys.setenv("MAPBOX_TOKEN" = mapbox_token) # for Orca

fig <- plot_mapbox(nyc_hoods_clean, split=~Name) 
fig <- fig %>%
  layout(
    mapbox = list(
      zoom = 6,
      style = "carto-positron"
    )
  ) 
fig <- fig %>%
  config(mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"))

fig


nyc_hoods_clean <- subset(nyc_hoods_clean, select = c("Name", "RegionID", "Borough", "geometry"))

nyc_hoods_clean <- nyc_hoods_clean %>%
  rename("areaName" = "Name")

#Merging StreetEasy_df with nyc_hoods_clean sf dataframe
master_df <- merge(StreetEasy_df, nyc_hoods_clean, by = c("areaName", "Borough"))






#Google map viz

#nyc_hoods_tidy <- tidy(nyc_hoods_sf, group=Name) 

#nyc <- get_googlemap(center = "New York, NY") 
#nyc

#ggmap(nyc) +
#  geom_sf(data=nyc_hoods_sf, 
#               color="blue") 

#ggmap(nyc_hoods_sf)