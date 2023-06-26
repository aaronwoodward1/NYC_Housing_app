###Choropleth Plot for NYC Housing App###
library(dplyr)
library(leaflet)
library(plotly)

#Setting color palette
pal <- colorNumeric("plasma", NULL)

#USE THIS CODE IN APP!!!
leaflet(filter(master_df, time == "2022-12-01")) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
              fillColor = ~pal(RentAskRentAll),
              label = ~paste0(areaName, ": ", formatC(RentAskRentAll, big.mark = ","))) %>%
  addLegend(pal = pal, values = ~RentAskRentAll, opacity = 1.0,
            labFormat = labelFormat())