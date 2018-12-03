

library(leaflet)
library(geojsonio)

# get polygon data
# https://github.com/simonepri/geo-maps/blob/master/info/countries-land.md
world <- geojson_read(
  "https://github.com/simonepri/geo-maps/releases/download/v0.6.0/countries-land-10km.geo.json", 
  what = "sp"
)

# generate random values
world@data$value <- runif(nrow(world@data))

# get color palette
color_pal <- colorNumeric(palette = "YlOrRd", domain = NULL)

# get leaflet map
leaflet() %>%
  # setView(lat = 50, lng = 15, zoom = 4) %>%
  addMapPane("background_map", zIndex = 410) %>%  # Level 1: bottom
  addMapPane("polygons", zIndex = 420) %>%        # Level 2: middle
  addMapPane("labels", zIndex = 430) %>%          # Level 3: top
  addProviderTiles(
    providers$Esri.WorldTerrain,
    options = pathOptions(pane = "background_map")
  ) %>%
  addPolygons(
    data = world, stroke = FALSE, smoothFactor = 0.2,
    fillOpacity = 0.6, fillColor = ~color_pal(value),
    options = pathOptions(pane = "polygons")
  ) %>%
  addProviderTiles(
    providers$Stamen.TonerLabels,
    options = pathOptions(pane = "labels"))