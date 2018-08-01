class_bipolar <- function(values, intervals, style){
  
  filter = values >= 0
  
  int_pos = trunc(intervals/2)
  int_neg = trunc(intervals/2)
  
  breaks_positive = classIntervals(values[filter], int_pos, style)$brks
  breaks_negative = classIntervals(values[!filter], int_neg, style)$brks
  
  breaks = c(
    breaks_negative[-length(breaks_negative)],
    0,
    breaks_positive[-1]
  )
  
  return(breaks)
  
}

mapplot <- function(data, idxid, year, intervals = 6, style = 'jenks', palette = 'YlOrRd') {
  
  mapdata = data$regions %>% 
    left_join(data$values %>% filter(IDXID == idxid & YEAR == year), 
              by = "ISOID")
  
  unit = data$indexes %>% filter(IDXID == idxid) %>% select(UNITS) %>% as.matrix() %>% as.vector()
  bipolar = data$indexes %>% filter(IDXID == idxid) %>% select(BIPOLAR) %>% as.matrix() %>% as.vector()
  
  breaks = NULL
  
  if (bipolar == 1)
    breaks = class_bipolar(mapdata$VALUE, intervals, style)
  else
    breaks = unique(classIntervals(mapdata$VALUE, intervals, style)$brks)
    
  
  qpal <- colorBin(palette, mapdata$VALUE,
                   bins = breaks)
  
  legendpal <- colorBin(palette, 
                        mapdata$VALUE,
                        reverse = TRUE,
                        bins = breaks)
  
  labels <- sprintf(
    "<strong>%s</strong><br/>%g %s",
    mapdata$name_local, mapdata$VALUE, unit
  ) %>% lapply(htmltools::HTML)
  
  leaflet(mapdata) %>% 
    addProviderTiles(providers$Wikimedia,
                     options = providerTileOptions(detectRetina = TRUE)) %>% 
    addPolygons(data = data$back,
                color = "#000000",
                fillOpacity = 0,
                opacity = 0.2,
                weight = 4) %>% 
    addPolygons(color = "#444444",
                weight = 0.5,
                smoothFactor = 0.5,
                opacity = 1,
                fillOpacity = 0.8,
                fillColor = ~qpal(VALUE),
                highlightOptions = highlightOptions(
                  color = "white", 
                  weight = 2, 
                  bringToFront = TRUE, 
                  sendToBack = TRUE),
                label = labels,
                labelOptions = labelOptions(
                  textsize = "14px",
                  direction = "auto")
                ) %>%
    addPolylines(data = data$rus_border, 
                 color = "#000000", 
                 smoothFactor = 0.5,
                 opacity = 0.2, 
                 weight = 5) %>% 
    addPolylines(data = data$rus_border, 
                 color = "#000000", 
                 smoothFactor = 0.5,
                 opacity = 0.8, 
                 weight = 1) %>% 
    addCircleMarkers(data = data$cities, 
                     radius = 2,
                     weight = 1,
                     color = 'black',
                     label = ~htmltools::htmlEscape(NAME_RU),
                     labelOptions = labelOptions(
                       permanent = T, 
                       textOnly = TRUE,
                       style = list("padding" = "5 px")
                     )
    ) %>% 
    addLegend(pal = legendpal, 
              values = ~VALUE, 
              opacity = 1, 
              title = unit,
              labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
}