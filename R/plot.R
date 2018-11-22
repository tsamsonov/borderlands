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

normalize_delta = function(x, y, method = 'percentage') {
  if (method == 'percentage') {
    100 * (y - x) / x
  } else if (method == 'ratio') {
    y / x
  } else if (method == 'delta') {
    y - x
  }
}

mapplot <- function(data, idxid, year, normalize = 'percentage', 
                    intervals = 6, style = 'jenks', palette = 'YlOrRd') {
  
  stopifnot(length(year) < 3)
  
  unit = data$indexes %>% 
    filter(IDXID == idxid) %>% 
    pull(UNITS)
  
  bipolar = data$indexes %>% 
    filter(IDXID == idxid) %>% 
    pull(BIPOLAR)
  
  # print(bipolar)
  
  sptype = data$indexes %>% 
    filter(IDXID == idxid) %>% 
    pull(SPTYPE)
  
  df = data$values %>% 
    filter(IDXID == idxid, YEAR %in% year)
  
  # print(df)
  
  if (length(year) == 2) {
    df = df %>% 
      group_by(IDXID, ISOID) %>%
      arrange(YEAR) %>%
      summarise(VALUE = normalize_delta(first(VALUE), last(VALUE)), normalize)
    
    if (normalize == 'percentage') {
      unit = '%'
      bipolar = 1
    } else if (normalize == 'ratio') {
      unit = 'доля'
    } else if (normalize == 'delta') {
      bipolar = 1
    }
  }
  
  # print(bipolar)
  
  mapdata = data$regions %>% left_join(df, by = "ISOID")
  
  breaks = NULL
  
  if (bipolar == 1)
    breaks = class_bipolar(mapdata$VALUE, intervals, style)
  else
    breaks = unique(classIntervals(mapdata$VALUE, intervals, style)$brks)
    
  
  qpal = colorBin(palette, mapdata$VALUE,
                   bins = breaks)
  
  legendpal = colorBin(palette, 
                        mapdata$VALUE,
                        reverse = TRUE,
                        bins = breaks)
  
  labels = sprintf(
    "<strong>%s</strong><br/>%g %s",
    mapdata$name_local, mapdata$VALUE, unit
  ) %>% lapply(htmltools::HTML)
  
  if (sptype == 'int' || length(year) == 2) {
    
    tmap_mode("view")
    ## tmap mode set to interactive viewing
    tm_basemap('OpenStreetMap') +
    tm_shape(data$back) +
      tm_borders(col = "black",
                 alpha = 0.2,
                 lwd = 4) +
    tm_shape(mapdata) + 
      tm_polygons('VALUE',
                  palette = palette,
                  breaks = breaks,
                  lwd = 1) +
    tm_shape(data$rus_border) +
      tm_lines(col = "black",
               alpha = 0.2,
               lwd = 5) +
      tm_lines(col = "black",
               alpha = 0.8,
               lwd = 1) +
    tm_shape(data$cities) +
      tm_dots(shape = 19, col = 'white', alpha = 0.8, size = 0.01) +
      tm_text('NAME_RU', auto.placement = TRUE, remove.overlap = TRUE) +
    tm_view(set.view = c(50, 50, 4))
    # 
    # leaflet(mapdata) %>% 
    #   addProviderTiles(providers$Wikimedia,
    #                    options = providerTileOptions(detectRetina = TRUE)) %>% 
    #   addPolygons(data = data$back,
    #               color = "#000000",
    #               fillOpacity = 0,
    #               opacity = 0.2,
    #               weight = 4) %>% 
    #   addPolygons(color = "#444444",
    #               weight = 0.5,
    #               smoothFactor = 0.5,
    #               opacity = 1,
    #               fillOpacity = 0.8,
    #               fillColor = ~qpal(VALUE),
    #               highlightOptions = highlightOptions(
    #                 color = "white", 
    #                 # bringToFront = TRUE, 
    #                 # sendToBack = TRUE,
    #                 weight = 2),
    #               label = labels,
    #               labelOptions = labelOptions(
    #                 textsize = "14px",
    #                 direction = "auto")
    #   ) %>% 
    #   addLegend(pal = legendpal, 
    #             values = ~VALUE, 
    #             opacity = 1, 
    #             title = unit,
    #             labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
    #   addPolylines(data = data$rus_border, 
    #                color = "#000000", 
    #                smoothFactor = 0.5,
    #                opacity = 0.2, 
    #                weight = 5) %>% 
    #   addPolylines(data = data$rus_border, 
    #                color = "#000000", 
    #                smoothFactor = 0.5,
    #                opacity = 0.8, 
    #                weight = 1) %>% 
    #   addCircleMarkers(data = data$cities, 
    #                    radius = 2,
    #                    weight = 1,
    #                    color = 'black',
    #                    label = ~htmltools::htmlEscape(NAME_RU),
    #                    labelOptions = labelOptions(
    #                      permanent = T,
    #                      # noHide = F,
    #                      textOnly = TRUE,
    #                      offset = c(3,-3),
    #                      style = list("padding" = "5 px")
    #                    )
    #   ) %>% 
    #   addCircleMarkers(data = data$capitals, 
    #                    radius = 3,
    #                    weight = 1,
    #                    color = 'black',
    #                    label = ~htmltools::htmlEscape(NAME_RU),
    #                    labelOptions = labelOptions(
    #                      permanent = T,
    #                      # noHide = F,
    #                      textOnly = TRUE,
    #                      offset = c(5,-5),
    #                      style = list("padding" = "5 px",
    #                                   "font-style" = "semibold",
    #                                   "font-size" = "12px")
    #                    )) %>% 
    #   setView(lat = 50, lng = 50, zoom = 4)
  } else {
    
    tmap_mode("view")
    ## tmap mode set to interactive viewing
    tm_basemap('OpenStreetMap') +
      tm_shape(data$back) +
        tm_borders(col = "black",
                 alpha = 0.2,
                 lwd = 4) +
      tm_shape(mapdata) + 
        tm_polygons(alpha = 0.5, 
                    col = 'white',
                    lwd = 1) +
      tm_shape(data$rus_border) +
        tm_lines(col = "black",
                 alpha = 0.2,
                 lwd = 5) +
        tm_lines(col = "black",
                 alpha = 0.8,
                 lwd = 1) +
      tm_shape(data$cities) +
        tm_dots(shape = 19, col = 'white', alpha = 0.8, size = 0.01) +
        tm_text('NAME_RU', auto.placement = TRUE, remove.overlap = TRUE) +
      tm_shape(mapdata) + 
        tm_bubbles(size = "VALUE", 
                   col = "red", 
                   legend.max.symbol.size = 5,
                   scale = 4, alpha = 0.5) +
      tm_view(set.view = c(50, 50, 4))
    
    # m = addCircles(m, lng = ~lon, lat = ~lat, weight = 1,
    #                radius = ~VALUE * 0.01)
  }
  
}