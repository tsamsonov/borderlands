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

normalize_delta <- function(x, y, method = 'percentage') {
  if (method == 'percentage') {
    100 * (y - x) / x
  } else if (method == 'ratio') {
    y / x
  } else if (method == 'delta') {
    y - x
  }
}

make_labels <- function(sizes, labels) {
  paste0("<div style='display: inline-block;height: ", 
         sizes, "px;margin-top: 4px;line-height: ", 
         sizes, "px;'>", labels, "</div>")
}

addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.5){
  
  # set legend features
  shapes = rep("50%", length(sizes))
  borders = colors
  
  fill = adjustcolor(colors, alpha = 0.5)
  
  legend_colors <- paste0(fill, "; width:", sizes, "px; height:", sizes, "px; border:1px solid ", borders, "; border-radius:", shapes)
  legend_labels <- make_labels(sizes, labels)
  
  return(addLegend(map, colors = legend_colors, labels = legend_labels, opacity = opacity))
}

mapplot <- function(data, idxid, year, normalize = 'percentage', 
                    intervals = 6, style = 'jenks', palette = 'YlOrRd',
                    scale = 1, roundto = NULL) {
  
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
    filter(IDXID == idxid, YEAR %in% year) %>% 
    mutate(VALUE = VALUE * scale)
  
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
  
  
  map = leaflet() %>%
    addMapPane(name = "Back", zIndex = 410) %>%
    addMapPane(name = "Data", zIndex = 420) %>%
    addMapPane(name = "Labels", zIndex = 430) %>% # higher zIndex rendered on top
    addProviderTiles(providers$CartoDB.PositronNoLabels,
                     options = providerTileOptions(pane = "Back")) %>%
    addPolygons(data = mapdata,
                options = pathOptions(pane = "Back"),
                color = "#000000",
                fillOpacity = 0,
                opacity = 0.2,
                weight = 4)
  
  # cartograms
  if (sptype == 'int' || length(year) == 2) {
      map = map %>% 
      addPolygons(data = mapdata,
                  fillColor = ~qpal(VALUE),
                  # group = "Data",
                  options = pathOptions(pane = "Data"),
                  color = "#444444",
                  weight = 0.5,
                  smoothFactor = 0.5,
                  opacity = 1,
                  fillOpacity = 0.8,
                  highlightOptions = highlightOptions(
                    color = "white",
                    bringToFront = TRUE,
                    sendToBack = TRUE,
                    weight = 2),
                  label = labels,
                  labelOptions = labelOptions(
                    textsize = "14px",
                    direction = "auto")
      ) %>%
      addLegend(data = mapdata,
                pal = legendpal,
                values = ~VALUE,
                opacity = 1,
                title = unit,
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
      
  # diagrams
  } else {
    
    # tmap_mode("view")
    # tm_basemap('OpenStreetMap') +
    #   tm_shape(data$back) +
    #     tm_borders(col = "black",
    #              alpha = 0.2,
    #              lwd = 4) +
    #   tm_shape(mapdata) + 
    #     tm_polygons(alpha = 0.5, 
    #                 col = 'white',
    #                 lwd = 1) +
    #   tm_shape(data$rus_border) +
    #     tm_lines(col = "black",
    #              alpha = 0.2,
    #              lwd = 5) +
    #     tm_lines(col = "black",
    #              alpha = 0.8,
    #              lwd = 1) +
    #   tm_shape(data$cities) +
    #     tm_dots(shape = 19, col = 'white', alpha = 0.8, size = 0.01) +
    #     tm_text('NAME_RU', auto.placement = TRUE, remove.overlap = TRUE) +
    #   tm_shape(mapdata) + 
    #     tm_bubbles(size = "VALUE", 
    #                col = "red", 
    #                legend.max.symbol.size = 5,
    #                scale = 4, alpha = 0.5) +
    #   tm_view(set.view = c(50, 50, 4))
    
    breaks = classIntervals(mapdata$VALUE, intervals, style)$brks
    
    centers = sf::st_centroid(mapdata) %>% 
      dplyr::bind_cols(st_coordinates(.) %>% as_tibble()%>% dplyr::rename('lon' = 1, 'lat' = 2)) %>% 
      mutate(class = cut(.$VALUE, breaks, labels = FALSE))
    
    if(!is.null(roundto)) {
      breaks = round(breaks, roundto)
    }
    
    class_labels = rev(paste(breaks[-1], '-', breaks[-1-intervals]))
    
    map = map %>% 
      addPolygons(data = mapdata,
                  fillColor = '#FFFFFF',
                  options = pathOptions(pane = "Data"),
                  color = "#444444",
                  weight = 0.5,
                  smoothFactor = 0.5,
                  opacity = 1,
                  fillOpacity = 0.5,
                  highlightOptions = highlightOptions(
                    color = "black",
                    bringToFront = TRUE,
                    sendToBack = TRUE,
                    weight = 2),
                  label = labels,
                  labelOptions = labelOptions(
                    textsize = "14px",
                    direction = "auto")
                  ) %>% 
      addCircleMarkers(data = centers,
                       lng = ~lon, lat = ~lat,
                       weight = 1,
                       options = pathOptions(pane = "Data"),
                       radius = ~class * 5) %>% 
      addLegendCustom(colors = rep("blue", intervals), labels = class_labels, sizes = (intervals:1) * 10)
  }
  
  map %>% 
    addPolylines(data = data$rus_border,
                 options = pathOptions(pane = "Labels"),
                 color = "#000000",
                 smoothFactor = 0.5,
                 opacity = 0.2,
                 weight = 5) %>%
    addPolylines(data = data$rus_border,
                 options = pathOptions(pane = "Labels"),
                 color = "#000000",
                 smoothFactor = 0.5,
                 opacity = 0.8,
                 weight = 1) %>%
    addWMSTiles(
      "http://autolab.geogr.msu.ru:8080/geoserver/ows?",
      layers = "ne:ncclpd_cities",
      options = WMSTileOptions(format = "image/png", transparent = TRUE, 
                               opacity = 0.8,
                               # detectRetina = TRUE, 
                               pane = "Labels")
    ) %>%
    setView(lat = 50, lng = 50, zoom = 4) %>% 
    frameWidget(width = '100%', height = 410)
}