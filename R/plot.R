# idxid = 0
# year = c(2009, 2016)
# segment = 2

get_segment_title = function(segment) {
  switch(as.character(segment),
         '1' = 'Норвегия—Финляндия',
         '2' = 'Эстония-Латвия—Беларусь—Украина',
         '3' = 'Кавказ',
         '4' = 'Казахстан',
         '5' = 'Китай—Монголия—КНДР',
         '6' = 'Литва-Польша')
}

gradplot = function(data, idxid, year, segment) {
  tab = data$values %>% 
    dplyr::filter(IDXID == idxid, YEAR %in% year)
  
  plotdata = data$gradients %>%
    filter(SEGMENT == segment) %>% 
    mutate(distance = cumsum(LENGTH) - LENGTH) %>% 
    left_join(tab, by = c("ISOID1" = "ISOID")) %>% 
    left_join(tab, by = c("ISOID2" = "ISOID", "YEAR" = "YEAR")) %>% 
    mutate(ratio = VALUE.x / VALUE.y,
           gradient = if_else(ratio < 1, -1/ratio + 1, ratio - 1)) %>% 
    arrange(ORD)
  
  ymn = min(plotdata$gradient)
  ymx = max(plotdata$gradient)
  
  ymin = -max(abs(ymn), abs(ymx))
  ymax = max(abs(ymn), abs(ymx))
  
  # Создаем вертикальные линии для стран
  countries = filter(plotdata, ENTER == 1, YEAR == year[1])
  
  line = list(
    type = "line",
    line = list(color = toRGB('black')),
    xref = "x",
    yref = "y"
  )
  
  lines = list()
  xcenters = vector()
  
  for (i in 1:nrow(countries)){
    line[["y0"]] = countries[i, "distance"]
    line[["y1"]] = countries[i, "distance"]
    line[["x0"]] = ymin
    line[["x1"]] = ymax
    lines = c(lines, list(line))
    xcenters[i] = mean(c(countries[i, "distance"], countries[i+1, "distance"]))
  }
  xcenters[i] = countries[i, "distance"] + 0.5 * countries[i, "LENGTH_PART"]
  
  line[["y0"]] = countries[i, "distance"] + countries[i, "LENGTH_PART"]
  line[["y1"]] = countries[i, "distance"] + countries[i, "LENGTH_PART"]
  line[["x0"]] = ymin
  line[["x1"]] = ymax
  lines = c(lines, list(line))
  
  # Создаем вертикальные линии для субъектов РФ
  regionsrf = filter(plotdata, ENTER1 == 1, YEAR == year[1])
  
  line = list(
    type = "line",
    opacity = 1,
    line = list(color = "grey", width = 1),
    xref = "x",
    yref = "y"
  )
  
  lines1 = list()
  xcenters1 = vector()
  for (i in 1:nrow(regionsrf)){
    line[["y0"]] = regionsrf[i, "distance"]
    line[["y1"]] = regionsrf[i, "distance"]
    line[["x0"]] = 0
    line[["x1"]] = ymax
    lines1 = c(lines1, list(line))
    xcenters1[i] = mean(c(regionsrf[i, "distance"], regionsrf[i+1, "distance"]))
  }
  xcenters1[i] = regionsrf[i, "distance"] + 0.5 * regionsrf[i, "LENGTH"]
  
  # Создаем вертикальные линии для субъектов зарубежных государств
  regionsfor = filter(plotdata, ENTER2 == 1, YEAR == year[1])
  
  line = list(
    type = "line",
    opacity = 1,
    line = list(color = "grey", width = 1),
    xref = "x",
    yref = "y"
  )
  
  lines2 = list()
  xcenters2 = vector()
  for (i in 1:nrow(regionsfor)){
    line[["y0"]] = regionsfor[i, "distance"]
    line[["y1"]] = regionsfor[i, "distance"]
    line[["x0"]] = ymin
    line[["x1"]] = 0
    lines2 = c(lines2, list(line))
    xcenters2[i] = mean(c(regionsfor[i, "distance"],regionsfor[i+1, "distance"]))
  }
  xcenters2[i] = regionsfor[i, "distance"] + 0.5 * regionsfor[i, "LENGTH"]
  
  # Формируем входные данные для рисования
  xvals = plotdata %>% filter(YEAR == year[1]) %>% pull(distance)
  yvals = plotdata %>% filter(YEAR == year[1]) %>% pull(gradient)
  tvals = plotdata %>% filter(YEAR == year[1]) %>% pull(NAME2)
  
  n = length(xvals)
  xvals[n+1] = xvals[n] + plotdata[nrow(plotdata), "LENGTH"]
  yvals[n+1] = yvals[n]
  tvals[n+1] = NA
  
  xvals2 = plotdata %>% filter(YEAR == year[2]) %>% pull(distance)
  yvals2 = plotdata %>% filter(YEAR == year[2]) %>% pull(gradient)
  tvals2 = plotdata %>% filter(YEAR == year[2]) %>% pull(NAME2)
  
  n = length(xvals2)
  xvals2[n+1] = xvals[n] + plotdata[nrow(plotdata), "LENGTH"]
  yvals2[n+1] = yvals[n]
  tvals2[n+1] = NA
  
  # рисуем
  plot_ly(x = yvals, 
          y = xvals, 
          type = 'scatter', 
          mode = 'lines', 
          fill = 'tozerox',
          name = year[1],
          text = tvals,
          line = list(shape = "hv")) %>%
    add_trace(x = yvals2,
              y = xvals2,
              name = year[2],
              text = tvals2,
              fill = 'tozerox') %>%
    layout(yaxis = list(title = 'Расстояние, км', 
                        autorange = "reversed",
                        showgrid = FALSE),
           xaxis = list(title = '',
                        gridwidth = 0.1,
                        gridcolor = toRGB("gray95")),
           title = get_segment_title(segment),
           shapes = c(lines1, lines2, lines)) %>%
    add_annotations(y = xcenters,
                    x = 0, #ymin - 0.05 * abs(ymin),
                    text = countries$COUNTRY,
                    font = list(sfamily = 'sans serif bold',
                                size = 14),
                    # bgcolor = 'white',
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    add_annotations(y = xcenters2,
                    x = 0.5*ymin,
                    font = list(size = 10),
                    text = regionsfor$NAME2,
                    # textangle = -90,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    add_annotations(y = xcenters1,
                    x = 0.7*ymax,
                    font = list(size = 10),
                    text = regionsrf$NAME1,
                    # textangle = -90,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE)
}