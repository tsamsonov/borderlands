library(plotly)
library(ggplot2)
library(rgdal)

setwd("/Volumes/Work/_Kolosov")
fgdb = "Borderlands.gdb"
# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list = ogrListLayers(fgdb)
print(fc_list)

# Читаем границы
fc <- readOGR(dsn=fgdb,layer="gradients",stringsAsFactors = FALSE)

# Упорядочиваем сегменты
fcorder <- fc[order(fc$ORD),]

# Преобразуем показатель
# fcorder$ginv <- ifelse(fcorder$grad_2013 < 1,
#                     -1/fcorder$grad_2013 + 1,
#                        fcorder$grad_2013 - 1)

fcorder$ginv <- ifelse(fcorder$grad_2013 < 1,
                       100 * (-1/fcorder$grad_2013 + 1),
                       100 * (fcorder$grad_2013 - 1))

# Отбираем сегменты
segments <- c(4,5)
kaz <- fcorder[fcorder$Segment %in% segments,]

# Формируем накопленное расстояние
kaz$distance <- 0
for (i in 2:nrow(kaz)){
  kaz@data[i, "distance"] <- kaz@data[i-1, "distance"] +
                             kaz@data[i-1, "LENGTH_GEO"]
}

# Ищем минимум и максимум
ymin <- min(kaz$ginv[!is.infinite(kaz$ginv)])
ymax <- max(kaz$ginv[!is.infinite(kaz$ginv)])

# Создаем вертикальные линии для стран
countries <- filter(kaz@data, Enter == 1)

line <- list(
  type = "line",
  line = list(color = toRGB('sienna4')),
  xref = "x",
  yref = "y"
)

lines <- list()
xcenters <- vector()
for (i in 1:nrow(countries)){
  line[["x0"]] <- countries[i, "distance"]
  line[["x1"]] <- countries[i, "distance"]
  line[["y0"]] <- ymin
  line[["y1"]] <- ymax
  lines <- c(lines, list(line))
  xcenters[i] <- mean(c(countries[i, "distance"],countries[i+1, "distance"]))
}
xcenters[i] <- countries[i, "distance"] + 0.5*countries[i, "SUM_LENGTH_GEO"]

line[["x0"]] <- countries[i, "distance"] + countries[i, "SUM_LENGTH_GEO"]
line[["x1"]] <- countries[i, "distance"] + countries[i, "SUM_LENGTH_GEO"]
line[["y0"]] <- ymin
line[["y1"]] <- ymax
lines <- c(lines, list(line))

# Создаем вертикальные линии для субъектов РФ
regionsrf <- filter(kaz@data, Enter1 == 1)

line <- list(
  type = "line",
  opacity = 0.5,
  line = list(color = "grey", width = 0.5),
  xref = "x",
  yref = "y"
)

lines1 <- list()
xcenters1 <- vector()
for (i in 1:nrow(regionsrf)){
  line[["x0"]] <- regionsrf[i, "distance"]
  line[["x1"]] <- regionsrf[i, "distance"]
  line[["y0"]] <- 0
  line[["y1"]] <- ymax
  lines1 <- c(lines1, list(line))
  xcenters1[i] <- mean(c(regionsrf[i, "distance"],regionsrf[i+1, "distance"]))
}
xcenters1[i] <- regionsrf[i, "distance"] + 0.5*regionsrf[i, "LENGTH_GEO"]

# Создаем вертикальные линии для субъектов зарубежных государств
regionsfor <- filter(kaz@data, Enter2 == 1)

line <- list(
  type = "line",
  opacity = 0.5,
  line = list(color = "grey", width = 0.5),
  xref = "x",
  yref = "y"
)

lines2 <- list()
xcenters2 <- vector()
for (i in 1:nrow(regionsfor)){
  line[["x0"]] <- regionsfor[i, "distance"]
  line[["x1"]] <- regionsfor[i, "distance"]
  line[["y0"]] <- ymin
  line[["y1"]] <- 0
  lines2 <- c(lines2, list(line))
  xcenters2[i] <- mean(c(regionsfor[i, "distance"],regionsfor[i+1, "distance"]))
}
xcenters2[i] <- regionsfor[i, "distance"] + 0.5*regionsfor[i, "LENGTH_GEO"]

# Формируем входные данные для рисования
xvals <- kaz$distance
yvals <- kaz$ginv
tvals <- kaz$F5

n <- length(xvals)
xvals[n+1] <- xvals[n] + kaz@data[n, "LENGTH_GEO"]
yvals[n+1] <- yvals[n]
tvals[n+1] <- NA

xvals2 <- xvals
yvals2 <- -1.5*cos(yvals*yvals)*cos(xvals)
tvals2 <- tvals

pl <- plot_ly(x = xvals, 
        y = yvals, 
        type = 'scatter', 
        mode = 'lines', 
        fill = 'tozeroy',
        name = '2013',
        line = list(shape = "hv"),
        text = tvals) %>%
      add_trace(x = xvals2, 
                y = yvals2,
                name = '2000',
                fill = 'tozeroy') %>%
      layout(xaxis = list(title = 'Расстояние, км', 
                          showgrid = FALSE),
             yaxis = list(title = 'Градиент',
                          gridwidth = 0.1,
                          # tickvals = -6:2,
                          gridcolor = toRGB("gray95")),
             shapes = c(lines1, lines2, lines)) %>%
      add_annotations(x = xcenters,
                      y = ymin-0.05*abs(ymin),
                      text = countries$Country,
                      bgcolor = 'white',
                      xref = "x",
                      yref = "y",
                      showarrow = FALSE) %>%
      add_annotations(x = xcenters2,
                      y = 0.5*ymin,
                      font = list(size = 8),
                      text = regionsfor$F5,
                      textangle = -90,
                      xref = "x",
                      yref = "y",
                      showarrow = FALSE) %>%
      add_annotations(x = xcenters1,
                      y = 0.7*ymax,
                      font = list(size = 8),
                      text = regionsrf$F2,
                      textangle = -90,
                      xref = "x",
                      yref = "y",
                      showarrow = FALSE)

pl
# export(pl, file = "plotp_twin.pdf")
