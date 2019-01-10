--- 
title: "Атлас Российского пограничья"
author: "Лаборатория геополитических исследований ИГ РАН"
# date: "2019-01-10"
site: bookdown::bookdown_site
output: bookdown::gitbook
documentclass: book
bibliography: [articles.bib, packages.bib]
biblio-style: apalike
link-citations: yes
github-repo: tsamsonov/borderlands
description: "Атлас охватывает широкий круг проблем в сфере внешних связей, экономики, социальной и этнокультурной ситуации, охраны окружающей среды в приграничных регионах России и сопредельных стран"
header-includes:
   - \usepackage[T2A]{fontenc}
   - \usepackage[utf8]{inputenc}
   - \usepackage[russian]{babel}
mainfont: Open Sans
---


```
## Loading required package: ggplot2
```

```
## 
## Attaching package: 'plotly'
```

```
## The following object is masked from 'package:ggplot2':
## 
##     last_plot
```

```
## The following object is masked from 'package:stats':
## 
##     filter
```

```
## The following object is masked from 'package:graphics':
## 
##     layout
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```
## Linking to GEOS 3.6.1, GDAL 2.1.3, PROJ 4.9.3
```

```
## Loading required package: htmlwidgets
```

```
## Reading layer `regions' from data source `/Users/tsamsonov/GitHub/borderlands/data/regions.gpkg' using driver `GPKG'
## Simple feature collection with 101 features and 64 fields
## geometry type:  MULTIPOLYGON
## dimension:      XY
## bbox:           xmin: 16.78521 ymin: 34.33996 xmax: 147.1588 ymax: 80.17487
## epsg (SRID):    4326
## proj4string:    +proj=longlat +datum=WGS84 +no_defs
```

```
## Warning in st_centroid.sf(regions): st_centroid assumes attributes are
## constant over geometries of x
```

```
## Warning in st_centroid.sfc(st_geometry(x), of_largest_polygon =
## of_largest_polygon): st_centroid does not give correct centroids for
## longitude/latitude data
```

```
## Reading layer `cities' from data source `/Users/tsamsonov/GitHub/borderlands/data/regions.gpkg' using driver `GPKG'
## Simple feature collection with 7343 features and 105 fields
## geometry type:  POINT
## dimension:      XY
## bbox:           xmin: -179.59 ymin: -90 xmax: 179.3833 ymax: 82.48332
## epsg (SRID):    4326
## proj4string:    +proj=longlat +datum=WGS84 +no_defs
```

```
## Reading layer `cities' from data source `/Users/tsamsonov/GitHub/borderlands/data/regions.gpkg' using driver `GPKG'
## Simple feature collection with 7343 features and 105 fields
## geometry type:  POINT
## dimension:      XY
## bbox:           xmin: -179.59 ymin: -90 xmax: 179.3833 ymax: 82.48332
## epsg (SRID):    4326
## proj4string:    +proj=longlat +datum=WGS84 +no_defs
```

```
## Reading layer `back' from data source `/Users/tsamsonov/GitHub/borderlands/data/regions.gpkg' using driver `GPKG'
## Simple feature collection with 1 feature and 3 fields
## geometry type:  MULTIPOLYGON
## dimension:      XY
## bbox:           xmin: 16.78521 ymin: 34.33996 xmax: 147.1588 ymax: 80.17487
## epsg (SRID):    4326
## proj4string:    +proj=longlat +datum=WGS84 +no_defs
```

```
## Reading layer `boundaries' from data source `/Users/tsamsonov/GitHub/borderlands/data/boundaries.gpkg' using driver `GPKG'
## Simple feature collection with 464 features and 20 fields
## geometry type:  MULTILINESTRING
## dimension:      XY
## bbox:           xmin: -141.0055 ymin: -55.12092 xmax: 140.9776 ymax: 70.07531
## epsg (SRID):    4326
## proj4string:    +proj=longlat +datum=WGS84 +no_defs
```

```
## Reading layer `grad_country' from data source `/Users/tsamsonov/GitHub/borderlands/data/boundaries.gpkg' using driver `GPKG'
## Simple feature collection with 78 features and 17 fields
## geometry type:  MULTILINESTRING
## dimension:      XY
## bbox:           xmin: 19.60955 ymin: 41.18886 xmax: 134.7755 ymax: 69.78523
## epsg (SRID):    4326
## proj4string:    +proj=longlat +datum=WGS84 +no_defs
```

# Введение {-}

Атлас подготовлен в результате реализации исследовательского проекта Российского научного фонда «Российское пограничье: вызовы соседства» (грант №14-18-03621) и представляет собой комплексное научно-справочное картографическое произведение, призванное дать целостное представление о развитии приграничных регионов России и сопредельных стран. Атлас носит междисциплинарный и проспективный характер и допускает постоянное обновление информации. В его основу положен принцип полимаштабности: проблемы пограничья показаны на четырех уровнях – макрорегиональном, общегосударственном, региональном и локальном. Он объединяет карты, отражающие ситуацию вдоль всей линии российской границы с обеих ее сторон в европейской и азиатской частях российского пограничья и на их детально изученных на муниципальном уровне ключевых участках пограничья. Использованы передовые способы картографирования границ – от нанесения точечной экспертной информации до автоматизированной обработки данных дистанционного зондирования, пространственного анализа и интерполяции средствами ГИС.

Атлас охватывает широкий круг проблем в сфере внешних связей, экономики, социальной и этнокультурной ситуации, охраны окружающей среды в приграничных регионах России и сопредельных стран. Значительное внимание уделено влиянию на развитие приграничного сотрудничества режима и функционирования границы в целом, исторической памяти и массовых стереотипов, «фантомным границам», утратившим свои главные функции, но продолжающим влиять на отношения людей и принимаемые решения. Интегрируя разнообразную информацию в разрезе всего пограничья и его отдельных частей, атлас представляет ее в систематизированной, сопоставимой и хорошо обозримой форме.

Атлас содержит, во-первых, карты на традиционные сюжеты (дифференциации пограничья по демографическим и социально-экономическим показателям, морфологии российских границ по «возрасту» и происхождению, легитимности, этнической контрастности, экологических взаимосвязей и трансграничных угроз); во-вторых, карты на новые, неизвестные ранее темы (потенциал трансграничного сотрудничества, объекты нового строительства, роль границ в этнических и иных конфликтах, барьерность границ, повседневная жизнь и мотивы приграничной мобильности населения, историко-символический ландшафт, фантомные границы); в-третьих, карты на основе инновационных методов картографирования (плотности автодорожной сети, населенных пунктов, динамики землепользования и лесопользования).

В работе восемь разделов: «Геополитическое положение», «Население», «Уровень жизни населения», «Экономика», «Динамика трансграничных взаимодействий», «Развитие трансграничных систем и экологические угрозы», «Конфликты в приграничье», «Соотношение культурных, ментальных и институциональных границ». Атлас содержит 85 карт в разном масштабе, их аналитические описания, табличный, графический и другой дополнительный материал и ориентирован на специалистов в области международных отношений и приграничного сотрудничества, а также на широкий круг читателей, интересующихся изучением российского пограничья (пограничными исследованиями).
