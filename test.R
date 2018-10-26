library(tidyr)
library(units)

regions2 = regions %>% 
  group_by(type) %>% 
  summarise(count = n(),
            zoom = sum(min_zoom))

cities2 = cities %>% select(NAME)

d = st_distance(cities2) %>% as.data.frame()

d2 = d %>% 
  tibble::rownames_to_column(var = 'city1') %>% 
  gather(city2, distance, -city1) %>% 
  dplyr::filter(distance < set_units(200000, m))

