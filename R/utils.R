library(sf)
library(tidyverse)

grads = st_read('data/boundaries.gpkg', 'gradients', stringsAsFactors = FALSE) %>% 
  select(SEGMENT = Segment, PART = Part, COUNTRY = Country, ENTER = Enter, BREAK = Break, ORD, 
         NAME1 = F2, ISOID1, ENTER1 = Enter1, BREAK1 = Break1, 
         NAME2 = F5, ISOID2, ISOID20, ENTER2 = Enter2, BREAK2 = Break2) %>% 
  mutate(LENGTH = units::set_units(st_length(.), km)) %>% 
  group_by(PART) %>% 
  mutate(LENGTH_PART = sum(LENGTH)) %>% 
  ungroup() %>% 
  arrange(ORD)

st_write(grads, 'data/boundaries.gpkg', 'grad_region', delete_layer = TRUE)

grads2 = grads %>%
  group_by(SEGMENT, PART, ISOID1, ISOID20) %>% 
  summarize_all(first) %>% 
  ungroup() %>% 
  mutate(LENGTH = units::set_units(st_length(.), km)) %>% 
  group_by(PART) %>%
  arrange(ORD) %>% 
  mutate(LENGTH_PART = sum(LENGTH),
         ENTER = as.integer(row_number() == 1),
         BREAK = as.integer(row_number() == n()),
         NAME2 = if_else(ISOID20 == ISOID2, NAME2, COUNTRY),
         ISOID2 = ISOID20) %>% 
  ungroup() %>% 
  group_by(ISOID1) %>%
  arrange(ORD) %>% 
  mutate(ENTER1 = as.integer(row_number() == 1),
         BREAK1 = as.integer(row_number() == n())) %>%
  ungroup() %>% 
  group_by(ISOID2, PART) %>%
  arrange(ORD) %>% 
  mutate(ENTER2 = as.integer(row_number() == 1),
         BREAK2 = as.integer(row_number() == n())) %>%
  ungroup() %>% 
  mutate(ISOID = str_sub(ISOID2, 1, 2)) %>% 
  select(SEGMENT, PART, COUNTRY, ISOID, ENTER:NAME1, ISOID1, ENTER1:LENGTH_PART)

st_write(grads2, 'data/boundaries.gpkg', 'grad_country', delete_layer = TRUE)
