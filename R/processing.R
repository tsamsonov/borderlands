library(sf)
library(dplyr)

con = DBI::dbConnect(RSQLite::SQLite(),
                     dbname = "data/borderlands.db"
)

regions = st_read('data/regions.gpkg',
                  'regions',
                  stringsAsFactors = FALSE)

regions = regions %>% mutate(area = as.integer(0.000001 * st_area(.)))

countries = st_read('data/regions.gpkg',
                  'countries',
                  stringsAsFactors = FALSE)

countries = countries %>% mutate(area = as.integer(0.000001 * st_area(.)))

REGIONS = DBI::dbReadTable(con, 'REGIONS') %>% 
  left_join(regions, by = "ISOID") %>% 
  mutate(AREA = area) %>% 
  select(ISOID, LEVEL, NAME, NAME_EN, ISOID_COUNTRY, AREA) %>% 
  left_join(countries, by = "ISOID") %>% 
  mutate(AREA = ifelse(is.na(area), AREA, area)) %>% 
  select(ISOID, LEVEL = LEVEL.x, NAME = NAME.x, NAME_EN, ISOID_COUNTRY, AREA)
  

dbWriteTable(con, c("REGIONS"), value=REGIONS, overwrite=TRUE, row.names=FALSE,
             field.types = c(ISOID = "TEXT",
                             LEVEL = "INTEGER",
                             NAME = "TEXT",
                             NAME_EN = "TEXT",
                             ISOID_COUNTRY = "TEXT",
                             AREA = "INTEGER"))
