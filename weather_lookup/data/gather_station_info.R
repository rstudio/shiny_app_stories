# Helper functions
library(tidyverse)
library(here)



# Downloaded from http://www.uszipcodelist.com/download.html on December 2, 2020
zip_db_cols <- c(
"zip" =   "i",
"type" =   "c",
"primary_city" =   "c",
"acceptable_cities" =   "c",
"unacceptable_cities" =   "-",
"state" =   "c",
"county" =   "c",
"timezone" =   "-",
"area_codes" =   "-",
"latitude" =   "n",
"longitude" =   "n",
"world_region" =   "-",
"country" =   "c",
"decommissioned" =   "-",
"estimated_population" =   "i",
"notes" =   "-"
)


zip_to_city <- read_csv(
  here('data/zip_code_database.csv'),
  col_types = zip_db_cols
) %>% 
  select(zip, primary_city, acceptable_cities, state, latitude, longitude) %>% 
  pivot_longer(
    cols = c(primary_city, acceptable_cities),
    values_to = "city"
  ) %>% 
  filter(!is.na(city)) %>% 
  distinct(zip, city, state, .keep_all = TRUE) %>% 
  transmute(
    zip = str_pad(zip, 5, pad = "0"),
    city = paste0(city, ", ", state)
  )

station_zip_codes <- read_table(
  "https://www1.ncdc.noaa.gov/pub/data/normals/1981-2010/station-inventories/zipcodes-normals-stations.txt",
  col_names = c("station", 'zip', 'name')
)

# Merge two datasets together and keep only a single station per city. 
# It may be better to check the data to choose the "best" station for 
# a given city, but that seems like a lot of work
station_to_city <- station_zip_codes %>% 
  left_join(zip_to_city, by = "zip") %>% 
  filter(!is.na(city)) %>% 
  select(-name)

write_rds(station_to_city, here("data/station_to_city.rds"))


