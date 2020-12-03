# This is the code I ran before being able to run examples

# # This installs the newest version of shiny (I had old one)
# remotes::install_github("rstudio/shiny")
# 
# # This was required to run the demo app
# remotes::install_github("rstudio/thematic")
# 
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(readr)
library(here)


station_to_city <- read_rds(here("data/station_to_city.rds"))
unique_cities <- unique(station_to_city$city)

# Look through raw text downloaded from NOAA servers for a given station and
# extract the table for the desired data_id text as a dataframe
extract_month_level_data <- function(data_id, file_lines){
  stringr::str_extract(
    file_lines,
    paste0("(?<=", data_id, ")((.|\\\n)*?)(?=\n \n)")
  ) %>% 
    str_remove_all("\\s+(?=\n)") %>% 
    str_remove_all("(?<=\n)\\s+") %>% 
    paste0("\n") %>% 
    readr::read_table2(col_names = c("month", 1:31)) %>% 
    mutate(type = data_id)
}

# Retreive desired data for a given station. For info on what ids are available
# see
# https://www1.ncdc.noaa.gov/pub/data/normals/1981-2010/documentation/1981-2010-normals-overview.pdf
get_data_for_station <- function(station_id, 
                                 data_ids = c("dly-tmax-normal",
                                              "dly-tavg-normal",
                                              "dly-tmin-normal"
                                              # , "ytd-prcp-normal",
                                              # "ytd-snow-normal"
                                 )){
  station_url <- paste0(
    "https://www1.ncdc.noaa.gov/pub/data/normals/1981-2010/products/auxiliary/station/",
    station_id,
    ".normals.txt"
  )
  station_text <- readr::read_file(url(station_url))
  
  purrr::map_dfr(
    data_ids, 
    extract_month_level_data, 
    file_lines = station_text
  ) %>% 
    pivot_longer(
      c(-month, -type),
      names_to = "day"
    ) %>% 
    mutate(
      value = as.numeric(str_remove(value, "[A-Z]"))
    ) %>% 
    filter(!(value %in% c(-8888, -9999))) %>% 
    mutate(
      # This mutate is after filter so we dont try and parse nonsense dates like
      # feb 31st
      date = lubridate::mdy(paste0(month, "-", day, "-2000"))
    )
}

# Extract temperature info from the station dataframe
get_temp_data <- function(station_data){
  station_data %>% 
    filter(str_detect(type, "dly-t")) %>% 
    mutate(
      type = str_remove_all(type, "dly-t|-normal"),
      # Results are given in tenths of a farenheit degree so we need to divide by 10 to get actual degree
      # See here https://www1.ncdc.noaa.gov/pub/data/normals/1981-2010/supplemental/readme-supp.txt
      value = value/10
    ) %>% 
    pivot_wider(
      names_from = type,
      values_from = value
    )
}
