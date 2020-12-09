# This is the code I ran before being able to run examples

# # This installs the newest version of shiny (I had old one)
# remotes::install_github("rstudio/shiny")
# 
# # This was required to run the demo app
# remotes::install_github("rstudio/thematic")
# 
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(stringr)
library(readr)
library(here)

# These are the temp related fields we care about
temperature_fields <- c("dly-tmax-normal", "dly-tavg-normal", "dly-tmin-normal")
# These are the temp related fields we care about
prcp_fields <- c("ytd-prcp-normal")

station_to_city <- read_rds(here("data/station_to_city.rds"))
unique_cities <- unique(station_to_city$city)

get_random_city <- function(){
  sample(unique_cities, 1)
}

# Takes an id for a station, grabs that stations text file from NOAA and returns it as a big string
get_station_text <- function(station_id){
  station_url <- paste0(
    "https://www1.ncdc.noaa.gov/pub/data/normals/1981-2010/products/auxiliary/station/",
    station_id,
    ".normals.txt"
  )
  
  readr::read_file(url(station_url))
}



# Look through raw text downloaded from NOAA servers for a given station and
# extract the table for the desired data_id text as a dataframe
extract_month_level_data <- function(data_id, file_lines){
  
  # If we're missing the data field dont bother trying to find it
  id_missing <- !str_detect(string = file_lines, pattern = data_id)
  if(id_missing){
    return(tibble(
      month = character(), 
      day = character(), 
      value = numeric(), 
      type = character()
      ))
  }
  stringr::str_extract(
    file_lines,
    paste0("(?<=", data_id, ")((.|\\\n)*?)(?=\n \n)")
  ) %>% 
    str_remove_all("\\s+(?=\n)") %>% 
    str_remove_all("(?<=\n)\\s+") %>% 
    paste0("\n") %>% 
    readr::read_table2(col_names = c("month", 1:31)) %>% 
    pivot_longer(
      c(-month),
      names_to = "day"
    ) %>% 
    filter(!(value %in% c(-8888, -9999))) %>% 
    transmute(
      type = data_id,
      value = as.numeric(str_remove(value, "[A-Z]")),
      # After filter so we dont try and parse nonsense dates like feb 31st
      date = lubridate::mdy(paste0(month, "-", day, "-2000"))
    )
}

# We use results of "NULL" as the indicator no data was found. This turns an
# empty df into that for us
nullify_empty_results <- function(extracted_data){
  station_has_data <- nrow(extracted_data) != 0
  if(station_has_data){
    return(extracted_data)
  } else {
    return(NULL)
  }
}

# Extract temperature info from the station text
get_temp_data <- function(station_text){
  extracted <- purrr::map_dfr(
     temperature_fields,
     extract_month_level_data, 
     file_lines = station_text
  ) %>% 
    mutate(
      type = str_remove_all(type, "dly-t|-normal"),
      # Results are given in tenths of a farenheit degree so we need to divide by 10 to get actual degree
      # See here https://www1.ncdc.noaa.gov/pub/data/normals/1981-2010/supplemental/readme-supp.txt
      value = value/10
    ) %>% 
    pivot_wider(names_from = c(type), values_from = c(value)) %>% 
    nullify_empty_results()
}

# Gotta make our own rolling mean function
rolling_mean <- function(values, window_width){
  i <- seq_along(values)
  map2(i, pmax(i-window_width, 1), seq) %>% 
    map_dbl(~{mean(values[.x])})
} 

# Extract temperature info from the station text
get_prcp_data <- function(station_text){
  extract_month_level_data("ytd-prcp-normal", station_text) %>% 
    mutate(
      type = str_remove_all(type, "ytd-|-normal"),
      # provided values are in "hundredths of inches": https://www1.ncdc.noaa.gov/pub/data/normals/1981-2010/readme.txt
      value = value/100,
      day_amnt = value - lag(value, default = 0),
      week_avg = rolling_mean(day_amnt, window_width = 7)
    ) %>% 
    nullify_empty_results()
}

# Add a nicely styled and centered label above a given input
labeled_input <- function(id, label, input){
  div(id = id,
      style = "display: grid; justify-items: center;",
      span(label, style = "font-size: small;"),
      input)
}
