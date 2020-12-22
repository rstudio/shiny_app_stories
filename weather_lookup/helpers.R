# Helper functions for weather gathering app
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(stringr)
library(readr)
library(here)


check_for_data <- function(station_text, data_id){
  # If we're missing the data field dont bother trying to find it
  id_missing <- !str_detect(string = station_text, pattern = data_id)
  if(id_missing){
    stop("Data not in provided station text")
  }
}


# Look through raw text downloaded from NOAA servers for a given station and
# extract the table for the desired data_id text as a dataframe
extract_month_level_data <- function(data_id, file_lines){
  check_for_data(file_lines, data_id)

  file_lines %>%
    # A semi hairy regex to extract block of relevant data
    stringr::str_extract(paste0("(?<=", data_id, ")((.|\\\n)*?)(?=\n \n)")) %>%
    str_remove_all("\\s+(?=\n)") %>%
    str_remove_all("(?<=\n)\\s+") %>%
    paste0("\n") %>%
    readr::read_table2(col_names = c("month", 1:31)) %>%
    pivot_longer(-month, names_to = "day") %>%
    filter(!(value %in% c(-8888, -9999))) %>%
    transmute(type = data_id,
              value = as.numeric(str_remove(value, "[A-Z]")),
              # After filter so we dont try and parse dates like feb 31st
              date = lubridate::mdy(paste0(month, "-", day, "-2000")))
}


# We need this sequence a good bit and it's a bit of a mouthful so let's store
# it here
twelve_month_seq <- seq(ymd("2000-1-1"), ymd("2000-12-1"), by = "months")


# Extract temperature info from the station text
get_temp_data <- function(station_text){
  c("dly-tmax-normal", "dly-tavg-normal", "dly-tmin-normal") %>%
    purrr::map_dfr(extract_month_level_data, file_lines = station_text) %>%
    mutate(type = str_remove_all(type, "dly-t|-normal"),
           # Results are given in tenths of a degree so we need to divide by 10
           # See https://www1.ncdc.noaa.gov/pub/data/normals/1981-2010/readme.txt
           value = value/10) %>%
    pivot_wider(names_from = c(type), values_from = c(value))
}


# Extract temperature info from the station text
get_prcp_data <- function(station_text){

  check_for_data(station_text, "mly-prcp-normal")

  months_prcp_avgs <- station_text %>%
    stringr::str_extract("(?<=mly-prcp-normal)((.|\\\n)*?)(?=\n)") %>%
    str_trim() %>%
    str_remove_all("[A-Z]") %>%
    str_split("\\s+") %>%
    pluck(1) %>%
    as.numeric()

  tibble(date = twelve_month_seq,
         # Data reported as 100ths of an inch
         # -7777 is used to denote a value that rounds to 0 but is not technically zero
         avg_precipitation = ifelse(months_prcp_avgs == -7777, 0, months_prcp_avgs/100))
}


# Offload ggplot2 logic for cleaner reactive logic in app
build_prcp_plot <- function(prcp_data){
  context_point <- tibble(
    label = c("9.47\": wettest month in Miami, FL"),
    avg_precipitation = c(9.47)
  )

  axis_breaks <- seq(8, 0, by = -2)
  axis_labels <- as.character(axis_breaks)
  axis_labels[1] <- paste(axis_labels[1], "inches")

  ggplot(prcp_data, aes(x = date, y = avg_precipitation)) +
    geom_richtext(data = context_point,
                  aes(x = mdy("12-25-2000"), label = label, y = avg_precipitation),
                  hjust = 1, vjust = 0, nudge_y = 0.05, nudge_x = 2,
                  label.color = NA, fill = NA,
                  label.padding = grid::unit(rep(0, 4), "pt")) +
    geom_hline(data = context_point, aes(yintercept = avg_precipitation)) +
    geom_rect(aes(xmin = date, xmax = date + months(1), ymin = 0, ymax = avg_precipitation),
              fill = "steelblue",
              color = "white") +
    geom_text(aes(x = date + days(15),
                  # Make sure people know zero isn't exactly zero
                  label = ifelse(avg_precipitation == 0, "> 0", format(avg_precipitation, digits = 3))),
              nudge_y = 0.05,
              hjust = 0.5,
              color = "black",
              size = 5,
              vjust = 0) +
    labs(title = "Monthly precipitation", y = "") +
    scale_y_continuous(breaks = axis_breaks, labels = axis_labels,
                       expand = expansion(mult = c(0, 0.075)))
}

build_temp_plot <- function(temp_data){

    extremes <- bind_rows(
      arrange(temp_data, -max, -avg, -min)[1,] %>%
        mutate(label = glue("Hottest day: {format(date, '%B %d')}<br>",
                            "Avg max temp = {format(max, digits = 3)}&#176;")),
      arrange(temp_data, min, avg, max)[1,] %>%
        mutate(label = glue("Coldest day: {format(date, '%B %d')}<br>",
                            "Avg min temp = {format(min, digits = 3)}&#176;"))
    )

    context_points <- tibble(
      label = c("107&#176;: hottest day in Pheonix, AZ", "-14.9&#176;: coldest day in Fairbanks, AK"),
      temp = c(107, -14.9)
    )

    axis_breaks <- seq(100, -10, by = -10)
    axis_labels <- as.character(axis_breaks)
    axis_labels[1] <- paste0(axis_labels[1], "&#176; F")
    ggplot(temp_data, aes(x = date, y = avg)) +
      geom_richtext(data = context_points,
                    aes(x = mdy("12-25-2000"), label = label, y = temp),
                    hjust = 1, vjust = c(0,1), nudge_y = c(1,-1), nudge_x = 2,
                    label.color = NA, fill = NA,
                    label.padding = grid::unit(rep(0, 4), "pt")) +
      geom_hline(data = context_points, aes(yintercept = temp)) +
      geom_ribbon(aes(ymin = min, ymax = max),
                  fill = "steelblue",
                  alpha = 0.25) +
      geom_line(color = "white") +
      geom_point(data = extremes) +
      geom_richtext(data = extremes,
                    aes(label = label, hjust = ifelse(month(date) < 6, 0, 1)),
                    nudge_y = -1,
                    label.color = NA,
                    # Gives us a transparent background so text pops better
                    fill = after_scale(alpha("white", .5)),
                    vjust = 1 ) +
      labs(title = "Daily temperature", y = "") +
      scale_y_continuous(breaks = axis_breaks, labels = axis_labels)
}

# Add a nicely styled and centered label above a given input
labeled_input <- function(id, label, input){
  div(id = id,
      style = "display: grid; justify-items: center;",
      span(label, style = "font-size: small;"),
      input)
}


# Map a function that may fail and simply remove the failures before returning
# This helps because we have lots of things that may fail such as http requests
# and extracting data from the text of a station. This is easier to just deal
# with errors rather than finding every edge case and building explicit logic
# branches for them
safe_map <- function(x, fn){
  map(x, possibly(fn, otherwise = NULL))
}


build_station_url <- function(station_id){
  paste0(
    "https://www1.ncdc.noaa.gov/pub/data/normals/1981-2010/products/auxiliary/station/",
    station_id,
    ".normals.txt"
  )
}


# Setup end-of-url hash to allow for bookmarking of city's based on url
make_url_hash <- function(city_name) paste0("#",str_replace_all(city_name, "\\s", "-"))
parse_url_hash <- function(hash_text) str_replace_all(hash_text, "-", " ") %>% str_remove("#")
