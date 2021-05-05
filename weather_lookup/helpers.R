# Helper functions for weather gathering app
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(stringr)
library(readr)
library(here)


# We need this sequence a good bit and it's a bit of a mouthful so let's store
# it here
twelve_month_seq <- seq(ymd("2000-1-1"), ymd("2000-12-1"), by = "months")

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
                            "Avg max temp = {format(max, digits = 3)}&#176;"),
               pos = max),
      arrange(temp_data, min, avg, max)[1,] %>%
        mutate(label = glue("Coldest day: {format(date, '%B %d')}<br>",
                            "Avg min temp = {format(min, digits = 3)}&#176;"),
               pos = min)
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
      geom_point(data = extremes, aes(y = pos)) +
      geom_richtext(data = extremes,
                    aes(y = pos, label = label, hjust = ifelse(month(date) < 6, 0, 1)),
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
      span(label, style = "font-size: small;"),
      input)
}

# Text grob with color matching current theme
themed_text <- function(text){
  grid::textGrob(
    text,
    gp = grid::gpar(col = thematic::thematic_get_option("fg"))
  )
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
