# This file takes data downloaded by download_from_NOAA.R script and combines it
# into two separate feather files

library(here)
library(arrow)
library(progressr)
library(lubridate)
options(progressr.enable = TRUE)

# We have an already build station to city df we are using for lookups
stations_to_download <- readr::read_rds(here("data/station_to_city.rds"))

print("Extracting temperature data")
with_progress({
  p <- progressor(steps = nrow(stations_to_download))

  temperature_data <- purrr::map2_dfr(
    stations_to_download$station,
    as.integer(seq_along(stations_to_download$station)),
    ~ {
      p()
      station_data <- readRDS(here(paste0("data/scrape_data/scraped_data/", .x, ".rds")))
      if (!is.null(station_data$temp)) {
        tibble(
          avg = station_data$temp$avg,
          max = station_data$temp$max,
          min = station_data$temp$min,
          month = as.integer(month(station_data$temp$date)),
          day = as.integer(day(station_data$temp$date)),
          station_index = as.integer(.y)
        )
      }
    }
  )
})

write_feather(temperature_data, here("data/temperature_data.feather"), compression = "zstd")


print("Extracting precipitation data")
with_progress({
  p <- progressor(steps = nrow(stations_to_download))

  precipitation_data <- purrr::map2_dfr(
    stations_to_download$station,
    as.integer(seq_along(stations_to_download$station)),
    ~ {
      p()
      station_data <- readRDS(paste0("scraped_data/", .x, ".rds"))
      if (!is.null(station_data$prcp_data)) {
        tibble(
          avg = station_data$prcp_data$avg_precipitation,
          month = as.integer(lubridate::month(station_data$prcp_data$date)),
          station_index = as.integer(.y)
        )
      }
    }
  )
})

write_feather(precipitation_data, here("data/precipitation_data.feather"), compression = "zstd")
