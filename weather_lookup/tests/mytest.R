# remotes::install_github("rstudio/shinytest")
library(shinytest)
library(bench)

run_use_test <- function(use_caching){
  # Make sure you have the latest version of shinytest from github so option passing
  # is available
  app <- ShinyDriver$new("./", phantomTimeout = 10000, loadTimeout = 10000,
                         options = list(cache = use_caching),
                         seed = 42)
  # Using a seed because the app randomly chooses city in rnd_city button and
  # initial back button

  app$snapshotInit("mytest")
  app$setInputs(city = "", timeout_ = 10000)
  app$setInputs(city = "Minneapolis, MN", timeout_ = 10000)
  app$setInputs(city = "", timeout_ = 10000)
  app$setInputs(city = "Houston, TX", timeout_ = 10000)
  app$setInputs(prev_city = "click", timeout_ = 10000)
  app$setInputs(prev_city = "click", timeout_ = 10000)
  app$setInputs(prev_city = "click", timeout_ = 10000)
  app$setInputs(city = "New York, NY", timeout_ = 10000)
  app$setInputs(prev_city = "click", timeout_ = 10000)
  app$setInputs(prev_city = "click", timeout_ = 10000)
}

speed_comparison <- bench::mark(
  run_use_test(use_caching = TRUE),
  run_use_test(use_caching = FALSE),
  iterations = 5,
  check = FALSE
)

speed_comparison
