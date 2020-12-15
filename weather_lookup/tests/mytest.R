# remotes::install_github("rstudio/shinytest")
library(shinytest)
library(bench)

run_use_test <- function(use_caching){
  # Make sure you have the latest version of shinytest from github so option passing
  # is available
  app <- ShinyDriver$new("./", phantomTimeout = 10000, loadTimeout = 10000,
                         options = list(cache = use_caching),
                         seed = 42)
  # Using a seed because the app randomly chooses city with rnd_city button
  app$snapshotInit("mytest")

  app$snapshot()
  app$setInputs(city = "Redwood City, CA", timeout_ = 10000)
  app$setInputs(prev_city = "click"      , timeout_ = 10000)
  app$setInputs(rnd_city = "click"       , timeout_ = 10000)
  app$setInputs(prev_city = "click"      , timeout_ = 10000)
  app$setInputs(rnd_city = "click"       , timeout_ = 10000)
  app$setInputs(prev_city = "click"      , timeout_ = 10000)
  app$setInputs(prev_city = "click"      , timeout_ = 10000)
  app$setInputs(prev_city = "click"      , timeout_ = 10000)
  app$snapshot()
}


speed_comparison <- bench::mark(
  run_use_test(use_caching = TRUE),
  run_use_test(use_caching = FALSE),
  iterations = 5,
  check = FALSE
)

speed_comparison
