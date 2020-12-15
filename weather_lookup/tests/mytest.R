# remotes::install_github("rstudio/shinytest")
library(shinytest)
library(bench)

run_use_test <- function(use_caching){
  # Make sure you have the latest version of shinytest from github so option passing
  # is available
  app <- ShinyDriver$new("./", phantomTimeout = 10000, loadTimeout = 10000,
                         options = list(cache = use_caching))
  app$snapshotInit("mytest")

  app$snapshot()
  app$setInputs(city = "Redwood City, CA")
  app$setInputs(prev_city = "click")
  app$setInputs(rnd_city = "click")
  app$setInputs(prev_city = "click")
  app$setInputs(rnd_city = "click")
  app$setInputs(prev_city = "click")
  app$setInputs(prev_city = "click")
  app$setInputs(prev_city = "click")
  app$snapshot()
}


bench::mark(
  run_use_test(use_caching = TRUE),
  run_use_test(use_caching = FALSE),
  iterations = 5,
  check = FALSE
)
