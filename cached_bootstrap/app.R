# install.packages('simpleboot')
# devtools::install_github("ropensci/skimr")
library(shiny)
library(tidyverse)
library(simpleboot)
library(rlang)
library(glue)

data("midwest")

# Log all the population variables
midwest <- midwest %>%
  select(-PID) %>%
  mutate(across(contains("pop"), log10))

numeric_vars <- midwest %>% select_if(is.numeric) %>% colnames()


# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Bootstrapped Regressions"),
    sidebarLayout(
        sidebarPanel(
          selectInput('x_var', "X variable", choices = numeric_vars, selected = "popdensity"),
          selectInput('y_var', "Y variable", choices = numeric_vars, selected = "percollege")
        ),
        mainPanel(
           plotOutput("scatter"),
           textOutput('boot_results')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$scatter <- renderPlot({
      midwest %>%
        ggplot(aes(x = .data[[input$x_var]], y = .data[[input$y_var]])) +
        geom_point()
    })

    bootstrap_results <- reactive({
      validate(
        need(input$y_var != input$x_var,
             "Need x and y variables to be different to perform regression.")
      )
      dependent <- midwest[[input$y_var]]
      independent <- midwest[[input$x_var]]
      lm.boot(lm(dependent ~ independent), R = 500)
    }) %>%
      bindCache(input$y_var, input$x_var)

    output$boot_results <- renderText({
      effect_size <- summary(bootstrap_results()$orig.lm)$coefficients[2,1]
      bs_se_w <- 1.96 * summary(bootstrap_results())$stdev.params["independent"]
      glue("Impact of increasing {input$x_var} by one unit is a change of {input$y_var} by ",
           "{round(effect_size - bs_se_w,3)} to {round(effect_size + bs_se_w, 3)}")
    })

}

# Run the application
shinyApp(ui = ui, server = server)
