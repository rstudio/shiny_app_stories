# install.packages('simpleboot')
library(shiny)
library(dplyr)
library(ggplot2)
library(bslib)

# Log all the population variables
midwest <- ggplot2::midwest %>%
  select(-PID) %>%
  mutate(across(contains("pop"), log10))

numeric_vars <- midwest %>% select_if(is.numeric) %>% colnames()

B <- 500 # Number of bootstrap resamples to do

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  titlePanel("Bootstrapped Regressions"),
  sidebarLayout(
    sidebarPanel(
      selectInput('x_var', "X variable", choices = numeric_vars, selected = "popdensity"),
      selectInput('y_var', "Y variable", choices = numeric_vars, selected = "percollege")
    ),
    mainPanel(
      uiOutput('boot_results'),
      plotOutput("scatter")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$scatter <- renderPlot({
    ggplot(midwest, aes(x = .data[[input$x_var]], y = .data[[input$y_var]])) +
      geom_abline(data = bootstrap_results(), aes(intercept = intercept, slope = effect_size), alpha = 0.1, color = "steelblue") +
      geom_point() +
      labs(subtitle = paste("Each blue line represents one of the", B, "bootstrap regression results"))
  }) %>%
    bindCache(input$y_var, input$x_var)

  bootstrap_results <- reactive({
    validate(need(input$y_var != input$x_var, "X and Y variables need to be different."))
    effect_sizes <- tibble(intercept = numeric(), effect_size = numeric())

    withProgress(message = 'Bootstrap resampling', max = B, {
      for(i in 1:B){
        if((i %% 50) == 0) incProgress(i, paste("Itteration", i))
        boot_data <- sample_frac(midwest, replace = TRUE)
        effect_sizes[i,] <- as.list(lm(boot_data[[input$y_var]] ~ boot_data[[input$x_var]])$coefficients)
      }
    })
    effect_sizes
  }) %>%
    bindCache(input$y_var, input$x_var)

  output$boot_results <- renderUI({
    effect_size <- round(quantile(bootstrap_results()$effect_size, c(0.025, 0.975)), 3)
    markdown(paste0("Impact of increasing `", input$x_var, "` by one unit is a change of `",
                    input$y_var, "` is __", effect_size[1], "__ to __", effect_size[2], "__"))
  })
}

shinyApp(ui = ui, server = server)
