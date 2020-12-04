library(shiny)
library(bslib)
library(ggplot2)
library(thematic)
library(ggtext)

source('helpers.R')

thematic_on(font = "auto")

my_theme <- bs_theme(
  bootswatch = "cerulean",
  base_font = font_google("Righteous"),
  "font-size-base" = "1.1rem"
)

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = my_theme,
    tags$head(
      tags$style(HTML("
     .shiny-input-container{
       margin-left: auto; 
       margin-right: auto;
     }
    "))),
    # Application title
    titlePanel("Explore your weather"),
    selectizeInput('city', 
                   "Find your city", 
                   choices = unique_cities, 
                   selected = "Ann Arbor, MI", 
                   multiple = FALSE),
    plotOutput("tempPlot")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  city_data <- reactive({
    validate(
      need(input$city != '', 'Search for your city')
    )
    
    # Find correct station id
    station_info <- filter(station_to_city, city == input$city)
    query_result <- NULL
    
    # Not every station has temperature data. This loops through all stations in
    # a city and tries to find one with temperature data. If there are a lot of
    # stations, this can take a while
    for (station_id in station_info$station){
      query_result <- purrr::quietly(get_data_for_station)(station_id)
      has_temp_data <- length(query_result$warnings) == 0 
      if(has_temp_data) break
    }
    if(is.null(query_result)){
      stop("Sorry, no temperature data is available for your city, try a nearby one.")
    }
    
    query_result$result
  })

  output$tempPlot <- renderPlot({
    city_data() %>%
      get_temp_data() %>% 
      ggplot(aes(x = date, y = avg)) +
      geom_ribbon(aes(ymin = min, ymax = max), 
                  fill = "steelblue", 
                  alpha = 0.5) +
      geom_line(color = "white") +
      labs(y = "temperature (&#176; F)",
           x = "",
           title = glue::glue("{input$city} temperature over year")) +
      scale_x_date(date_labels = "%B")+ 
      theme(text = element_text(size = 18),
            axis.title.y = ggtext::element_markdown(size = 18))   
  })
}


# Run the application 
shinyApp(ui = ui, server = server) %>% run_with_themer()
