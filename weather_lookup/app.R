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
      # Some css that makes everything above plot center aligned
      tags$style(HTML("
       #header {
        display: grid;
        grid-template-columns: repeat(3, 1fr);
        grid-template-rows: repeat(2, 1fr);
        justify-items: center;
        align-items: flex-start;
      }

      #header > h2 { grid-area: 1 / 1 / 2 / 4; }
      #prev_city { grid-area: 2 / 1 / 3 / 2; }
      .shiny-input-container { grid-area: 2 / 2 / 3 / 3; }
      #rnd_city { grid-area: 2 / 3 / 3 / 4; }
    "))),
    # Application title
    div(id = "header",
      titlePanel("Explore your weather"),
      selectizeInput('city', 
                     label = NULL,
                     choices = unique_cities, 
                     selected = "Ann Arbor, MI", 
                     multiple = FALSE),
      actionButton('prev_city', textOutput('prev_city_label')),
      actionButton('rnd_city', "Random City")
      
    ),
    plotOutput("tempPlot")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
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
  }) %>% 
    shiny::bindCache(input$city)
  
  # These hold book keeping stuff so we can have a back button
  # Start previous city button at a random city by setting the current city as
  # random The observe below then moves that into the previous city value
  current_city <- reactiveVal(get_random_city())
  previous_city <- reactiveVal(NULL)
  
  observe({
    validate(
      need(input$city != '', 'Search for your city')
    )
    # Set the previous city to the non-updated current city. 
    # We need an isolate() here to avoid causing this call 
    # to trigger the reactive() update in a loop
    previous_city(isolate(current_city()))
    # Current city now can be updated to the newly selected city
    current_city(input$city)
  })
  
  output$prev_city_label <- renderText({ previous_city() })
  
  observeEvent(input$prev_city, {
    updateSelectizeInput(
      session = session,
      inputId = "city",
      selected = isolate(previous_city())
    )
  })
  
  observeEvent(input$rnd_city, {
    updateSelectizeInput(
      session = session,
      inputId = "city",
      selected = get_random_city()
    )
  })
  
  output$tempPlot <- renderCachedPlot({
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
  },
  # Don't waste time redrawing the same plot again
  cacheKeyExpr = { list(input$city) })
}


# Run the application 
shinyApp(ui = ui, server = server) 
# %>% run_with_themer()
