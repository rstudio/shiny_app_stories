library(shiny)
library(bslib)
library(ggplot2)
library(thematic)
library(ggtext)
library(glue)

source('helpers.R')

# Builds theme object to be supplied to ui
my_theme <- bs_theme(
  bootswatch = "cerulean",
  base_font = font_google("Righteous"),
  "font-size-base" = "1.1rem"
)
# Let thematic know to use the font from bs_lib
thematic_on(font = "auto")

# Used to format annotations for extreme days
date_fmt <- "%B %d"

# We have an already build station to city df we are using for lookups
station_to_city <- read_rds(here("data/station_to_city.rds"))

# Some cities have multiple stations but we only want users to see unique cities
unique_cities <- unique(station_to_city$city)
# We start with a random city in the back button and have a random city jump button
get_random_city <- function(){ sample(unique_cities, 1) }

ui <- fluidPage(
    theme = my_theme,
    tags$head(
      # Some css that makes everything above plot center aligned
      tags$style(HTML("
      #header {
        display: grid;
        justify-items: center;
        grid-template-columns: repeat(3, 1fr);
        grid-template-rows: repeat(2, 1fr);
        align-items: flex-start;
      }

      #header > h2   { grid-area: 1 / 1 / 2 / 4; }
      #city-selector { grid-area: 2 / 2 / 3 / 3; }
      #prev_city_btn { grid-area: 2 / 1 / 3 / 2; }
      #rnd_city_btn  { grid-area: 2 / 3 / 3 / 4; }
      
      button { width: 200px; }
      button > div {
        text-overflow: ellipsis;
        overflow: hidden;
        white-space: nowrap;
      }
    "))),
    div(id = "header",
      titlePanel("Explore your weather"),
      labeled_input(
        'city-selector', 
        "Search for a city",
        selectizeInput('city', 
                       label = NULL,
                       choices = unique_cities, 
                       selected = "Ann Arbor, MI", 
                       multiple = FALSE)
      ),
      labeled_input(
        "prev_city_btn", 
        "Return to previous city",
        actionButton('prev_city', textOutput('prev_city_label'))
      ),
      labeled_input(
        "rnd_city_btn", 
        "Try a random city",
        actionButton('rnd_city', icon('dice'))
      )
    ),
    plotOutput("tempPlot"),
    plotOutput("prcpPlot")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  city_data <- reactive({
    validate(need(input$city != '', 'Search for your city'))
    
    withProgress(message = 'Fetching data from NOAA', {
    incProgress(0, detail = "Gathering city's station ids")
    stations <- filter(station_to_city, city == input$city)$station
    
    # Figure out how many steps 
    n_stations <- length(stations)
    
    temp_data <- NULL
    prcp_data <- NULL
    
    # Not every station has temperature data. This loops through all stations in
    # a city and tries to find one with temperature data. If there are a lot of
    # stations, this can take a while
    for (i in 1:n_stations){
      
      incProgress(i/n_stations, detail = paste( "Checking station", i, "for data"))

      # This is long but doesnt change
      station_url_prefix <- "https://www1.ncdc.noaa.gov/pub/data/normals/1981-2010/products/auxiliary/station"
      
      # Constructs the correct url for station data and downloads it
      try({
        station_txt <- glue("{station_url_prefix}/{stations[i]}.normals.txt") %>%
          readr::read_file()
      })
      
      # We only want to look for temperature or precipitation data if we havent already found it
      # the get_*_data() functions will simply give back NULL if the data isn't present so checking
      # if current value is null will tell us if we still need to look for the data
      if(is.null(temp_data)){
        temp_data <- get_temp_data(station_txt)
      }
     if(is.null(prcp_data)){
        prcp_data <- get_prcp_data(station_txt)
      }
    }
    
    incProgress(1, detail = "Packaging data for app")
    list(temperature = temp_data, percipitation = prcp_data)
    })
    
  }) %>% 
    shiny::bindCache(input$city)
  
  # These hold book keeping stuff so we can have a back button
  # Start previous city button at a random city by setting the current city as
  # random The observe below then moves that into the previous city value
  current_city <- reactiveVal(get_random_city())
  previous_city <- reactiveVal(NULL)
  
  observe({
    req(input$city)
    # Set the previous city to the non-updated current city. 
    # We need an isolate() here to avoid causing this call 
    # to trigger the reactive() update in a loop
    previous_city(isolate(current_city()))
    # Current city now can be updated to the newly selected city
    current_city(input$city)
  })
  
  output$prev_city_label <- renderText({ previous_city() })
  
  observe({
    updateSelectizeInput(
      session = session,
      inputId = "city",
      selected = isolate(previous_city())
    )
  }) %>% bindEvent(input$prev_city)
  
  observe({
    updateSelectizeInput(
      session = session,
      inputId = "city",
      selected = get_random_city()
    )
  }) %>% bindEvent(input$rnd_city)
  
  output$tempPlot <- renderPlot({
    validate(
      need(
        city_data()$temperature, 
        glue("Sorry, no temperature data is available for {input$city}, try a nearby city.")
      )
    )
    
    withProgress(message = 'Building temperature plot', {
      incProgress(0/2, detail = "Finding hottest and coldest days")
      
      extremes <- bind_rows(
        arrange(city_data()$temperature, -max, -avg, -min)[1,] %>% 
          mutate(label = glue("Hotest day: {format(date, date_fmt)}<br>",
                                    "Avg max temp = {format(max, digits = 3)}&#176;")), 
        arrange(city_data()$temperature, min, avg, max)[1,] %>% 
          mutate(label = glue("Coldest day: {format(date, date_fmt)}<br>",
                                    "Avg min temp = {format(min, digits = 3)}&#176;"))
      )
    
      incProgress(1/2, detail = "Rendering plot")
      
      ggplot(city_data()$temperature, aes(x = date, y = avg)) +
        geom_ribbon(aes(ymin = min, ymax = max), 
                    fill = "steelblue", 
                    alpha = 0.5) +
        geom_line(color = "white") +
        geom_point(data = extremes) +
        ggtext::geom_richtext(
          data = extremes, 
          aes(label = label, hjust = ifelse(month(date) < 6, 0, 1)),
          nudge_y = -1,
          label.color = NA, 
          fill = after_scale(alpha("white", .5)), # Gives us a transparent background so text pops better
          vjust = 1 ) +
        labs(y = "temperature (&#176; F)",
             x = "",
             title = glue("{input$city} temperature over year")) +
        scale_x_date(date_labels = "%B")+ 
        theme(text = element_text(size = 18),
              axis.title.y = ggtext::element_markdown(size = 18))   
      
    })
  }) %>% shiny::bindCache(input$city)
 
  output$prcpPlot <- renderPlot({
  
    validate(
      need(
        city_data()$percipitation, # is NULL when no data available
        glue("Sorry, no percipitation data is available for {input$city}, try a nearby city.")
      )
    )
    
    withProgress(message = 'Building precipitation plot', {
      incProgress(0/2, detail = "Finding wettest day")
      
      wettest_day <- arrange(city_data()$percipitation, -day_amnt) %>% 
        head(1) %>% 
        mutate(label = glue(
          "Wettest day: {format(date, date_fmt)}<br>",
          "Avg percipitation = {format(day_amnt, digits = 3)}\""))
      
      incProgress(1/2, detail = "Rendering plot")
      
      ggplot(city_data()$percipitation, aes(x = date,y = day_amnt)) + 
        geom_point() + 
        geom_line(aes(y = week_avg), color = "steelblue", size = 3) +
        ggtext::geom_richtext(
          data = wettest_day, 
          aes(label = label, hjust = ifelse(month(date) < 6, 0, 1)),
          nudge_x = 3,
          label.color = NA,
          fill = after_scale(alpha("white", .5)), # Gives us a transparent background so text pops better
          vjust = 1 ) +
        scale_x_date(date_labels = "%B")+ 
        theme(text = element_text(size = 18),
              axis.title.y = ggtext::element_markdown(size = 18)) +
        labs(y = "inches of precipitation",
             x = "",
             title = glue("{input$city} precipitation over year"),
             caption = "Points show daily average, line is rolling weekly average")
      
    })
    }) %>% shiny::bindCache(input$city)
}


# Run the application 
shinyApp(ui = ui, server = server) 
# %>% run_with_themer()
