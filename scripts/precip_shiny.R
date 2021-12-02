# precip shiny

library(shiny)
library(rnoaa)
library(leaflet)



#ui



ui <- fluidPage(
  #Application Title
  titlePanel("Weather Radar"),
  
  fluidRow(column(10,
                  
                  sliderInput(
                    "date",
                    "Observation Date",
                    value = Sys.Date() - 1,
                    min = as.Date("2020-08-01"),
                    max = Sys.Date() - 1,
                    
                    width = '100%'
                    
                  )
  )),
  
  fluidRow(column(8,
                  selectInput(
                    "variable",
                    "Variable",
                    choices = c("Precipitation",
                                "Snowfall",
                                "Snow_depth",
                                "Average_temp",
                                "Minimum_temp",
                                "Maximum_temp")
                  ))),
  
 
  
  fluidRow(column(12,
                  
                  leaflet::leafletOutput("plot", width = '80%' , height = 600))
  )
)



#server

server <- function(input, output, session) {
  
  data <- reactive({
    weather_coords %>% filter(date == input$date) %>% 
      select(date, latitude, longitude, variable = input$variable)
  })

  
  
  
  output$plot <- leaflet::renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(
        data = data(),
        lng = ~longitude,
        lat = ~latitude,
        radius = ~ sqrt(variable),
        color = "red",
        stroke = TRUE,
        fillOpacity = 1
      )
    
    
  })
  
}




shinyApp(ui,server)
