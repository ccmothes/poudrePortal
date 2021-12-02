# radar shiny app
library(tidyverse)
library(leaflet)
library(shiny)
library(lubridate)
library(hms)
library(shinyTime)

#source("scripts/get_local.R")


#ui



ui <- fluidPage(theme = bs_theme(bootswatch = "darkly"),
  #Application Title
  titlePanel("Weather Radar"),
  
  fluidRow(column(12,
                  
                  sliderInput(
                    "date",
                    "Observation Date",
                    value = Sys.Date(),
                    min = as.Date("2020-08-01"),
                    max = Sys.Date(),
                    timezone = "-0600",
                    width = '100%'
                    
  )
  )),
  
  fluidRow(column(6,
                  sliderInput(
                    "time",
                    "Time",
                    value = strptime("12:00", "%H:%M"),
                    min = strptime("00:00", "%H:%M"),
                    max = strptime("23:50", "%H:%M"),
                    timeFormat = "%H:%M",
                    timezone = "-0600",
                    width = '100%',
                    step = 1800,
                    animate = animationOptions(interval = 2000)
                  # timeInput(
                  #   "time",
                  #   "Time",
                  #   value = Sys.time(),
                  #   minute.steps = 10
                  ))),
  
  # fluidRow(column(12,
  #                 sliderInput(
  #                   "time",
  #                   "Time",
  #                   value = "12:00:00",
  #                   min = "1:00:00",
  #                   max = "24:00:00")
  # 
  #                 )),

  
  fluidRow(column(12,
                  
                  leaflet::leafletOutput("plot", width = '80%' , height = 600))
  )
)



#server

server <- function(input, output, session) {
  
  time <- reactive({
   # value <- dateToTimeList(time)
    paste(c(dateToTimeList(input$time)$hour, dateToTimeList(input$time)$min, 
            dateToTimeList(input$time)$sec), collapse = ':')


  })


  
  output$plot <- leaflet::renderLeaflet({
    leaflet() %>% 
    addTiles() %>% 
      #leaflet::setView(lng = -90.07, lat = 29.95, zoom = 7) %>% 
      #addCircleMarkers(data = locations, radius = 3, color = "red") %>% 
      addPolygons(data = st_transform(watersheds, 4326),
                  color = "red",
                  opacity = 1,
                  popup = ~Site) %>%
      # addCircleMarkers(data = location_stations, color = "red",
      #                  stroke = TRUE, fillOpacity = 1) %>% 
      addWMSTiles(
        "https://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0q-t.cgi?",
        layers = "nexrad-n0q-wmst",
        options = WMSTileOptions(format = "image/png", transparent = TRUE,
                                 time = as.POSIXct(paste(input$date, time()), 
                                                   format="%Y-%m-%d %H:%M", tz = "UTC"))
      )
                                 
    
  })
  
}




shinyApp(ui,server)


# try 'shinyTime r package to enter sepcific date and time and
# 'update' button so it only calls new radar image when ready. OR choose
# range and it forms an animation











