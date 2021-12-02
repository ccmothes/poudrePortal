#time loop test

library(shiny)
library(bslib)
library(dplyr)
library(leaflet)
library(plotly)
library(readr)
library(sf)

camPeak_simple <- readRDS("data/camPeakSimple.RDS")
weather_coords <- readRDS("data/weather_coords.RDS")
daily_data <- readRDS("data/daily_data.RDS")

#from shinyTime:
dateToTimeList <- function(value){
  if(is.null(value)) return(NULL)
  posixlt_value <- unclass(as.POSIXlt(value))
  time_list <- lapply(posixlt_value[c('hour', 'min', 'sec')], function(x) {
    sprintf("%02d", trunc(x))
  })
  return(time_list)
}



ui <- navbarPage(
  theme = bslib::bs_theme(
    bootswatch = "flatly",
    #bg = "#FFFFFF",
    #fg = "#000",
    primary = "#186D03",
    secondary = "#DD5B27",
    success = "#f28e35",
    base_font = font_google("Cairo")),
  
  "Portal Demo",
  id = "nav",
  
  tabPanel(
    "Interactive Map",
    # tags$style("
    #     #controls {
    #       background-color: #ddd;
    #       opacity: 0.5;
    #     }
    #     #controls:hover{
    #       opacity: 1;
    #     }
    #            "),
    
    sidebarLayout(
      position = "right",
      
      mainPanel(
        leaflet::leafletOutput(
        "map", width = '100%' , height = 800
      ),
      
      plotlyOutput("plot1", width = "100%")
      
      ),
      
      sidebarPanel(
        h4("Weather Explorer"),
        sliderInput(
          "date",
          label = "Observation Date:", 
          value = as.Date("2021-08-30"),
          min = as.Date("2015-10-01"),
          max = Sys.Date(),
          timezone = "-0600",
          width = '100%'
          
        ),
        sliderInput(
          "time",
          "Radar Time (MST):",
          value = strptime("12:00", "%H:%M"),
          min = strptime("00:00", "%H:%M"),
          max = strptime("23:50", "%H:%M"),
          timeFormat = "%H:%M",
          timezone = "-0600",
          width = '100%',
          step = 900,
          animate = animationOptions(interval = 3000)
        ),
        selectInput(
          "variable",
          "Weather Variable:",
          choices = c(
            "Precipitation",
            "Snowfall",
            "Snow Depth" = "Snow_depth",
            "Minimum Temperature" = "Minimum_temp", "Maximum Temperature" = "Maximum_temp",
            "Average Temperature" = "Average_temp"
          )
        ),
        em("Circle size represents variable value"),
        h4("----------------------------"),
        h4("Data Explorer"),
        br(),
        em("Click on a watershed and choose the date range and variable you want to view"),
        br(),
        br(),
        sliderInput(
          "range",
          "Date Range:",
          value = c(as.Date("2015-10-01"), as.Date("2019-09-30")),
          min = as.Date("2015-10-01"),
          max = as.Date("2019-09-30"),
          timezone = "-0600",
          width = '100%'
          
        ),
        selectInput("streamVar", "Sensor Variable:", choices = c(
          "Precipitation" = "P_mm",
          "Air Temperature" = "Ta_C",
          "Soil Temperature" = "Ts_C",
          "Snow Depth" = "Snow_depth_cm",
          "Average Daily Discharge" = "Discharge_Ls",
          "Total Daily Discharge" = "Q_mm")),
        br(),
        em("Click on a weather station and choose which variable to plot against the watershed data"),
        br(),
        br(),
        selectInput("weatherVar", "Weather Station Variable:", choices = c(
          "Precipitation", "Snowfall", "Snow Depth" = "Snow_depth",
          "Minimum Temperature" = "Minimum_temp", "Maximum Temperature" = "Maximum_temp",
          "Average Temperature" = "Average_temp")),
        br(),
        strong("Note: some data may be missing for certain dates/variables")
        
          
          
        
      )
    )
  ),
  
  
  tabPanel(
    "Sentinel Explorer",
    h2("Sentinel interactive map here hopefully....")
  )
)


server <-  function(input, output, session){
  
  
  weather <- reactive({
    weather_coords %>% filter(date == input$date) %>% 
      dplyr::select(date, id, latitude, longitude, variable = input$variable) %>% 
      filter(!is.na(variable))
  })
  
  time <- reactive({
    # value <- dateToTimeList(time)
    paste(c(dateToTimeList(input$time)$hour, dateToTimeList(input$time)$min, 
            dateToTimeList(input$time)$sec), collapse = ':')
  })

  
  output$map <- leaflet::renderLeaflet({
    leaflet() %>%
      addTiles(layerId = "A", group = "Open Street Map") %>%
      addProviderTiles("Esri.WorldImagery", layerId = "C", group = "Satellite") %>% 
    addMapPane("fire", zIndex = 410) %>% 
      addMapPane("watersheds", zIndex = 420) %>% 
      addMapPane("weather", zIndex = 430) %>% 
      addPolygons(
        data = daily_data,
        layerId = ~Site,
        color = "blue",
        opacity = 1,
        popup = ~ Site,
        group = "watersheds",
        options = pathOptions(pane = "watersheds")
      ) %>%
      addPolygons(
        data = camPeak_simple,
        color = NA,
        weight = 1,
        smoothFactor = 0.5,
        opacity = 1.0,
        fillOpacity = 0.9,
        fillColor = ~ colorFactor("Reds", Severity)(Severity),
        group = "Cameron Peak Fire",
        options = pathOptions(pane = "fire")
      ) %>%
      addScaleBar(position = "bottomright") %>%
      
      addLayersControl(
        baseGroups = c("Open Street Map", "Satellite"),
        overlayGroups = c("Watersheds",  "Cameron Peak Fire", "Weather Stations"),
        position = "topleft",
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup(c("Weather Stations", "Cameron Peak Fire"))  

    
    
  })
  
  observe({
    
    
    leafletProxy("map") %>%
      clearMarkers() %>% 
    addCircleMarkers(
      data = weather(),
      layerId = ~id,
      lng = ~ longitude,
      lat = ~ latitude,
      radius = ~ sqrt(variable),
      color = "red",
      stroke = TRUE,
      fillOpacity = 1,
      popup = paste("Station:", weather()$id, "<br>",
                   input$variable, weather()$variable
                   ),
      group = "Weather Stations",
      options = pathOptions(pane = "weather")

    )
  })
  
  observe({
    
    leafletProxy("map") %>% 
      removeTiles(layerId = "B") %>% 
    addWMSTiles(
      layerId = "B",
      "https://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0q-t.cgi?",
      layers = "nexrad-n0q-wmst",
      options = WMSTileOptions(
        format = "image/png",
        transparent = TRUE,
        time = as.POSIXct(paste(input$date, time()),
                          format = "%Y-%m-%d %H:%M", tz = "UTC"),
        group = "Radar"
      )
    ) 
    
    
  })
  
  clicked_map <- reactiveValues(clickedShape=NULL)
  observeEvent(input$map_shape_click,{
    clicked_map$clickedShape <- input$map_shape_click
  })
  
  selected_watershed <- reactive({
    clicked_map$clickedShape
  })
  
  clicked_station <- reactiveValues(clickedMarker = NULL)
  observeEvent(input$map_marker_click,{
    clicked_station$clickedMarker <- input$map_marker_click$id
  })
  
  selected_station <- reactive({
    clicked_station$clickedMarker
  })
  

  selected_stream <- reactive({
    if(is.null(clicked_map$clickedShape))
      return(NULL)
    
    filter(daily_data, Site == selected_watershed()) %>% 
      filter(as.Date(Date) >= input$range[1] & as.Date(Date) <= input$range[2]) %>% 
      dplyr::rename(variable = input$streamVar)
  })
  
  selected_weather <- reactive({
    if(is.null(clicked_station$clickedMarker))
      return(NULL)
    
    filter(weather_coords, id == selected_station()) %>% 
      mutate(date = as.POSIXct(date, format = "%m/%d/%Y")) %>% 
      filter(as.Date(date) >= input$range[1] & as.Date(date) <= input$range[2]) %>% 
      dplyr::rename(variable = input$weatherVar)
  
  })
  

    output$plot1 <- renderPlotly({
      
      
      stream <- selected_stream()
      station <- selected_weather()
      
      
      if (is.null(stream) & is.null(station))
        return(NULL)
        

      if (is.null(station)) {
          plotly::plot_ly() %>%
          add_lines(x = stream$Date,
                    y = stream$variable,
                    name = "Watershed Data") %>% 
          plotly::layout(yaxis = list(title = paste0("Watershed Sensor ", input$streamVar)))
        
        
       } else{

        plotly::plot_ly() %>%
          add_lines(x = stream$Date,
                    y = stream$variable,
                    name = "Watershed Data") %>%
          add_lines(
              x = station$date,
               y = station$variable,
               name = "Weather Data",
               yaxis = "y2",
               color = I("red"),
               opacity = 0.8
             ) %>%
             plotly::layout(
               #title = "Data Test",
               yaxis2 = list(
                side = "right",
                title = paste("Weather Station ", input$weatherVar),
                overlaying = "y"
              ),
              yaxis = list(title = paste0("Watershed Sensor ", input$streamVar))
            )

      }



    
  })
  

}


shinyApp(ui,server)

  
