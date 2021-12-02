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
water_qual <- readRDS("data/water_qual.RDS")

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
    base_font = font_google("Cairo")
  ),
  
  "Portal Demo",
  id = "nav",
  
  tabPanel("Data Explorer",
           # tags$style("
           #     #controls {
           #       background-color: #ddd;
           #       opacity: 0.5;
           #     }
           #     #controls:hover{
           #       opacity: 1;
           #     }
           #            "),
           
           fluidPage(#sidebarLayout(
             #position = "right",
             fluidRow(
               column(4,
                      leaflet::leafletOutput(
                        "map1", width = '100%' , height = 400
                      ),
                      em(
                        "Click on an object to view time series plots to the right"
                      ),),
               
               
               column(
                 8,
                 
                 sliderInput(
                   "range",
                   "",
                   value = c(as.Date("2015-10-01"), as.Date("2021-08-25")),
                   min = as.Date("2015-10-01"),
                   max = as.Date("2021-08-25"),
                   timezone = "-0600",
                   width = '100%'
                   
                 ),
                 plotlyOutput("precip", width = "100%", height = 120),
                 plotlyOutput("q", width = "100%", height = 150),
                 selectInput(
                   "streamVar",
                   "Other Sensor Variables:",
                   choices = c(
                     "Air Temperature" = "Ta_C",
                     "Soil Temperature" = "Ts_C",
                     "Snow Depth" = "Snow_depth_cm",
                     "Total Daily Discharge" = "Q_mm"
                   )
                 ),
                 plotlyOutput("other", width = "100%", height = 150),
                 selectInput("qual", "Water Quality Variables:",
                             choices = c("Turbidity", "DOC", "DTN", "pH",
                                         "ANC", "SC", "Na", "NH4", "K", "Mg",
                                         "Ca", "F", "Cl", "NO3", "PO4", "SO4")),
                 plotlyOutput("waterQual", width = "100%", height = 150),
                 # h4(
                 #   "NOAA Weather Viewer"
                 # ),

                 selectInput(
                   "weatherVar",
                   "NOAA Weather Stations",
                   choices = c(
                     "Precipitation",
                     "Snowfall",
                     "Snow Depth" = "Snow_depth",
                     "Minimum Temperature" = "Minimum_temp",
                     "Maximum Temperature" = "Maximum_temp",
                     "Average Temperature" = "Average_temp"
                   )
                 ),
                 plotlyOutput("noaa", width = "100%", height = 150),
                 strong("Note: some data may be missing for certain dates/variables"),
                 #plotlyOutput("plot1", width = "100%")
                 
               )
               
               
             ))),
  
  
  tabPanel(
    "Interactive Map",
    
    sidebarLayout(
      position = "right",
      
      mainPanel(leaflet::leafletOutput(
        "map2", width = '100%' , height = 800
      )),
      sidebarPanel(
        strong(
          "This map will also include a Sentinel Imagery explorer and the ability to turn on/off datasets to view study site/sensor locations"
        ),
        
        
        # absolutePanel(
        #   id = "controls",
        #   class = "panel panel-default",
        #   fixed = TRUE,
        #   draggable = TRUE,
        #   top = 90,
        #   left = 20,
        #   right = "auto",
        #   bottom = "auto",
        #   width = 500,
        #   height = "auto",
        #   style = "opacity: 0.9; background-color: white; padding: 0 20px 20px 20px",
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
            "Minimum Temperature" = "Minimum_temp",
            "Maximum Temperature" = "Maximum_temp",
            "Average Temperature" = "Average_temp"
          )
        ),
        em("Circle size represents variable value")
      )
      
    )
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

  
  output$map1 <- leaflet::renderLeaflet({
    leaflet() %>%
      addTiles(group = "Open Street Map") %>%
      addProviderTiles("Esri.WorldImagery", layerId = "C", group = "Satellite") %>%
      addWMSTiles(
        sprintf(
          "https://%s/arcgis/services/%s/MapServer/WmsServer",
          "basemap.nationalmap.gov",
          "USGSTopo"
        ),
        group = "USGS Topo",
        attribution = paste0(
          "<a href='https://www.usgs.gov/'>",
          "U.S. Geological Survey</a> | ",
          "<a href='https://www.usgs.gov/laws/policies_notices.html'>",
          "Policies</a>"
        ),
        layers = "0"
      ) %>% 
      addMapPane("fire", zIndex = 410) %>%
      addMapPane("watersheds", zIndex = 420) %>%
      addMapPane("waterqual", zIndex = 430) %>%
      addMapPane("weather", zIndex = 440) %>%
      addPolygons(
        data = daily_data,
        layerId = ~ Site,
        color = "blue",
        opacity = 1,
        popup = ~ Site,
        group = "Watersheds",
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
      addCircleMarkers(
        data = water_qual,
        layerId = ~ SiteLabel,
        lng = ~ long,
        lat = ~ lat,
        radius = 5,
        color = "black",
        fillColor = "yellow",
        stroke = TRUE,
        weight = 1,
        fillOpacity = 1,
        popup = paste(
          "Site:",
          water_qual$Site,
          "<br>",
          "Site Type:",
          water_qual$SiteType,
          "<br>",
          "CP Fire:",
          water_qual$Trt_CP
        ),
        
        group = "Water Quality Sensors",
        options = pathOptions(pane = "waterqual")
      ) %>%
      #addLegend("bottomright", data = daily_data, group = "watersheds", colors = "blue", labels = "Watersheds") %>% 
      #addLegend("bottomright", data = water_qual, group = "Water Quality Censors", colors = "yellow", labels = "Water Quality Sensors") %>% 
      #addLegend("bottomright", values = camPeak_simple, group = "Cameron Peak Fire", pal = ~ colorFactor("Reds", Severity)(Severity)) %>% 
      addScaleBar(position = "bottomright") %>%
      
      addLayersControl(
        baseGroups = c("USGS Topo", "Open Street Map", "Satellite"),
        overlayGroups = c(
          "Watersheds",
          "Water Quality Sensors",
          "Weather Stations",
          "Cameron Peak Fire"
        ),
        position = "topleft",
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      hideGroup("Cameron Peak Fire")
    
    
    
  })
  
  observe({
    
    input$nav
    
    tab1 <- leafletProxy("map1") %>%
      removeMarker("Weather Stations") %>% 
    addCircleMarkers(
      data = weather(),
      layerId = ~id,
      lng = ~ longitude,
      lat = ~ latitude,
      radius = 5,
      color = "black",
      fillColor = "red",
      stroke = TRUE,
      weight = 1,
      fillOpacity = 1,
      popup = paste("Station:", weather()$id
                   ),
      group = "Weather Stations",
      options = pathOptions(pane = "weather")) #%>% 
      #addLegend("bottomright", values = weather(), colors = "red", group = "Weather Stations")
      
      tab2 <- leafletProxy("map2") %>%
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
  

  selected_stream <- reactive({
    if(is.null(input$map1_shape_click))
      return(NULL)
    
    filter(daily_data, Site == input$map1_shape_click) %>% 
      filter(as.Date(Date) >= input$range[1] & as.Date(Date) <= input$range[2]) %>% 
      dplyr::rename(variable = input$streamVar)
  })
  
  selected_weather <- reactive({
    if(is.null(input$map1_marker_click))
      return(NULL)

    if(input$map1_marker_click$id %in% weather_coords$id){

       filter(weather_coords, id == input$map1_marker_click$id) %>%
        mutate(date = as.POSIXct(date, format = "%m/%d/%Y")) %>%
        filter(as.Date(date) >= input$range[1] & as.Date(date) <= input$range[2]) %>%
        dplyr::rename(variable = input$weatherVar)



    } else{
      return(NULL)
    }
    
  })
  
  selected_qual <- reactive({
    if(is.null(input$map1_marker_click))
      return(NULL)
    
    if(input$map1_marker_click$id %in% water_qual$SiteLabel){
      filter(water_qual, SiteLabel == input$map1_marker_click$id) %>% 
        filter(as.Date(Date) >= input$range[1] & as.Date(Date) <= input$range[2]) %>% 
        dplyr::rename(variable = input$qual)
      
      
      
    } else {
      return(NULL)
    }

       
   
  })
  
  output$precip <- renderPlotly({
    
    #stream <- selected_stream()
    
    if(is.null(selected_stream()))
      return(NULL)
    
    
    plotly::plot_ly() %>% 
      add_bars(x = selected_stream()$Date, y = selected_stream()$P_mm, name = "Precipitation (mm)") %>% 
      plotly::layout(yaxis = list(title = "P (mm)", autorange = "reversed"),
                     xaxis = list(range = c(input$range[1], input$range[2]),
                                  showgrid = TRUE))
    
  })
  
  output$q <- renderPlotly({
    
   # stream <- selected_stream
    
    
    if(is.null(selected_stream()))
      return(NULL)
    
    plot_ly() %>%
      add_lines(x = selected_stream()$Date,
                y = selected_stream()$Discharge_Ls,
                name = "Average Daily Discharge") %>%
      plotly::layout(yaxis = list(title ="Q (L/s)"),
                     xaxis = list(range = c(input$range[1], input$range[2]),
                                  showgrid = T))
    
    
  })
  
  output$other <- renderPlotly({
    
    # stream <- selected_stream
    
    
    if(is.null(selected_stream()))
      return(NULL)
    
    plot_ly() %>%
      add_lines(x = selected_stream()$Date,
                y = selected_stream()$variable,
                name = input$streamVar) %>%
      plotly::layout(yaxis = list(title = input$streamVar),
                     xaxis = list(range = c(input$range[1], input$range[2]),
                                  showgrid = T))
    
    
  })
  
  output$waterQual <- renderPlotly({

    quality <- selected_qual()

    if(is.null(quality))
      return(NULL)

    plotly::plot_ly() %>%
      add_lines(x = quality$Date,
                y = quality$variable,
                name = input$qual) %>%
      plotly::layout(yaxis = list(title = input$qual),
                     xaxis = list(range = c(input$range[1], input$range[2]),
                                  showgrid = T))


  })

  output$noaa <- renderPlotly({

    station <- selected_weather()

    if(is.null(station))
      return(NULL)

    if(input$weatherVar == "Precipitation"){

      plotly::plot_ly() %>%
        add_bars(x = station$date, y = station$variable, name = "Precipitation (mm)") %>%
        plotly::layout(yaxis = list(title = "P (mm)", autorange = "reversed"),
                       xaxis = list(range = c(input$range[1], input$range[2]),
                                    showgrid = TRUE))
    }else {

      plot_ly() %>%
        add_lines(x = station$date,
                  y = station$variable,
                  name = paste(input$weatherVar)) %>%
        plotly::layout(yaxis = list(title = input$weatherVar),
                       xaxis = list(range = c(input$range[1], input$range[2]),
                                    showgrid = T))


    }



  })

    # output$plot1 <- renderPlotly({
    #   
    #   
    #   stream <- selected_stream()
    #   station <- selected_weather()
    #   
    #   
    #   if (is.null(stream) & is.null(station))
    #     return(NULL)
    #     
    # 
    #   if (is.null(station)) {
    #       plotly::plot_ly() %>%
    #       add_lines(x = stream$Date,
    #                 y = stream$variable,
    #                 name = "Watershed Data") %>% 
    #       plotly::layout(yaxis = list(title = paste0("Watershed Sensor ", input$streamVar)))
    #     
    #     
    #    } else{
    # 
    #     plotly::plot_ly() %>%
    #       add_lines(x = stream$Date,
    #                 y = stream$variable,
    #                 name = "Watershed Data") %>%
    #       add_lines(
    #           x = station$date,
    #            y = station$variable,
    #            name = "Weather Data",
    #            yaxis = "y2",
    #            color = I("red"),
    #            opacity = 0.8
    #          ) %>%
    #          plotly::layout(
    #            #title = "Data Test",
    #            yaxis2 = list(
    #             side = "right",
    #             title = paste("Weather Station ", input$weatherVar),
    #             overlaying = "y"
    #           ),
    #           yaxis = list(title = paste0("Watershed Sensor ", input$streamVar))
    #         )
    # 
    #   }



    
  
   
    #map tab ------------------------ 
    
    
    output$map2 <- leaflet::renderLeaflet({
      leaflet() %>%
        addTiles(layerId = "A", group = "Open Street Map") %>%
        addProviderTiles("Esri.WorldImagery", layerId = "C", group = "Satellite") %>%
        addWMSTiles(
          sprintf(
            "https://%s/arcgis/services/%s/MapServer/WmsServer",
            "basemap.nationalmap.gov",
            "USGSTopo"
          ),
          group = "USGS Topo",
          attribution = paste0(
            "<a href='https://www.usgs.gov/'>",
            "U.S. Geological Survey</a> | ",
            "<a href='https://www.usgs.gov/laws/policies_notices.html'>",
            "Policies</a>"
          ),
          layers = "0"
        ) %>% 
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
          baseGroups = c("USGS Topo", "Open Street Map", "Satellite"),
          overlayGroups = c("Watersheds",  "Cameron Peak Fire", "Weather Stations"),
          position = "topright",
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        hideGroup(c("Weather Stations", "Cameron Peak Fire"))  
      
      
      
    })
    
    # observe({
    #   
    #   leafletProxy("map2") %>%
    #     clearMarkers() %>% 
    #     addCircleMarkers(
    #       data = weather(),
    #       layerId = ~id,
    #       lng = ~ longitude,
    #       lat = ~ latitude,
    #       radius = ~ sqrt(variable),
    #       color = "red",
    #       stroke = TRUE,
    #       fillOpacity = 1,
    #       popup = paste("Station:", weather()$id, "<br>",
    #                     input$variable, weather()$variable
    #       ),
    #       group = "Weather Stations",
    #       options = pathOptions(pane = "weather")
    #       
    #     )
    # })
    # 
    observe({
      
      leafletProxy("map2") %>% 
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
    

}


shinyApp(ui,server)

  
