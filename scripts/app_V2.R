#time loop test

library(shiny)
library(bslib)
library(dplyr)
library(leaflet)
library(plotly)
library(readr)
library(sf)

camPeak_simple <- readRDS("data/camPeakSimple.RDS")
weather_data <- readRDS("data/weather_coords.RDS") %>% 
  rename(Site = id, Date = date, long = longitude, lat = latitude) %>% 
  mutate(source = "NOAA")
water_data <- readRDS("data/water_data.RDS") %>% arrange(Date)


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
                      ),
                      hr(),
                      actionButton("clear", "Clear Plots")),
               
               
               column(
                 8,
                 
                 sliderInput(
                   "range",
                   "",
                   value = c(as.Date("2015-10-01"), as.Date("2021-10-01")),
                   min = as.Date("2015-10-01"),
                   max = as.Date("2021-10-01"),
                   timezone = "-0600",
                   width = '100%'
                   
                 ),
                 p("Precipitation"),
                 
                 plotlyOutput("precip", width = "100%", height = 120),
                 selectInput(
                   "streamVar",
                   "Streamflow",
                   choices = c(
                     "Discharge" = "discharge_Ls",
                     "Stage" = "stage_cm"
                   )),
                 plotlyOutput("q", width = "100%", height = 150),
                 selectInput("qual", "Water Quality",
                             choices = c("Turbidity" = "Turbidity", "pH" = "pH", "DO" = "DO", "Conductivity" = "Conductivity")),
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
                 strong("Note: some data may be missing for certain dates/variables")
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
  
  weather1 <- reactive({
    weather_data %>% filter(Date == input$range) %>% 
      dplyr::select(Date, Site, lat, long, variable = input$weatherVar) %>% 
      filter(!(is.na(variable)))
    
  })
  
  weather2 <- reactive({
    weather_data %>% filter(Date == input$date) %>%
      dplyr::select(Date, Site, lat, long, variable = input$variable) %>%
      filter(!is.na(variable))
  })
  
  time <- reactive({
    # value <- dateToTimeList(time)
    paste(c(dateToTimeList(input$time)$hour, dateToTimeList(input$time)$min, 
            dateToTimeList(input$time)$sec), collapse = ':')
  })

  pal <- colorFactor(palette = "Spectral", water_data$source)
  
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
      addMapPane("water", zIndex = 440) %>%
      addMapPane("weather", zIndex = 430) %>%
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
        data = water_data,
        layerId = ~ Site,
        lng = ~ long,
        lat = ~ lat,
        radius = 6,
        color = "black",
        fillColor = ~ pal(source),
        stroke = TRUE,
        weight = 1,
        fillOpacity = 1,
        popup = paste(
          "Source:",
          water_data$source,
          "<br>",
          "Site:",
          water_data$Site
        ),

        options = pathOptions(pane = "water")
      ) %>%
      # addCircleMarkers(
      #   data = weather1(),
      #   layerId = ~Site,
      #   lng = ~ long,
      #   lat = ~ lat,
      #   radius = 4,
      #   color = "black",
      #   fillColor = "gray50",
      #   stroke = TRUE,
      #   weight = 1,
      #   fillOpacity = 0.6,
      #   popup = paste("Station:", weather1()$Site
      #   ),
      #   group = "Weather Stations",
      #   options = pathOptions(pane = "weather")) %>%
    addLegend("topright", data = weather1(), colors = "black", group = "Weather Stations", labels = "NOAA Weather Stations") %>% 
    
      # addLegend("bottomright", data = daily_data, group = "watersheds", colors = "blue", labels = "Watersheds") %>%
      # addLegend("bottomright", data = water_qual, group = "Water Quality Censors", colors = "yellow", labels = "Water Quality Sensors") %>%
      # addLegend("bottomright", values = camPeak_simple, group = "Cameron Peak Fire", pal = ~ colorFactor("Reds", Severity)(Severity)) %>%
      addLegend("topright", data = water_data, values = ~source, 
                pal = pal, title = "Data source") %>% 
      
      addScaleBar(position = "bottomright") %>%
      
      addLayersControl(
        baseGroups = c("USGS Topo", "Open Street Map", "Satellite"),
        overlayGroups = c(
          "Weather Stations"
          
        ),
        position = "topleft",
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      hideGroup(c("Weather Stations", "Cameron Peak Fire"))
    
    
    
  })
  

  observe({

    input$nav

    tab1 <- leafletProxy("map1") %>%
      removeMarker("Weather Stations") %>%
    addCircleMarkers(
      data = weather1(),
      layerId = ~Site,
      lng = ~ long,
      lat = ~ lat,
      radius = 4,
        color = "black",
        fillColor = "gray50",
        stroke = TRUE,
        weight = 1,
        fillOpacity = 0.6,
        popup = paste("Station:", weather1()$Site
        ),
        group = "Weather Stations",
        options = pathOptions(pane = "weather")) #%>%
      #addLegend("topright", data = weather1(), colors = "black", group = "Weather Stations", labels = "NOAA Weather Stations")
      

      tab2 <- leafletProxy("map2") %>%
        clearMarkers() %>%
        addCircleMarkers(
                data = weather2(),
                layerId = ~Site,
                lng = ~ long,
                lat = ~ lat,
                radius = ~ sqrt(variable),
                color = "red",
                stroke = TRUE,
                fillOpacity = 1,
                popup = paste("Station:", weather2()$Site, "<br>",
                              input$variable, weather2()$variable
                ),
                group = "Weather Stations",
                options = pathOptions(pane = "weather")

              )


  })


  # plotlys
  
  df <- reactiveVal(bind_rows(water_data, weather_data) %>% mutate(key = 1:nrow(.)))
  combined <- reactiveVal(data.frame())
  
  
  filtered_df <- reactive({
    res <- df() %>% filter(as.Date(Date) >= input$range[1] & as.Date(Date) <= input$range[2])
    #res <- res %>% rename(streamflow = input$streamVar, quality = input$qual)
    res
    
    
  })
  
  observeEvent(input$map1_marker_click, {
    
    combined(bind_rows(combined(),
                       filtered_df() %>% 
                         #df() %>% filter(key %in% filtered_df()$key) %>%
                         filter(Site == input$map1_marker_click))) 
    
    
    #df(df() %>% filter(!key %in% filtered_df()$key))
    
  })
  
  final_df <- reactive({
    combined() %>% rename(streamflow = input$streamVar, quality = input$qual,
                          weather = input$weatherVar)
    
  })
  
  
  
  output$precip <- renderPlotly({
    
    
    if(nrow(combined()) == 0)
      return(NULL)
    
    
    plotly::plot_ly() %>% 
      add_bars(x = combined()$Date, y = combined()$precip_mm, name = ~combined()$Site,
               color = ~ combined()$Site) %>% 
      plotly::layout(yaxis = list(title = "P (mm)", autorange = "reversed"),
                     xaxis = list(range = c(input$range[1], input$range[2]),
                                  showgrid = TRUE))
    
  })
  
  output$q <- renderPlotly({
    
    
    if(nrow(combined()) == 0)
      return(NULL)
    
    plot_ly() %>%
      add_lines(x = final_df()$Date,
                y = final_df()$streamflow,
                name = ~final_df()$Site,
                linetype = ~ final_df()$Site) %>%
      plotly::layout(yaxis = list(title = input$streamVar),
                     xaxis = list(range = c(input$range[1], input$range[2]),
                                  showgrid = T))
    
    
  })
  
  
  
  
  output$waterQual <- renderPlotly({
    
    if(nrow(combined()) == 0)
      return(NULL)
    
    if(!(input$qual %in% names(combined())))
      return(NULL)
    
    plotly::plot_ly() %>%
      add_lines(x = final_df()$Date,
                y = final_df()$quality,
                name = ~final_df()$Site,
                linetype = ~ final_df()$Site) %>%
      plotly::layout(yaxis = list(title = input$qual),
                     xaxis = list(range = c(input$range[1], input$range[2]),
                                  showgrid = T))
    
    
  })
  
  
  
  output$noaa <- renderPlotly({
    
    if(nrow(combined()) == 0)
      return(NULL)
    
    if(!(input$weatherVar %in% names(combined())))
      return(NULL)
    
    
    if(input$weatherVar == "Precipitation"){
      
      plotly::plot_ly() %>%
        add_bars(x = final_df()$Date, y = final_df()$weather, name = ~ final_df()$Site,
                 color = ~ final_df()$Site) %>%
        plotly::layout(yaxis = list(title = "P (mm)", autorange = "reversed"),
                       xaxis = list(range = c(input$range[1], input$range[2]),
                                    showgrid = TRUE))
    }else {
      
      plot_ly() %>%
        add_lines(x = final_df()$Date,
                  y = final_df()$weather,
                  name = ~ final_df()$Site) %>%
        plotly::layout(yaxis = list(title = input$weatherVar),
                       xaxis = list(range = c(input$range[1], input$range[2]),
                                    showgrid = T))
      
      
    }
    
    
    
  })
  
  
  observeEvent(input$clear, {
    combined(data.frame())
  })
  
  
    # 
    # #map tab ------------------------ 
    # 
    # 
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
        addMapPane("water", zIndex = 430) %>%
        addMapPane("weather", zIndex = 420) %>%
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
          overlayGroups = c("Cameron Peak Fire", "Weather Stations"),
          position = "topright",
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        hideGroup( "Cameron Peak Fire")



    })

    # observe({
    # 
    #   leafletProxy("map2") %>%
    #     clearMarkers() %>%
    #     addCircleMarkers(
    #       data = weather2(),
    #       layerId = ~Site,
    #       lng = ~ long,
    #       lat = ~ lat,
    #       radius = ~ sqrt(variable),
    #       color = "red",
    #       stroke = TRUE,
    #       fillOpacity = 1,
    #       popup = paste("Station:", weather2()$Site, "<br>",
    #                     input$variable, weather2()$variable
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

  
