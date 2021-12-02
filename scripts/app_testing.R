# app testing
library(tidyverse)
library(leaflet.extras)
library(leaflet)

water_data <- readRDS("data/water_data.RDS")
weather_data <- readRDS("portal_demo/data/weather_coords.RDS") %>% 
  rename(Site = id, Date = date, long = longitude, lat = latitude) %>% 
  mutate(source = "NOAA")

ui <- 
  
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
        p(strong("Precipitation")),
        
        plotlyOutput("precip", width = "100%", height = 120),
        selectInput(
          "streamVar",
          "Streamflow",
          choices = c(
            "Stage" = "stage_cm",
            "Discharge" = "discharge_Ls"
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
      
      
    )
  )
  
  
  
server <-  function(input, output, session){
    
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
      # addPolygons(
      #   data = camPeak_simple,
      #   color = NA,
      #   weight = 1,
      #   smoothFactor = 0.5,
      #   opacity = 1.0,
      #   fillOpacity = 0.9,
      #   fillColor = ~ colorFactor("Reds", Severity)(Severity),
      #   group = "Cameron Peak Fire",
      #   options = pathOptions(pane = "fire")
      # ) %>%
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
        
        #group = "Water Quality Sensors",
        options = pathOptions(pane = "water")
      ) %>%
      addCircleMarkers(
        data = weather_data,
        layerId = ~Site,
        lng = ~ long,
        lat = ~ lat,
        radius = 4,
        color = "black",
        fillColor = "gray50",
        stroke = TRUE,
        weight = 1,
        fillOpacity = 0.6,
        popup = paste("Station:", weather_data$Site
        ),
        group = "Weather Stations",
        options = pathOptions(pane = "weather")) %>% 
      #addLegend("bottomright", data = daily_data, group = "watersheds", colors = "blue", labels = "Watersheds") %>% 
      #addLegend("bottomright", data = water_qual, group = "Water Quality Censors", colors = "yellow", labels = "Water Quality Sensors") %>% 
      #addLegend("bottomright", values = camPeak_simple, group = "Cameron Peak Fire", pal = ~ colorFactor("Reds", Severity)(Severity)) %>% 
      addLegend("topright", data = water_data, values = ~source, 
                 pal = pal, title = "Data source") %>% 
      addLegend("topright", data = weather_data, colors = "black", group = "Weather Stations", labels = "NOAA Weather Stations") %>% 
      
      addScaleBar(position = "bottomright") %>%
      
      addLayersControl(
        baseGroups = c("USGS Topo", "Open Street Map", "Satellite"),
        overlayGroups = c(
          "Sensors",
          "Weather Stations"

        ),
        position = "topleft",
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      hideGroup("Weather Stations")
    
    
    
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
  
  
  # sites <- reactive({
  #   if(nrow(df()) == 0)
  #     return(data.frame())
  #   
  #   df() %>% 
  #    filter(as.Date(Date) >= input$range[1] & as.Date(Date) <= input$range[2]) %>% 
  #              dplyr::rename(q = input$streamVar, qual = input$qual)
  # })
  
 
    
  
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
  
    
}




shinyApp(ui,server)
