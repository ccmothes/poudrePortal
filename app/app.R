#time loop test

library(shiny)
library(bslib)
library(dplyr)
library(leaflet)
library(plotly)
library(readr)
library(sf)
library(shinyWidgets)
library(stringr)

camPeak_simple <- readRDS("data/camPeakSimple.RDS")


weather_data <- readRDS("data/weather_update.RDS") %>% 
  rename(Site = id, Date = date, long = longitude, lat = latitude) %>% 
  mutate(source = "NOAA") %>% 
  mutate(p = if_else(!(is.na(Precipitation)), "Precipitation", "")) %>% 
  mutate(s = if_else(!(is.na(Snowfall)), "Snow", "")) %>% 
  mutate(sd = if_else(!(is.na(Snow_depth)), "Snow", "")) %>% 
  mutate(mt = if_else(!(is.na(Minimum_temp)), "Temperature", "")) %>% 
  mutate(mt2 = if_else(!(is.na(Maximum_temp)), "Temperature", "")) %>%
  mutate(at = if_else(!(is.na(Average_temp)), "Temperature", "")) %>% 
  mutate(data_available = paste(p,s,sd,mt,mt2,at))
  
  
qual_vars <- c("water_temp_C", "Chla", "Turbidity", "DOC", "DTN", "pH",
               "ANC", "SC", "Na", "NH4", "K", "Mg", "Ca", "F", "Cl",
               "NO3", "PO4", "SO4")

#read in updated file
water_data <- readRDS("data/water_data_update.RDS") %>% arrange(Date) %>% 
  #mutate(source_spec = if_else(source == "CSU_Kampf", "CSU-Stephanie Kampf", source)) %>%
  #mutate(source = if_else(source == "CSU_Kampf", "CSU", source)) %>% 
  # mutate(p = if_else(!(is.na(precip_mm)), "Precipitation", "")) %>% 
  # mutate(s = if_else(!(is.na(stage_cm)), "Streamflow", "")) %>% 
  # mutate(d = if_else(!(is.na(discharge_Ls)), "Streamflow", "")) %>% 
  # mutate(t = if_else(!(is.na(Turbidity)), "Water Quality", "")) %>% 
  # mutate(p2 = if_else(!(is.na(pH)), "Water Quality", "")) %>% 
  # mutate(d2 = if_else(!(is.na(DO)), "Water Quality", "")) %>% 
  # mutate(c = if_else(!(is.na(Conductivity)), "Water Quality", "")) %>% 
  # mutate(category = paste(p,s,d,t,p2,d2,c)) 
  mutate(p = if_else(!(is.na(precip_mm)), "Precipitation", ""),
         temp = if_else(if_any(c("Average_temp", "Soil_temp"), ~!is.na(.)), "Temperature", ""),
         stream = if_else(if_any(c("stage_cm", "discharge_Ls"), ~!is.na(.)), "Streamflow", ""),
         snow = if_else(!(is.na(Snow_depth)), "Snow", ""),
         wq = if_else(if_any(qual_vars, ~!is.na(.)), "Water Quality", ""),
         data_available = paste(p, temp, stream, snow, wq))
  
  
  
# get available data for each site
data_avail <- water_data %>% group_by(Site) %>% 
  summarise(across(c("p","stream", "temp", "snow", "wq"), ~paste(unique(.), collapse = ""))) %>% 
  mutate(data_available = paste(p, stream, temp, snow, wq)) %>% 
  dplyr::select(Site, data_available)


sites <- water_data %>% distinct(Site, .keep_all = TRUE) %>% dplyr::select(Site, source, long, lat) %>% 
  left_join(data_avail, by = "Site")

#clean up, remove extra columns from weather and water data
weather_data <- weather_data %>% dplyr::select(-c(p,s,sd,mt,mt2,at))

water_data <- water_data %>% dplyr::select(-c(p, temp, stream, snow, wq))


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
  ) %>% 
    bslib::bs_add_rules(sass::sass_file("www/style.scss")),

  
  
  "Poudre Portal Demo",
  id = "nav",
  
  tabPanel("Data Explorer",

           fluidPage(
             fluidRow(
               column(5,
                      tabsetPanel(
                        tabPanel(
                          "Map",
                          leaflet::leafletOutput("map1", width = '100%' , height = 500)
                      ),
                      tabPanel("Table", div(DT::dataTableOutput("table"), style = "font-size:80%")
                      )),
               br(),
               em("Click on a site to view time series plots to the right"),
               hr(),
               actionButton("clear", "Clear Plots"),
               br(),
               br(),
               pickerInput("sourceChoice", "Filter by Source:",
                          choices = c("CSU_Kampf", "CSU_Ross", "FoCo", "USFS", "USGS"),
                          selected = c("CSU_Kampf", "CSU_Ross", "FoCo", "USFS", "USGS"),
                          multiple = TRUE),
               checkboxGroupButtons(
                 inputId = "varChoice",
                 label = "Filter by Category:",
                 choices = c("Precipitation", "Snow", "Streamflow", "Water Quality", "Temperature"),
                 selected = c("Precipitation", "Snow", "Streamflow", "Water Quality", "Temperature"),
                 direction = "horizontal",
                 individual = TRUE,
                 status = "primary",
                 checkIcon = list(
                   yes = icon("check-square"),
                   no = icon("square-o")
                 ))

               ),

                column(7,
                       fluidRow(
                        sliderInput(
                          "range",
                          "",
                          value = c(as.Date("2020-01-01"), as.Date("2021-10-01")),
                          min = as.Date("2015-10-01"),
                          max = as.Date("2021-10-01"),
                          timezone = "-0600",
                          width = '100%'
                          
                        ),
                        p(strong("Precipitation")),
                        
                        plotlyOutput("precip", width = "100%", height = 160),
                       
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
                          ),
                          selected = "Snowfall"
                        ),
                        plotlyOutput("noaa", width = "100%", height = 190),
                        selectInput(
                          "streamVar",
                          "Streamflow",
                          choices = c("Discharge" = "discharge_Ls",
                                      "Stage" = "stage_cm")
                        ),
                        
                        plotlyOutput("q", width = "100%", height = 190),
                        selectInput(
                          "qual",
                          "Water Quality",
                          choices = all_of(qual_vars)
                          
                        ),
                        plotlyOutput("waterQual", width = "100%", height = 190),

                        strong("Note: some data may be missing for certain dates/variables")

                      )
                )
             ))),
  
  tabPanel(
    "Weather Explorer",
    
    sidebarLayout(
      position = "right",
      
      mainPanel(leaflet::leafletOutput(
        "map2", width = '100%' , height = 800
      )),
      sidebarPanel(
        switchInput(inputId = "radarButton", label = "Radar", value = FALSE, inline = TRUE,
                    onStatus = "success", offStatus = "danger"),
        sliderInput(
          "date",
          label = "Observation Date:",
          value = Sys.Date() - 2,
          min = as.Date("2015-10-01"),
          max = Sys.Date(),
          dragRange = FALSE,
          timezone = "-0600",
          width = '100%'
          
        ),
        
        sliderInput(
          "time",
          "Time (UTC):",
          #value = strptime(format(Sys.time(), "%H:%M", tz = "GMT"), "%H:%M", tz = "GMT"),
          value = strptime("12:00", "%H:%M", tz = "GMT"),
          min = strptime("00:00", "%H:%M", tz = "GMT"),
          max = strptime("23:59", "%H:%M", tz = "GMT"),
          timeFormat = "%H:%M",
          timezone = "GMT",
          width = '100%',
          step = 900, 
          animate = animationOptions(interval = 3000, loop = TRUE)
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
        em("Click on a station to view raw values. Data last updated 1/25/22"),
        br(),
        br(),
        pickerInput("studySites", "Study Sites:",
                    choices = c("CSU_Kampf", "CSU_Ross", "FoCo", "USFS", "USGS"),
                    selected = c("CSU_Kampf", "CSU_Ross", "FoCo", "USFS", "USGS"),
                    multiple = TRUE),
        hr(),
        br(),
        p("Link to Sentinel Explorer", a(href="", "here"), em("(not active yet)"))
      )
      
    )
  )
)


server <-  function(input, output, session){
  
  weather1 <- reactive({
    
    if(is.null(input$varChoice))
      return(weather_data %>% filter(is.na(data_available)))
    
    weather_data %>% 
      rename(variable = input$weatherVar) %>% 
      filter(Date == input$range & str_detect(data_available, paste(input$varChoice, collapse = "|"))) 

  })
  
  weather2 <- reactive({
    weather_data %>% filter(Date == input$date) %>%
      dplyr::select(Date, Site, lat, long, variable = input$variable) %>%
      filter(!is.na(variable))
  })
  
  time <- reactive({
    paste(c(dateToTimeList(input$time)$hour, dateToTimeList(input$time)$min, 
            dateToTimeList(input$time)$sec), collapse = ':')
  })
  
  
  water_data_filtered <- reactive({
    
    if(is.null(input$varChoice))
      return(water_data %>% filter(is.na(data_avail)))
    
    water_data %>% filter(source %in% input$sourceChoice) %>% 
      filter(str_detect(data_available, paste(input$varChoice, collapse = "|")))
  })
  
  water_data_filtered2 <- reactive({
    
    
    water_data %>% filter(source %in% input$studySites)
    
  })
  
  
  
  Precipitation <- c("Precipitation", "precip_mm")
  Snow <- c("Snow", "Snowfall", "Snow_depth")
  Streamflow <- c("Streamflow", "stage_cm", "discharge_Ls")
  WaterQuality <- c("Water Quality", qual_vars)
  Temperature <- c("Temperature", "Minimum_temp", "Maximum_temp", "Average_temp", "Soil_temp")
  

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
    addLegend("topright", data = weather_data, colors = "black", group = "Weather Stations", labels = "NOAA Weather Stations") %>% 
    
      addLegend("topright", data = water_data, values = ~source, 
                pal = pal, title = "Data source") %>% 
      
      addScaleBar(position = "bottomright") %>%
      
      addLayersControl(
        baseGroups = c("USGS Topo", "Open Street Map", "Satellite"),
        overlayGroups = c(
          "Weather Stations", "Cameron Peak Fire"
          
        ),
        position = "topleft",
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      hideGroup(c("Cameron Peak Fire"))
    
  })
  

  output$table <- DT::renderDataTable(sites, rownames = FALSE,
                                      options = list(autoWidth = TRUE, scrollX = TRUE,
                                      scrollY = "200px", scrollCollapse = TRUE,
                                      paging = FALSE, float = "left"),
                                      width = "80%", height = "70%")
  
  
  tableProxy <- DT::dataTableProxy("table")
    


  observe({

    input$nav

    tab1 <- leafletProxy("map1") %>%
      #removeMarker("Weather Stations") %>%
      clearMarkers() %>% 
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
        options = pathOptions(pane = "weather")) %>%
      addCircleMarkers(
              data = water_data_filtered(),
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
                water_data_filtered()$source,
                "<br>",
                "Site:",
                water_data_filtered()$Site
              ),
              #group = "water",

              options = pathOptions(pane = "water")
            )

      tab2 <- leafletProxy("map2") %>%
        clearMarkers() %>%
        addCircleMarkers(
                data = weather2(),
                layerId = ~Site,
                lng = ~ long,
                lat = ~ lat,
                radius = if(input$variable %in% c("Snowfall",
                                                  "Snow_depth")) {~sqrt(variable)} else {~variable},
                color = "black",
                weight = 5,
                stroke = TRUE,
                fillOpacity = 1,
                fillColor = "black",
                popup = paste("Station:", weather2()$Site, "<br>",
                              paste0(input$variable, ":"), weather2()$variable,
                              if(input$variable %in% c("Precipitation", "Snowfall",
                                                       "Snow_depth")) {"mm"} else {"degrees Celcius"}
                ),
                group = "Weather Stations",
                options = pathOptions(pane = "weather")

              ) %>% 
        
        addCircleMarkers(
          data = water_data_filtered2(),
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
            water_data_filtered2()$source,
            "<br>",
            "Site:",
            water_data_filtered2()$Site
          ),
          group = "Study Sites",
          
          options = pathOptions(pane = "sites")
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
                         filter(Site %in% input$map1_marker_click))) 
    
    
    #df(df() %>% filter(!key %in% filtered_df()$key))
    
  })
  
  observeEvent(input$table_rows_selected, {
    
    tableSelected <- sites[input$table_rows_selected,]
    
    combined(bind_rows(combined(),
                       filtered_df() %>% 
                         filter(Site %in% tableSelected$Site)))
  })
  
  final_df <- reactive({
    combined() %>% rename(streamflow = input$streamVar, quality = input$qual,
                          weather = input$weatherVar) %>% 
      arrange(Date)
    
  })
  
  
  
  output$precip <- renderPlotly({
    
    
    if(nrow(combined()) == 0)
      return(NULL)
    
    
    plotly::plot_ly() %>% 
      add_bars(x = combined()$Date, y = combined()$precip_mm, name = ~combined()$Site,
               color = ~ combined()$Site) %>% 
      plotly::layout(yaxis = list(title = "P (mm)", autorange = "reversed"),
                     xaxis = list(range = c(input$range[1], input$range[2]),
                                  showgrid = TRUE),
                     legend = list(orientation = "h", x = 0.01, y = 1.4))
    
  })
  
  output$q <- renderPlotly({
    
    
    if(nrow(combined()) == 0)
      return(NULL)
    

    plot_ly(final_df()) %>%
      add_trace(x = final_df()$Date,
                y = final_df()$streamflow,
                name = ~final_df()$Site,
                linetype = ~ final_df()$Site,
                mode = "lines+markers") %>%
      plotly::layout(yaxis = list(title = input$streamVar),
                     xaxis = list(range = c(input$range[1], input$range[2]),
                                  showgrid = T),
                     legend = list(orientation = "h", x = 0.01, y = 1.2))

    
  })
  
  
  
  
  output$waterQual <- renderPlotly({
    
    if(nrow(combined()) == 0)
      return(NULL)
    
    if(!(input$qual %in% names(combined())))
      return(NULL)
    
    plotly::plot_ly(final_df()) %>%
      add_trace(x = final_df()$Date,
                y = final_df()$quality,
                name = ~final_df()$Site,
                mode = 'lines+markers',
                linetype = ~ final_df()$Site) %>%
      plotly::layout(yaxis = list(title = input$qual),
                     xaxis = list(range = c(input$range[1], input$range[2]),
                                  showgrid = T),
                     legend = list(orientation = "h", x = 0.01, y = 1.2))
    
    
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
                                    showgrid = TRUE),
                       legend = list(orientation = "h", x = 0.01, y = 1.4))
    }else {
      
      plot_ly(final_df()) %>%
        add_trace(x = final_df()$Date,
                  y = final_df()$weather,
                  name = ~ final_df()$Site,
                  mode = "lines+markers") %>%
        plotly::layout(yaxis = list(title = input$weatherVar, range = list(0, max(final_df()$weather))),
                       xaxis = list(range = c(input$range[1], input$range[2]),
                                    showgrid = T),
                       legend = list(orientation = "h", x = 0.01, y = 1.2))
      
      
    }
    
    
    
  })
  
  
  observeEvent(input$clear, {
    combined(data.frame())
    
    tableProxy %>% DT::selectRows(NULL)
    
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
        addMapPane("sites", zIndex = 430) %>%
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
        addLegend("topright", data = weather_data, colors = "black", 
                  labels = "NOAA Weather Stations") %>% 
        
        addLegend(pal = colorNumeric(palette = c("#646464", "#04e9e7", "#019ff4", "#0300f4",
                                                 "#02fd02", "#01c501", "#008e00", "#fdf802",
                                                 "#e5bc00", "#fd9500", "#fd0000", "#d40000",
                                                 "#bc0000", "#f800fd", "#9854c6", "#fdfdfd"),
                                     domain = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75)),
                  values = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75),
                  title = "Radar Base Reflectivity (dBZ)",
                  position = "bottomright", group = "Radar Legend") %>% 
        addLegend("topright", data = water_data, values = ~source, 
                  pal = pal, title = "Data source") %>% 

        addLayersControl(
          baseGroups = c("USGS Topo", "Open Street Map", "Satellite"),
          overlayGroups = c("Cameron Peak Fire", "Weather Stations", "Study Sites", "Radar Legend"),
          position = "topright",
          options = layersControlOptions(collapsed = TRUE)
        ) %>%
        hideGroup(c("Cameron Peak Fire", "Radar Legend"))



    })


  observe({
    
    if(input$radarButton == TRUE)
      leafletProxy("map2") %>%
      showGroup("Radar") %>% 
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
    
    if(input$radarButton == FALSE)
      leafletProxy("map2") %>% 
      removeControl("Radar") %>% 
      removeTiles(layerId = "B")
  })
  


}


shinyApp(ui,server)

  
