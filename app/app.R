#Set up ------------------------------

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

qual_vars <- c("water_temp_C", "Chla", "Turbidity", "Conductivity", "DOC", "DTN", "pH",
               "ANC", "SC", "Na", "NH4", "K", "Mg", "Ca", "F", "Cl",
               "NO3", "PO4", "SO4", "TSS")

#read in updated files
weather_data <- readRDS("data/weather_app.RDS") %>% 
  #remove snotel precip data for now since its cumulative
  mutate(Precipitation = if_else(source == 'SNOTEL', NA_real_, Precipitation))

water_data <- readRDS("data/water_app.RDS")
sites <- readRDS("data/sites_table.RDS")


#from shinyTime:
dateToTimeList <- function(value){
  if(is.null(value)) return(NULL)
  posixlt_value <- unclass(as.POSIXlt(value))
  time_list <- lapply(posixlt_value[c('hour', 'min', 'sec')], function(x) {
    sprintf("%02d", trunc(x))
  })
  return(time_list)
}


# ui ----------------------------------------------------


ui <- fluidPage (
  class = "container-all",
  navbarPage(
    theme = bslib::bs_theme(
      bootswatch = "flatly",
      version = 4,
      #bg = "#FFFFFF",
      #fg = "#000",
      primary = "#1E4D2B",
      secondary = "#D9782D",
      success = "#f28e35",
      base_font = font_google("Cairo")
    ) %>%
      bslib::bs_add_rules(sass::sass_file("www/style.scss")),
    
    title = HTML("Poudre Portal <em>Beta Version</em>"),
    windowTitle = "Poudre Portal",
    id = "nav",
    
    # Homepage ------------------------------------------------------------
    tabPanel(
      "Home",
      htmlTemplate(
        "www/homepage.html",
        button_data = actionButton("button_data", "Data Explorer"),
        button_weather = actionButton("button_weather", "Weather Explorer"),
        button_usgs = actionButton("button_usgs", "USGS Water Data", onclick="window.open('https://waterdata.usgs.gov/nwis')"),
        button_noaa = actionButton("button_noaa", "NOAA Weather Data", onclick="window.open('https://www.ncei.noaa.gov/cdo-web/')"),
        button_usda = actionButton("button_usda", "USDA SNOTEL data", onclick="window.open('https://www.ncei.noaa.gov/cdo-web/')"),
        button_ross = actionButton("button_ross", "Ross Lab @CSU", onclick="window.open('https://rossyndicate.com/')"),
        button_kampf = actionButton("button_kampf", "Kampf Lab @CSU", onclick="window.open('https://www.nrel.colostate.edu/investigator/stephanie-kampf-homepage/')"),
        button_rhoades = actionButton("button_rhoades", HTML("Chuck Rhoades <br> USFS Biogeochemistry Lab"), 
                                      onclick="window.open('https://www.fs.usda.gov/rmrs/research-labs/fort-collins-biogeochemistry-laboratory')"),
        button_cofc = actionButton("button_cofc", HTML("City of Fort Collins <br> Source Water Monitoring"),
                                   onclick="window.open('https://www.fcgov.com/utilities/what-we-do/water/water-quality/source-water-monitoring/')")
        
        
        
      )
    ),
    # About --------------------------------------------------------------
    tabPanel(
      "About",
      h5("Some about info regarding the project and researchers")
    ),
    
    navbarMenu(
      "Explore",
      # Data Explorer ------------------------------------------------------
      tabPanel("Data Explorer",
               
               fluidPage(fluidRow(
                 column(
                   5,
                   tabsetPanel(
                     tabPanel(
                       "Map",
                       leaflet::leafletOutput("map1", width = '100%' , height = 500)
                     ),
                     tabPanel("Table", div(DT::dataTableOutput("table"), style = "font-size:80%"))
                   ),
                   br(),
                   em("Click on a site to view time series plots to the right"),
                   hr(),
                   actionButton("clear", "Clear Plots"),
                   br(),
                   br(),
                   pickerInput(
                     "sourceChoice",
                     "Filter by Source:",
                     choices = c(
                       "CSU_Kampf",
                       "CSU_Ross",
                       "FoCo",
                       "USFS",
                       "USGS",
                       "NOAA",
                       "SNOTEL"
                     ),
                     selected = c(
                       "CSU_Kampf",
                       "CSU_Ross",
                       "FoCo",
                       "USFS",
                       "USGS",
                       "NOAA",
                       "SNOTEL"
                     ),
                     multiple = TRUE
                   ),
                   checkboxGroupButtons(
                     inputId = "varChoice",
                     label = "Filter by Category:",
                     choices = c(
                       "Precipitation",
                       "Snow",
                       "Streamflow",
                       "Water Quality",
                       "Temperature"
                     ),
                     selected = c(
                       "Precipitation",
                       "Snow",
                       "Streamflow",
                       "Water Quality",
                       "Temperature"
                     ),
                     direction = "horizontal",
                     individual = TRUE,
                     status = "primary",
                     checkIcon = list(yes = icon("square-check"),
                                      no = icon("square"))
                   )
                   
                 ),
                 
                 column(
                   7,
                   fluidRow(
                     sliderInput(
                       "range",
                       "",
                       value = c(as.Date("2020-01-01"), as.Date("2021-10-01")),
                       min = as.Date("2019-01-01"),
                       max = as.Date("2022-02-01"),
                       timezone = "-0600",
                       width = '100%'
                       
                     ),
                     #p(strong("Precipitation")),
                     selectInput(
                       "precipVar",
                       "Precipitation/Snow",
                       choices = c("Precipitation",
                                   "Snow Depth" = "Snow_depth",
                                   "Snowfall")
                     ),
                     plotlyOutput("precip", width = "100%", height = 160),
                     
                     selectInput(
                       "tempVar",
                       "Temperature",
                       choices = c(
                         "Average Temperature" = "Average_temp",
                         "Minimum Temperature" = "Minimum_temp",
                         "Maximum Temperature" = "Maximum_temp",
                         "Soil Temperature" = "Soil_temp"
                       )
                     ),
                     plotlyOutput("temp", width = "100%", height = 190),
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
                       choices = all_of(qual_vars),
                       selected = "Turbidity"
                       
                     ),
                     plotlyOutput("waterQual", width = "100%", height = 190),
                     
                     strong("Note: some data may be missing for certain dates/variables"),
                     br(),
                     br()
                     
                   )
                 )
               ))),
      # Weather Explorer -----------------------------------------------------------
      tabPanel(
        "Weather Explorer",
        
        sidebarLayout(
          position = "right",
          
          mainPanel(leaflet::leafletOutput(
            "map2", width = '100%' , height = 800
          )),
          sidebarPanel(
            switchInput(
              inputId = "radarButton",
              label = "Radar",
              value = FALSE,
              inline = TRUE,
              onStatus = "success",
              offStatus = "danger"
            ),
            sliderInput(
              "date",
              label = "Observation Date:",
              value = Sys.Date() - 1,
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
                "Average Temperature" = "Average_temp",
                "Soil Temperature" = "Soil_temp"
              )
            ),
            em("Click on a station to view raw values. Data last updated 1/25/22"),
            br(),
            br(),
            pickerInput(
              "studySites",
              "Study Sites:",
              choices = c("CSU_Kampf", "CSU_Ross", "FoCo", "USFS", "USGS"),
              selected = c("CSU_Kampf", "CSU_Ross", "FoCo", "USFS", "USGS"),
              multiple = TRUE
            ),
            hr(),
            br(),
            p(
              class = "p-sent",
              "Link to Sentinel Explorer",
              a(href = "https://ccmothes.users.earthengine.app/view/poudreportal-gee", "here"),
              br(),
              em("(Note: this application is slow and still under development)")
            )
          )
          
        )
      )
    ),
    # Fire Stories ---------------------------------------------------------------
    tabPanel(
      "Fire Stories",
      h5("Stories, articles, resources etc. related to Colorado wildfires")
    ),
    # Contact --------------------------------------------------------------------
    tabPanel("Contact",
             h5("Contact info here"))
  ),
  tags$footer(includeHTML("www/footer.html"))
)





server <-  function(input, output, session){
  
  #update tabPanels from homepage buttons
  observeEvent(input$button_data, {
    
    updateTabsetPanel(session, "nav",
                      selected = "Data Explorer")

  })
  
  observeEvent(input$button_weather, {
    
    updateTabsetPanel(session, "nav",
                      selected = "Weather Explorer")
    
  })
  
  weather1 <- reactive({
    
    if(is.null(input$varChoice))
      return(weather_data %>% filter(is.na(data_available)))
    
    weather_data %>% 
      #rename(variable = input$weatherVar) %>% 
      filter(source %in% input$sourceChoice) %>% 
      filter(Date == input$range & str_detect(data_available, paste(input$varChoice, collapse = "|"))) 

  })
  
  weather2 <- reactive({
    weather_data %>% filter(Date == input$date) %>%
      dplyr::select(Date, Site, source, lat, long, variable = input$variable) %>%
      filter(!is.na(variable))
  })
  
  time <- reactive({
    paste(c(dateToTimeList(input$time)$hour, dateToTimeList(input$time)$min, 
            dateToTimeList(input$time)$sec), collapse = ':')
  })
  
  
  water_data_filtered <- reactive({
    
    if(is.null(input$varChoice))
      return(water_data %>% filter(is.na(data_available)))
    
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
  pal_weather <- colorFactor(palette = c("black", "grey70"), weather_data$source)
  
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
    addLegend("topright", data = weather_data, values = ~source, 
             pal = pal_weather, labels = c("NOAA", "SNOTEL"),
              group = "Weather Stations", title = "Weather Stations") %>% 
    
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
        fillColor = ~pal_weather(source),
        #fillColor = "gray50",
        stroke = TRUE,
        weight = 1,
        fillOpacity = 0.85,
        popup = paste("Station:", weather1()$Site,
                      "<br>",
                      "Source:", weather1()$source
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
                              'Source:', weather2()$source, "<br>",
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
    
    combined(
      bind_rows(combined(),
                #filtered_df() %>%
                df() %>%
                  #df() %>% filter(key %in% filtered_df()$key) %>%
                  filter(Site %in% input$map1_marker_click)) 
      
    )
    
    
    #df(df() %>% filter(!key %in% filtered_df()$key))
    
  })
  
  observeEvent(input$table_rows_selected, {
    
    tableSelected <- sites[input$table_rows_selected,]
    
    combined(bind_rows(combined(),
                       filtered_df() %>% 
                         filter(Site %in% tableSelected$Site)))
  })
  
  final_df <- reactive({
    combined() %>% rename(
      streamflow = input$streamVar,
      quality = input$qual,
      precip = input$precipVar,
      temp = input$tempVar
    ) %>%
      filter(as.Date(Date) >= input$range[1] &
               as.Date(Date) <= input$range[2]) %>%
      
      arrange(Date)
    
  })
  
  
  
  output$precip <- renderPlotly({
    
    
    if(nrow(combined()) == 0)
      return(NULL)
    
    if(input$precipVar == "Precipitation"){
      
      plotly::plot_ly() %>%
        add_bars(x = final_df()$Date, y = final_df()$precip, name = ~ final_df()$Site,
                 color = ~ final_df()$Site) %>%
        plotly::layout(yaxis = list(title = "P (mm)", autorange = "reversed"),
                       xaxis = list(range = c(input$range[1], input$range[2]),
                                    showgrid = TRUE),
                       showlegend = TRUE,
                       legend = list(orientation = "h", x = 0.01, y = 1.4))
    }else {
      
      plot_ly(final_df()) %>%
        add_trace(x = final_df()$Date,
                  y = final_df()$precip,
                  name = ~ final_df()$Site,
                  linetype = ~ final_df()$Site,
                  mode = "lines+markers") %>%
        plotly::layout(yaxis = list(title = paste(input$precipVar, "mm"), range = list(0, max(final_df()$precip))),
                       xaxis = list(range = c(input$range[1], input$range[2]),
                                    showgrid = T),
                       showlegend = TRUE,
                       legend = list(orientation = "h", x = 0.01, y = 1.2))
      
      
    }
    
    
    
    # plotly::plot_ly() %>% 
    #   add_bars(x = combined()$Date, y = combined()$precip_mm, name = ~combined()$Site,
    #            color = ~ combined()$Site) %>% 
    #   plotly::layout(yaxis = list(title = "P (mm)", autorange = "reversed"),
    #                  xaxis = list(range = c(input$range[1], input$range[2]),
    #                               showgrid = TRUE),
    #                  legend = list(orientation = "h", x = 0.01, y = 1.4))
    
  })
  
  output$temp <- renderPlotly({
    
    if(nrow(combined()) == 0)
      return(NULL)
    # 
    # if(!(input$weatherVar %in% names(combined())))
    #   return(NULL)
    
    
    # if(input$weatherVar == "Precipitation"){
    #   
    #   plotly::plot_ly() %>%
    #     add_bars(x = final_df()$Date, y = final_df()$weather, name = ~ final_df()$Site,
    #              color = ~ final_df()$Site) %>%
    #     plotly::layout(yaxis = list(title = "P (mm)", autorange = "reversed"),
    #                    xaxis = list(range = c(input$range[1], input$range[2]),
    #                                 showgrid = TRUE),
    #                    legend = list(orientation = "h", x = 0.01, y = 1.4))
    # }else {
    #   
      plot_ly(final_df()) %>%
        add_trace(x = final_df()$Date,
                  y = final_df()$temp,
                  name = ~ final_df()$Site,
                  linetype = ~ final_df()$Site,
                  mode = "lines+markers") %>%
        plotly::layout(yaxis = list(title = paste(input$tempVar, "C"), range = list(0, max(final_df()$temp))),
                       xaxis = list(range = c(input$range[1], input$range[2]),
                                    showgrid = T),
                       showlegend = TRUE,
                       legend = list(orientation = "h", x = 0.01, y = 1.2))
      
      
    #}
    
    
    
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
                     showlegend = TRUE,
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
                linetype = ~ final_df()$Site,
                connectgaps = TRUE) %>%
      plotly::layout(yaxis = list(title = input$qual),
                     xaxis = list(range = c(input$range[1], input$range[2]),
                                  showgrid = T),
                     showlegend = TRUE,
                     legend = list(orientation = "h", 
                                   x = 0.01, y = 1.2))
    
    
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
                  labels = "Weather Stations") %>% 
        
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

  
