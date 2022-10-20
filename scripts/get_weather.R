#weather testing

library(tidyverse)
library(raster)
library(leaflet)
library(sf)

#noaa data --------------------------------------------

library(rnoaa)

options(noaakey = "VAOckuKZRAFPIOuippCpMBAUPTiAtVMn")


# data availability
cpc_prcp() #25km res, entire US precip, daily, up to present
ghcnd() # all weather data from a single weather site
ghcnd_search() #same but clean dataset

isd() #get and parse NOAA ISD data, annual

#plot stations
# Get station table
stations <- isd_stations()
## plot stations
### remove incomplete cases, those at 0,0
df <- stations[complete.cases(stations$lat, stations$lon), ]
df <- df[df$lat != 0, ]
### make plot
library("leaflet")
leaflet(data = df) %>%
  addTiles() %>%
  addCircles()
#only stations in fort collins and loveland, ones in mountains are south of RMNP

# ghcnd has more
stations_ghcnd <- ghcnd_stations()
saveRDS(stations_ghcnd, "data/stations_ghcnd.RDS")

nearby_stations <- meteo_distance(station_data = stations_ghcnd, lat = 40.55873,
                                  long = -105.1794, radius = 10)
#174 stations, most in the city

meteo_tidy_ghcnd() #creates a tidy ghcnd dataset from a single monitor


ncdc(datasetid='PRECIP_HLY', locationid='ZIP:28801', datatypeid='HPCP',
     startdate = '2020-05-01', enddate = '2020-05-10')

# test

locations <- locations %>% mutate(id = 1:10) %>% dplyr::select(-Id) %>% 
  rename('longitude' = "POINT_X", 'latitude' = "POINT_Y") %>% 
  st_transform(4326)

# get stations around stream sensor locations
location_stations <- meteo_nearby_stations(
  lat_lon_df = locations,
  lat_colname = "latitude",
  lon_colname = "longitude",
  station_data = stations_ghcnd,
  year_min = 2015,
  year_max = 2021,
  radius = 30
) %>%
  bind_rows %>% 
  distinct(id, .keep_all = TRUE)

saveRDS(location_stations, "data/location_stations.RDS")


leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(data = locations) %>% 
  addCircleMarkers(data = location_stations, radius = ~ sqrt(distance), color = "red",
             stroke = TRUE, fillOpacity = 1) %>% 
  addWMSTiles(
    "http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi",
    layers = "nexrad-n0r-900913-m10m", 
    options = WMSTileOptions(format = "image/png", transparent = TRUE),
    attribution = "Weather data © 2012 IEM Nexrad"
  )

tile_times <-
  c('900913-m50m', '900913-m45m', '900913-m40m', '900913-m35m', '900913-m30m', 
    '900913-m25m', '900913-m20m', '900913-m15m', '900913-m10m', '900913-m05m', 
    '900913')


# WEATHER MAP -----------------------------------------

##get weather data for all nearby stations

weather_data <- meteo_pull_monitors(location_stations$id, date_min = "2015-10-01",
                                    date_max = Sys.Date())
#this takes a while...save as RDATA
saveRDS(weather_data, "data/weather_data.RDS")
#not data for all stations/dates...
weather_data <- readRDS("data/weather_data.RDS")

#tie to coordinates
weather_coords <- weather_data %>% left_join(location_stations, by = "id") %>% 
  dplyr::select(id, date, Precipitation = prcp, Snowfall = snow, Snow_depth = snwd,
         Minimum_temp = tmin, Maximum_temp = tmax, Average_temp = tavg, latitude,
         longitude)

saveRDS(weather_coords, "data/weather_coords.RDS")

#filter to single date
weather_date <- weather_coords %>% filter(date == "2021-07-21")


leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(data = weather_date, radius = ~ sqrt(Precipitation), color = "red",
                   stroke = TRUE, fillOpacity = 1)



#RADAR DATA ------------------------------------------

library(owmr)
Sys.setenv(OWM_API_KEY = "87f0f75ce17f0e1d1f24c49081bd1bbd")

#example
owm_data <- find_city("Malaga", units = "metric") %>%
  owmr_as_tibble()
leaflet() %>% addTiles() %>%
  add_weather(
    owm_data,
    template = "<b>{{name}}</b>, {{temp}}°C",
    icon = owm_data$weather_icon
  )


urlTemplate <-  'https://mesonet.agron.iastate.edu/cache/tile.py/1.0.0/nexrad-n0q-{timestamp}/{zoom}/{x}/{y}.png'


#can't figure out how to get historical radar data



test <- raster("https://mesonet.agron.iastate.edu/request/gis/n0r2gtiff.php?dstr=200711010025")


url <- "https://noaa-nexrad-level2.s3.amazonaws.com/2021/01/01/FOP1/FOP120210101_000224_V06"


leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(data = locations) %>% 
  addCircleMarkers(data = location_stations, radius = ~ sqrt(distance), color = "red",
                   stroke = TRUE, fillOpacity = 1) %>% 
  #addTiles("https://mesonet.agron.iastate.edu/cache/tile.py/1.0.0/hrrr::REFD-F0000-0/6/15/24.png")
  addWMSTiles(
    "https://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0q-t.cgi?",
    layers = "nexrad-n0q-wmst",
    options = WMSTileOptions(format = "image/png", transparent = TRUE,
                             time = "2021-08-29T21:00:00Z")
  )



# SNOTEL station data ---------------------------------------
library(soilDB)
sf::sf_use_s2(FALSE)
source("scripts/getSNOTEL.R")

# use cam peak polygon as boundary/aoi
camPeak_box <- readRDS("data/camPeakSimple.RDS") %>% 
  st_bbox() %>% 
  st_as_sfc() %>% 
  st_as_sf() %>% 
  st_transform(4326)

#test to decide which buffer distance to use
aoi <- camPeak_box %>%
  sf::st_buffer(dist = 0.25)


snotel <- getSNOTEL(camPeak_box, dist = 0.25, years = 2016:2022)

#clean snotel
snotel_clean <- snotel %>% 
  #filter relevant vars
  filter(sensor.id %in% c("STO.I_2", "TAVG.D", "TMIN.D", "TMAX.D", "PREC.I", "SNWD.I")) %>%
  #convert geometry to lat/long
  mutate(long = unlist(map(geometry,1)),
         lat = unlist(map(geometry,2))) %>% 
  st_drop_geometry() %>% 
  pivot_wider(names_from = "sensor.id", values_from = "value") %>% 
  #convert precip and snow to cm from in
  mutate(Precipitation = PREC.I * 2.54,
         Snow_depth = SNWD.I * 2.54) %>% 
  #select and rename columns to match big dataset
  dplyr::select(Site = Name, source = Network, Date, Average_temp = TAVG.D,
                Minimum_temp = TMIN.D, Maximum_temp = TMAX.D, Snow_depth,
                Soil_temp = STO.I_2, Precipitation, long, lat)

## bind to current weather data
weather_data <- readRDS("app/data/weather_update.RDS")

weather_update <- bind_rows(weather_data, snotel_clean)

#update weather file
saveRDS(weather_update, "app/data/weather_update.RDS")
saveRDS(weather_update, "app/data/weather_update.RDS")

