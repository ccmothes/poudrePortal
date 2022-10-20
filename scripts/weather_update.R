# base code for updating weather data
library(dplyr)
library(rnoaa)

options(noaakey = "VAOckuKZRAFPIOuippCpMBAUPTiAtVMn")


stations <- readRDS("data/location_stations.RDS")

weather_data <- readRDS("data/weather_update.RDS")

#delete cached data
ghcnd_cache$delete_all()


#still searches all station data...may not need to do this? just re-do entire search?
#takes ~5 mins

weather_data_new <- meteo_pull_monitors(stations$id, date_min = max(weather_data$date))


weather_update <- bind_rows(weather_data, weather_data_new) %>% left_join(stations, by = "id") %>%
  mutate(Precipitation = prcp*0.1,
         Minimum_temp = tmin*0.1,
         Maximum_temp = tmax*0.1, 
         Average_temp = tavg*0.1,
         source = "NOAA") %>% 
  dplyr::select(Site = id, Date = date, Precipitation, Snowfall = snow, Snow_depth = snwd,
                Minimum_temp, Maximum_temp, Average_temp, lat = latitude,
                long = longitude)



saveRDS(weather_update, "data/weather_update.RDS")
saveRDS(weather_update, "app/data/weather_update.RDS")
