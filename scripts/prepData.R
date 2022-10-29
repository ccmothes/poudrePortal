# prep datasets for shinyapp

library(dplyr)
library(readr)
library(stringr)

# clean datasets to add availble data and date range column for table

weather_data <- readRDS("app/data/weather_update.RDS") %>% 
  mutate(precip = if_else(!(is.na(Precipitation)), "Precipitation", ""),
         temp = if_else(if_any(c("Average_temp", "Maximum_temp", "Minimum_temp", "Soil_temp"), ~!is.na(.)), "Temperature", ""),
         snow = if_else(if_any(c("Snowfall", "Snow_depth"), ~!is.na(.)), "Snow", ""),
         data_available = paste(precip, temp, snow))


qual_vars <- c("water_temp_C", "Chla", "Turbidity", "Conductivity", "DOC", "DTN", "pH",
               "ANC", "SC", "Na", "NH4", "K", "Mg", "Ca", "F", "Cl",
               "NO3", "PO4", "SO4", "TSS")

#read in updated file
water_data <- readRDS("app/data/water_data_update.RDS") %>% arrange(Date) %>% 
  mutate(Snow_depth = Snow_depth*10) %>% 
  mutate(p = if_else(!(is.na(precip_mm)), "Precipitation", ""),
         temp = if_else(if_any(c("Average_temp", "Soil_temp"), ~!is.na(.)), "Temperature", ""),
         stream = if_else(if_any(c("stage_cm", "discharge_Ls"), ~!is.na(.)), "Streamflow", ""),
         snow = if_else(!(is.na(Snow_depth)), "Snow", ""),
         wq = if_else(if_any(all_of(qual_vars), ~!is.na(.)), "Water Quality", ""),
         data_available = paste(p, temp, stream, snow, wq))



# get available data for each unique site for water and weather
data_avail_water <- water_data %>% group_by(Site) %>% 
  summarise(across(c("p","stream", "temp", "snow", "wq"), ~paste(unique(.), collapse = ""))) %>% 
  mutate(data_available = paste(p, stream, temp, snow, wq)) %>% 
  dplyr::select(Site, data_available)


data_avail_weather <- weather_data %>% group_by(Site) %>% 
  summarise(across(c("precip","snow","temp"), ~paste(unique(.), collapse = ""))) %>% 
  mutate(data_available = paste(precip, snow, temp)) %>% 
  dplyr::select(Site, data_available)

#combine water and weather sites for table, add date range

sites_water <- 
  water_data %>%
  group_by(Site) %>%
  mutate(start_date = min(Date),
         end_date = max(Date)) %>%
  ungroup() %>%
  distinct(Site, .keep_all = TRUE) %>% 
  dplyr::select(Site, source, long, lat, start_date, end_date) %>%
  left_join(data_avail_water, by = "Site")

sites_weather <-
  weather_data %>% 
  group_by(Site) %>%
  mutate(start_date = min(Date),
         end_date = max(Date)) %>%
  ungroup() %>% 
  distinct(Site, .keep_all = TRUE) %>% 
  dplyr::select(Site, source, long, lat, start_date, end_date) %>%
  left_join(data_avail_weather, by = "Site")

sites <- bind_rows(sites_water, sites_weather)

saveRDS(sites, "app/data/sites_table.RDS")

#clean up, remove extra columns from weather and water data
weather_data <- weather_data %>% dplyr::select(-c(precip, snow, temp))

water_data <- water_data %>% dplyr::select(-c(p, temp, stream, snow, wq)) %>% 
  rename(Precipitation = precip_mm)

saveRDS(weather_data, "app/data/weather_app.RDS")
saveRDS(water_data, "app/data/water_app.RDS")

