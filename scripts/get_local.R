# load and manage data from local file

library(readr)
library(purrr)
library(dplyr)
library(sf)
library(lubridate)

# CSU - Stephanie Kampf's data ----------------------------------------------

#read in all daily excel files

#updated data
sites <- c("andrews", "Bighorn", "Dry", "Michigan", "Mill")
names(sites) <- c("Andrews Creek", "Bighorn Creek", "Dry Creek", "Michigan River", "Mill Creek")

files <- vector("list", length = length(sites))


for (i in 1:length(sites)){
  
  files[[i]] <- purrr::map(list.files(
    path = "data/stephanie",
    pattern = sites[i],
    full.names = TRUE
  ), read_csv) %>% 
    purrr::map(function(x) mutate(x, Date = as.POSIXct(Date, format = "%m/%d/%Y"))) %>% 
    set_names(rep(names(sites[i]), length(.))) %>% 
    bind_rows(.id = "Site") %>% 
    #collapse rows (some dup dates with diff times)
    #first remove time from date column
    mutate(Date = as.Date(Date)) %>% 
    group_by(Site, Date) %>% 
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
  
}


daily_data <- bind_rows(files)

#old data
# daily_data <-
#   purrr::map(list.files(
#     path = "data/",
#     pattern = "daily",
#     full.names = TRUE
#   ),
#   read_csv) %>% set_names(c("Andrews Creek", "Bighorn Creek", "Dry Creek", "Michigan River", "Mill Creek")) %>% 
#   bind_rows(.id = "Site") %>% 
#   mutate(Date = as.POSIXct(Date, format = "%m/%d/%Y"))


# read in locations and watersheds

locations <- read_sf("data/locations.shp")
watersheds <- read_sf("data/watersheds.shp")


#convert watersheds and points to ee_object for sentinel map

# watershed_ee <- sf_as_ee(watersheds)
# 
# locations_ee <- sf_as_ee(locations)


#tie daily data to watersheds
## Remove this, using point locations instead
# daily_data <- left_join(watersheds, daily_data, by = "Site") %>% 
#   st_transform(4326)

saveRDS(daily_data, "data/daily_data.RDS")

#UPDATE tie data to sensor coordinates (kicked out soil temp sensor info)

daily_data <- readRDS("data/daily_data.RDS")

locations_stream <- locations %>% filter(type == "stream gage") %>% 
  rename(Site = name, lat = POINT_Y, long = POINT_X) %>% as_tibble()

daily_data_refined <- daily_data %>% 
  left_join(locations_stream, by = 'Site') %>% 
  mutate(source = "CSU_Kampf") %>% 
  as_tibble() %>% 
  #NOTE calling Ta_C average temp just to match NOAA, but is just temp from single reading
  # I think... ASK STEPHANIE
  select(Site, source, Date, long, lat, Snow_depth = Snow_depth_cm, precip_mm = P_mm, stage_cm = Stage_cm,
         discharge_Ls = Discharge_Ls, Average_temp = Ta_C, Soil_temp = Ts_C) 

  


# CSU - Matt Ross Reservoir Data -----------------------------------------------

res_data <- read_csv("data/final_reservoir_data_for_caitlin.csv") %>% 
  # remove long draw road washout (no data)
  filter(Stream != "Long Draw Road Washout") %>% 
  #rename to lat/long
  rename(long = POINT_X, lat = POINT_Y) %>% 
  #fix coords for two sites
  mutate(long = replace(long, Site_Code == "BMR", -105.8363862),
         long = replace(long, Site_Code == "CHR", -105.8427182),
         lat = replace(lat, Site_Code == "BMR", 40.60059258),
         lat = replace(lat, Site_Code == "CHR", 40.60232576)) %>% 
  #remove DUPLICATE and blanks
  filter(SAMPLE_TTYPE == "NORM") %>% 
  #remove unnessecary columns and rename Stream to match
  dplyr::select(-c(ID, Number, BOTTLE_NOTES, LAB_NOTES, SAMPLE_TTYPE, Sample_Name,
                   Site_Code, Time)) %>% 
  rename(Site = Stream, water_temp_C = Temp) %>% 
  mutate(source = "CSU_Ross",
         Date = as.Date(as.POSIXct(Date, format = "%m/%d/%Y")))
  

#test for dup dates...lump coords for Chambers Outflow, CHR -> CHD
# # and for barnes meadow outflow BMR -> BMD
# 
# res_data %>% mutate(long = replace(long, Site_Code == "BMR", -105.8363862),
#                     long = replace(long, Site_Code == "CHR", -105.8427182),
#                     lat = replace(lat, Site_Code == "BMR", 40.60059258),
#                     lat = replace(lat, Site_Code == "CHR", 40.60232576)) %>% 
#   #see if there are duplicate dates
#   filter(Stream %in% c("Chambers Lake Outflow", "Barnes Meadow Reservoir Outflow")) %>% 
#   distinct(Stream, Date) %>% View()




# USFS - Chuck Roads water quality data -----------------------------------------

cp_coords <- read_csv("data/CamPeak_Coordinates.csv") %>% rename(Site =  SITE,
                                                                 long = "X_WGS84",
                                                                 lat = "Y_WGS84")


#test how many sites have dup dates
readxl::read_excel("data/CamPk_toMothes_trial.xlsx") %>% 
  group_by(Site, Date) %>% filter(n()>1) %>% View()

waterQual <- readxl::read_excel("data/CamPk_toMothes_trial.xlsx") %>% 
  left_join(cp_coords, by = "Site") %>% 
  # Don't average, these are quality control dups that were left in the dataset
  # group_by(Site, Date, SiteLabel, SiteType, Trt_CP) %>% 
  # summarise(across(where(is.numeric),
  #                  ~ mean(.x, na.rm = TRUE))) %>%
  #ungroup()

  #remove second dup instead, but NOTE MENTION THIS TO CHUCK/TIM
  mutate(Date = as.Date(Date)) %>% 
  distinct(Site, Date, .keep_all = TRUE) %>% 
  mutate(source = "USFS") %>% 
  dplyr::select(Site, Date, source, Turbidity:lat)

#saveRDS(waterQual, "data/water_qual.RDS")

#UPDATE add in all variables

# waterQual <- readRDS("data/water_qual.RDS") %>% mutate(source = "USFS") %>% 
#   dplyr::select(Site, Date, source, Turbidity:lat)



#this was a mistake
# waterQual_refined <- waterQual %>% 
#   mutate(stage_cm = Stage_PBR_ft * 30.48, source = "USFS") %>% 
#   dplyr::select(Site, source, Date, stage_cm, Turbidity, DO = DOC, pH, long, lat)
# 


# City FoCo - Jared Heath water data -------------------------------------

sites2 <- c("Buckhorn_Canyon", "Lions_Park", "Manners_Bridge",
            "Indian_Meadows", "Poudre_Park", "Poudre_River_Mouth", "Rustic")

vars <- c("stage_ft","discharge_cfs", "turbidity", "pH", "DO", "conductivity")

files2 <- vector("list", length = length(sites2))

for (i in 1:length(sites2)){
  
  files2[[i]] <- purrr::map(list.files(
    path = "data",
    pattern = sites2[i],
    full.names = TRUE
  ), read_csv) %>% 
    purrr::map(function(x) mutate(x, 
                                  Date = format(as.POSIXct(Reading, format = "%Y/%m/%d %H:%M:%S"), 
                                                format = "%m/%d/%Y"))) %>% 
    set_names(rep(names(sites2[i]), length(.))) %>% 
    bind_rows(.id = "Site") %>% 
    dplyr::select(Site, Date, Value, Unit) %>% 
    filter(Unit %in% c("in", "ft", "cfs", "uS/cm", "pH", "ntu", "mg/L")) %>% 
    mutate(row = row_number(),
           variable = case_when(Unit == "ft" ~ "stage_ft",
                                Unit == "in" ~ "precip_in",
                                Unit == "cfs" ~ "discharge_cfs",
                                Unit == "ntu" ~ "turbidity",
                                Unit == "pH" ~ "pH",
                                Unit == "mg/L" ~ "DO",
                                Unit == "uS/cm" ~ "conductivity")) %>% 
    tidyr::pivot_wider(names_from = variable, values_from = Value) %>%
    group_by(Date) %>% 
    summarise(across(contains("precip"), ~ sum(.x, na.rm = TRUE)),
              across(contains(vars), ~ mean(.x, na.rm = TRUE))) %>% 
    mutate(Site = sites2[i], 
           Date = as.Date(as.POSIXct(Date, format = "%m/%d/%Y")))
  
  
}

foco_data <- bind_rows(files2)


#combine with coordinates

foco_coords <- readxl::read_xlsx("data/foco_coords.xlsx")

# change to metric
foco_data_refined <- foco_data %>% left_join(foco_coords, by = "Site") %>% 
  mutate(precip_mm = precip_in * 25.4,
         stage_cm = stage_ft * 30.48,
         discharge_Ls = discharge_cfs * 28.3168,
         source = "FoCo") %>% 
  dplyr::select(Site, source, Date, lat, long, precip_mm, stage_cm, discharge_Ls,
                Turbidity = turbidity, pH, DO, Conductivity = conductivity)

#save this to send to katie
write.csv(foco_data_refined, "data/foco_data_clean.csv")

#test out plotly
test <- foco_data %>% 
  filter(Site == "Poudre_River_Mouth")

#something weird happening with discharge values

plotly::plot_ly() %>%
  plotly::add_lines(x = test$Date,
            y = test$stage_ft,
            name = "Stage") %>%
  plotly::layout(yaxis = list(title = "Stage"),
                 xaxis = list(range = c(min, max),
                              showgrid = T))


# USGS sensor data -------------------------------------
library(dataRetrieval)


sites3 <- c("06751145", "06751150", "06751490", "06752260", 
           "06752280", "06746110", "06746095")

#get meta data
site_meta <- readNWISsite(sites3) %>%
  dplyr::select(
    site_no,
    station_name = station_nm,
    long = dec_long_va,
    lat = dec_lat_va,
    datum = dec_coord_datum_cd,
    altitude = alt_va
  )


#test out

#get daily data for discharge
#NOTE on the map viewer, it shows gage ht is avail, but is not with this package
#for some reason...
x <- readNWISdv(sites3, "00060", "2021-01-01", Sys.Date())


#now join with meta to get coords
usgs_sites <- left_join(site_meta, x, by = "site_no") %>% 
  dplyr::select(site_no, station_name, long, lat, datum, source = agency_cd,
                date = Date,  discharge_cfs = X_00060_00003)

#convert to metric
usgs_sites_refined <- usgs_sites %>% 
  mutate(discharge_Ls = discharge_cfs * 28.3168) %>% 
  select(Site = site_no, source, Date = date,
         lat, long, discharge_Ls)



# combine all datasets --------------------------------

water_data <- bind_rows(daily_data_refined, foco_data_refined, res_data, waterQual, usgs_sites_refined) 



saveRDS(water_data, "data/water_data.RDS")
#save updated version
saveRDS(water_data, "data/water_data_update.RDS")
saveRDS(water_data, "app/data/water_data_update.RDS")

# test plotly -----------------------------------

test_plotly <- water_data %>% 
  filter(Site == "Poudre_Park")

plotly::plot_ly() %>%
  plotly::add_lines(x = test_plotly$Date,
            y = test_plotly$discharge_Ls,
            name = "Average Daily Discharge") %>%
  plotly::layout(yaxis = list(title ="Q (L/s)"),
                 xaxis = list(range = c(min(test_plotly$Date), max(test_plotly$Date)),
                              showgrid = T))



#test leaflet ------------------------------------------------

