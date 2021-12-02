# load and manage data from local file

library(readr)
library(purrr)
library(dplyr)
library(sf)

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
    bind_rows(.id = "Site")
  
}

daily_data <- bind_rows(files) %>%
  distinct(Date, Site, .keep_all = TRUE)

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
daily_data <- left_join(watersheds, daily_data, by = "Site") %>% 
  st_transform(4326)

saveRDS(daily_data, "portal_demo/data/daily_data.RDS")

#UPDATE tie data to sensor coordinates (kicked out soil temp sensor info)

locations_stream <- locations %>% filter(type == "stream gage") %>% 
  rename(Site = name, lat = POINT_Y, long = POINT_X)

daily_data_refined <- daily_data %>% 
  left_join(locations_stream, by = 'Site') %>% 
  mutate(source = "CSU_Kampf") %>% 
  select(Site, source, Date, long, lat, precip_mm = P_mm, stage_cm = Stage_cm,
         discharge_Ls = Discharge_Ls)


# USFS - Chuck Roads water quality data -----------------------------------------

cp_coords <- read_csv("data/CamPeak_Coordinates.csv") %>% rename(Site =  SITE,
                                                                 long = "X_WGS84",
                                                                 lat = "Y_WGS84")
waterQual <- readxl::read_excel("data/CamPk_toMothes_trial.xlsx") %>% 
  left_join(cp_coords, by = "Site") %>% 
  group_by(Site, Date, SiteLabel, SiteType, Trt_CP) %>% summarise(across(where(is.numeric),
                                            ~mean(.x, na.rm = TRUE))) %>% 
  ungroup()


saveRDS(waterQual, "portal_demo/data/water_qual.RDS")

#UPDATE CONVERT TO METRIC

waterQual_refined <- waterQual %>% 
  mutate(stage_cm = Stage_PBR_ft * 30.48, source = "USFS") %>% 
  dplyr::select(Site, source, Date, stage_cm, Turbidity, DO = DOC, pH, long, lat)



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
           Date = as.POSIXct(Date, format = "%m/%d/%Y"))
  
  
}

foco_data <- bind_rows(files2) %>%
  distinct(Date, Site, .keep_all = TRUE)


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

#get daily data fro discharge
x <- readNWISdv(sites3, "00060", "2020-01-01", Sys.Date())


#now join with meta to get coords
usgs_sites <- left_join(site_meta, x, by = "site_no") %>% 
  dplyr::select(site_no, station_name, long, lat, datum, source = agency_cd,
                date = Date,  discharge_cfs = X_00060_00003)

#convert to metric
usgs_sites_refined <- usgs_sites %>% 
  mutate(discharge_Ls = discharge_cfs * 28.3168,
         source = "USGS") %>% 
  select(Site = site_no, source, Date = date,
         lat, long, discharge_Ls)



# combine all datasets --------------------------------

water_data <- bind_rows(daily_data_refined, foco_data_refined, waterQual_refined, usgs_sites_refined) %>% 
  mutate(Date = lubridate::as_date(Date))



saveRDS(water_data, "data/water_data.RDS")

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

