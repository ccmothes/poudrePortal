# code to update portal dataset

# set up -----------------------------------------

library(readr)
library(purrr)
library(dplyr)
library(sf)
library(lubridate)
library(tidyr)

#read in old dataset

dat <- readRDS("data/water_data_update.RDS")

#function to convert q_mm to L/s for discharge

discharge_convert <- function(q_mm, watershed_data, watershed){
  
  area_km <- watershed_data %>%
              filter(NAME == watershed) %>% 
              pull(area_km2)
  
  q_m <- q_mm/1000
  area_m2 <- area_km * 1e6 
  
  return(q_m * area_m2 / 86.4)
  
}


# UPDATE FALL 2022 ---------------------------------------------------

## CSU Stephanie Kampf ------------------------------------------------

#read in watershed area for discharge conversion
watershed_area <- readxl::read_xlsx("data/ws_area_kampf.xlsx")

# daily stream
stream_daily <- read_csv("data/stephanie/hydroshare_update/stream_daily.csv") %>% 
  pivot_longer(!Date,
               names_to = c("site", ".value"),
               names_sep = "_",
               values_drop_na = TRUE) %>% 
  rename(q_mm = Q, stage_cm = stage) %>% 
  #Remove mtn campus for now (no data on watershed area)
  filter(site != "mtcampus") %>% 
  rowwise() %>% 
  mutate(discharge_Ls = discharge_convert(q_mm = q_mm, watershed_data = watershed_area, watershed = site))


# daily precip
precip_daily <- read_csv("data/stephanie/hydroshare_update/P_daily.csv") %>% 
  pivot_longer(!Date, 
               names_to = "site",
               values_to = "precip_mm") %>% 
  #remove mtcampus for now
  filter(site != "mtcampus")


# pull daily files to get temp and snow vars
files <- list.files("data/stephanie/hydroshare_update/met/", full.names = TRUE)

site_names <- c("bighorn", "dry", "joewright_burn", "joewright", "mtcampus", "tunnel")

met <- vector("list", length = length(files))

for (i in 1:length(files)){
  
  met[[i]] <- read_csv(files[i]) %>% 
    mutate(site = site_names[i]) %>% 
    dplyr::select(site, contains(c("date", "Ta", "Ts_5", "snow")))
  
}

met_daily <- bind_rows(met) %>% 
  #remove mtcampus again for now
  filter(site != "mtcampus") %>% 
  dplyr::select(site, Date = date, Average_temp = Ta, Snow_depth = snow, Soil_temp = Ts_5)


#combine all and tie to coords

site_coords <- read_csv("data/stephanie/hydroshare_update/metadata.csv") %>%
  filter(Name != "mtcampus") %>% 
  distinct(Name, Longitude, Latitude, .keep_all = TRUE) %>% 
  filter(Type == "stage" | Name %in% c("michigan", "joewright") & Type == "rain" | Name == "joewright_burn" & Type == "Ta") %>%  
  dplyr::select(site = Name, long = Longitude, lat = Latitude)

all_daily <- met_daily %>% 
  full_join(precip_daily, by = c("Date", "site")) %>% 
  full_join(stream_daily, by = c("Date", "site")) %>%
  left_join(site_coords, by = "site") %>% 
  mutate(Date = as.Date(as.POSIXct(Date, format = "%m/%d/%Y")),
         lat = as.numeric(lat)) %>% 
  mutate(Site = case_when(site == "bighorn" ~ "Bighorn Creek",
                          site == "dry" ~ "Dry Creek",
                         site == "joewright_burn" ~ "Joe Wright Reservoir Burned",
                         site == "joewright" ~ "Joe WRight Reservoir Unburned",
                         site == "tunnel" ~ "Tunnel",
                         site == "aspen" ~ "Aspen Glenn",
                         site == "greyrock" ~ "Grey Rock",
                         site == "mill" ~ "Mills Creek", 
                         site == "skin" ~ "Skin Gulch",
                         site == "bl4" ~ "Blue Lake 4",
                         site == "montgomery" ~ "Montgomery",
                         site == "michigan" ~ "Michigan River",
                         site == "dadd" ~ "Dadd Gulch",
                         site == "hewlett" ~ "Hewlett Gulch",
                         site == "hill" ~ "Hill Gulch",
                         site == "michiganditch" ~ "Michigan Ditch",
                         site == "washout" ~ "Washout"),
         source = "CSU_Kampf") %>% 
  dplyr::select(Site, source, Date, long, lat, Average_temp, Snow_depth, Soil_temp, precip_mm, 
                stage_cm, discharge_Ls) %>% 
  #filter out NA dates
  filter(!is.na(Date))
 
#rename site's to match dat
#rename columns to match and select
#add source column
#NOTE that there may be two sets of coords for ~2 sites, fix in app script?


#combine to full dataset
water_data_update <- bind_rows(dat, all_daily) %>% 
  #some duplicates with overlap in dates. Keep new dataset (which will be the second obs due to bind_row order)
  group_by(Site, Date) %>% 
  filter(row_number() == n()) %>% 
  ungroup() %>% 
  #still some NAs in Dates from original dataset
  filter(!(is.na(Date)))


#rename old dataset, save new one
file.rename("data/water_data_update.RDS", "data/water_data_update_old10-22.RDS") #10.19.22
saveRDS(water_data_update, "data/water_data_update.RDS")
saveRDS(water_data_update, "app/data/water_data_update.RDS")



# CSU Ross Lab Update 10/27/22 ------------------------------------------------------
library(mapview)

# test that site coords match between new and old datasets

old_ross <- readRDS("data/water_data_update.RDS") %>% 
  filter(source == "CSU_Ross") 

old_ross_sites <- old_ross %>% 
  distinct(Site, long, lat)


new_ross <- read_csv("data/Poudre Portal Reservoir Chemistry/CPF_Reservoir_chemistry_up_to_083122.csv") %>% 
  # remove long draw inlet and Poudre @ Timberline/ no data
  filter(!(Site %in% c("Long Draw Inlet", "Poudre at Timberline")))

new_ross_sites <- new_ross %>% distinct(Site, Lat, Long)

# find dups
new_ross_sites %>% group_by(Site) %>% summarise(n = n()) %>% filter(n > 1) %>% View()
# differing coords for long draw outlet and reservoir, peterson outlet, peterson reservoir, sleeping elephant



new_ross_sites %>% 
  filter(!is.na(Long)) %>% 
  st_as_sf(coords = c("Long", "Lat"), crs = 4326) %>%
  mutate(long = unlist(map(.$geometry,1)),
         lat = unlist(map(.$geometry,2))) %>% 
  mapview(legend = FALSE)

# Long draw and Peterson res/outlet coords are swapped. These are the correct ones:
#LD Res: -105.77725307, 40.50087627
#LD Outlet -105.7709459, 40.5036571
#Peterson Res: -105.79434, 40.55936515
#Peterson Outlet: -105.7929021, 40.560638874

# 2021 coords were placed on a map, 2022 are from GPS unit so use updated ones

#test differing site names 21/22
new_ross_sites %>%  
  full_join(old_ross, by = "Site") %>% View()


#okay now rename new ross to bind with old ross and edit site names/coords

new_ross <- new_ross %>% 
  select(-c(X1, Site_Code, Status, Watershed, Watershed_Level, Location)) %>% 
  rename(lat = Lat, long = Long, Chla = ChlA) %>% 
  mutate(source = "CSU_Ross")
  

# edit old site names to bind to new data
all_ross <- bind_rows(old_ross, new_ross) %>%
  mutate(
    Site = case_when(
      Site == "Barnes Meadow Reservoir Outflow" ~ "Barnes Meadow Outflow",
      Site == "Chambers Lake" ~ "Chambers Reservoir",
      Site == "Chambers Lake Inflow" ~ "Chambers Inlet",
      Site == "Chambers Lake Outflow" ~ "Chambers Outflow",
      Site == "Comanche Reservoir Inflow" ~ "Comanche Inflow",
      Site == "Comanche Reservoir Outflow" ~ "Comanche Outflow",
      Site == "Hourglass Reservoir Inflow" ~ "Hourglass Inflow",
      Site == "Hourglass Reservoir Outflow" ~ "Hourglass Outflow",
      Site == "Joe Wright Reservoir Inflow" ~ "Joe Wright Inlet",
      Site == "Long Draw Reservoir Outflow" ~ "Long Draw Outlet",
      Site == "Peterson Reservoir Outflow" ~ "Peterson Outlet",
      TRUE ~ Site
    )
  ) 


#get df with correct coords to replace old ones with
ross_coords <- 
  new_ross %>% 
  distinct(Site, lat, long) %>% 
  #replace LD and Peterson with correct coords
  mutate(long = replace(long, Site == "Long Draw Outlet", -105.7709459),
         lat = replace(lat, Site == "Long Draw Outlet", 40.5036571),
         long = replace(long, Site == "Long Draw Reservoir", -105.77725307),
         lat = replace(lat, Site == "Long Draw Reservoir" , 40.50087627),
         long = replace(long, Site == "Peterson Reservoir", -105.79434),
         lat = replace(lat, Site == "Peterson Reservoir", 40.55936515),
         long = replace(long, Site == "Peterson Outlet", -105.7929021),
         lat = replace(lat, Site == "Peterson Outlet", 40.560638874)) %>% 
  #remove NA sleeping elephant
  filter(!is.na(long)) %>% 
  distinct(Site, lat, long) %>% 
  rename(lat_new = lat, long_new = long)

#left join with correct site coords
all_ross_corrected <-
  all_ross %>% left_join(ross_coords, by = "Site") %>%
  #now keep new coords
  dplyr::select(-c(lat, long)) %>%
  rename(lat = lat_new, long = long_new) %>% 
  #some duplicates with overlap in dates. Keep new dataset (which will be the second obs due to bind_row order)
  group_by(Site, Date) %>% 
  filter(row_number() == n()) %>% 
  ungroup()


# add to full water file and save updated version
water_data_update <- dat %>% 
  filter(source != "CSU_Ross") %>% 
  bind_rows(all_ross_corrected)

# SAVED 10.29.22
saveRDS(water_data_update, "data/water_data_update.RDS")
saveRDS(water_data_update, "app/data/water_data_update.RDS")

