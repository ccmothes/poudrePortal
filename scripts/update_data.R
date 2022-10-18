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

discharge_convert <- function(q_mm, watershed){
  
  area_km <- watershed_area %>%
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
  #Remove mtn campus for now
  filter(site != "mtcampus")
  
stream_daily %>% 
  rowwise() %>% 
  mutate(discharge_Ls = discharge_convert(q_mm = q_mm, watershed = site))

