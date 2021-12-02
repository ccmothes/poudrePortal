# read in data from database (hydroshare)
## NOTE EACH TIME THIS IS RUN, SITE TRACKS AS A DOWNLOAD

library(readr)
library(purrr)
library(sf)

# read in files from hydroshare
get_daily <- function(url, name) {
  read_csv(url) %>%
    mutate(Site = name)
}

#list of urls by site name
daily <- data.frame(name = c("Andrews", "Bighorn", "Dry", "Michigan", "Mill"),
                    url = c("https://www.hydroshare.org/resource/8d8b3c4e3d9c4f538cea53d81791c41e/data/contents/Andrews_daily_1619.csv",
                            "https://www.hydroshare.org/resource/8d8b3c4e3d9c4f538cea53d81791c41e/data/contents/Bighorn_daily_1619.csv",
                            "https://www.hydroshare.org/resource/8d8b3c4e3d9c4f538cea53d81791c41e/data/contents/Dry_daily_1619.csv",
                            "https://www.hydroshare.org/resource/8d8b3c4e3d9c4f538cea53d81791c41e/data/contents/Michigan_daily_1619.csv",
                            "https://www.hydroshare.org/resource/8d8b3c4e3d9c4f538cea53d81791c41e/data/contents/Mill_daily_1619.csv"))

data <- vector("list", length = 5)
                    
for (i in 1:5){
  data[[i]] <- get_daily(url = daily[i,2], name = daily[i,1])
}

daily_data <- bind_rows(data)

#spatial zip file, download as temp file
temp <- tempfile()
temp2 <- tempfile()

download.file("https://www.hydroshare.org/resource/8d8b3c4e3d9c4f538cea53d81791c41e/data/contents/spatial.zip",
              temp)
unzip(zipfile = temp, exdir = temp2)

#see what files are in here
list.files(temp2, recursive = TRUE)

#locations and watersheds shapefiles

#polygon
watersheds <- read_sf(list.files(temp2, recursive = TRUE, pattern = "watersheds.shp$",
                                 full.names = TRUE))

#points
locations <- read_sf(list.files(temp2, recursive = TRUE, pattern = "locations.shp$",
                                 full.names = TRUE))
