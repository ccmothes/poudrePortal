#get usgs hydrology data

library(dataRetrieval)
library(leaflet)
library(dplyr)


att <- paste0("<a href='https://www.usgs.gov/'>",
              "U.S. Geological Survey</a> | ",
              "<a href='https://www.usgs.gov/laws/policies_notices.html'>",
              "Policies</a>")

getURL <- function(service, host = "basemap.nationalmap.gov") {
  sprintf("https://%s/arcgis/services/%s/MapServer/WmsServer", host, service)
}


leaflet() %>%
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
  )



# dataRetrieval

# string of sensor names in poudre watershed

sites <- c("06751145", "06751150", "06751490", "06752260", 
           "06752280", "06746110", "06746095")

#get meta data
site_meta <- readNWISsite(sites) %>%
  dplyr::select(
    site_no,
    station_name = station_nm,
    long = dec_long_va,
    lat = dec_lat_va,
    datum = dec_coord_datum_cd,
    altitude = alt_va
  )


#see what data is available
data_avail <- whatNWISdata(siteNumber = sites, service = "dv", statCd = "00003")
# only discharge data avail


pcode <- readNWISpCode("00060")


x <- readNWISstat(siteNumbers = sites, parameterCd = "00060",
                  statReportType = "daily", startDate = "2020", endDate = "2021")

x2 <- readNWISdata(sites = sites, parameterCd = "00060", service = "stat",
                   statReportType = "daily", startDate = "2020-01-01",
                   endDate = "2015-01-30")

#get daily data fro discharge
x3 <- readNWISdv(sites, "00060", "2020-01-01", Sys.Date())


#now join with meta to get coords
usgs_sites <- left_join(site_meta, x3, by = "site_no") %>% 
  dplyr::select(site_no, station_name, long, lat, datum, source = agency_cd,
                date = Date,  discharge_cfs = X_00060_00003)
               



#test out other variables
y <- readNWISdv(siteNumbers = sites, startDate = "2020", endDate = "2021",
                parameterCd = "00060")


qwData <- readNWISdata(state_cd = "CO",
                       startDate = "2021-01-01",
                       drain_area_va_min = 50, qw_count_nu=50, 
                       qw_attributes="expanded",
                       qw_sample_wide="wide",
                       list_of_search_criteria = c("state_cd",
                                                   "drain_area_va",
                                                   "obs_count_nu"),
                       service="qw")
#none in the poudre watershed
