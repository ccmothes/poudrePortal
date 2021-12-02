#fire data

library(rgdal)
library(sf)
library(leaflet)
library(raster)
library(rmapshaper)


camPeak <- st_read("data/CamPeak_SBS.shp") %>% st_transform(4326) %>% 
  st_make_valid()

camPeak_prj <-  st_read("data/CamPeak_SBS.shp") %>% 
  st_make_valid()

#
library(viridis) # My favorite palette for maps
wardpal <- colorFactor(viridis(7), camPeak$Severity)

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = camPeak, color = NA, weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.9,
              fillColor = ~colorFactor("Reds", Severity)(Severity)) %>% 
  addScaleBar()

#try simplifying polygons
camPeak_simple <- st_simplify(camPeak, dTolerance = 0.1, preserveTopology = TRUE) %>% 
  ms_simplify() #this aids in faster plotting, save this object for shiny
saveRDS(camPeak_simple, "data/camPeakSimple.RDS")

#still takes a while to plot
leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = camPeak_simple, color = NA, weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.9,
              fillColor = ~colorFactor("Reds", Severity)(Severity)) %>% 
  addScaleBar()


#convert to raster for faster plotting


r1 <- raster(ext = extent(camPeak_prj), res = 20, crs = crs(camPeak_prj)) 
r2 <- raster(ext = extent(camPeak), res = 20, crs = crs(camPeak_prj)) 

#create severity value to populate raster cells
camPeak_prj <- camPeak_prj %>% mutate(severity_value = case_when(
  Severity == "Unburned / Very Low" ~ 1,
  Severity == "Low" ~ 2,
  Severity == "Moderate" ~ 3,
  Severity == "High" ~ 4
))

camPeak_ras <- rasterize(camPeak_prj, r1, field = "severity_value", 
                         fun = max, na.rm = TRUE)

camPeak_ras84 <- projectRaster(camPeak_ras, crs = 4326)

#
leaflet() %>% 
  addTiles() %>% 
  addRasterImage(camPeak_ras84, colors = colorNumeric("Reds", values(camPeak_ras84),
                                                    na.color = "transparent"))

# try fasterize
library(fasterize)


r_sf <- fasterize::raster(camPeak_prj, res = 30)

camPeak_fasterize <- fasterize(camPeak_prj, r_sf, 
                               field = "severity_value", fun = "max")
# save this one
writeRaster(camPeak_fasterize, "data/camPeakSeverity.tif")

#
leaflet() %>% 
  addTiles() %>% 
  addRasterImage(camPeak_fasterize, colors = colorNumeric("Reds", values(camPeak_fasterize),
                                                      na.color = "transparent")) %>% 
  addPolygons(data = camPeak_simple, color = NA, weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.8,
              fillColor = ~colorFactor("Reds", Severity)(Severity)) %>% 
  addScaleBar()


# check out other files

citSci <- st_read("data/CitSciLocations.shp")
foco <- st_read("data/FoCoWaterLocations.shp")
rmrs <- st_read("data/RMRSLocations.shp")
#just points, no attributes


#try different rasterize approach

