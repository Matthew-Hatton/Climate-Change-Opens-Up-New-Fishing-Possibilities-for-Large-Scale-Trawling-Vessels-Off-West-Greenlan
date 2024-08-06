rm(list = ls()) #reset
library(ggplot2)
library(rnaturalearth)
library(tidyverse)
library(sf)
library(furrr)
library(stars)
library(raster)
library(viridis)

##### Initialise #####
source("./packages/nowtpipe.R")
plan(multisession)
basemap <- ne_countries(scale = "large", country = "Greenland",returnclass = "sf") %>% 
  st_transform(crs = 3035) #import basemap
Domain <- readRDS("./fishing/Future Prediction/Objects/processed/Buffered Domain.rds") %>% #load domain polygon
  st_transform(crs = 3035)#transform domain crs
col_grid <- rgb(235, 235, 235, 100, maxColorValue = 255) #transparent lines in background of plots
add_month_column <- function(df) {
  df$month <- as.numeric(substr(df$date, 6, 7))
  return(df)
}

### Global Fishing Watch ### 
GFW_sf <- list.files( "./fishing/Global Fishing Watch/finished data", pattern = "\\GFW_201[2-9]\\.csv$", full.names = TRUE) %>% 
  future_map(.,.f = read.csv) %>% 
  do.call(rbind,.)

GFW_sf_prepared <- GFW_sf %>%
  subset(select = c("cell_ll_lon", "cell_ll_lat", "fishing_hours")) %>%
  rename(x = cell_ll_lon, y = cell_ll_lat, z = fishing_hours)

# Convert to raster
raster_object <- rasterFromXYZ(GFW_sf_prepared)
stars_object <- st_as_stars(raster_object)

# Set the CRS to EPSG:4326
stars_object <- st_set_crs(stars_object, 4326)
stars_object_transformed <- st_transform(stars_object, crs = 3035) %>% 
  st_as_sf() %>% 
  st_intersection(Domain)

saveRDS(stars_object_transformed,"./GFW data stars.rds")
