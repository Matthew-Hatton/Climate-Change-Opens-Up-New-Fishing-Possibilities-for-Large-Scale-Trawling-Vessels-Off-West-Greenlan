rm(list = ls()) #reset

library(rnaturalearth)
library(sf)
library(dplyr)
library(ggplot2)
library(stars)
library(patchwork)
library(raster)
library(viridis)
sf_use_s2(FALSE)

### MAPS
col_grid <- rgb(235, 235, 235, 100, maxColorValue = 255) #faded gridlines
iceland <- ne_countries(country = "Iceland",scale = "large",returnclass = "sf") %>% 
  st_transform(crs = 3035)
canada <- ne_countries(country = "Canada",scale = "large",returnclass = "sf") %>% 
  st_transform(crs = 3035)
domain <- readRDS("./fishing/Future Prediction/Objects/processed/Buffered Domain.rds") %>% 
  st_transform(crs = 3035)
basemap <- ne_countries(country = "Greenland",scale = "large",returnclass = "sf") %>% 
  st_transform(crs = 4326)
worldmap <- ne_countries(scale = "medium",returnclass = "sf") %>% 
  st_crop(xmin = -180,xmax = 45,ymin = 30,ymax = 90)

world <- ne_countries(scale = "large", returnclass = "sf")

ortho_proj <- "+proj=ortho +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs" #define orthographic proj

world_ortho <- st_transform(world, crs = ortho_proj) #change to orthographic

circle <- st_buffer(st_point(c(0, 0)), dist = 6500000) #create circle (for white background in orthographic earth)
circle_sf <- st_sfc(circle, crs = ortho_proj) #create geom
circle_sf <- st_transform(circle_sf, crs = st_crs(world_ortho)) #transform to crs


### DOMAIN
domain <- readRDS("./fishing/Future Prediction/Objects/processed/Buffered Domain.rds") %>% 
  st_transform(crs = 3035)

dom_join <- st_union(domain[1,], domain[2,]) %>% 
  st_buffer(dist = 30000)

### GFW
stars_fishing <- readRDS("./GFW data stars.RDS")
sum_fishing_hours <- stars_fishing %>%
  group_by(geometry) %>%
  summarise(total_fishing_hours = mean(z)) #mean per pixel

sum_fishing_hours <- sum_fishing_hours %>% 
  filter(total_fishing_hours > 0.01) %>% 
  st_as_stars() #convert to stars

### ICE
cop_path <- "./fishing/Global Fishing Watch/RAW/allregioncop.nc" #where is the ncdf ice data?
tst_raster <- brick(cop_path) #bricks together all rasters
tst_raster[tst_raster > 0.15] <- 1 #ice cover less than 15% doesn't count
tst_raster[tst_raster < 1] <- NA #everything less than threshold doesn't exist
extent <- extent(0,360,55,90) #define cropping window (smaller = faster)
crop_raster <- crop(tst_raster,extent) #crop to zone
#plot(crop_raster) #check crop

ice_2012_onwards <- crop_raster[[145:240]] #crop to correct year (Jan 2012-Dec 2019)
names(ice_2012_onwards[[13]]) <- "ice" #alter layer name of current layer
names(ice_2012_onwards[[22]]) <- "ice" #alter layer name of current layer

tst_sf_max <- rasterToPolygons(ice_2012_onwards[[2]]) %>% #convert to polygon
  st_as_sf() %>% #... then to sf object
  group_by("ice") %>% #calculate a mean sea-ice
  summarise(ice = mean("ice")) %>%  #as above
  st_transform(crs = 3035) %>% #top down view 3035
  st_make_valid() #fix geometry

tst_sf_min <- rasterToPolygons(ice_2012_onwards[[23]]) %>% #convert to polygon
  st_as_sf() %>% #... then to sf object
  group_by("ice") %>% #calculate a mean sea-ice
  summarise(ice = mean("ice")) %>%  #as above
  st_transform(crs = 3035) %>% #top down view 3035
  st_make_valid() #fix geometry

### PLOT
p1 <- ggplot() +
  geom_sf(data = dom_join,fill = "red",alpha = 0.2,color = "NA") + #domain
  geom_sf(data = tst_sf_max,fill = "#D7E7E7",alpha = 0.7) + #winter ice
  geom_sf(data = tst_sf_min,fill = "white") + #summer ice
  geom_sf(data = dom_join,color = "black",fill = "NA") + #domain  (for black line)
  geom_sf(data = basemap,fill = "white",color = "black") + #greenland
  geom_stars(data = sum_fishing_hours) + #GFW data
  scale_fill_viridis(na.value = NA, trans = "log10",option = "inferno",
                     breaks = c(1/60, 1/6, 1, 20), labels = c("1 min", "10 mins", "1 hr", "20 hrs"),
                     name = "Annual Fishing Activity") +
  geom_sf(data = canada,color = "black",fill = "black") + #canada
  annotate("text", label = "• Summer Ice \nExtent",
           vjust = 0.5, hjust = 0.5, angle = 0, size = 2.5, colour = "black",
           x = 3800000, y = 7100000) +
  annotate("text", label = "• Winter Ice \nExtent",
           vjust = 0.5, hjust = 0.5, angle = 0, size = 2.5, colour = "black",
           x = 1500000, y = 6170000) +
  guides(fill = guide_colourbar(title.hjust = 0.5, title.position = "left",
                                title.theme = element_text(angle = 90, size = 8),
                                label.theme = element_text(size = 6), 
                                barheight = 13, barwidth = 0.5)) +
  theme(
    panel.background = element_rect(fill = '#5E9EA0', color = "black"),
    plot.background = element_rect(fill = "transparent", color = NA), 
    panel.grid = element_line(color = "grey90")
  ) +
  coord_sf(xlim = c(1000000,3900000),ylim = c(5000000,7200000)) +
  labs(x = "Longitude",y = "Latitude") +
  NULL

p2 <- ggplot() +
  geom_sf(data = circle_sf,fill = "white",color = "black") +
  geom_sf(data = world_ortho,fill = "black", color = NA) + 
  geom_sf(data = dom_join, fill = "red", alpha = 0.6, color = NA) +
  geom_sf(data = basemap, fill = "white") +  # Ensure landmasses are white
  coord_sf(datum = NA) +  # Remove default coordinate system
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = NA, color = NA), # Transparent background
    plot.background = element_rect(fill = NA, color = NA),
    panel.grid.major = element_blank(), # No grid lines
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.minor = element_blank()
  ) ##WORLD MAP

together <- p1 + inset_element(p2, left = 0, bottom = 0, right = 1.74, top = 0.295) #world map in bottom left

ggsave("./fishing/Future Prediction/Paper/5 figures/greenland domain.tiff",
       dpi = 1200,
       width = 170,
       height = 99,
       unit = "mm",
       bg = "transparent",
       plot = together) #save out

zoom <- ggplot() +
  geom_sf(data = dom_join,fill = "red",alpha = 0.2,color = "NA") + #domain
  geom_sf(data = tst_sf_max,fill = "#D7E7E7",alpha = 0.7) + #winter ice
  geom_sf(data = tst_sf_min,fill = "white") + #summer ice
  geom_sf(data = dom_join,color = "black",fill = "NA") + #domain  (for black line)
  geom_sf(data = basemap,fill = "white",color = "black") + #greenland
  geom_stars(data = sum_fishing_hours) + #GFW data
  scale_fill_viridis(na.value = NA, trans = "log10",option = "inferno",
                     breaks = c(1/60, 1/6, 1, 20), labels = c("1 min", "10 mins", "1 hr", "20 hrs"),
                     name = "Annual Fishing Activity") +
  geom_sf(data = canada,color = "black",fill = "black") + #canada
  guides(fill = guide_colourbar(title.hjust = 0.5, title.position = "left",
                                title.theme = element_text(angle = 90, size = 8),
                                label.theme = element_text(size = 6), 
                                barheight = 13, barwidth = 0.5)) +
  theme(
    panel.background = element_rect(fill = '#5E9EA0', color = "black"),
    plot.background = element_rect(fill = "transparent", color = NA), 
    panel.grid = element_line(color = "grey90"),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  coord_sf(xlim = c(1450000, 2500000), ylim = c(5150000, 6670000)) +
  labs(x = "Longitude",y = "Latitude") +
  NULL
final <- together + zoom
ggsave("./fishing/Future Prediction/Paper/5 figures/greenland domain.tiff",
       dpi = 1200,
       bg = "white",
       plot = final) #save out
# zoom
# ggsave("./fishing/Future Prediction/Paper/5 figures/greenland zoom.tiff",
#        dpi = 1200,
#        bg = "white",
#        plot = zoom) #save out

### PLOT
zoom_out <- ggplot() +
  geom_sf(data = dom_join,fill = "red",alpha = 0.2,color = "NA") + #domain
  geom_sf(data = tst_sf_max,fill = "#D7E7E7",alpha = 0.4,color = "NA") + #winter ice
  geom_sf(data = tst_sf_min,fill = "white",alpha = 1,color = "NA") + #summer ice
  geom_sf(data = dom_join,color = "black",fill = "NA") + #domain  (for black line)
  geom_sf(data = basemap,fill = "white",color = "black") + #greenland
  #geom_stars(data = sum_fishing_hours) + #GFW data
  scale_fill_viridis(na.value = NA, trans = "log10",option = "inferno",
                     breaks = c(1/60, 1/6, 1, 20), labels = c("1 min", "10 mins", "1 hr", "20 hrs"),
                     name = "Annual Fishing Activity") +
  geom_sf(data = canada,color = "black",fill = "black") + #canada
  geom_sf(data = iceland,color = "black",fill = "black") +
  annotate("text", label = "• Summer Ice \nExtent",
           vjust = 0.5, hjust = 0.5, angle = 0, size = 3, colour = "black",
           x = 3800000, y = 7100000) +
  annotate("text", label = "• Winter Ice \nExtent",
           vjust = 0.5, hjust = 0.5, angle = 0, size = 3, colour = "black",
           x = 1500000, y = 6170000) +
  guides(fill = guide_colourbar(title.hjust = 0.5, title.position = "left",
                                title.theme = element_text(angle = 90, size = 8),
                                label.theme = element_text(size = 6), 
                                barheight = 13, barwidth = 0.5)) +
  theme(
    panel.background = element_rect(fill = '#5E9EA0', color = "black"),
    plot.background = element_rect(fill = "transparent", color = NA), 
    panel.grid = element_line(color = "white")
  ) +
  coord_sf(xlim = c(700000,4500000),ylim = c(5000000,8000000)) +
  labs(x = "Longitude",y = "Latitude") +
  NULL
#zoom_out
# ggsave("./fishing/Future Prediction/Paper/5 figures/greenland zoom_out.tiff",
#        dpi = 1200,
#        bg = "white",
#        plot = zoom_out) #save out


