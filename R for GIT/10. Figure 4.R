rm(list = ls())

library(rmaxent)
library(patchwork)
habitat_model <- readRDS("./fishing/Future Prediction/Future Parametrisation/Objects/FinalModel.RDS")
lam <- parse_lambdas(habitat_model)[[1]] #extract lambdas
NM <- read.csv("./fishing/Future prediction/Objects/processed/NM_presentFINAL.csv",header = TRUE) %>% 
  st_as_sf(coords = c("X","Y"),crs = 3035)
NM_future <- read.csv("./fishing/Future prediction/Objects/processed/NM_futureFINAL.csv") %>% 
  st_as_sf(coords = c('X', 'Y'), crs = 3035) #load future

#find min and max
min_ice_volume = 0
max_ice_volume = max(NM$Ice_conc) * max(NM$Ice_Thickness)

#find median values
median_temp_present <- median(NM$Temperature)
median_temp_future <- median(NM_future$Temperature)

median_sal_present <- median(NM$Salinity)
median_sal_future <- median(NM_future$Salinity)

median_snow_present <- median(NM$Snow_Thickness)
median_snow_future <- median(NM_future$Snow_Thickness)

median_DIN_present <- median(NM$DIN) 
median_DIN_future <- median(NM_future$DIN)

median_phyt_present <- median(NM$Phytoplankton)
median_phyt_future <- median(NM_future$Phytoplankton)

median_bathy <- median(NM$Bathymetry)

median_mean_temp_present <- median(NM$MeanTemperature)
median_mean_ice_present <- median(NM$MeanIce)

median_mean_temp_future <- median(NM_future$MeanTemperature)
median_mean_ice_future <- median(NM_future$MeanIce)

median_sediment <- median(NM$RoundedGrainSize)

#having calulcated all the median values, now need to step through ice thickness and concentration
ice_conc_seq <- seq(0,max(NM$Ice_conc),length.out = 100)
ice_thickness_seq <- seq(0,max(NM$Ice_Thickness),length.out = 100)

grid <- expand.grid(Ice_conc = ice_conc_seq,Ice_Thickness = ice_thickness_seq)

values <- data.frame(MeanTemperature = rep(median_mean_temp_present,dim(grid)[1]),
                     MeanIce = rep(median_mean_ice_present,dim(grid)[1]),
                     Bathymetry = rep(median_bathy,dim(grid)[1]),
                     Phytoplankton = rep(median_phyt_present,dim(grid)[1]),
                     DIN = rep(median_DIN_present,dim(grid)[1]),
                     Snow_Thickness = rep(median_snow_present,dim(grid)[1]),
                     Salinity = rep(median_sal_present,dim(grid)[1]),
                     Temperature = rep(median_temp_present,dim(grid)[1]),
                     RoundedGrainSize = rep(median_sediment,dim(grid)[1]))

## habitat_model can now predict when given points. So let's give it a hypothetical point where all the medians are true
hypo_present <- cbind(grid,values)

projection <- rmaxent::project(lambdas = habitat_model,newdata = hypo_present)

hypo_present$prediction <- projection$prediction_logistic

coords <- st_coordinates(NM) #get coords
NM_fishing <- cbind(NM, latitude = coords[, "Y"], longitude = coords[, "X"]) %>% 
  st_drop_geometry() #create new lat lon cols and drop geometry

### FUTURE
values_future <- data.frame(MeanTemperature = rep(median_mean_temp_future,dim(grid)[1]),
                     MeanIce = rep(median_mean_ice_future,dim(grid)[1]),
                     Bathymetry = rep(median_bathy,dim(grid)[1]),
                     Phytoplankton = rep(median_phyt_future,dim(grid)[1]),
                     DIN = rep(median_DIN_future,dim(grid)[1]),
                     Snow_Thickness = rep(median_snow_future,dim(grid)[1]),
                     Salinity = rep(median_sal_future,dim(grid)[1]),
                     Temperature = rep(median_temp_future,dim(grid)[1]),
                     RoundedGrainSize = rep(median_sediment,dim(grid)[1]))
hypo_future <- cbind(grid,values_future)

projection_future <- rmaxent::project(lambdas = habitat_model,newdata = hypo_future) #project into the future
hypo_future$prediction <- projection_future$prediction_logistic #attach projection

#attach years so that we can facet
hypo_present$period <- "2010s"
hypo_future$period <- "2090s"
NM$period <- "2010s"
NM_future <- NM_future %>% subset(select = -c(latitude,longitude))
NM_future$period <- "2090s"


all_data <- rbind(NM,NM_future) #bind for density
all <- rbind(hypo_present,hypo_future) #bind for contour

## PLOT
ggplot() +
  geom_raster(data = all,aes(x = Ice_Thickness,y = Ice_conc,fill = prediction),interpolate = TRUE) +
  geom_density_2d(data = filter(all_data,Ice_conc > 0.15),aes(x = Ice_Thickness,y = Ice_conc),color = "black",alpha = 0.3) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "orange", midpoint = 0.5,
                       limits = c(0, 1)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(fill = "Habitat Suitability") +
  facet_wrap(~period,ncol = 1,strip.position = "right") +
  labs(x = "Ice Thickness",
       y = "Ice Concentration") +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12,face = "bold"),
        strip.background = element_rect(color = "black",fill = "white"),
        strip.text = element_text(size = 14,face = "bold"),
        legend.title = element_text(size = 8,face = "bold"),
        legend.text = element_text(size = 8),
        legend.position = "top",
        panel.spacing=unit(1,"lines")) +
  guides(fill = guide_colourbar(title.hjust = 0.5, title.position = "top",
                                title.theme = element_text(angle = 0, size = 14,face = "bold"),
                                label.theme = element_text(size = 12), 
                                barheight = 0.25, barwidth = 13)) +
  NULL
ggsave("./fishing/Future Prediction/Paper/5 figures/step through ice.png",
       dpi = 1200,width = 17,unit = "cm")
