rm(list = ls()) #reset

##NEED TO REMAP GFW DATA NOW
library(dismo)
library(rJava)
library(tidyverse)
library(rnaturalearth)
library(sf)

NM_fishing <- read.csv("./fishing/Future prediction/Objects/processed/NM_presentFINAL.csv",header = TRUE) #read in climatalogical
NM <- split(NM_fishing,list(NM_fishing$Year,NM_fishing$Month)) #split by year and month

GFW_mean <- read.csv("./fishing/Future Prediction/Objects/cropped GFW/GFW_mean_all_years.csv") %>% 
  st_as_sf(coords = c("longitude","latitude"),crs = 3035) #read in yearly GFW means
GFW_list <- split(GFW_mean,list(GFW_mean$year,GFW_mean$month)) #split by year and month

#assign GFW to NM points
for (i in seq_along(NM)) {
  gfw <- GFW_list[[i]]
  nm <- NM[[i]] #access NM file
  nm <- st_as_sf(nm,coords = c("X","Y"),crs = 3035) #create geometry
  distances <- st_distance(gfw,nm) %>% 
    as.data.frame() #each point is a row. each column is the distance to a NEMO point. calculate distances
  min_distances_indices <- apply(distances, 1, which.min)  #we need to find the location of the minimum distance
  nm$fishing_hours <- 0  #initialise new column
  for (j in seq_along(min_distances_indices)){
    nm$fishing[min_distances_indices[j]] <- sum(gfw$fishing_hours[j]+nm$fishing[min_distances_indices[j]]) #add all closest points
  }
  if (i == 1) { #append
    NM_fishing <- nm
  } else {
    NM_fishing <- rbind(NM_fishing, nm)
  }
  print(paste0(i,"/",length(NM)))
}

#build the maxent model
NM_coords <- data.frame(fishing = NM_fishing$fishing)
NM_coords$fishing <- ifelse(NM_coords$fishing > 0, 1, 0) #convert to binary
NM_coords_vector <- as.vector(NM_coords[,1])
NM_predictors <- NM_fishing %>% subset(select = -c(Year,Month,fishing_hours,fishing)) %>%  #define predictors (will eventually go off information value)
                    st_drop_geometry()
habitat_model <- maxent(x = NM_predictors,p = NM_coords_vector)
NM_fishing <- NM_fishing %>% mutate(fishing = NM_coords$fishing)
write.csv(NM_fishing,"./fishing/Future prediction/Objects/processed/NM_presentFINAL.csv")
saveRDS(habitat_model,"./fishing/Future Prediction/Future Parametrisation/Objects/FinalModel.RDS") #save model
