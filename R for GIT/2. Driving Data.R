rm(list = ls())
library(tidyverse)
library(sf)
library(rnaturalearth)
library(ranger)

domain <- readRDS("./fishing/Future Prediction/Objects/processed/Buffered Domain.rds") %>% 
  st_transform(crs = 3035) #load domain
NM_present <- list.files("./fishing/Future Prediction/NEMO output", pattern = "\\.201[2-9]\\.rds$", full.names = TRUE) %>% 
  future_map(.,.f = readRDS) %>%  #read in files - each element is its own month
  bind_rows() %>% 
  group_by(longitude,latitude,Year,Month) %>% 
  summarise(Temperature = mean(Temperature,na.rm = TRUE),
            Salinity = mean(Salinity,na.rm = TRUE),
            Ice_conc = mean(Ice_conc,na.rm = TRUE),
            Ice_Thickness = mean(Ice_Thickness,na.rm = TRUE),
            Snow_Thickness = mean(Snow_Thickness,na.rm = TRUE),
            DIN = mean(DIN,na.rm = TRUE),
            Phytoplankton = mean(Phytoplankton,na.rm = TRUE),
            Bathymetry = mean(Bathymetry,na.rm = TRUE)) %>%  #sorts slab_layer calculation
  ungroup() %>% 
  group_by(Year,Month) %>% 
  mutate(MeanTemperature = mean(Temperature),
         MeanIce = mean(Ice_conc)) %>%  # add mean over whole domain
  st_as_sf(coords = c("longitude","latitude"),crs = 4326) %>% 
  st_transform(crs = 3035)

#sediment
habitat <- st_read(dsn = "./fishing/Future prediction/Objects/processed/GreenlandHabitatClasses.kml") %>% 
  st_transform(crs = 3035) %>% 
  subset(select = -c(Description)) #load in sediment map

habitat$Name <- c(1,2,3,4,5,6,7) #rename to numerical
NM_present <- st_join(x = NM_present,y = habitat) #join data with habitat

median_grain_size_mapping <- c(
  "1" = 0.004,  # Bedrock with Mud
  "2" = 0.1,    # Muddy Sand
  "3" = 0.5,    # Gravelly Mud
  "4" = 50,     # Coarse Rocky Ground
  "5" = 0.004,  # Mud
  "6" = 2,      # Gravelly Sand
  "7" = 0.25    # Bedrock with Sand
)
NM_present <- NM_present %>%
  mutate(MedianGrainSize = median_grain_size_mapping[as.character(Name)]) %>% 
  subset(select = -c(Name)) #convert to median grain sizes

NM_habitat <- NM_present

#train 
train_data <- subset(NM_habitat,!is.na(MedianGrainSize)) #extract not NA (to train)
train_geom <- train_data$geometry #save training geom
coords_train <- st_coordinates(train_geom) #get coords
coords_train <- cbind(coords_train, latitude = coords_train[, "Y"], longitude = coords_train[, "X"]) %>% 
  st_drop_geometry() #create new lat lon cols and drop geometry
coords_train <- subset(coords_train,select = -c(X,Y)) #chop out x,y
train_data <- st_drop_geometry(train_data) #drop geom col (for RF)

#test
test_data <- subset(NM_habitat,is.na(MedianGrainSize)) #extract NA (to test)
test_geom <- test_data$geometry #save test geom
coords_test <- st_coordinates(test_geom) #get coords
coords_test <- cbind(coords_test, latitude = coords_test[, "Y"], longitude = coords_test[, "X"]) %>% 
  st_drop_geometry() #create new lat lon cols and drop geometry
coords_test <- subset(coords_test,select = -c(X,Y)) #chop out x,y
test_data <- st_drop_geometry(test_data) #drop geom col (for RF)


rf_model <- ranger(MedianGrainSize ~ ., data = train_data) #train model

predictions <- predict(rf_model, data = test_data)$predictions #predict NAs

train_data <- cbind(train_data,coords_train) #reattach geometry
test_data <- cbind(test_data,coords_test) #reattatch coordinates

test_data$MedianGrainSize <- as.numeric(predictions) #back to numerics

complete <- bind_rows(train_data, test_data) #attach train to test
complete$MedianGrainSize <- as.factor(complete$MedianGrainSize) #to catagorical

NM_present <- complete

NM_present <- NM_present %>% 
  st_as_sf(coords = c("longitude","latitude"),crs = 3035)

coords <- st_coordinates(NM_present) #get coords
coords <- cbind(coords, latitude = coords_train[, "Y"], longitude = coords_train[, "X"]) %>% 
  st_drop_geometry() #create new lat lon cols and drop geometry (better for saving)
NM_present <- cbind(NM_present,coords)
NM_present <- NM_present %>% 
  st_drop_geometry()

write.csv(NM_present,"./fishing/Future prediction/Objects/processed/NM_presentFINAL.csv",
          row.names = FALSE) #save out present day data


### Sediment is the same in the future, so we can just do the climate drivers and join the sediment spatially...
NM_future <- list.files("./fishing/Future Prediction/NEMO output", pattern = "\\.209[2-9]\\.rds$", full.names = TRUE) %>% 
  future_map(.,.f = readRDS) %>%  #read in files - each element is its own month
  bind_rows() %>% 
  group_by(longitude,latitude,Year,Month) %>% 
  summarise(Temperature = mean(Temperature,na.rm = TRUE),
            Salinity = mean(Salinity,na.rm = TRUE),
            Ice_conc = mean(Ice_conc,na.rm = TRUE),
            Ice_Thickness = mean(Ice_Thickness,na.rm = TRUE),
            Snow_Thickness = mean(Snow_Thickness,na.rm = TRUE),
            DIN = mean(DIN,na.rm = TRUE),
            Phytoplankton = mean(Phytoplankton,na.rm = TRUE),
            Bathymetry = mean(Bathymetry,na.rm = TRUE)) %>%  #sorts slab_layer calculation
  ungroup() %>% 
  group_by(Year,Month) %>% 
  mutate(MeanTemperature = mean(Temperature),
         MeanIce = mean(Ice_conc)) %>%  # add mean over whole domain
  st_as_sf(coords = c("longitude","latitude"),crs = 4326) %>% 
  st_transform(crs = 3035) %>% 
  st_join(x = .,y = NM_present)

coords <- st_coordinates(NM_future) #get coords
coords <- cbind(coords, latitude = coords[, "Y"], longitude = coords[, "X"]) %>% 
  st_drop_geometry() #create new lat lon cols and drop geometry
NM_future <- cbind(NM_future,coords)
NM_future <- NM_future %>% 
  st_drop_geometry()

write.csv(NM_future,"./fishing/Future prediction/Objects/processed/NM_futureFINAL.csv",
          row.names = FALSE) #save out present day data

