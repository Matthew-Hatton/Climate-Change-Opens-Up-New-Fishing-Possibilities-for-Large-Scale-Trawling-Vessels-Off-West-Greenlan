rm(list = ls()) #reset
options(java.parameters = "-Xmx2000m")
library(dismo)
library(rJava)
library(tidyverse)
library(sf)

NM <- read.csv("./fishing/Future prediction/Objects/processed/NM_presentFINAL.csv",header = TRUE) #load data
cutoff <- seq(0.1,1,0.1) #test different cutoffs
years <- seq(2012,2019)
NM$fishing <- ifelse(NM$fishing > 0, 1, 0) #convert to binary (values below 1 hour filtered out)
partition <- split(NM,NM$Year)
#empty lists for storing results
result_list <- list()
percentage_test_list <- list()
percentage_train_list <- list()

for (i in seq_along(partition)) {
  print(paste0(i,"/",length(seq_along(partition))))
  test <- partition[[i]] #choose test set
  train <- do.call(rbind, partition[-i]) #choose training set
  
  #prep data for model
  NM_coords <- data.frame(fishing = train$fishing) #get fishing data for training
  NM_coords_vector <- as.vector(NM_coords[,1]) #...as a vector
  NM_predictors <- train %>% subset(select = -c(latitude,longitude,Year,Month,Sediment,
                                                distance_to_N,distance_to_S,distance_to_Q,fishing)) #define predictors for NEMO experiment
  #run model
  habitat_model <- maxent(x = NM_predictors,p = NM_coords_vector) #run model
  result_list[[i]] <- habitat_model@results #save results *AUC/permutation importance*
  
  #test model on unseen data
  test_fishing <- test$fishing #store fishing for check
  test_validation <- test %>% subset(select = -c(latitude,longitude,Year,Month,Sediment,
                                                 distance_to_N,distance_to_S,distance_to_Q,fishing)) #pull validation set
  prediction <- predict(object = habitat_model,x = test_validation) #predict fishing - probability of fishing
  percentage_matching_test <- data.frame(year = rep(years[i],length(cutoff)),
                                                    cutoffs = cutoff,
                                                    percentage = rep(1,length(cutoff)))
  #calculate percentage for test
  for (j in seq_along(cutoff)) {
    rounded_prediction <- ifelse(prediction < cutoff[j], 0, 1) #convert to binary
    validation_df <- data.frame(actual = test_fishing,#round prediction based on cutoff
                                prediction = rounded_prediction) #creates actual v prediction
    matching_entries <- sum(validation_df$actual == validation_df$prediction) #how many of these entries match?
    total_entries <- nrow(validation_df)
    percentage_matching <- (matching_entries / total_entries) * 100 #and what is that as a percentage?
    percentage_matching_test$percentage[j] <- percentage_matching
  }
  percentage_test_list[[i]] <- percentage_matching_test
  
  #test model on training data
  train_fishing <- train$fishing #store fishing for check
  train_validation <- train %>% subset(select = -c(latitude,longitude,Year,Month,Sediment,
                                                   distance_to_N,distance_to_S,distance_to_Q,fishing)) #pull validation set
  prediction <- predict(object = habitat_model,x = train_validation) #predict fishing - probability of fishing
  percentage_matching_train <- data.frame(year = rep(years[i],length(cutoff)),
                                         cutoffs = cutoff,
                                         percentage = rep(1,length(cutoff)))
  #calculate percentage for test
  for (j in seq_along(cutoff)) {
    rounded_prediction <- ifelse(prediction < cutoff[j], 0, 1) #round prediction based on cutoff
    validation_df <- data.frame(actual = train_fishing,
                                prediction = rounded_prediction) #creates actual v prediction
    matching_entries <- sum(validation_df$actual == validation_df$prediction) #how many of these entries match?
    total_entries <- nrow(validation_df)
    percentage_matching <- (matching_entries / total_entries) * 100 #and what is that as a percentage?
    percentage_matching_train$percentage[j] <- percentage_matching
  }
  percentage_train_list[[i]] <- percentage_matching_train
}
saveRDS(result_list,"./fishing/Future Prediction/Objects/k-fold model results NEMO-ice.RDS")
saveRDS(percentage_test_list,"./fishing/Future Prediction/Objects/k-fold percentage matching test data NEMO-ice.RDS")
saveRDS(percentage_train_list,"./fishing/Future Prediction/Objects/k-fold percentage matching train data NEMO-ice.RDS")