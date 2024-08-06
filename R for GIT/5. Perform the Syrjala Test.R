#syrjala test for statistical significance of the distribution of two populations
library(ecespa)
library(dplyr)
library(tictoc)
set.seed(710)
tic()
habitat_model <- readRDS("./fishing/Future Prediction/Future Parametrisation/Objects/FinalModel.RDS")#load model

NM <- read.csv("./fishing/Future prediction/Objects/processed/NM_presentFINAL.csv",header = TRUE)
year <- seq(2012,2019)
months <- seq(1,12)
NM_coords <- data.frame(fishing = NM$fishing)
NM_coords$fishing <- ifelse(NM_coords$fishing > 0, 1, 0) #convert to binary
NM_coords_vector <- as.vector(NM_coords[,1])
NM_predictors <- NM %>% subset(select = c(Salinity,Temperature,Ice_Thickness,
                                          Bathymetry,Ice_conc,DIN,Snow_Thickness,
                                          MeanTemperature,RoundedGrainSize,
                                          Phytoplankton)) #extract predictors only


fishing_pred <- dismo::predict(object = habitat_model,x = NM_predictors) #make future predictions
binary_fishing_pred <- ifelse(fishing_pred < 0.5,0,1)


NM$fishing_pred <- binary_fishing_pred
p_values <- vector(length = 96)
for (i in seq_along(years)) {
  for (j in seq_along(months)) {
    index <- (i - 1) * length(months) + j #index for storing
    tmp <- filter(NM,Year == year[i],Month == months[j])
    pred_act <- data.frame(prediction = tmp$fishing_pred,
                           actual = ifelse(tmp$fishing > 0,1,0))
    coords <- data.frame(x = tmp$longitude,
                         y = tmp$latitude)
    var1 <- pred_act$prediction
    var2 <- pred_act$actual
    stest <- syrjala(coords,var1,var2,999)
    #p value is the proportion of simulated greater than or equal to the observed
    p_val <- length(stest[stest$ks.sim >= stest$ks.obs])/length(stest$ks.sim)
    p_values[index] <- p_val#save p-value
    print(paste0("Finished Month ",j," Year ",year[i]))
  }
}


## PLOT
vline_positions <- seq(12, 12 * 8, 12)

month_labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

#repeat the labels 8 times
all_month_labels <- rep(month_labels, 8)
ggplot(data = month_p,aes(x = month,p_val)) +
  geom_line(color = "#E7771E") +
  geom_point(color = "#E7771E") +
  geom_hline(yintercept = 0.05,linetype = "dashed") +
  geom_vline(xintercept = vline_positions,linetype = "longdash") +
  scale_x_continuous(breaks = seq(1, 96), labels = all_month_labels)+
  scale_y_continuous(breaks = c(0,0.05,0.25,0.5,0.75,1),labels = c(0,0.05,0.25,0.5,0.75,1)) +
  labs(x = "Month",y = "P Value") +
  theme(panel.grid.major.x = element_line(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("./fishing/Future Prediction/Figures/changingPvalues.png",width = 33.867,height = 19.05,units = "cm")
