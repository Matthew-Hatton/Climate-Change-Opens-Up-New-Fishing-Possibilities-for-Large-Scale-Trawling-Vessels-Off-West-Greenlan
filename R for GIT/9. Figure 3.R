### PERMUTATION IMPORTANCE

library(rmaxent)
basemap <- ne_countries(country = "Greenland",scale = "large",returnclass = "sf") %>% 
  st_transform(crs = 4326)
habitat_model <- readRDS("./fishing/Future Prediction/Future Parametrisation/Objects/FinalModel.RDS")

permutation_importance <- data.frame(Variable = c("Maximum Depth",
                                                  "Ice Thickness",
                                                  "Ice Concentration",
                                                  "DIN",
                                                  "Phytoplankton",
                                                  "Snow Thickness",
                                                  "Salinity",
                                                  "Mean Domain\n Ice Concentration",
                                                  "Temperature",
                                                  "Mean Domain\n Temperature",
                                                  "Sediment"),
                                     Permutation_Importance = c(28.2,
                                                                27.3,
                                                                11.5,
                                                                1,
                                                                6.9,
                                                                7.6,
                                                                3.4,
                                                                6.6,
                                                                5.3,
                                                                1.9,
                                                                0.2)) #values extracted from model (just type habitat_model into console)
#reorder for nicer plot
permutation_importance$Variable <- factor(permutation_importance$Variable, 
                                          levels = permutation_importance$Variable[order(permutation_importance$Permutation_Importance)])

ggplot(permutation_importance, aes(x = Variable, y = Permutation_Importance)) +
  geom_bar(stat = "identity",fill = "orange",color = "black") +
  coord_flip() +  #make horizontal
  scale_y_continuous(expand = c(0,0)) +
  xlab("Variable") +
  ylab("Permutation Importance (%)") +
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        axis.text = element_text(face = "bold",size = 8),
        axis.title.x = element_text(face = "bold",size = 12))
ggsave("./fishing/Future Prediction/Paper/5 figures/permutation importance.tiff",
       width = 17/2,dp = 1200,unit = "cm",background = "white")

