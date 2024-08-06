rm(list = ls())
library(sf)
library(tidyverse)
library(rnaturalearth)
library(rmaxent)
library(ggplot2)
library(viridis)
library(cowplot)
library(furrr)
library(purrr)
library(dismo)

### MAPS
domain <- readRDS("./fishing/Future Prediction/Objects/processed/Buffered Domain.rds") %>% 
  st_transform(crs = 3035)
basemap <- ne_countries(scale = "large", country = "Greenland", returnclass = "sf") %>% 
  st_transform(crs = 3035)

### MODEL AND DATA
habitat_model <- readRDS("./fishing/Future Prediction/Future Parametrisation/Objects/FinalModel.RDS")
NM_present <- read.csv("./fishing/Future prediction/Objects/processed/NM_presentFINAL.csv",header = TRUE) %>% 
  st_as_sf(coords = c('X','Y'),crs = 3035) #load present
NM_future <- read.csv("./fishing/Future prediction/Objects/processed/NM_futureFINAL.csv") %>% 
  st_as_sf(coords = c('X', 'Y'), crs = 3035) #load future

#what vars are we using?
columns <- c("Salinity", "Temperature", "Ice_Thickness", "Bathymetry", "Ice_conc",
             "DIN","MeanTemperature","Snow_Thickness","MeanIce","Phytoplankton",
             "RoundedGrainSize")


#predict for given
predict_fishing_model <- function(data, year, month) {
  data %>%
    filter(Year == year, Month == month) %>%
    mutate(fishing_model = rmaxent::project(lambdas = habitat_model, newdata = .)$prediction_logistic) %>%
    subset(select = c(geometry, fishing_model)) #need to fix, data is giving the non-filtered version so need to save in var and then pass that in :)
}

#calculate averages and produce maps that are the bottom panel of figure 5
process_all_predictions <- function(data, years, months, save = FALSE) {
  results <- list()
  
  for (month in months) {
    monthly_predictions <- lapply(years, function(year) predict_fishing_model(data, year, month)) %>%
      bind_rows()
    
    #calc average over all years
    average_fishing_model <- monthly_predictions %>%
      group_by(geometry) %>%
      summarise(average_fishing_model = median(fishing_model, na.rm = TRUE)) %>%
      ungroup()
    
    #back to sf
    average_df <- st_as_sf(average_fishing_model)
    results[[paste0("Month_", month)]] <- average_df
    
    if (save) { #save?
      plot <- ggplot() +
        geom_sf(data = average_df, aes(color = average_fishing_model)) +
        scale_color_viridis_c(limits = c(0, 1)) +
        geom_sf(data = basemap, fill = "grey90", color = "black") +
        labs(color = "Probability of Fishing", x = "Longitude", y = "Latitude") +
        theme(panel.background = element_rect(fill = 'white'),
              panel.grid = element_line(color = 'grey80'),
              axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              legend.position = "none",
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank()) +
        coord_sf(xlim = c(1400000, 2500000), ylim = c(5200000, 6890000)) +
        NULL
      
      ggsave(filename = paste0("./fishing/Future Prediction/Paper/5 figures/fishing probability/future/new/Fishing probability Month_", month, " 2010s.png"),
             plot = plot, width = 16.9335, height = 9.525, units = "cm")
    }
  }
  
  results
}

all_predictions_present <- process_all_predictions(NM_present,years = 2012:2019, months = 1:12,save = FALSE) #apply to present
all_predictions_future <- process_all_predictions(NM_future,years = 2092:2099, months = 1:12,save = FALSE) #apply to future

#calc differences function
calculate_difference <- function(present, future) {
  differences <- lapply(seq_along(present), function(i) {
    present_avg <- present[[i]]$average_fishing_model
    future_avg <- future[[i]]$average_fishing_model
    data.frame(present_avg = present_avg,
               future_avg = future_avg,
               difference = future_avg - present_avg,
               geometry = present[[i]]$geometry)
  })
  return(differences)
}

differences <- calculate_difference(all_predictions_present,all_predictions_future)
combined_differences <- bind_rows(
  lapply(1:12, function(i) {
    df <- differences[[i]]
    df$month <- i
    return(df)
  })
)

### FILLED VIOLIN
p <- ggplot() +
  geom_violin(data = combined_differences,aes(x = as.factor(month),y = difference))
mywidth <- 0.45 #might take some trial and error
vl_fill <- data.frame(ggplot_build(p)$data) %>%
  mutate(xnew = x- mywidth*violinwidth, xend = x+ mywidth*violinwidth)
vl_poly <- vl_fill %>%
  subset(select = c(xnew, xend, y, group)) %>%
  pivot_longer(-c(y, group), names_to = "oldx", values_to = "x") %>% 
  arrange(y) %>%
  split(., .$oldx) %>%
  map(., function(x) {
    if(all(x$oldx == "xnew")) x <- arrange(x, desc(y))
    x
  }) %>%
  bind_rows()

p1 <- ggplot() +
  geom_hline(yintercept = 0,linetype = "dashed") +
  geom_polygon(data = vl_poly, aes(x, y, group = group), 
               color= "black", linewidth = 1, fill = NA) +  
  geom_segment(data = vl_fill, aes(x = xnew, xend = xend, y = y, yend = y,
                                   color = y)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "orange", midpoint = 0,
                        limits = c(-0.25, 0.7)) +
  labs(x = "Month",
       y = "Change in Habitat Suitability",
       color = "Change in Habitat Suitability") +
  scale_x_discrete(breaks = seq(1, 12),
                   labels = c("Jan", "Feb", "Mar",
                              "Apr", "May", "Jun",
                              "Jul", "Aug", "Sep",
                              "Oct", "Nov", "Dec")) +
  geom_violin(data = combined_differences,aes(x = as.factor(month),y = difference,),fill = NA,
              draw_quantiles = c(0.05,0.5,0.95)) +
  theme_classic() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14, face = "bold"),
        axis.title.x = element_blank(),
        legend.position = c(0.73, 0.85),
        legend.direction = "horizontal",
        legend.title = element_text(angle = 0, size = 10, face = "bold"),  # Customize legend title
        legend.text = element_text(size = 10)) +
  guides(color = guide_colourbar(title.hjust = 0.5, title.position = "top",
                                title.theme = element_text(angle = 0, size = 12,face = "bold"),
                                label.theme = element_text(size = 14),
                                barheight = 0.5, barwidth = 15)) +

  NULL

ggsave(paste0("./fishing/Future Prediction/Paper/5 figures/fishing probability/change/filled violin.tiff"),
        dpi = 1200,plot = p1)