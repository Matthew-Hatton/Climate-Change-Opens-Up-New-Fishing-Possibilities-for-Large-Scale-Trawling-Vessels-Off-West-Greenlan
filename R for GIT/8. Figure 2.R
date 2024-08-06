rm(list = ls())
library(tidyverse)
library(sf)
library(ggtext)

x_2010 <- as.Date("2010-01-01")
x_2019 <- as.Date("2019-01-01")
x_2040 <- as.Date("2040-01-01")
x_2049 <- as.Date("2049-01-01")
x_2090 <- as.Date("2090-01-01")
x_2099 <- as.Date("2099-01-01")

TS <- readRDS("Objects/TS.rds") %>% 
  filter(Compartment != "Offshore D") %>% 
  group_by(Year) %>%
  summarise(
    Salinity_avg = mean(Salinity_avg, na.rm = TRUE),
    Temperature_avg = mean(Temperature_avg, na.rm = TRUE),
    DIN_avg = mean(DIN_avg, na.rm = TRUE),
    Phytoplankton_avg = mean(Phytoplankton_avg, na.rm = TRUE),
    Ice_Thickness_avg = mean(Ice_Thickness_avg, na.rm = TRUE),
    Ice_conc_avg = mean(Ice_conc_avg, na.rm = TRUE),
    Snow_Thickness_avg = mean(Snow_Thickness_avg,na.rm = TRUE)
  )


col_grid <- rgb(235, 235, 235, 100, maxColorValue = 255)
TS_long <- TS %>%
  pivot_longer(cols = -Year, names_to = "variable", values_to = "value")

p1 <- ggplot(TS_long, aes(x = Year, y = value)) +
  geom_smooth(color = "darkslategrey",alpha = 0.7,se = FALSE,linewidth = 0.3) +
  geom_line(size = 0.2, color = "#6332CD") +  
  facet_wrap(~factor(variable,levels = c('DIN_avg',"Phytoplankton_avg",
                                         "Ice_Thickness_avg","Ice_conc_avg","Snow_Thickness_avg",
                                         "Temperature_avg","Salinity_avg")), scales = "free_y", ncol = 1, 
             strip.position = "left", 
             labeller = as_labeller(c(DIN_avg = "DIN\n (mmol N/m³)",
                                      Ice_conc_avg = "Ice\n Concentration\n (fraction)", Ice_Thickness_avg = "Ice\n Thickness\n (m)",
                                      Snow_Thickness_avg = "Snow\n Thickness (m)",
                                      Phytoplankton_avg = "Phytoplankton\n (mmol N/m³)",
                                      Temperature_avg = "Temperature\n (°C)", Salinity_avg = "Salinity\n (ppt)"))) +
  scale_x_continuous(breaks = c(1975,2012,2019,2092,2099),
                     labels = c(1975,2012,2019,2092,2099)) +
  labs(y = "Monthly domain average") +
  theme(strip.placement = "outside",
        strip.text.y = element_text(size = 6, colour = "black", angle = 0),
        panel.spacing = unit(5, "mm", data = NULL),
        panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(size = 10,angle = 45,vjust = 0.65),
        axis.text.y = element_text(size = 8),
        strip.background = element_blank(),
        panel.grid.major = element_line(color = col_grid),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(size = 0.5))  # Place facet labels outside the plot area
p1$layers <- c(geom_rect(xmin = 2012,xmax = 2019,ymin = -Inf,ymax = Inf, fill = "grey90",alpha = 0.3,color = "NA"),
                geom_rect(xmin = 2092,xmax = 2099,ymin = -Inf,ymax = Inf, fill = "grey90",alpha = 0.3,color = "NA"),
                p1$layers)
p1 

ggsave("./fishing/Future Prediction/Paper/5 figures/time series.tiff",
       width = 17,dp = 1200,unit = "cm")

