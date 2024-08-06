rm(list = ls()) #reset

basemap <- ne_countries(country = "Greenland",scale = "large",returnclass = "sf") %>% 
  st_transform(crs = 3035)
NM_2010s <- read.csv("./fishing/Future prediction/Objects/processed/NM_presentFINAL.csv",header = TRUE) %>% 
  subset(select = c(Month,Year,Ice_conc,Ice_Thickness,X,Y)) %>% 
  mutate(Ice_volume = Ice_conc*Ice_Thickness)
NM_2090s <- read.csv("./fishing/Future prediction/Objects/processed/NM_futureFINAL.csv",header = TRUE) %>% 
  subset(select = c(Month,Year,Ice_conc,Ice_Thickness,X,Y)) %>% 
  mutate(Ice_volume = Ice_conc*Ice_Thickness)

NM_difference <- data.frame(Month = NM_2010s$Month,
                            X = NM_2010s$X,
                            Y = NM_2010s$Y,
                            Ice_volume = NM_2090s$Ice_volume - NM_2010s$Ice_volume) %>% 
  st_as_sf(coords = c('X','Y'),crs = 3035)

tolerance <- 0.001

# Create the new column in the data frame
NM_difference <- NM_difference %>%
  mutate(Ice_volume_category = case_when(
    Ice_volume > tolerance ~ 1,
    Ice_volume < -tolerance ~ -1,
    TRUE ~ 0
  ))

ggplot() +
  geom_sf(data = NM_difference,aes(color = as.character(Ice_volume_category)),size = 0.1) +
  geom_sf(data = basemap,fill = "white",color = "black") + #greenland
  facet_wrap(~Month) +
  scale_color_manual(values = c("1" = "green",
                                "0" = "white",
                                "-1" = "red")) +
  labs(color = "Change in Ice Volume") +
  coord_sf(xlim = c(1450000, 2500000), ylim = c(5150000, 6670000)) +
  guides(colour = guide_legend(override.aes = list(size=5)))

ggsave("./fishing/Future Prediction/Paper/5 figures/ice volume change.tiff",
       dpi = 1200,unit = "cm")
