## calculate proportion of sediment in the domain that is not covered by ice
#want x axis to be months, y axis to be proportion of sediment
#color of line to be sediment type
#2 cols with decades 2010s 2090s
rm(list = ls())
library(tidyverse)
grain_size_categories <- function(grain_size) {
  if (grain_size >= 256) {
    return("Rock")
  } else if (grain_size >= 2) {
    return("Gravel")
  } else if (grain_size >= 0.0625) {
    return("Sand")
  } else {
    return("Mud")
  }
}
NM_2010s <- read.csv("./fishing/Future prediction/Objects/processed/NM_presentFINAL.csv",header = TRUE) %>% 
  subset(select = c(Month,Year,Ice_conc,RoundedGrainSize))
NM_2090s <- read.csv("./fishing/Future prediction/Objects/processed/NM_futureFINAL.csv",header = TRUE) %>% 
  subset(select = c(Month,Year,Ice_conc))
NM_2090s$RoundedGrainSize <- NM_2010s$RoundedGrainSize

### now let's filter out ice
NM_2010s <- NM_2010s %>% 
  filter(Ice_conc < 0.15)

NM_2090s <- NM_2090s %>% 
  filter(Ice_conc < 0.15)

## PRESENT
present_summary <- NM_2010s %>%
  group_by(Year, Month, RoundedGrainSize) %>%
  summarise(count = n()) %>%
  ungroup()

present_proportion <- present_summary %>%
  group_by(Month, RoundedGrainSize) %>%
  summarise(avg_count = mean(count)) %>%
  ungroup() %>% 
  st_drop_geometry() %>% 
  mutate(Category = sapply(RoundedGrainSize, grain_size_categories))

summarised_present <- present_proportion %>%
  group_by(Month, Category) %>%
  summarise(
    avg_count = sum(avg_count, na.rm = TRUE),
    .groups = 'drop'
  )

total_avg_count <- summarised_present %>%
  group_by(Month) %>%
  summarise(total_avg_count = sum(avg_count, na.rm = TRUE))

normalised_present <- summarised_present %>%
  left_join(total_avg_count, by = "Month") %>%
  mutate(proportion = avg_count / total_avg_count) %>%
  subset(select = c(-total_avg_count))

normalised_present$period <- "2010s"
## FUTURE
future_summary <- NM_2090s %>%
  group_by(Year, Month, RoundedGrainSize) %>%
  summarise(count = n()) %>%
  ungroup()

future_proportion <- future_summary %>%
  group_by(Month, RoundedGrainSize) %>%
  summarise(avg_count = mean(count)) %>%
  ungroup() %>% 
  st_drop_geometry() %>% 
  mutate(Category = sapply(RoundedGrainSize, grain_size_categories))

summarised_future <- future_proportion %>%
  group_by(Month, Category) %>%
  summarise(
    avg_count = sum(avg_count, na.rm = TRUE),
    .groups = 'drop'
  )

total_avg_count <- summarised_future %>%
  group_by(Month) %>%
  summarise(total_avg_count = sum(avg_count, na.rm = TRUE))

normalised_future <- summarised_future %>%
  left_join(total_avg_count, by = "Month") %>%
  mutate(proportion = avg_count / total_avg_count) %>%
  subset(select = c(-total_avg_count))

normalised_future$period <- "2090s"
all <- rbind(normalised_present,normalised_future)

ggplot() + 
  geom_line(data = all,aes(x = Month,y = proportion,color = as.character(Category))) +
  geom_point(data = all,aes(x = Month,y = proportion,color = as.character(Category)),size = 0.5) +
  scale_x_continuous(limits = c(1,12),breaks = seq(1,12),labels = c("Jan", "Feb", "Mar", "Apr", "May",
                                                                    "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  scale_y_continuous(limits = c(0,0.7),breaks = seq(0,0.7,0.1)) +
  
  labs(x = "Month",y = "Domain Sediment Proportion",color = "Sediment Class") +
  facet_wrap(~period,ncol = 2) +
  scale_color_manual(labels = c("Gravel" = "Gravel (2 - 256mm)",
                                "Sand" = "Sand (0.0625 - 2mm)",
                                "Mud" = "Mud (< 0.0625mm)"),
                     values = c("Gravel" = "grey15", "Mud" = "#d55e00", "Sand" = "#f4b942"),
                     breaks = c("Mud","Sand","Gravel")) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10,face = "bold"),
        axis.text = element_text(size = 8),
        legend.title = element_text(size = 10,face = "bold"),
        legend.text = element_text(size = 8),
        legend.position = "top",
        strip.background = element_rect(color = "black",fill = "white"),
        strip.text = element_text(size = 12,face = "bold")) +
  NULL
ggsave("./fishing/Future Prediction/Paper/5 figures/sedimentproportions.tiff",
       dpi = 1200,width = 17,unit = "cm",height = 29.7/4)
