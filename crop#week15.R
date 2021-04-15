veg <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/vegetable_oil.csv')

library(tidyverse)
library(scales)
library(leaflet)
library(sf)
library(ggthemes)
library(gganimate)
library(showtext)
library(magick)
library(ggplot2)
library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")

dummyd <- crossing(year = unique(veg$year), code = unique(world$iso_a3)) %>% 
  left_join(veg %>% distinct(entity, code)) 
  

veg_impute <- dummyd %>% 
  left_join(veg %>% select(-code), by = c("year", "entity")) %>% 
  distinct()  
veg_join <- world %>% 
  left_join(veg_impute, by = c("iso_a3" = "code")) %>% 
  st_as_sf()
  
 p <- veg_join %>% 
  filter(year == 2008) %>% 
  mutate(cropoil = crop_oil) %>% 
  ggplot(aes(fill = cropoil)) +
  geom_sf(color = "gray", lwd = 0.25) +
  geom_text(data = data.frame(x = -160, y = -40, label = "NA"), 
            aes(x, y, label = label),
            inherit.aes = F, hjust = 0) +
  geom_rect(aes(xmin = -165, xmax = -175, ymin = -35, ymax = -45), inherit.aes = F, fill = "#CEEAF6") +
  
  geom_text(data = data.frame(x = 20, y = -120, 
                              label = ""), 
            aes(x, y, label = label),
            inherit.aes = F, hjust = 0) +  
  labs(fill = NULL,
       x = NULL,
       y = NULL,
       title = "Crop oil production in 2008",caption = "Data from Our World in Data  | Viz by Viktor") +
  theme_pander() +
  theme(legend.position = "bottom",
        axis.text = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.text =  element_text( hjust = 0.5),
        legend.key.width = unit(10, "mm"),
        legend.key.height = unit(3, "mm")
        )+
  transition_states(cropoil, transition_length = 4, state_length = 1) + shadow_mark() +
  enter_grow() 
