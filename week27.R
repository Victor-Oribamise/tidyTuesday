library(ggplot2)
library(treemapify)
library(treemap)
library(ggmap)
library(raster)
library(maptools)
library(randomcoloR)
library(tidyverse)
holidays <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-06/holidays.csv')

holidays %>% group_by(country, month) %>%tally -> tab1

tab1 = tab1[complete.cases(tab1),]
 

tab1=tab1[order(match(tab1$month, month.abb)), ]
 
library(randomcoloR)
n <- 12
mypalette <- distinctColorPalette(n)
mapdata <- map_data("world") %>%
  filter(!(region=="Antarctica")) %>%
  filter(!(region=="Greenland")) %>%
  filter(!(region=="French Southern and Antarctic Lands")) %>% 
  mutate(region = recode(region,
                         USA="United States",
                         UK="United Kingdom"))
ggplot() + 
  geom_map(data = mapdata, map = mapdata, aes(map_id=region), fill="white", color=NA) +
  geom_map(data= tab1, aes(map_id=country, fill=as.factor(month)), map=mapdata) + 
  expand_limits(x = mapdata$long, y = mapdata$lat)+
   ggthemes::theme_map() +
  theme(
           
        plot.title = element_text(size=12, hjust = 0.5),
        plot.background = element_rect(fill = "white", color=NA),
        legend.position = "bottom",
        legend.justification = "center",
        legend.background=element_blank(),
        legend.key = element_rect(colour = NA, fill = NA), 
        legend.box.background = element_blank(),
        legend.text=element_text(size=rel(1)))  +
  	guides(fill=guide_legend(title="", ncol=6)) +
  ggtitle("Independence day celebration by months") +
  labs(caption = "Data from Wikipedia and Isabella | Viz by Viktor") +
  theme(plot.caption= element_text(size=12, vjust=0.2))+ 
  scale_fill_manual(values=mypalette)
