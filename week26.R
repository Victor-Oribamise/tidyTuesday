library(ggplot2)
library(dplyr)
library(reshape2)
library(ggraph)
library(igraph)
library(tidyverse)
library(RColorBrewer) 
library(ggdendro)
library(maps)
library(viridis)
library(rgdal)
library(plyr)
library(randomcoloR)

animal_rescues <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-29/animal_rescues.csv')
animal_rescues %>% group_by(borough,property_type, incident_notional_cost, latitude,longitude, animal_group_parent) %>%tally -> tab1

tab1= tab1[complete.cases(tab1), ]

tab2 = tab1 %>% group_by(latitude,longitude,borough,property_type, incident_notional_cost, animal_group_parent)%>%tally

tab2=tab2%>%
   filter(incident_notional_cost != "NULL" & longitude !="NULL" & latitude != "NULL" )

tab2=tab2%>%
   filter(incident_notional_cost != "NULL" & longitude !="0" & latitude != "0" )
tab2 = tab2%>% 
	filter(animal_group_parent !="Unknown - Animal rescue from below ground - Farm animal"& animal_group_parent !="Unknown - Animal rescue from water - Farm animal" & animal_group_parent !="Unknown - Domestic Animal Or Pet" & animal_group_parent !="Unknown - Heavy Livestock Animal"& animal_group_parent != "Unknown - Wild Animal")



UK <- map_data("world") %>% filter(region=="UK")

colnames(tab2)[1] = c("lat")
colnames(tab2)[2] = c("long")
colnames(tab2)[3] = c("name")
colnames(tab2)[5] = c("incident_nat_cost")
shape <- readOGR(dsn="~\spatialggplot", layer="london_sport")
proj4string(shape) <- CRS("+init=epsg:27700")
shape.f <- fortify(shape, region = "ons_label")
shape.f <- merge(shape.f, shape@data, by.x = "id", by.y = "ons_label")


shape.f = left_join(tab2, shape.f, by ="name")

c23 <- c(
  "dodgerblue2", "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "black", "gold1",
  "skyblue2", "#FB9A99", # lt pink
  "palegreen2",
  "#CAB2D6", # lt purple
  "#FDBF6F", # lt orange
  "gray70", "khaki2",
  "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
  "darkturquoise", "green1", "yellow4", "yellow3")
  
ggplot(shape.f, aes(long.y, lat.y, group = group, fill = animal_group_parent)) + geom_polygon() + 
    coord_equal() + labs(x = "Easting (m)", y = "Northing (m)", fill = "", caption="Data from london.gov | Viz by Viktor") + 
    ggtitle("Types of animals rescued in London by boroughs")+
   theme_void()+
   theme(legend.position = "bottom",
        #axis.text = element_blank(),
        text = element_text(color = "navy", size=12),
        plot.title = element_text(hjust = 0.3),
        plot.subtitle = element_text(hjust = 0.5),
        legend.text =  element_text( hjust = 0.5),
		legend.key.width = unit(5, "mm"),
		legend.key.height = unit(5, "mm"), 
		legend.margin=margin(0,0,0,0),
		legend.box.margin=margin(10,10,20,10),
		legend.title = element_blank(),
		plot.caption = element_text(size= 10, hjust=0.95, color = "navy"),
    plot.background = element_rect(fill = "cornsilk2", color = NA))+
  scale_fill_manual(values=c23)
