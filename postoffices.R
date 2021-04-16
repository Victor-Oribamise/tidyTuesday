post_offices <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-13/post_offices.csv')
library(ggplot2) 
library(usmap)
library(dplyr)
library(rgeos)


post_offices %>% filter(is.na(discontinued)) %>% group_by(state) %>%tally -> tab1

colnames(tab1)[1] <- "state"
colnames(tab1)[2] <- "value"

centroid_labels <- utils::read.csv(system.file("extdata", paste0("us_", "states", "_centroids.csv"), package = "usmap"), stringsAsFactors = FALSE)


data_labels <- centroid_labels %>%  
  left_join(. , tab1, by = c("abbr" = "state"))



plot_usmap(data=tab1, values="value", color='cyan1', labels=T) + 
  geom_text(data = data_labels, ggplot2::aes(x = x, y = y,label = ''), color = "gray")+
  labs(title = "US post offices count per states", caption = 'Data from Blevins & Helbock, 2021, Harvard Dataverse | viz by Viktor', size=22, fill="Count of US post offices per states" ) +
  theme(legend.position = "right",
        text = element_text(color = "navy", size=12),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.text =  element_text( hjust = 0.8),
        legend.key.width = unit(3, "mm"),
        legend.key.height = unit(10, "mm"))
