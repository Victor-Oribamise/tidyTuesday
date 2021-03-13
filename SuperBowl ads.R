library(ggplot2)


tuesdata <- tidytuesdayR::tt_load('2021-03-02')
youtube <- tuesdata$youtube

youtube %>% count(year, patriotic) %>% filter(patriotic)

youtube_radar <- youtube %>%
  select(-favorite_count, -kind, -etag) %>%
  group_by(brand) %>%
  summarize(Funny = mean(funny),
            `Quick Product` = mean(show_product_quickly),
            Patriotic = mean(patriotic),
            Celebrity = mean(celebrity),
            Danger = mean(danger),
            Animals = mean(animals),
            Sex = mean(use_sex),
            Count = n()) %>%
  slice_max(n = 9, order_by = Count, with_ties = FALSE)


library(reshape)


library(data.table)
setDT(youtube_radar)
df = youtube_radar

df[,c("Count")] <- list(NULL)
new = melt(df, id.vars = c("brand"))

p=ggplot(new, aes(x=brand, y=value, fill=variable)) + 
  geom_bar(stat="identity",col="black",width=1)  + 
  coord_polar(start = pi/3) + theme_bw() + theme(axis.text.y = element_blank()) + 
  theme(axis.ticks = element_blank(), axis.title = element_blank()) +
  labs(face="bold",fill = 'Ad types',title = "Superbowl Ads from 2000-2020.",caption = "Data from 538  | Viz by Viktor")

p+ theme(panel.background = element_rect(fill='lightblue', colour='black'), panel.grid = element_line(colour = "gray",size=0.1), plot.background = element_rect("white"), legend.key = element_rect("white"), axis.text.x = element_text(size = 8, family = "Arial",face = 'bold'))
