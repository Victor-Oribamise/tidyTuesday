library(readr)
library(ggplot2)
library(dplyr)

earn <- read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/earn.csv')

earn2<-
  earn %>%
  filter(age == "25 to 54 years", year == c(2010, 2020)) %>%
  filter(race != "All Races", sex != "Both Sexes") %>%
  group_by(year,sex,race,age) %>%
  summarise(means = mean(median_weekly_earn)) %>%
  mutate('race.sex' = paste(as.character(race), as.character(sex)))



legend_title <- ""

p=earn2 %>% ggplot(aes(x=year, y=means, colour=interaction(race, sex))) %>%
  +geom_point() %>%
  + geom_line(aes(group = interaction(race,sex))) %>%
  + expand_limits(y=0) %>%
  #+ scale_x_continuous("Year") %>%
  + scale_y_continuous("Income mean", labels = scales::dollar_format()) %>%
  + theme_bw() %>%
  + theme(panel.grid.major.x = element_blank()) %>%
  + xlab("Year") %>%
  + scale_colour_manual(legend_title,values=c("Darkblue", "Red", "Orange", "Green", "Purple", "Brown"), breaks = c("Asian.Men", "Black or African American.Men", "White.Men", "Asian.Women", "Black or African American.Women", "White.Women"), labels = c("Asian Men", "Black or African American Men", "White Men", "Asian Women", "Black or African American Women", "White Women")) %>%
  + scale_x_continuous(breaks =as.numeric(as.character(c("2010.0", "2012.5", "2015.0", "2017.5", "2020.0"))), labels=as.numeric(as.character(c("2010", "2012", "2015", "2017", "2020")))) %>%
  + labs(title = "Gender pay gap by race in the US from 2010-2020.",caption = "Data from BLS  | Viz by Viktor")


p+ theme(panel.background = element_rect(fill='#e8cfb3', colour='black'), panel.grid = element_line(colour = "gray",size=0.0), plot.background = element_rect("#e8cfb3"), legend.key = element_rect("#e8cfb3"))
