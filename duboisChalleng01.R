dub1 <- read.csv()
library(reshape2)
library(ggplot2)
p=ggplot(dub1, aes(Year)) +
geom_line(aes(y = Colored, linetype = "Colored")) +
geom_line(aes(y = White, linetype = "White")) +
scale_linetype_manual(values = c(Colored = 1, White = 5)) +
guides(linetype = guide_legend(keywidth = unit(50, "pt"))) +
theme(legend.position = "bottom") +
coord_flip()+
scale_y_reverse(breaks = seq(0, 100, 5),
                  minor_breaks = 0,
                  limits = c(100,0)) +
scale_linetype(name = NULL) +labs(x="", y ="PERCENTS")+  theme(legend.title = element_blank())+
scale_x_continuous(breaks= c(1790,1800,1810,1820,1830,1840,1850,1860,1870,1880,1890), labels = c(1790,1800,1810,1820,1830,1840,1850,1860,1870,1880,1890))
p+ theme(panel.background = element_rect(fill='#e8cfb3', colour='black'), panel.grid = element_line(colour = "red",size=0.05), plot.background = element_rect("#e8cfb3"), legend.key = element_rect("#e8cfb3")) + ggtitle("COMPARATIVE INCREASE OF WHITE AND COLORED\nPOPULATION OF GEORGIA.")+theme(plot.title=element_text(hjust=0.5))


p + scale_y_discrete(breaks = c(0,100,78,42,45,30,35,20,22,36,21, 90, 46, 30, 55, 34, 31, 16, 11, 34, 20), labels= c(100, 95, 90, 85, 80, 75, 70, 65, 60, 55, 50, 45, 40, 35, 30, 25, 20, 15, 10, 5, 0))
