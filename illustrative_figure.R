library(tidyverse)

###SET WORKING DIRECTORY###
setwd("")

###READ IN DATA (if applicable)###
predict.mean <- read.csv("predict.mean.csv", header = T, stringsAsFactors = F) #requires mean prediction data

###Figure 1###
#uses data from mean predictions using coefficients from model 3 and subsets to Browsers for demonstration purposes
#set group colours
group.colours <- c(Croppers = "#EEDD88", Browsers = "#EE8866",  Farmers = "#77AADD", Scrapers = "#99DDFF", Excavators = "#44BB99")

g <- ggplot()+
  geom_line(data = subset(predict.mean, predict.mean$Group=="Browsers"), aes(x=P1, y=richness, col = Group), size = 3)+
  geom_line(data = subset(predict.mean, predict.mean$Group=="Browsers"), aes(x=P1, y=richnessi), size = 3, col = "darkgrey")+
  geom_line(data = subset(predict.mean, predict.mean$Group=="Browsers"), aes(x=P1, y=richnessid, col = Group), size = 3, linetype = "dotted")+
  scale_y_continuous(name = "Ecosystem Function", breaks = c(0, 4, 8, 12), limits = c(2.5,12))+
  geom_segment(aes(x = 0, y = 8.715, xend = 1, yend = 8.715), colour = "black", size = 3)+ #null
  geom_segment(aes(x = 1, y = 2.5, xend = 1, yend = 4.6), colour = "black", size = 1.1,
               arrow = arrow(length = unit(0.02, "npc"), type = "closed", ends = "both"))+ #identity effect at P=1
  geom_segment(aes(x = 0.42, y = 4.56, xend = 0.42, yend = 10.43), colour = "black", size = 1,
               arrow = arrow(length = unit(0.02, "npc"), type = "closed", ends = "both"))+ #interaction effect
  geom_segment(aes(x = 0.42, y = 2.5, xend = 0.42, yend = 4.16), colour = "black", size = 1,
               arrow = arrow(length = unit(0.02, "npc"), type = "closed", ends = "both"))+ #identity effect
  geom_vline(xintercept=0.2, linetype = "dotted")+ #evenness
  ggtitle("DIVERSITY-INTERACTIONS MODEL for a 5 group community: Overall level \nof ecosystem function is a product of identity and interaction effects")+
  scale_colour_manual(values=group.colours)+
  xlab(bquote("Proportional Abundance"~(P[i])))+
  annotate("text", x=0.45, y=6, label="Interaction Effect", hjust="inward", angle = 90, size=9)+
  annotate("text", x=0.45, y=2.6, label="Identity Effect", hjust="inward", angle = 90, size=9)+
  annotate("text", x=0.99, y=3.5, label="Identity effect of functional group i", hjust="inward", size=9)+
  annotate("text", x=0.2, y=11.7, label="Maximum evenness\nbetween functional groups", size=9)+
  annotate("text", x=1.02, y=8.7146, size = 9, label="NULL MODEL", hjust="outward")+
  annotate("text", x=1.02, y=10.1889, size = 9, label="IDENTITY MODEL", hjust="outward")+
  annotate("text", x=1.02, y=5.54, size = 9, label=   "PAIRWISE INTERACTIONS MODEL", hjust="outward")+
  theme(legend.title=element_blank(), legend.position="none",
        axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.margin = unit(c(1.5,30,1,1), "lines"), text = element_text(size=30),
        plot.title = element_text(size=30, hjust=0.5))+
  coord_cartesian(xlim = c(0, 1), clip = 'off')
