###DATASET SUBSET NUMBERS###
#1 coral metrics (richness, calc rate, cover)
#2 coral recruitment
#3 algal cover

###PLOTTING###
#predictions of coral reef benthic metrics based on coefficents from Diversity-Interactions models 2 and 3

library(tidyverse)
library(ggplot2)
library(gridExtra)
library(boot)
library(cowplot)

###SET WORKING DIRECTORY###
setwd("")

###READ IN DATA (if applicable)###
survey_data <- read.csv("survey_data.csv", header = T, stringsAsFactors = F)
predict.mean <- read.csv("predict.mean.csv", header = T, stringsAsFactors = F)
ss1predict <- read.csv("ss1predict.csv", header = T, stringsAsFactors = F)
ss2predict <- read.csv("ss2predict.csv", header = T, stringsAsFactors = F)
ss3predict <- read.csv("ss3predict.csv", header = T, stringsAsFactors = F)
predict.full <- read.csv("predict.full.csv", header = T, stringsAsFactors = F)

###SUBSET SURVEY DATA###
#to remove NA in response variable
subset1 <- survey_data %>% filter(diadema<150) %>% #Remove diadema outlier 25sd above mean
  drop_na(diadema) %>% #Drop surveys with NA diadema
  drop_na(coral.richness) %>% #Drop surveys with NA in coral metrics (richness, calc rate, cover)
  arrange(subregionID)
subset2 <- survey_data %>% filter(diadema<150) %>% #Remove diadema outlier 25sd above mean
  drop_na(diadema) %>% #Drop surveys with NA diadema
  drop_na(recruits) %>% #Drop surveys with NA in coral recruitment
  arrange(subregionID)
subset3 <- survey_data %>% filter(diadema<150) %>% #Remove diadema outlier 25sd above mean
  drop_na(diadema) %>% #Drop surveys with NA diadema
  drop_na(calc.cover) %>% #Drop surveys with NA in algal cover
  arrange(subregionID)

#set group colours
group.colours <- c(Croppers = "#EEDD88", Browsers = "#EE8866",  Farmers = "#7E8BD6FF", Scrapers = "#99DDFF", Excavators = "#44BB99")


###FIGURE 2, S3 and S4###
#predictions of coral reef benthic metrics using coefficients from model 3 (mean and full 1000 bootstrapped iterations)
g1 <- ggplot()+
  geom_line(data = predict.full, aes(x=P1, y=richness, group = subgroup, col = Group), size = 2, alpha = 0.01)+
  geom_line(data = predict.mean, aes(x=P1, y=richness, group = Group, col = Group), size = 2.2, linetype = "dotted")+
  geom_line(data = ss1predict, aes(x=P1, y=richness, col = Group), size = 2.4)+
  scale_colour_manual(values=group.colours)+
  geom_vline(xintercept=0.2, linetype = "dotted")+
  ylim(0,17)+
  xlab(NULL)+
  ylab(bquote("Coral Richness (no.species/10m"^2*")"))+
  annotate("text", x=0, y=17, label="(a) Coral richness", hjust=0, size=9)+
  annotate("text", x=0, y=15.3, label="p < 0.05", hjust=0, size = 9)+
  theme(legend.title=element_blank(), legend.position="none", legend.key = element_rect(fill = "white"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_blank(), text = element_text(size=30))

g2 <- ggplot()+
  geom_line(data = predict.full, aes(x=P1, y=calcrate, group = subgroup, col = Group), size = 2, alpha = 0.01)+
  geom_line(data = predict.mean, aes(x=P1, y=calcrate, group = Group, col = Group), size = 2.2, linetype = "dotted")+
  geom_line(data = ss1predict, aes(x=P1, y=calcrate, col = Group), size =2.4)+
  scale_colour_manual(values=group.colours)+
  geom_vline(xintercept=0.2, linetype = "dotted")+
  ylim(0,7)+
  xlab(NULL)+
  ylab(bquote("Calcification Rate (kg CaCO"[3]*"m"^-2*"year"^-1*")"))+
  annotate("text", x=0, y=7, label="(c) Calcification rate",hjust=0, size=9)+
  annotate("text", x=0, y=6.3, label="p < 0.05", hjust= 0, size = 9)+
  theme(legend.title=element_blank(), legend.position="none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_blank(), text = element_text(size=30))

g3 <- ggplot()+
  geom_line(data = predict.full, aes(x=P1, y=total, group = subgroup, col = Group), size = 2, alpha = 0.01)+
  geom_line(data = predict.mean, aes(x=P1, y=total, group = Group, col = Group), size = 2.2, linetype = "dotted")+
  geom_line(data = ss1predict, aes(x=P1, y=total, col = Group), size = 2.4)+
  scale_colour_manual(values=group.colours)+
  geom_vline(xintercept=0.2, linetype = "dotted")+
  scale_y_continuous(name = bquote("Total Coral Area "(m^2)), breaks = c(0, 3, 6, 9), limits = c(0,11))+
  annotate("text", x=0, y=11, label="(b) Total coral area", hjust=0, size=9)+
  xlab(NULL)+
  ylab(bquote("Total Coral Area "(m^2)))+
  annotate("text", x=0, y=9.9, label="p < 0.05", hjust = 0, size = 9)+
  theme(legend.title=element_blank(), legend.position=c(.85,.85), legend.key = element_rect(fill = "white"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text.x = element_blank(), text = element_text(size=30))

g4 <- ggplot()+
  geom_line(data = predict.full, aes(x=P1, y=recruits, group = subgroup, col = Group), size = 2, alpha = 0.01)+
  geom_line(data = predict.mean, aes(x=P1, y=recruits, group = Group, col = Group), size = 2.2, linetype = "dotted")+
  geom_line(data = ss2predict, aes(x=P1, y=recruits, col = Group), size = 2.4)+
  scale_colour_manual(values=group.colours)+
  geom_vline(xintercept=0.2, linetype = "dotted")+
  scale_y_continuous(name = bquote("Coral Recruitment "(individual/m^2)), breaks = c(0, 2, 4, 6), limits = c(0,6))+
  xlab(NULL)+
  annotate("text", x=0, y=5.5, label="p = 0.43", hjust=0, size = 9)+
  annotate("text", x=0, y=6, label="(d) Coral recruitment", hjust=0, size = 9)+
  theme(legend.title=element_blank(), legend.position="none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_blank(), text = element_text(size=30))

g5 <- ggplot()+
  geom_line(data = predict.full, aes(x=P1, y=weedy, group = subgroup, col = Group), size = 2, alpha = 0.01)+
  geom_line(data = predict.mean, aes(x=P1, y=weedy, group = Group, col = Group), size = 2.2, linetype = "dotted")+
  geom_line(data = ss1predict, aes(x=P1, y=weedy, col = Group), size = 2.4)+
  geom_vline(xintercept=0.2, linetype = "dotted")+
  scale_colour_manual(values=group.colours)+
  scale_y_continuous(name=bquote("Weedy Coral Area " (m^2)), limits = c(0,6), labels = c(0,2,4,6))+
  xlab(NULL)+
  annotate("text", x=0, y=6, label="(a) Weedy coral area", hjust=0, size=9)+
  annotate("text", x=0, y=5.4, label="p = 0.05", hjust=0, size=9)+
  theme(legend.title=element_blank(), legend.position="none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_blank(), text = element_text(size=30))

g6 <- ggplot()+
  geom_line(data = predict.full, aes(x=P1, y=stress, group = subgroup, col = Group), size = 2, alpha = 0.01)+
  geom_line(data = predict.mean, aes(x=P1, y=stress, group = Group, col = Group), size = 2.2, linetype = "dotted")+
  geom_line(data = ss1predict, aes(x=P1, y=stress, col = Group), size = 2.4)+
  geom_vline(xintercept=0.2, linetype = "dotted")+
  scale_colour_manual(values=group.colours)+
  scale_y_continuous(name=bquote("Stress-Tolerant Coral Area " (m^2)), limits = c(0,3), labels = c(0,1,2,3))+
  xlab(NULL)+
  annotate("text", x=0, y=3, label="(b) Stress-tolerant coral area", hjust=0, size=9)+
  annotate("text", x=0, y=2.7, label="p < 0.05", hjust=0, size=9)+
  theme(legend.title=element_blank(), legend.position=c(.85,.85),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_blank(), text = element_text(size=30))

g7 <- ggplot()+
  geom_line(data = predict.full, aes(x=P1, y=comp, group = subgroup, col = Group), size = 2, alpha = 0.01)+
  geom_line(data = predict.mean, aes(x=P1, y=comp, group = Group, col = Group), size = 2.2, linetype = "dotted")+
  geom_line(data = ss1predict, aes(x=P1, y=comp, col = Group), size = 2.4)+
  geom_vline(xintercept=0.2, linetype = "dotted")+
  scale_colour_manual(values=group.colours)+
  scale_y_continuous(name = bquote("Competitive Coral Area "(m^2)), limits = c(0,0.6), labels = c(0, 0.2, 0.4, 0.6))+
  xlab("Proportional Abundance")+
  annotate("text", x=0, y=0.6, label="(c) Competitive coral area", hjust=0, size=9)+
  annotate("text", x=0, y=0.54, label="p < 0.05", hjust=0, size=9)+
  theme(legend.title=element_blank(), legend.position="none", legend.key = element_rect(fill = "white"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        panel.border = element_blank(), text = element_text(size=30))

g8 <- ggplot()+
  geom_line(data = predict.full, aes(x=P1, y=gen, group = subgroup, col = Group), size = 2, alpha = 0.01)+
  geom_line(data = predict.mean, aes(x=P1, y=gen, group = Group, col = Group), size = 2.2, linetype = "dotted")+
  geom_line(data = ss1predict, aes(x=P1, y=gen, col = Group), size = 2.4)+
  geom_vline(xintercept=0.2, linetype = "dotted")+
  scale_colour_manual(values=group.colours)+
  scale_y_continuous(name=bquote("Generalist Coral Area "(m^2)), limits = c(0,1.5), labels = c(0,0.5,"1.0",1.5))+
  xlab("Proportional Abundance")+
  annotate("text", x=0, y=1.5, label="(d) Generalist coral area", hjust=0, size=9)+
  annotate("text", x=0, y=1.35, label="p < 0.05", hjust=0, size=9)+
  theme(legend.title=element_blank(), legend.position="none", legend.key = element_rect(fill = "white"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        panel.border = element_blank(), text = element_text(size=30))

g9 <- ggplot()+
  geom_line(data = predict.full, aes(x=P1, y=fleshy*100, group = subgroup, col = Group), size = 2, alpha = 0.01)+
  geom_line(data = predict.mean, aes(x=P1, y=fleshy*100, group = Group, col = Group), size = 2.2, linetype = "dotted")+
  geom_line(data = ss3predict, aes(x=P1, y=fleshy*100, col = Group), size = 2.4)+
  geom_vline(xintercept=0.2, linetype = "dotted")+
  scale_colour_manual(values=group.colours)+
  scale_y_continuous(name = "Fleshy Macroalgal Cover (%)", limits = c(0,100))+
  xlab(NULL)+
  annotate("text", x=0, y=100, label="(e) Fleshy macroalgal cover", hjust=0, size = 9)+
  annotate("text", x=0, y=90, label="p = 0.10", hjust=0, size = 9)+
  theme(legend.title=element_blank(), legend.position="none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_blank(),
        text = element_text(size=30))

g10 <- ggplot()+
  geom_line(data = predict.full, aes(x=P1, y=calc*100, group = subgroup, col = Group), size = 2, alpha = 0.01)+
  geom_line(data = predict.mean, aes(x=P1, y=calc*100, group = Group, col = Group), size = 2.2, linetype = "dotted")+
  geom_line(data = ss3predict, aes(x=P1, y=calc*100, col = Group), size = 2.4)+
  geom_vline(xintercept=0.2, linetype = "dotted")+
  scale_colour_manual(values=group.colours)+
  scale_y_continuous(name = "Calcareous Macroalgal Cover (%)", limits = c(0,100))+
  xlab(NULL)+
  annotate("text", x=0, y=100, label="(f) Calcareous macroalgal cover", hjust=0, size = 9)+
  annotate("text", x=0, y=90, label="p < 0.05", hjust=0, size = 9)+
  theme(legend.title=element_blank(), legend.position="none", legend.key = element_rect(fill = "white"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_blank(), text = element_text(size=30))

g11 <- ggplot()+
  geom_line(data = predict.full, aes(x=P1, y=turf*100, group = subgroup, col = Group), size = 2, alpha = 0.01)+
  geom_line(data = predict.mean, aes(x=P1, y=turf*100, group = Group, col = Group), size = 2.2, linetype = "dotted")+
  geom_line(data = ss3predict, aes(x=P1, y=turf*100, col = Group), size = 2.4)+
  geom_vline(xintercept=0.2, linetype = "dotted")+
  scale_colour_manual(values=group.colours)+
  scale_y_continuous(name = "Turf Algal Cover (%)", limits = c(0,100))+
  xlab("Proportional Abundance")+
  annotate("text", x=0, y=100, label="(g) Turf algal cover", hjust=0, size = 9)+
  annotate("text", x=0, y=90, label="p < 0.05", hjust=0, size = 9)+
  theme(legend.title=element_blank(), legend.position="none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size=30))

g12 <- ggplot()+
  geom_line(data = predict.full, aes(x=P1, y=cca*100, group = subgroup, col = Group), size = 2, alpha = 0.02)+
  geom_line(data = predict.mean, aes(x=P1, y=cca*100, group = Group, col = Group), size = 2.2, linetype = "dotted")+
  geom_line(data = ss3predict, aes(x=P1, y=cca*100, col = Group), size = 2.4)+
  geom_vline(xintercept=0.2, linetype = "dotted")+
  scale_colour_manual(values=group.colours)+
  scale_y_continuous(name = "CCA Cover (%)", limits = c(0,100))+
  xlab("Proportional Abundance")+
  annotate("text", x=0, y=100, label="(h) CCA cover", hjust=0, size = 9)+
  annotate("text", x=0, y=90, label="p < 0.05", hjust=0, size = 9)+
  theme(legend.title=element_blank(), legend.position="none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size=30))

#boxplots of raw survey data for coral reef benthic metrics (1. y axis limited and 2. full)
b1 <- ggplot(subset1, aes(x= NA, y=coral.richness)) + 
  geom_boxplot(size=1.5)+
  ylim(0,17)+
  xlab(" ")+
  ylab(" ")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_line(colour = "white"),
        axis.text.y = element_text(colour = "white"))+
  scale_x_discrete(labels=" ")

b1.2 <- ggplot(subset1, aes(x= NA, y=coral.richness)) + 
  geom_boxplot(size=1.5)+
  xlab(" ")+
  ylab(bquote("Coral Richness (no.species/10m"^2*")"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour="black"),
        axis.ticks.x = element_blank(), text = element_text(size=30))+
  scale_x_discrete(labels=" ")

b2 <- ggplot(subset1, aes(x= NA, y=calc.rate)) + 
  geom_boxplot(size=1.5)+
  ylim(0,7)+
  xlab(" ")+
  ylab(" ")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_line(colour = "white"),
        axis.text.y = element_text(colour = "white"))+
  scale_x_discrete(labels=" ")

b2.2 <- ggplot(subset1, aes(x= NA, y=calc.rate)) + 
  geom_boxplot(size=1.5)+
  xlab(" ")+
  ylab(bquote("Calcification Rate (kg CaCO"[3]*"/m"^-2*"/year"^1*")"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour="black"),
        axis.ticks.x = element_blank(), text = element_text(size=30))+
  scale_x_discrete(labels=" ")

b3 <- ggplot(subset1, aes(x= NA, y=coral.area)) + 
  geom_boxplot(size=1.5)+
  ylim(0,11)+
  xlab(" ")+
  ylab(" ")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_line(colour = "white"),
        axis.text.y = element_text(colour = "white"))+
  scale_x_discrete(labels=" ")

b3.2 <- ggplot(subset1, aes(x= NA, y=coral.area)) + 
  geom_boxplot(size=1.5)+
  xlab(" ")+
  ylab(bquote("Total Coral Area "(m^2)))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour="black"),
        axis.ticks.x = element_blank(), text = element_text(size=30))+
  scale_x_discrete(labels=" ")

b4 <- ggplot(subset2, aes(x= NA, y=recruits)) + 
  geom_boxplot(size=1.5)+
  ylim(0,5)+
  xlab(" ")+
  ylab(" ")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_line(colour = "white"),
        axis.text.y = element_text(colour = "white"))+
  scale_x_discrete(labels=" ")

b4.2 <- ggplot(subset2, aes(x= NA, y=recruits)) + 
  geom_boxplot(size=1.5)+
  xlab(" ")+
  ylab(bquote("Coral Recruitment "(individual/m^2)))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour="black"),
        axis.ticks.x = element_blank(), text = element_text(size=30))+
  scale_x_discrete(labels=" ")

b5 <- ggplot(subset1, aes(x= NA, y=weedy.area)) + 
  geom_boxplot(size=1.5)+
  ylim(0,6)+
  xlab(" ")+
  ylab(" ")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank(), axis.line = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_line(colour = "white"),
        axis.text.y = element_text(colour = "white"))+
  scale_x_discrete(labels=" ")

b5.2 <- ggplot(subset1, aes(x= NA, y=weedy.area)) + 
  geom_boxplot(size=1.5)+
  xlab(" ")+
  ylab(bquote("Weedy Coral Area "(m^2)))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour="black"),
        axis.ticks.x = element_blank(), text = element_text(size=30))+
  scale_x_discrete(labels=" ")

b6 <- ggplot(subset1, aes(x= NA, y=stress.tol.area)) + 
  geom_boxplot(size=1.5)+
  ylim(0,3)+
  xlab(" ")+
  ylab(" ")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank(), axis.line = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_line(colour = "white"),
        axis.text.y = element_text(colour = "white"))+
  scale_x_discrete(labels=" ")

b6.2 <- ggplot(subset1, aes(x= NA, y=stress.tol.area)) + 
  geom_boxplot(size=1.5)+
  xlab(" ")+
  ylab(bquote("Stress-Tolerant Coral Area "(m^2)))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour="black"),
        axis.ticks.x = element_blank(), text = element_text(size=30))+
  scale_x_discrete(labels=" ")

b7 <- ggplot(subset1, aes(x= NA, y=competitive.area)) + 
  geom_boxplot(size=1.5)+
  ylim(0,0.6)+
  xlab(" ")+
  ylab(" ")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank(), axis.line = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_line(colour = "white"),
        axis.text.y = element_text(colour = "white"))+
  scale_x_discrete(labels=" ")

b7.2 <- ggplot(subset1, aes(x= NA, y=competitive.area)) + 
  geom_boxplot(size=1.5)+
  xlab(" ")+
  ylab(bquote("Competitive Coral Area "(m^2)))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour="black"),
        axis.ticks.x = element_blank(), text = element_text(size=30))+
  scale_x_discrete(labels=" ")

b8 <- ggplot(subset1, aes(x= NA, y=generalist.area)) + 
  geom_boxplot(size=2)+
  ylim(0,1.5)+
  xlab(" ")+
  ylab(" ")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank(), axis.line = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_line(colour = "white"),
        axis.text.y = element_text(colour = "white"))+
  scale_x_discrete(labels=" ")

b8.2 <- ggplot(subset1, aes(x= NA, y=generalist.area)) + 
  geom_boxplot(size=1.5)+
  xlab(" ")+
  ylab(bquote("Generalist Coral Area "(m^2)))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour="black"),
        axis.ticks.x = element_blank(), text = element_text(size=30))+
  scale_x_discrete(labels=" ")

b9 <- ggplot(subset3, aes(x= NA, y=fleshy.cover)) + 
  geom_boxplot(size=1.5)+
  ylim(0,100)+
  xlab(" ")+
  ylab(" ")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_line(colour = "white"),
        axis.text.y = element_text(colour = "white"))+
  scale_x_discrete(labels=" ")


b9.2 <- ggplot(subset3, aes(x= NA, y=fleshy.cover)) + 
  geom_boxplot(size=1.5)+
  xlab(" ")+
  ylab("Fleshy Macroalgal Cover (%)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour="black"),
        axis.ticks.x = element_blank(), text = element_text(size=30))+
  scale_x_discrete(labels=" ")

b10 <- ggplot(subset3, aes(x= NA, y=calc.cover)) + 
  geom_boxplot(size=1.5)+
  ylim(0,100)+
  xlab(" ")+
  ylab(" ")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_line(colour = "white"),
        axis.text.y = element_text(colour = "white"))+
  scale_x_discrete(labels=" ")

b10.2 <- ggplot(subset3, aes(x= NA, y=calc.cover)) + 
  geom_boxplot(size=1.5)+
  xlab(" ")+
  ylab("Calcareous Macroalgal Cover (%)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour="black"),
        axis.ticks.x = element_blank(), text = element_text(size=30))+
  scale_x_discrete(labels=" ")

b11 <- ggplot(subset3, aes(x= NA, y=turf.cover)) + 
  geom_boxplot(size=1.5)+
  ylim(0,100)+
  xlab(" ")+
  ylab(" ")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_line(colour = "white"),
        axis.text.y = element_text(colour = "white"))+
  scale_x_discrete(labels=" ")

b11.2 <- ggplot(subset3, aes(x= NA, y=turf.cover)) + 
  geom_boxplot(size=1.5)+
  xlab(" ")+
  ylab("Turf Algal Cover (%)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour="black"),
        axis.ticks.x = element_blank(), text = element_text(size=30))+
  scale_x_discrete(labels=" ")

b12 <- ggplot(subset3, aes(x= NA, y=cca.cover)) + 
  geom_boxplot(size=1.5)+
  ylim(0,100)+
  xlab(" ")+
  ylab(" ")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_line(colour = "white"),
        axis.text.y = element_text(colour = "white"))+
  scale_x_discrete(labels=" ")

b12.2 <- ggplot(subset3, aes(x= NA, y=cca.cover)) + 
  geom_boxplot(size=1.5)+
  xlab(" ")+
  ylab("CCA Cover (%)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour="black"),
        axis.ticks.x = element_blank(), text = element_text(size=30))+
  scale_x_discrete(labels=" ")

#Figure 2
plot_grid(b1, g1, b3, g3, b2, g2, b4, g4, b9, g9, b10, g10, b11, g11, b12, g12, ncol=4, align = "vh", axis = "rlbt", rel_widths = c(1,4,1,4))

#Figure S3
plot_grid(b5, g5, b6, g6, b7, g7, b8, g8, ncol=4, align = "vh", axis = "rlbt", rel_widths = c(1,4,1,4)) #with limited boxplots

#Figure S4
plot_grid(b1.2, b3.2, b2.2, b4.2, b9.2, b10.2, b11.2, b12.2, b5.2, b6.2, b7.2, b8.2, ncol=4, align = "vh", axis = "rlbt") #all coral boxplots


###Figure 3###
#predictions of coral reef benthic metrics using coefficients from model 3 with separate identity and interaction components
g13 <- ggplot()+
  geom_line(data = predict.mean, aes(x=P1, y=richness, group = Group, col = Group), size = 2.4)+
  geom_line(data = predict.mean, aes(x=P1, y=richnessid, group = Group, col = Group), size = 2, linetype = "dotted")+
  scale_colour_manual(values=group.colours)+
  geom_vline(xintercept=0.2, linetype = "dotted")+
  geom_segment(aes(x = 0.22, y = 4.2, xend = 0.22, yend = 9.6), colour = "black", size = 1,
               arrow = arrow(length = unit(0.02, "npc"), type = "closed", ends = "both"))+ #interaction effect
  ylim(0,17)+
  xlab(NULL)+
  ylab(bquote("Coral Richness (no.species/10m"^2*")"))+
  annotate("text", x=0, y=17, label="(a) Coral richness", hjust=0, size=9)+
  theme(legend.title=element_blank(), legend.position="none", legend.key = element_rect(fill = "white"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_blank(), text = element_text(size=30))

g14 <- ggplot()+
  geom_line(data = predict.mean, aes(x=P1, y=calcrate, group = Group, col = Group), size = 2.4)+
  geom_line(data = predict.mean, aes(x=P1, y=calcrateid, group = Group, col = Group), size = 2, linetype = "dotted")+
  scale_colour_manual(values=group.colours)+
  geom_vline(xintercept=0.2, linetype = "dotted")+
  geom_segment(aes(x = 0.22, y = 0.1, xend = 0.22, yend = 3), colour = "black", size = 1,
               arrow = arrow(length = unit(0.02, "npc"), type = "closed", ends = "both"))+ #interaction effect
  ylim(0,7)+
  xlab(NULL)+
  ylab(bquote("Calcification Rate (kg CaCO"[3]*"m"^-2*"year"^-1*")"))+
  annotate("text", x=0, y=7, label="(c) Calcification rate",hjust=0, size=9)+
  theme(legend.title=element_blank(), legend.position="none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_blank(), text = element_text(size=30))

g15 <- ggplot()+
  geom_line(data = predict.mean, aes(x=P1, y=total, group = Group, col = Group), size = 2.4)+
  geom_line(data = predict.mean, aes(x=P1, y=totalid, group = Group, col = Group), size = 2, linetype = "dotted")+
  scale_colour_manual(values=group.colours)+
  geom_vline(xintercept=0.2, linetype = "dotted")+
  geom_segment(aes(x = 0.22, y = 0.2, xend = 0.22, yend = 4.3), colour = "black", size = 1,
               arrow = arrow(length = unit(0.02, "npc"), type = "closed", ends = "both"))+ #interaction effect
  scale_y_continuous(name = bquote("Total Coral Area "(m^2)), breaks = c(0, 3, 6, 9), limits = c(0,11))+
  annotate("text", x=0, y=11, label="(b) Total coral area", hjust=0, size=9)+
  xlab(NULL)+
  ylab(bquote("Total Coral Area "(m^2)))+
  theme(legend.title=element_blank(), legend.position=c(.85,.85), legend.key = element_rect(fill = "white"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text.x = element_blank(), text = element_text(size=30))

g16 <- ggplot()+
  geom_line(data = predict.mean, aes(x=P1, y=recruits, group = Group, col = Group), size = 2.4)+
  geom_line(data = predict.mean, aes(x=P1, y=recruitsid, group = Group, col = Group), size = 2, linetype = "dotted")+
  scale_colour_manual(values=group.colours)+
  geom_vline(xintercept=0.2, linetype = "dotted")+
  geom_segment(aes(x = 0.22, y = 1.3, xend = 0.22, yend = 1.66), colour = "black", size = 1,
               arrow = arrow(length = unit(0.02, "npc"), type = "closed", ends = "both"))+ #interaction effect
  scale_y_continuous(limits = c(0, 3), labels = c(0, 1, 2, 3))+
  xlab(NULL)+
  ylab(bquote("Coral Recruitment "(individual/m^2)))+
  annotate("text", x=0, y=3, label="(d) Coral recruitment", hjust=0, size = 9)+
  theme(legend.title=element_blank(), legend.position="none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_blank(), text = element_text(size=30))

g17 <- ggplot()+
  geom_line(data = predict.mean, aes(x=P1, y=fleshy*100, group = Group, col = Group), size = 2.6)+
  geom_line(data = predict.mean, aes(x=P1, y=fleshyid*100, group = Group, col = Group), size = 2, linetype = "dotted")+
  scale_colour_manual(values=group.colours)+
  geom_vline(xintercept=0.2, linetype = "dotted")+
  geom_segment(aes(x = 0.22, y = 8.17, xend = 0.22, yend = 20), colour = "black", size = 1,
               arrow = arrow(length = unit(0.02, "npc"), type = "closed", ends = "both"))+ #interaction effect
  scale_y_continuous(name = "Fleshy Macoalgal Cover (%)", limits = c(0,50))+
  annotate("text", x=0, y=50, label="(e) Fleshy macroalgal cover", hjust=0, size=9)+
  xlab(NULL)+
  theme(legend.title=element_blank(), legend.position="none", legend.key = element_rect(fill = "white"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text.x = element_blank(), text = element_text(size=30))

g18 <- ggplot()+
  geom_line(data = predict.mean, aes(x=P1, y=calc*100, group = Group, col = Group), size = 2.6)+
  geom_line(data = predict.mean, aes(x=P1, y=calcid*100, group = Group, col = Group), size = 2, linetype = "dotted")+
  scale_colour_manual(values=group.colours)+
  geom_vline(xintercept=0.2, linetype = "dotted")+
  geom_segment(aes(x = 0.22, y = 4.2, xend = 0.22, yend = 6), colour = "black", size = 1,
               arrow = arrow(length = unit(0.02, "npc"), type = "closed", ends = "both"))+ #interaction effect
  scale_y_continuous(name = "Calcareous Macoalgal Cover (%)", limits = c(0,25))+
  annotate("text", x=0, y=25, label="(f) Calcareous macroalgal cover", hjust=0, size=9)+
  xlab(NULL)+
  theme(legend.title=element_blank(), legend.position="none", legend.key = element_rect(fill = "white"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text.x = element_blank(), text = element_text(size=30))

g19 <- ggplot()+
  geom_line(data = predict.mean, aes(x=P1, y=turf*100, group = Group, col = Group), size = 2.6)+
  geom_line(data = predict.mean, aes(x=P1, y=turfid*100, group = Group, col = Group), size = 2, linetype = "dotted")+
  scale_colour_manual(values=group.colours)+
  geom_vline(xintercept=0.2, linetype = "dotted")+
  geom_segment(aes(x = 0.22, y = 25, xend = 0.22, yend = 50), colour = "black", size = 1,
               arrow = arrow(length = unit(0.02, "npc"), type = "closed", ends = "both"))+ #interaction effect
  scale_y_continuous(name = "Turf Algal Cover (%)", limits = c(0,100))+
  annotate("text", x=0, y=100, label="(g) Turf algal cover", hjust=0, size=9)+
  xlab("Proportional Abundance")+
  theme(legend.title=element_blank(), legend.position="none", legend.key = element_rect(fill = "white"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text = element_text(size=30))

g20 <- ggplot()+
  geom_line(data = predict.mean, aes(x=P1, y=cca*100, group = Group, col = Group), size = 2.4)+
  geom_line(data = predict.mean, aes(x=P1, y=ccaid*100, group = Group, col = Group), size = 2, linetype = "dotted")+
  scale_colour_manual(values=group.colours)+
  geom_vline(xintercept=0.2, linetype = "dotted")+
  scale_y_continuous(name = "CCA Cover (%)", limits = c(0,25))+
  annotate("text", x=0, y=25, label="(h) CCA cover", hjust=0, size=9)+
  xlab("Proportional Abundance")+
  theme(legend.title=element_blank(), legend.position="none", legend.key = element_rect(fill = "white"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text = element_text(size=30))

#Figure 3
plot_grid(g13, g15, g14, g16, g17, g18, g19, g20, ncol=2, align = "vh", axis = "rlbt")


###Figure S2###
#predictions of coral reef benthic metrics using coefficients from model 2: identity model
g21 <- ggplot()+
  geom_line(data = predict.mean, aes(x=P1, y=richnessi, group = Group, col = Group), size = 2)+
  scale_colour_manual(values=group.colours)+
  geom_vline(xintercept=0.2, linetype = "dotted")+
  ylim(0,17)+
  xlab(NULL)+
  ylab(bquote("Coral Richness (no.species/10m"^2*")"))+
  annotate("text", x=0, y=17, label="(a) Coral richness", hjust=0, size=9)+
  theme(legend.title=element_blank(), legend.position="none", legend.key = element_rect(fill = "white"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_blank(), text = element_text(size=30))

g22 <- ggplot()+
  geom_line(data = predict.mean, aes(x=P1, y=calcratei, group = Group, col = Group), size = 2)+
  scale_colour_manual(values=group.colours)+
  geom_vline(xintercept=0.2, linetype = "dotted")+
  ylim(0,7)+
  xlab(NULL)+
  ylab(bquote("Calcification Rate (kg CaCO"[3]*"m"^-2*"year"^-1*")"))+
  annotate("text", x=0, y=7, label="(c) Calcification rate",hjust=0, size=9)+
  theme(legend.title=element_blank(), legend.position="none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_blank(), text = element_text(size=30))

g23 <- ggplot()+
  geom_line(data = predict.mean, aes(x=P1, y=totali, group = Group, col = Group), size = 2)+
  scale_colour_manual(values=group.colours)+
  geom_vline(xintercept=0.2, linetype = "dotted")+
  scale_y_continuous(breaks = c(0, 3, 6, 9), limits = c(0,10))+
  annotate("text", x=0, y=10, label="(b) Total coral area", hjust=0, size=9)+
  xlab(NULL)+
  ylab(bquote("Total Coral Area "(m^2)))+
  theme(legend.title=element_blank(), legend.position=c(.85,.85), legend.key = element_rect(fill = "white"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text.x = element_blank(), text = element_text(size=30))

g24 <- ggplot()+
  geom_line(data = predict.mean, aes(x=P1, y=recruitsi, group = Group, col = Group), size = 2)+
  scale_colour_manual(values=group.colours)+
  geom_vline(xintercept=0.2, linetype = "dotted")+
  scale_y_continuous(limits = c(0, 6), labels = c(0, 2, 4, 6))+
  xlab(NULL)+
  ylab(bquote("Coral Recruitment "(individual/m^2)))+
  annotate("text", x=0, y=6, label="(d) Coral recruitment", hjust=0, size = 9)+
  theme(legend.title=element_blank(), legend.position="none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_blank(), text = element_text(size=30))

g25 <- ggplot()+
  geom_line(data = predict.mean, aes(x=P1, y=fleshyi*100, group = Group, col = Group), size = 2)+
  scale_colour_manual(values=group.colours)+
  geom_vline(xintercept=0.2, linetype = "dotted")+
  scale_y_continuous(name = "Fleshy Macoalgal Cover (%)", limits = c(0,100))+
  annotate("text", x=0, y=100, label="(e) Fleshy macroalgal cover", hjust=0, size=9)+
  xlab(NULL)+
  theme(legend.title=element_blank(), legend.position="none", legend.key = element_rect(fill = "white"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text.x = element_blank(), text = element_text(size=30))

g26 <- ggplot()+
  geom_line(data = predict.mean, aes(x=P1, y=fleshyi*100, group = Group, col = Group), size = 2)+
  scale_colour_manual(values=group.colours)+
  geom_vline(xintercept=0.2, linetype = "dotted")+
  scale_y_continuous(name = "Calcareous Macoalgal Cover (%)", limits = c(0,100))+
  annotate("text", x=0, y=100, label="(f) Calcareous macroalgal cover", hjust=0, size=9)+
  xlab(NULL)+
  theme(legend.title=element_blank(), legend.position="none", legend.key = element_rect(fill = "white"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        axis.text.x = element_blank(), text = element_text(size=30))

g27 <- ggplot()+
  geom_line(data = predict.mean, aes(x=P1, y=turfi*100, group = Group, col = Group), size = 2)+
  scale_colour_manual(values=group.colours)+
  geom_vline(xintercept=0.2, linetype = "dotted")+
  scale_y_continuous(name = "Turf Algal Cover (%)", limits = c(0,100))+
  annotate("text", x=0, y=100, label="(g) Turf algal cover", hjust=0, size=9)+
  xlab("Proportional Abundance")+
  theme(legend.title=element_blank(), legend.position="none", legend.key = element_rect(fill = "white"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text = element_text(size=30))

g28 <- ggplot()+
  geom_line(data = predict.mean, aes(x=P1, y=ccai*100, group = Group, col = Group), size = 2)+
  scale_colour_manual(values=group.colours)+
  geom_vline(xintercept=0.2, linetype = "dotted")+
  scale_y_continuous(name = "CCA Cover (%)", limits = c(0,100))+
  annotate("text", x=0, y=100, label="(h) CCA cover", hjust=0, size=9)+
  xlab("Proportional Abundance")+
  theme(legend.title=element_blank(), legend.position="none", legend.key = element_rect(fill = "white"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        text = element_text(size=30))

#Figure S2
plot_grid(g21, g23, g22, g24, g25, g26, g27, g28, ncol=2, align = "vh", axis = "rlbt")
