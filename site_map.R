library(tidyverse)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(rgeos)

###SET WORKING DIRECTORY###
setwd()

###READ IN DATA###
site <- read.csv("Site.csv", header = T, stringsAsFactors = F)
ecoregion <- read.csv("Ecoregion.csv", header = T, stringsAsFactors = F)
subregion <- read.csv("Subregion.csv", header = T, stringsAsFactors = F)
survey_data <- read.csv("survey_data.csv", header = T, stringsAsFactors = F) #requires data from initial manipulation

###SUBSET SURVEY DATA###
#to remove NA in response variable (surveys not included in analysis)
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

surveys <- rbind(subset1, subset2, subset3)
surveys <- select(surveys, -c("X")) %>% distinct_all() #dataset of all surveys used in analysis

surveys <- surveys %>% left_join(site, by = c("siteID"), suffix = c("",".y"))
surveys <- surveys %>% left_join(ecoregion, by = c("ecoregionID"), suffix = c("",".y"))
surveys <- surveys %>% left_join(subregion, by = c("subregionID"), suffix = c("",".y"))

###PLOT MAP: Figure S1###
#get axis limits
range(site$long)
range(site$lat)

#plot site map
theme_set(theme_bw())

survey_data$ecoregionID2 <- as.character(survey_data$ecoregionID) #create second ecoregion column as character

world <- ne_countries(scale = "medium", returnclass = "sf")
  
ggplot(data = world)+
  geom_sf()+
  geom_point(data=surveys, aes(x=long, y=lat, col= ecoregion), size=3)+
  scale_color_manual(values=c('#7E8BD6FF', '#EE8866', '#EEDD88', '#FFAABB', '#44BB99'))+ #set manual colours
  coord_sf(xlim= c(-96.13191 , -61.19500), ylim = c(9.10623 , 27.92167))+ #set axis limits
  xlab(NULL)+
  ylab(NULL)+
  theme(panel.grid.major = element_line(colour = "transparent"),
        legend.title=element_blank(), legend.position=c(.85,.85), legend.key = element_rect(fill = "white"),
        text = element_text(size=30), plot.margin = unit(c(1,2,1,1), "lines"))