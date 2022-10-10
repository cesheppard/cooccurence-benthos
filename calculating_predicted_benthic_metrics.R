###DATASET SUBSET NUMBERS###
#1 coral metrics (richness, calc rate, cover)
#2 coral recruitment
#3 algal cover

###Terms help###
#richness: coral richness
#calcrate: coral calcification rate
#total: total coral cover
#recruits: coral recruitment
#weedy: weedy coral cover
#stress: stress-tolerant coral cover
#comp: competitive coral cover
#gen: generalist coral cover
#fleshy: fleshy macroalgal cover
#calc: calcareous macroalgal cover
#turf: turf algal cover
#cca: crustose-coralline algal cover

library(tidyverse)

###SET WORKING DIRECTORY###
setwd("")

###READ IN DATA (if applicable)###
survey_data <- read.csv("survey_data.csv", header = T, stringsAsFactors = F)


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

surveys <- rbind(subset1, subset2, subset3)
surveys <- select(surveys, -c("X")) %>% distinct_all() #dataset of all surveys used in analysis

###CREATE DATASETS FOR PREDICTIONS###
#Create dataset for mean predictions up to P=1 (dotted lines on figures)
predict.mean <- data.frame(matrix(ncol = 3, nrow = 101))
colnames(predict.mean) <- c("Group", "P1", "P2")
predict.mean$P1 <- seq(0, 1, by = 0.01)
predict.mean$P2 <- (1-predict.mean$P1)/4
predict.mean <- predict.mean %>% rbind(predict.mean, predict.mean, predict.mean, predict.mean)
predict.mean$Group[1:101] <- "Croppers"
predict.mean$Group[102:202] <- "Browsers"
predict.mean$Group[203:303] <- "Farmers"
predict.mean$Group[304:404] <- "Scrapers"
predict.mean$Group[405:505] <- "Excavators"

#Create datasets for mean predictions up to max proportional abundance observed in raw data (thick solid line on figures)
#required for each subset separately as max proportional abundance observed differs between benthic metrics
ss1predict <- rbind(predict.mean %>% filter(Group == "Croppers", P1<=max(subset1$Pn.croppers)),
                    predict.mean %>% filter(Group == "Browsers", P1<=max(subset1$Pn.browsers)),
                    predict.mean %>% filter(Group == "Farmers", P1<=max(subset1$Pn.farmers)),
                    predict.mean %>% filter(Group == "Scrapers", P1<=max(subset1$Pn.scrapers)),
                    predict.mean %>% filter(Group == "Excavators", P1<=max(subset1$Pn.excavators)),
                    c("Croppers", max(subset1$Pn.croppers), (1-max(subset1$Pn.croppers))/4),
                    c("Browsers", max(subset1$Pn.browsers), (1-max(subset1$Pn.browsers))/4),
                    c("Farmers", max(subset1$Pn.farmers), (1-max(subset1$Pn.farmers))/4),
                    c("Scrapers", max(subset1$Pn.scrapers), (1-max(subset1$Pn.scrapers))/4),
                    c("Excavators", max(subset1$Pn.excavators), (1-max(subset1$Pn.excavators))/4)) %>%
  arrange(Group, P1) %>%
  mutate(P1 = as.numeric(P1), P2 = as.numeric(P2)) #mean predictions up to max P for subset1 (coral metrics; richness, calc rate, cover)

ss2predict <- rbind(predict.mean %>% filter(Group == "Croppers", P1<=max(subset2$Pn.croppers)),
                    predict.mean %>% filter(Group == "Browsers", P1<=max(subset2$Pn.browsers)),
                    predict.mean %>% filter(Group == "Farmers", P1<=max(subset2$Pn.farmers)),
                    predict.mean %>% filter(Group == "Scrapers", P1<=max(subset2$Pn.scrapers)),
                    predict.mean %>% filter(Group == "Excavators", P1<=max(subset2$Pn.excavators)),
                    c("Croppers", max(subset2$Pn.croppers), (1-max(subset2$Pn.croppers))/4),
                    c("Browsers", max(subset2$Pn.browsers), (1-max(subset2$Pn.browsers))/4),
                    c("Farmers", max(subset2$Pn.farmers), (1-max(subset2$Pn.farmers))/4),
                    c("Scrapers", max(subset2$Pn.scrapers), (1-max(subset2$Pn.scrapers))/4),
                    c("Excavators", max(subset2$Pn.excavators), (1-max(subset2$Pn.excavators))/4)) %>%
  arrange(Group, P1) %>%
  mutate(P1 = as.numeric(P1), P2 = as.numeric(P2)) #mean predictions up to max P for subset2 (coral recruitment)

ss3predict <- rbind(predict.mean %>% filter(Group == "Croppers", P1<=max(subset3$Pn.croppers)),
                    predict.mean %>% filter(Group == "Browsers", P1<=max(subset3$Pn.browsers)),
                    predict.mean %>% filter(Group == "Farmers", P1<=max(subset3$Pn.farmers)),
                    predict.mean %>% filter(Group == "Scrapers", P1<=max(subset3$Pn.scrapers)),
                    predict.mean %>% filter(Group == "Excavators", P1<=max(subset3$Pn.excavators)),
                    c("Croppers", max(subset3$Pn.croppers), (1-max(subset3$Pn.croppers))/4),
                    c("Browsers", max(subset3$Pn.browsers), (1-max(subset3$Pn.browsers))/4),
                    c("Farmers", max(subset3$Pn.farmers), (1-max(subset3$Pn.farmers))/4),
                    c("Scrapers", max(subset3$Pn.scrapers), (1-max(subset3$Pn.scrapers))/4),
                    c("Excavators", max(subset3$Pn.excavators), (1-max(subset3$Pn.excavators))/4)) %>%
  arrange(Group, P1) %>%
  mutate(P1 = as.numeric(P1), P2 = as.numeric(P2)) #mean predictions up to max P for subset3 (algal cover)

  
#add mean depth, total herbivore biomass, year and diadema to mean prediction datasets
predict.mean <- predict.mean %>%
  mutate(depth1 = mean(subset1$depth),
         b.herbivore1 = mean(subset1$b.herbivore),
         year1 = round(mean(subset1$year)),
         diadema1 = mean(subset1$diadema),
         depth2 = mean(subset2$depth),
         b.herbivore2 = mean(subset2$b.herbivore),
         year2 = round(mean(subset2$year)),
         diadema2 = mean(subset2$diadema),
         depth3 = mean(subset3$depth),
         b.herbivore3 = mean(subset3$b.herbivore),
         year3 = round(mean(subset3$year)),
         diadema3 = mean(subset3$diadema)) #mean predictions up to P=1

ss1predict <- ss1predict %>%
  mutate(depth1 = mean(subset1$depth),
         b.herbivore1 = mean(subset1$b.herbivore),
         year1 = round(mean(subset1$year)),
         diadema1 = mean(subset1$diadema)) #mean predictions up to max P for subset1 (coral metrics; richness, calc rate, cover)

ss2predict <- ss2predict %>%
  mutate(depth2 = mean(subset2$depth),
         b.herbivore2 = mean(subset2$b.herbivore),
         year2 = round(mean(subset2$year)),
         diadema2 = mean(subset2$diadema)) #mean predictions up to max P for subset2 (coral recruitment)

ss3predict <- ss3predict %>%
  mutate(depth3 = mean(subset3$depth),
         b.herbivore3 = mean(subset3$b.herbivore),
         year3 = round(mean(subset3$year)),
         diadema3 = mean(subset3$diadema)) #mean predictions up to max P for subset3 (algal cover)


#Create dataset for predictions from all 1000 iterations of bootstrapped models
predict.full <- data.frame(matrix(ncol = 3, nrow = 101))
colnames(predict.full) <- c("Group", "P1", "P2")
predict.full$P1 <- seq(0, 1, by = 0.01)
predict.full$P2 <- (1-predict.full$P1)/4
predict.full <- predict.full %>% rbind(predict.full, predict.full, predict.full, predict.full)
predict.full$Group[1:101] <- "Croppers"
predict.full$Group[102:202] <- "Browsers"
predict.full$Group[203:303] <- "Farmers"
predict.full$Group[304:404] <- "Scrapers"
predict.full$Group[405:505] <- "Excavators"
predict.full <- predict.full %>% slice(rep(1:n(), each = 1000))
predict.full$iteration <- rep(c(1:1000),505)
predict.full$subgroup <- paste(predict.full$Group, predict.full$iteration)
predict.full <- predict.full %>% arrange(Group, iteration)

#add mean depth, total herbivore biomass, year and diadema
predict.full <- predict.full %>%
  mutate(depth1 = mean(subset1$depth),
         b.herbivore1 = mean(subset1$b.herbivore),
         year1 = round(mean(subset1$year)),
         diadema1 = mean(subset1$diadema), #means for subset1 (coral metrics; richness, calc rate, cover)
         depth2 = mean(subset2$depth),
         b.herbivore2 = mean(subset2$b.herbivore),
         year2 = round(mean(subset2$year)),
         diadema2 = mean(subset2$diadema), #mean predictions up to max P for subset2 (coral recruitment)
         depth3 = mean(subset3$depth),
         b.herbivore3 = mean(subset3$b.herbivore),
         year3 = round(mean(subset3$year)),
         diadema3 = mean(subset3$diadema)) #mean predictions up to max P for subset3 (algal cover)


###CALCUALTE PREDICTIONS###
#read in model coefficients (if applicable)
richness3 <- read.csv("richness3.csv", header = T, stringsAsFactors = F, row.names = 1)
richness2 <- read.csv("richness2.csv", header = T, stringsAsFactors = F, row.names = 1)
calcrate3 <- read.csv("calcrate3.csv", header = T, stringsAsFactors = F, row.names = 1)
calcrate2 <- read.csv("calcrate2.csv", header = T, stringsAsFactors = F, row.names = 1)
total3 <- read.csv("total3.csv", header = T, stringsAsFactors = F, row.names = 1)
total2 <- read.csv("total2.csv", header = T, stringsAsFactors = F, row.names = 1)
recruits3 <- read.csv("recruits3.csv", header = T, stringsAsFactors = F, row.names = 1)
recruits2 <- read.csv("recruits2.csv", header = T, stringsAsFactors = F, row.names = 1) #coral metrics; richness, recruitment, calc rate, cover

weedy3 <- read.csv("weedy3.csv", header = T, stringsAsFactors = F, row.names = 1)
stress3 <- read.csv("stress3.csv", header = T, stringsAsFactors = F, row.names = 1)
comp3 <- read.csv("comp3.csv", header = T, stringsAsFactors = F, row.names = 1)
gen3 <- read.csv("gen3.csv", header = T, stringsAsFactors = F, row.names = 1) #trait-based coral cover

fleshy3 <- read.csv("fleshy3.csv", header = T, stringsAsFactors = F, row.names = 1)
fleshy2 <- read.csv("fleshy2.csv", header = T, stringsAsFactors = F, row.names = 1)
calc3 <- read.csv("calc3.csv", header = T, stringsAsFactors = F, row.names = 1)
calc2 <- read.csv("calc2.csv", header = T, stringsAsFactors = F, row.names = 1)
turf3 <- read.csv("turf3.csv", header = T, stringsAsFactors = F, row.names = 1)
turf2 <- read.csv("turf2.csv", header = T, stringsAsFactors = F, row.names = 1)
cca3 <- read.csv("cca3.csv", header = T, stringsAsFactors = F, row.names = 1)
cca2 <- read.csv("cca2.csv", header = T, stringsAsFactors = F, row.names = 1) #algal cover


#calculate y for mean prediction dataset
#1. model 3 coefficients | 2. model 3 coefficients: identity effect only | 3. model 2 coefficients
predict.mean <- rbind(
  predict.mean %>% filter(Group == "Croppers") %>%
    mutate(richness = exp(P1*richness3[1,1]+
                            P2*richness3[2,1]+
                            P2*richness3[3,1]+
                            P2*richness3[4,1]+
                            P2*richness3[5,1]+
                            depth1*richness3[6,1]+
                            b.herbivore1*richness3[7,1]+
                            year1*richness3[8,1]+
                            diadema1*richness3[9,1]+
                            P1*P2*richness3[10,1]+
                            P1*P2*richness3[11,1]+
                            P1*P2*richness3[12,1]+
                            P1*P2*richness3[13,1]+
                            P2*P2*richness3[14,1]+
                            P2*P2*richness3[15,1]+
                            P2*P2*richness3[16,1]+
                            P2*P2*richness3[17,1]+
                            P2*P2*richness3[18,1]+
                            P2*P2*richness3[19,1])),
  predict.mean %>% filter(Group == "Browsers") %>%
    mutate(richness = exp(P2*richness3[1,1]+
                            P1*richness3[2,1]+
                            P2*richness3[3,1]+
                            P2*richness3[4,1]+
                            P2*richness3[5,1]+
                            depth1*richness3[6,1]+
                            b.herbivore1*richness3[7,1]+
                            year1*richness3[8,1]+
                            diadema1*richness3[9,1]+
                            P2*P1*richness3[10,1]+
                            P2*P2*richness3[11,1]+
                            P2*P2*richness3[12,1]+
                            P2*P2*richness3[13,1]+
                            P1*P2*richness3[14,1]+
                            P1*P2*richness3[15,1]+
                            P1*P2*richness3[16,1]+
                            P2*P2*richness3[17,1]+
                            P2*P2*richness3[18,1]+
                            P2*P2*richness3[19,1])),
  predict.mean %>% filter(Group == "Farmers") %>%
    mutate(richness = exp(P2*richness3[1,1]+
                            P2*richness3[2,1]+
                            P1*richness3[3,1]+
                            P2*richness3[4,1]+
                            P2*richness3[5,1]+
                            depth1*richness3[6,1]+
                            b.herbivore1*richness3[7,1]+
                            year1*richness3[8,1]+
                            diadema1*richness3[9,1]+
                            P2*P2*richness3[10,1]+
                            P2*P1*richness3[11,1]+
                            P2*P2*richness3[12,1]+
                            P2*P2*richness3[13,1]+
                            P2*P1*richness3[14,1]+
                            P2*P2*richness3[15,1]+
                            P2*P2*richness3[16,1]+
                            P1*P2*richness3[17,1]+
                            P1*P2*richness3[18,1]+
                            P2*P2*richness3[19,1])),
  predict.mean %>% filter(Group == "Scrapers") %>%
    mutate(richness = exp(P2*richness3[1,1]+
                            P2*richness3[2,1]+
                            P2*richness3[3,1]+
                            P1*richness3[4,1]+
                            P2*richness3[5,1]+
                            depth1*richness3[6,1]+
                            b.herbivore1*richness3[7,1]+
                            year1*richness3[8,1]+
                            diadema1*richness3[9,1]+
                            P2*P2*richness3[10,1]+
                            P2*P2*richness3[11,1]+
                            P2*P1*richness3[12,1]+
                            P2*P2*richness3[13,1]+
                            P2*P2*richness3[14,1]+
                            P2*P1*richness3[15,1]+
                            P2*P2*richness3[16,1]+
                            P2*P1*richness3[17,1]+
                            P2*P2*richness3[18,1]+
                            P1*P2*richness3[19,1])),
  predict.mean %>% filter(Group == "Excavators") %>%
    mutate(richness = exp(P2*richness3[1,1]+
                            P2*richness3[2,1]+
                            P2*richness3[3,1]+
                            P2*richness3[4,1]+
                            P1*richness3[5,1]+
                            depth1*richness3[6,1]+
                            b.herbivore1*richness3[7,1]+
                            year1*richness3[8,1]+
                            diadema1*richness3[9,1]+
                            P2*P2*richness3[10,1]+
                            P2*P2*richness3[11,1]+
                            P2*P2*richness3[12,1]+
                            P2*P1*richness3[13,1]+
                            P2*P2*richness3[14,1]+
                            P2*P2*richness3[15,1]+
                            P2*P1*richness3[16,1]+
                            P2*P2*richness3[17,1]+
                            P2*P1*richness3[18,1]+
                            P2*P1*richness3[19,1]))
)

predict.mean <- rbind(
  predict.mean %>% filter(Group == "Croppers") %>%
    mutate(richnessid = exp(P1*richness3[1,1]+
                            P2*richness3[2,1]+
                            P2*richness3[3,1]+
                            P2*richness3[4,1]+
                            P2*richness3[5,1]+
                            depth1*richness3[6,1]+
                            b.herbivore1*richness3[7,1]+
                            year1*richness3[8,1]+
                            diadema1*richness3[9,1])),
  predict.mean %>% filter(Group == "Browsers") %>%
    mutate(richnessid = exp(P2*richness3[1,1]+
                            P1*richness3[2,1]+
                            P2*richness3[3,1]+
                            P2*richness3[4,1]+
                            P2*richness3[5,1]+
                            depth1*richness3[6,1]+
                            b.herbivore1*richness3[7,1]+
                            year1*richness3[8,1]+
                            diadema1*richness3[9,1])),
  predict.mean %>% filter(Group == "Farmers") %>%
    mutate(richnessid = exp(P2*richness3[1,1]+
                            P2*richness3[2,1]+
                            P1*richness3[3,1]+
                            P2*richness3[4,1]+
                            P2*richness3[5,1]+
                            depth1*richness3[6,1]+
                            b.herbivore1*richness3[7,1]+
                            year1*richness3[8,1]+
                            diadema1*richness3[9,1])),
  predict.mean %>% filter(Group == "Scrapers") %>%
    mutate(richnessid = exp(P2*richness3[1,1]+
                            P2*richness3[2,1]+
                            P2*richness3[3,1]+
                            P1*richness3[4,1]+
                            P2*richness3[5,1]+
                            depth1*richness3[6,1]+
                            b.herbivore1*richness3[7,1]+
                            year1*richness3[8,1]+
                            diadema1*richness3[9,1])),
  predict.mean %>% filter(Group == "Excavators") %>%
    mutate(richnessid = exp(P2*richness3[1,1]+
                            P2*richness3[2,1]+
                            P2*richness3[3,1]+
                            P2*richness3[4,1]+
                            P1*richness3[5,1]+
                            depth1*richness3[6,1]+
                            b.herbivore1*richness3[7,1]+
                            year1*richness3[8,1]+
                            diadema1*richness3[9,1]))
)

predict.mean <- rbind(
  predict.mean %>% filter(Group == "Croppers") %>%
    mutate(richness2 = exp(P1*richness2[1,1]+
                              P2*richness2[2,1]+
                              P2*richness2[3,1]+
                              P2*richness2[4,1]+
                              P2*richness2[5,1]+
                              depth1*richness2[6,1]+
                              b.herbivore1*richness2[7,1]+
                              year1*richness2[8,1]+
                              diadema1*richness2[9,1])),
  predict.mean %>% filter(Group == "Browsers") %>%
    mutate(richness2 = exp(P2*richness2[1,1]+
                              P1*richness2[2,1]+
                              P2*richness2[3,1]+
                              P2*richness2[4,1]+
                              P2*richness2[5,1]+
                              depth1*richness2[6,1]+
                              b.herbivore1*richness2[7,1]+
                              year1*richness2[8,1]+
                              diadema1*richness2[9,1])),
  predict.mean %>% filter(Group == "Farmers") %>%
    mutate(richness2 = exp(P2*richness2[1,1]+
                              P2*richness2[2,1]+
                              P1*richness2[3,1]+
                              P2*richness2[4,1]+
                              P2*richness2[5,1]+
                              depth1*richness2[6,1]+
                              b.herbivore1*richness2[7,1]+
                              year1*richness2[8,1]+
                              diadema1*richness2[9,1])),
  predict.mean %>% filter(Group == "Scrapers") %>%
    mutate(richness2 = exp(P2*richness2[1,1]+
                              P2*richness2[2,1]+
                              P2*richness2[3,1]+
                              P1*richness2[4,1]+
                              P2*richness2[5,1]+
                              depth1*richness2[6,1]+
                              b.herbivore1*richness2[7,1]+
                              year1*richness2[8,1]+
                              diadema1*richness2[9,1])),
  predict.mean %>% filter(Group == "Excavators") %>%
    mutate(richness2 = exp(P2*richness2[1,1]+
                              P2*richness2[2,1]+
                              P2*richness2[3,1]+
                              P2*richness2[4,1]+
                              P1*richness2[5,1]+
                              depth1*richness2[6,1]+
                              b.herbivore1*richness2[7,1]+
                              year1*richness2[8,1]+
                              diadema1*richness2[9,1]))
)

predict.mean <- rbind(
  predict.mean %>% filter(Group == "Croppers") %>%
    mutate(calcrate = exp(P1*calcrate3[1,1]+
                            P2*calcrate3[2,1]+
                            P2*calcrate3[3,1]+
                            P2*calcrate3[4,1]+
                            P2*calcrate3[5,1]+
                            depth1*calcrate3[6,1]+
                            b.herbivore1*calcrate3[7,1]+
                            year1*calcrate3[8,1]+
                            diadema1*calcrate3[9,1]+
                            P1*P2*calcrate3[10,1]+
                            P1*P2*calcrate3[11,1]+
                            P1*P2*calcrate3[12,1]+
                            P1*P2*calcrate3[13,1]+
                            P2*P2*calcrate3[14,1]+
                            P2*P2*calcrate3[15,1]+
                            P2*P2*calcrate3[16,1]+
                            P2*P2*calcrate3[17,1]+
                            P2*P2*calcrate3[18,1]+
                            P2*P2*calcrate3[19,1])),
  predict.mean %>% filter(Group == "Browsers") %>%
    mutate(calcrate = exp(P2*calcrate3[1,1]+
                            P1*calcrate3[2,1]+
                            P2*calcrate3[3,1]+
                            P2*calcrate3[4,1]+
                            P2*calcrate3[5,1]+
                            depth1*calcrate3[6,1]+
                            b.herbivore1*calcrate3[7,1]+
                            year1*calcrate3[8,1]+
                            diadema1*calcrate3[9,1]+
                            P2*P1*calcrate3[10,1]+
                            P2*P2*calcrate3[11,1]+
                            P2*P2*calcrate3[12,1]+
                            P2*P2*calcrate3[13,1]+
                            P1*P2*calcrate3[14,1]+
                            P1*P2*calcrate3[15,1]+
                            P1*P2*calcrate3[16,1]+
                            P2*P2*calcrate3[17,1]+
                            P2*P2*calcrate3[18,1]+
                            P2*P2*calcrate3[19,1])),
  predict.mean %>% filter(Group == "Farmers") %>%
    mutate(calcrate = exp(P2*calcrate3[1,1]+
                            P2*calcrate3[2,1]+
                            P1*calcrate3[3,1]+
                            P2*calcrate3[4,1]+
                            P2*calcrate3[5,1]+
                            depth1*calcrate3[6,1]+
                            b.herbivore1*calcrate3[7,1]+
                            year1*calcrate3[8,1]+
                            diadema1*calcrate3[9,1]+
                            P2*P2*calcrate3[10,1]+
                            P2*P1*calcrate3[11,1]+
                            P2*P2*calcrate3[12,1]+
                            P2*P2*calcrate3[13,1]+
                            P2*P1*calcrate3[14,1]+
                            P2*P2*calcrate3[15,1]+
                            P2*P2*calcrate3[16,1]+
                            P1*P2*calcrate3[17,1]+
                            P1*P2*calcrate3[18,1]+
                            P2*P2*calcrate3[19,1])),
  predict.mean %>% filter(Group == "Scrapers") %>%
    mutate(calcrate = exp(P2*calcrate3[1,1]+
                            P2*calcrate3[2,1]+
                            P2*calcrate3[3,1]+
                            P1*calcrate3[4,1]+
                            P2*calcrate3[5,1]+
                            depth1*calcrate3[6,1]+
                            b.herbivore1*calcrate3[7,1]+
                            year1*calcrate3[8,1]+
                            diadema1*calcrate3[9,1]+
                            P2*P2*calcrate3[10,1]+
                            P2*P2*calcrate3[11,1]+
                            P2*P1*calcrate3[12,1]+
                            P2*P2*calcrate3[13,1]+
                            P2*P2*calcrate3[14,1]+
                            P2*P1*calcrate3[15,1]+
                            P2*P2*calcrate3[16,1]+
                            P2*P1*calcrate3[17,1]+
                            P2*P2*calcrate3[18,1]+
                            P1*P2*calcrate3[19,1])),
  predict.mean %>% filter(Group == "Excavators") %>%
    mutate(calcrate = exp(P2*calcrate3[1,1]+
                            P2*calcrate3[2,1]+
                            P2*calcrate3[3,1]+
                            P2*calcrate3[4,1]+
                            P1*calcrate3[5,1]+
                            depth1*calcrate3[6,1]+
                            b.herbivore1*calcrate3[7,1]+
                            year1*calcrate3[8,1]+
                            diadema1*calcrate3[9,1]+
                            P2*P2*calcrate3[10,1]+
                            P2*P2*calcrate3[11,1]+
                            P2*P2*calcrate3[12,1]+
                            P2*P1*calcrate3[13,1]+
                            P2*P2*calcrate3[14,1]+
                            P2*P2*calcrate3[15,1]+
                            P2*P1*calcrate3[16,1]+
                            P2*P2*calcrate3[17,1]+
                            P2*P1*calcrate3[18,1]+
                            P2*P1*calcrate3[19,1]))
)

predict.mean <- rbind(
  predict.mean %>% filter(Group == "Croppers") %>%
    mutate(calcrateid = exp(P1*calcrate3[1,1]+
                              P2*calcrate3[2,1]+
                              P2*calcrate3[3,1]+
                              P2*calcrate3[4,1]+
                              P2*calcrate3[5,1]+
                              depth1*calcrate3[6,1]+
                              b.herbivore1*calcrate3[7,1]+
                              year1*calcrate3[8,1]+
                              diadema1*calcrate3[9,1])),
  predict.mean %>% filter(Group == "Browsers") %>%
    mutate(calcrateid = exp(P2*calcrate3[1,1]+
                              P1*calcrate3[2,1]+
                              P2*calcrate3[3,1]+
                              P2*calcrate3[4,1]+
                              P2*calcrate3[5,1]+
                              depth1*calcrate3[6,1]+
                              b.herbivore1*calcrate3[7,1]+
                              year1*calcrate3[8,1]+
                              diadema1*calcrate3[9,1])),
  predict.mean %>% filter(Group == "Farmers") %>%
    mutate(calcrateid = exp(P2*calcrate3[1,1]+
                              P2*calcrate3[2,1]+
                              P1*calcrate3[3,1]+
                              P2*calcrate3[4,1]+
                              P2*calcrate3[5,1]+
                              depth1*calcrate3[6,1]+
                              b.herbivore1*calcrate3[7,1]+
                              year1*calcrate3[8,1]+
                              diadema1*calcrate3[9,1])),
  predict.mean %>% filter(Group == "Scrapers") %>%
    mutate(calcrateid = exp(P2*calcrate3[1,1]+
                              P2*calcrate3[2,1]+
                              P2*calcrate3[3,1]+
                              P1*calcrate3[4,1]+
                              P2*calcrate3[5,1]+
                              depth1*calcrate3[6,1]+
                              b.herbivore1*calcrate3[7,1]+
                              year1*calcrate3[8,1]+
                              diadema1*calcrate3[9,1])),
  predict.mean %>% filter(Group == "Excavators") %>%
    mutate(calcrateid = exp(P2*calcrate3[1,1]+
                              P2*calcrate3[2,1]+
                              P2*calcrate3[3,1]+
                              P2*calcrate3[4,1]+
                              P1*calcrate3[5,1]+
                              depth1*calcrate3[6,1]+
                              b.herbivore1*calcrate3[7,1]+
                              year1*calcrate3[8,1]+
                              diadema1*calcrate3[9,1]))
)

predict.mean <- rbind(
  predict.mean %>% filter(Group == "Croppers") %>%
    mutate(calcrate2 = exp(P1*calcrate2[1,1]+
                              P2*calcrate2[2,1]+
                              P2*calcrate2[3,1]+
                              P2*calcrate2[4,1]+
                              P2*calcrate2[5,1]+
                              depth1*calcrate2[6,1]+
                              b.herbivore1*calcrate2[7,1]+
                              year1*calcrate2[8,1]+
                              diadema1*calcrate2[9,1])),
  predict.mean %>% filter(Group == "Browsers") %>%
    mutate(calcrate2 = exp(P2*calcrate2[1,1]+
                              P1*calcrate2[2,1]+
                              P2*calcrate2[3,1]+
                              P2*calcrate2[4,1]+
                              P2*calcrate2[5,1]+
                              depth1*calcrate2[6,1]+
                              b.herbivore1*calcrate2[7,1]+
                              year1*calcrate2[8,1]+
                              diadema1*calcrate2[9,1])),
  predict.mean %>% filter(Group == "Farmers") %>%
    mutate(calcrate2 = exp(P2*calcrate2[1,1]+
                              P2*calcrate2[2,1]+
                              P1*calcrate2[3,1]+
                              P2*calcrate2[4,1]+
                              P2*calcrate2[5,1]+
                              depth1*calcrate2[6,1]+
                              b.herbivore1*calcrate2[7,1]+
                              year1*calcrate2[8,1]+
                              diadema1*calcrate2[9,1])),
  predict.mean %>% filter(Group == "Scrapers") %>%
    mutate(calcrate2 = exp(P2*calcrate2[1,1]+
                              P2*calcrate2[2,1]+
                              P2*calcrate2[3,1]+
                              P1*calcrate2[4,1]+
                              P2*calcrate2[5,1]+
                              depth1*calcrate2[6,1]+
                              b.herbivore1*calcrate2[7,1]+
                              year1*calcrate2[8,1]+
                              diadema1*calcrate2[9,1])),
  predict.mean %>% filter(Group == "Excavators") %>%
    mutate(calcrate2 = exp(P2*calcrate2[1,1]+
                              P2*calcrate2[2,1]+
                              P2*calcrate2[3,1]+
                              P2*calcrate2[4,1]+
                              P1*calcrate2[5,1]+
                              depth1*calcrate2[6,1]+
                              b.herbivore1*calcrate2[7,1]+
                              year1*calcrate2[8,1]+
                              diadema1*calcrate2[9,1]))
)

predict.mean <- rbind(
  predict.mean %>% filter(Group == "Croppers") %>%
    mutate(total = exp(P1*total3[1,1]+
                            P2*total3[2,1]+
                            P2*total3[3,1]+
                            P2*total3[4,1]+
                            P2*total3[5,1]+
                            depth1*total3[6,1]+
                            b.herbivore1*total3[7,1]+
                            year1*total3[8,1]+
                            diadema1*total3[9,1]+
                            P1*P2*total3[10,1]+
                            P1*P2*total3[11,1]+
                            P1*P2*total3[12,1]+
                            P1*P2*total3[13,1]+
                            P2*P2*total3[14,1]+
                            P2*P2*total3[15,1]+
                            P2*P2*total3[16,1]+
                            P2*P2*total3[17,1]+
                            P2*P2*total3[18,1]+
                            P2*P2*total3[19,1])),
  predict.mean %>% filter(Group == "Browsers") %>%
    mutate(total = exp(P2*total3[1,1]+
                            P1*total3[2,1]+
                            P2*total3[3,1]+
                            P2*total3[4,1]+
                            P2*total3[5,1]+
                            depth1*total3[6,1]+
                            b.herbivore1*total3[7,1]+
                            year1*total3[8,1]+
                            diadema1*total3[9,1]+
                            P2*P1*total3[10,1]+
                            P2*P2*total3[11,1]+
                            P2*P2*total3[12,1]+
                            P2*P2*total3[13,1]+
                            P1*P2*total3[14,1]+
                            P1*P2*total3[15,1]+
                            P1*P2*total3[16,1]+
                            P2*P2*total3[17,1]+
                            P2*P2*total3[18,1]+
                            P2*P2*total3[19,1])),
  predict.mean %>% filter(Group == "Farmers") %>%
    mutate(total = exp(P2*total3[1,1]+
                            P2*total3[2,1]+
                            P1*total3[3,1]+
                            P2*total3[4,1]+
                            P2*total3[5,1]+
                            depth1*total3[6,1]+
                            b.herbivore1*total3[7,1]+
                            year1*total3[8,1]+
                            diadema1*total3[9,1]+
                            P2*P2*total3[10,1]+
                            P2*P1*total3[11,1]+
                            P2*P2*total3[12,1]+
                            P2*P2*total3[13,1]+
                            P2*P1*total3[14,1]+
                            P2*P2*total3[15,1]+
                            P2*P2*total3[16,1]+
                            P1*P2*total3[17,1]+
                            P1*P2*total3[18,1]+
                            P2*P2*total3[19,1])),
  predict.mean %>% filter(Group == "Scrapers") %>%
    mutate(total = exp(P2*total3[1,1]+
                            P2*total3[2,1]+
                            P2*total3[3,1]+
                            P1*total3[4,1]+
                            P2*total3[5,1]+
                            depth1*total3[6,1]+
                            b.herbivore1*total3[7,1]+
                            year1*total3[8,1]+
                            diadema1*total3[9,1]+
                            P2*P2*total3[10,1]+
                            P2*P2*total3[11,1]+
                            P2*P1*total3[12,1]+
                            P2*P2*total3[13,1]+
                            P2*P2*total3[14,1]+
                            P2*P1*total3[15,1]+
                            P2*P2*total3[16,1]+
                            P2*P1*total3[17,1]+
                            P2*P2*total3[18,1]+
                            P1*P2*total3[19,1])),
  predict.mean %>% filter(Group == "Excavators") %>%
    mutate(total = exp(P2*total3[1,1]+
                            P2*total3[2,1]+
                            P2*total3[3,1]+
                            P2*total3[4,1]+
                            P1*total3[5,1]+
                            depth1*total3[6,1]+
                            b.herbivore1*total3[7,1]+
                            year1*total3[8,1]+
                            diadema1*total3[9,1]+
                            P2*P2*total3[10,1]+
                            P2*P2*total3[11,1]+
                            P2*P2*total3[12,1]+
                            P2*P1*total3[13,1]+
                            P2*P2*total3[14,1]+
                            P2*P2*total3[15,1]+
                            P2*P1*total3[16,1]+
                            P2*P2*total3[17,1]+
                            P2*P1*total3[18,1]+
                            P2*P1*total3[19,1]))
)

predict.mean <- rbind(
  predict.mean %>% filter(Group == "Croppers") %>%
    mutate(totalid = exp(P1*total3[1,1]+
                           P2*total3[2,1]+
                           P2*total3[3,1]+
                           P2*total3[4,1]+
                           P2*total3[5,1]+
                           depth1*total3[6,1]+
                           b.herbivore1*total3[7,1]+
                           year1*total3[8,1]+
                           diadema1*total3[9,1])),
  predict.mean %>% filter(Group == "Browsers") %>%
    mutate(totalid = exp(P2*total3[1,1]+
                           P1*total3[2,1]+
                           P2*total3[3,1]+
                           P2*total3[4,1]+
                           P2*total3[5,1]+
                           depth1*total3[6,1]+
                           b.herbivore1*total3[7,1]+
                           year1*total3[8,1]+
                           diadema1*total3[9,1])),
  predict.mean %>% filter(Group == "Farmers") %>%
    mutate(totalid = exp(P2*total3[1,1]+
                           P2*total3[2,1]+
                           P1*total3[3,1]+
                           P2*total3[4,1]+
                           P2*total3[5,1]+
                           depth1*total3[6,1]+
                           b.herbivore1*total3[7,1]+
                           year1*total3[8,1]+
                           diadema1*total3[9,1])),
  predict.mean %>% filter(Group == "Scrapers") %>%
    mutate(totalid = exp(P2*total3[1,1]+
                           P2*total3[2,1]+
                           P2*total3[3,1]+
                           P1*total3[4,1]+
                           P2*total3[5,1]+
                           depth1*total3[6,1]+
                           b.herbivore1*total3[7,1]+
                           year1*total3[8,1]+
                           diadema1*total3[9,1])),
  predict.mean %>% filter(Group == "Excavators") %>%
    mutate(totalid = exp(P2*total3[1,1]+
                           P2*total3[2,1]+
                           P2*total3[3,1]+
                           P2*total3[4,1]+
                           P1*total3[5,1]+
                           depth1*total3[6,1]+
                           b.herbivore1*total3[7,1]+
                           year1*total3[8,1]+
                           diadema1*total3[9,1]))
)

predict.mean <- rbind(
  predict.mean %>% filter(Group == "Croppers") %>%
    mutate(total2 = exp(P1*total2[1,1]+
                         P2*total2[2,1]+
                         P2*total2[3,1]+
                         P2*total2[4,1]+
                         P2*total2[5,1]+
                         depth1*total2[6,1]+
                         b.herbivore1*total2[7,1]+
                         year1*total2[8,1]+
                         diadema1*total2[9,1])),
  predict.mean %>% filter(Group == "Browsers") %>%
    mutate(total2 = exp(P2*total2[1,1]+
                         P1*total2[2,1]+
                         P2*total2[3,1]+
                         P2*total2[4,1]+
                         P2*total2[5,1]+
                         depth1*total2[6,1]+
                         b.herbivore1*total2[7,1]+
                         year1*total2[8,1]+
                         diadema1*total2[9,1])),
  predict.mean %>% filter(Group == "Farmers") %>%
    mutate(total2 = exp(P2*total2[1,1]+
                         P2*total2[2,1]+
                         P1*total2[3,1]+
                         P2*total2[4,1]+
                         P2*total2[5,1]+
                         depth1*total2[6,1]+
                         b.herbivore1*total2[7,1]+
                         year1*total2[8,1]+
                         diadema1*total2[9,1])),
  predict.mean %>% filter(Group == "Scrapers") %>%
    mutate(total2 = exp(P2*total2[1,1]+
                         P2*total2[2,1]+
                         P2*total2[3,1]+
                         P1*total2[4,1]+
                         P2*total2[5,1]+
                         depth1*total2[6,1]+
                         b.herbivore1*total2[7,1]+
                         year1*total2[8,1]+
                         diadema1*total2[9,1])),
  predict.mean %>% filter(Group == "Excavators") %>%
    mutate(total2 = exp(P2*total2[1,1]+
                         P2*total2[2,1]+
                         P2*total2[3,1]+
                         P2*total2[4,1]+
                         P1*total2[5,1]+
                         depth1*total2[6,1]+
                         b.herbivore1*total2[7,1]+
                         year1*total2[8,1]+
                         diadema1*total2[9,1]))
)

predict.mean <- rbind(
  predict.mean %>% filter(Group == "Croppers") %>%
    mutate(recruits = exp(P1*recruits3[1,1]+
                            P2*recruits3[2,1]+
                            P2*recruits3[3,1]+
                            P2*recruits3[4,1]+
                            P2*recruits3[5,1]+
                            depth2*recruits3[6,1]+
                            b.herbivore2*recruits3[7,1]+
                            year2*recruits3[8,1]+
                            diadema2*recruits3[9,1]+
                            P1*P2*recruits3[10,1]+
                            P1*P2*recruits3[11,1]+
                            P1*P2*recruits3[12,1]+
                            P1*P2*recruits3[13,1]+
                            P2*P2*recruits3[14,1]+
                            P2*P2*recruits3[15,1]+
                            P2*P2*recruits3[16,1]+
                            P2*P2*recruits3[17,1]+
                            P2*P2*recruits3[18,1]+
                            P2*P2*recruits3[19,1])),
  predict.mean %>% filter(Group == "Browsers") %>%
    mutate(recruits = exp(P2*recruits3[1,1]+
                            P1*recruits3[2,1]+
                            P2*recruits3[3,1]+
                            P2*recruits3[4,1]+
                            P2*recruits3[5,1]+
                            depth2*recruits3[6,1]+
                            b.herbivore2*recruits3[7,1]+
                            year2*recruits3[8,1]+
                            diadema2*recruits3[9,1]+
                            P2*P1*recruits3[10,1]+
                            P2*P2*recruits3[11,1]+
                            P2*P2*recruits3[12,1]+
                            P2*P2*recruits3[13,1]+
                            P1*P2*recruits3[14,1]+
                            P1*P2*recruits3[15,1]+
                            P1*P2*recruits3[16,1]+
                            P2*P2*recruits3[17,1]+
                            P2*P2*recruits3[18,1]+
                            P2*P2*recruits3[19,1])),
  predict.mean %>% filter(Group == "Farmers") %>%
    mutate(recruits = exp(P2*recruits3[1,1]+
                            P2*recruits3[2,1]+
                            P1*recruits3[3,1]+
                            P2*recruits3[4,1]+
                            P2*recruits3[5,1]+
                            depth2*recruits3[6,1]+
                            b.herbivore2*recruits3[7,1]+
                            year2*recruits3[8,1]+
                            diadema2*recruits3[9,1]+
                            P2*P2*recruits3[10,1]+
                            P2*P1*recruits3[11,1]+
                            P2*P2*recruits3[12,1]+
                            P2*P2*recruits3[13,1]+
                            P2*P1*recruits3[14,1]+
                            P2*P2*recruits3[15,1]+
                            P2*P2*recruits3[16,1]+
                            P1*P2*recruits3[17,1]+
                            P1*P2*recruits3[18,1]+
                            P2*P2*recruits3[19,1])),
  predict.mean %>% filter(Group == "Scrapers") %>%
    mutate(recruits = exp(P2*recruits3[1,1]+
                            P2*recruits3[2,1]+
                            P2*recruits3[3,1]+
                            P1*recruits3[4,1]+
                            P2*recruits3[5,1]+
                            depth2*recruits3[6,1]+
                            b.herbivore2*recruits3[7,1]+
                            year2*recruits3[8,1]+
                            diadema2*recruits3[9,1]+
                            P2*P2*recruits3[10,1]+
                            P2*P2*recruits3[11,1]+
                            P2*P1*recruits3[12,1]+
                            P2*P2*recruits3[13,1]+
                            P2*P2*recruits3[14,1]+
                            P2*P1*recruits3[15,1]+
                            P2*P2*recruits3[16,1]+
                            P2*P1*recruits3[17,1]+
                            P2*P2*recruits3[18,1]+
                            P1*P2*recruits3[19,1])),
  predict.mean %>% filter(Group == "Excavators") %>%
    mutate(recruits = exp(P2*recruits3[1,1]+
                            P2*recruits3[2,1]+
                            P2*recruits3[3,1]+
                            P2*recruits3[4,1]+
                            P1*recruits3[5,1]+
                            depth2*recruits3[6,1]+
                            b.herbivore2*recruits3[7,1]+
                            year2*recruits3[8,1]+
                            diadema2*recruits3[9,1]+
                            P2*P2*recruits3[10,1]+
                            P2*P2*recruits3[11,1]+
                            P2*P2*recruits3[12,1]+
                            P2*P1*recruits3[13,1]+
                            P2*P2*recruits3[14,1]+
                            P2*P2*recruits3[15,1]+
                            P2*P1*recruits3[16,1]+
                            P2*P2*recruits3[17,1]+
                            P2*P1*recruits3[18,1]+
                            P2*P1*recruits3[19,1]))
)

predict.mean <- rbind(
  predict.mean %>% filter(Group == "Croppers") %>%
    mutate(recruitsid = exp(P1*recruits3[1,1]+
                              P2*recruits3[2,1]+
                              P2*recruits3[3,1]+
                              P2*recruits3[4,1]+
                              P2*recruits3[5,1]+
                              depth2*recruits3[6,1]+
                              b.herbivore2*recruits3[7,1]+
                              year2*recruits3[8,1]+
                              diadema2*recruits3[9,1])),
  predict.mean %>% filter(Group == "Browsers") %>%
    mutate(recruitsid = exp(P2*recruits3[1,1]+
                              P1*recruits3[2,1]+
                              P2*recruits3[3,1]+
                              P2*recruits3[4,1]+
                              P2*recruits3[5,1]+
                              depth2*recruits3[6,1]+
                              b.herbivore2*recruits3[7,1]+
                              year2*recruits3[8,1]+
                              diadema2*recruits3[9,1])),
  predict.mean %>% filter(Group == "Farmers") %>%
    mutate(recruitsid = exp(P2*recruits3[1,1]+
                              P2*recruits3[2,1]+
                              P1*recruits3[3,1]+
                              P2*recruits3[4,1]+
                              P2*recruits3[5,1]+
                              depth2*recruits3[6,1]+
                              b.herbivore2*recruits3[7,1]+
                              year2*recruits3[8,1]+
                              diadema2*recruits3[9,1])),
  predict.mean %>% filter(Group == "Scrapers") %>%
    mutate(recruitsid = exp(P2*recruits3[1,1]+
                              P2*recruits3[2,1]+
                              P2*recruits3[3,1]+
                              P1*recruits3[4,1]+
                              P2*recruits3[5,1]+
                              depth2*recruits3[6,1]+
                              b.herbivore2*recruits3[7,1]+
                              year2*recruits3[8,1]+
                              diadema2*recruits3[9,1])),
  predict.mean %>% filter(Group == "Excavators") %>%
    mutate(recruitsid = exp(P2*recruits3[1,1]+
                              P2*recruits3[2,1]+
                              P2*recruits3[3,1]+
                              P2*recruits3[4,1]+
                              P1*recruits3[5,1]+
                              depth2*recruits3[6,1]+
                              b.herbivore2*recruits3[7,1]+
                              year2*recruits3[8,1]+
                              diadema2*recruits3[9,1]))
)

predict.mean <- rbind(
  predict.mean %>% filter(Group == "Croppers") %>%
    mutate(recruits2 = exp(P1*recruits2[1,1]+
                              P2*recruits2[2,1]+
                              P2*recruits2[3,1]+
                              P2*recruits2[4,1]+
                              P2*recruits2[5,1]+
                              depth2*recruits2[6,1]+
                              b.herbivore2*recruits2[7,1]+
                              year2*recruits2[8,1]+
                              diadema2*recruits2[9,1])),
  predict.mean %>% filter(Group == "Browsers") %>%
    mutate(recruits2 = exp(P2*recruits2[1,1]+
                              P1*recruits2[2,1]+
                              P2*recruits2[3,1]+
                              P2*recruits2[4,1]+
                              P2*recruits2[5,1]+
                              depth2*recruits2[6,1]+
                              b.herbivore2*recruits2[7,1]+
                              year2*recruits2[8,1]+
                              diadema2*recruits2[9,1])),
  predict.mean %>% filter(Group == "Farmers") %>%
    mutate(recruits2 = exp(P2*recruits2[1,1]+
                              P2*recruits2[2,1]+
                              P1*recruits2[3,1]+
                              P2*recruits2[4,1]+
                              P2*recruits2[5,1]+
                              depth2*recruits2[6,1]+
                              b.herbivore2*recruits2[7,1]+
                              year2*recruits2[8,1]+
                              diadema2*recruits2[9,1])),
  predict.mean %>% filter(Group == "Scrapers") %>%
    mutate(recruits2 = exp(P2*recruits2[1,1]+
                              P2*recruits2[2,1]+
                              P2*recruits2[3,1]+
                              P1*recruits2[4,1]+
                              P2*recruits2[5,1]+
                              depth2*recruits2[6,1]+
                              b.herbivore2*recruits2[7,1]+
                              year2*recruits2[8,1]+
                              diadema2*recruits2[9,1])),
  predict.mean %>% filter(Group == "Excavators") %>%
    mutate(recruits2 = exp(P2*recruits2[1,1]+
                              P2*recruits2[2,1]+
                              P2*recruits2[3,1]+
                              P2*recruits2[4,1]+
                              P1*recruits2[5,1]+
                              depth2*recruits2[6,1]+
                              b.herbivore2*recruits2[7,1]+
                              year2*recruits2[8,1]+
                              diadema2*recruits2[9,1]))
)

predict.mean <- rbind(
  predict.mean %>% filter(Group == "Croppers") %>%
    mutate(weedy = exp(P1*weedy3[1,1]+
                         P2*weedy3[2,1]+
                         P2*weedy3[3,1]+
                         P2*weedy3[4,1]+
                         P2*weedy3[5,1]+
                         depth1*weedy3[6,1]+
                         b.herbivore1*weedy3[7,1]+
                         year1*weedy3[8,1]+
                         diadema1*weedy3[9,1]+
                         P1*P2*weedy3[10,1]+
                         P1*P2*weedy3[11,1]+
                         P1*P2*weedy3[12,1]+
                         P1*P2*weedy3[13,1]+
                         P2*P2*weedy3[14,1]+
                         P2*P2*weedy3[15,1]+
                         P2*P2*weedy3[16,1]+
                         P2*P2*weedy3[17,1]+
                         P2*P2*weedy3[18,1]+
                         P2*P2*weedy3[19,1])),
  predict.mean %>% filter(Group == "Browsers") %>%
    mutate(weedy = exp(P2*weedy3[1,1]+
                         P1*weedy3[2,1]+
                         P2*weedy3[3,1]+
                         P2*weedy3[4,1]+
                         P2*weedy3[5,1]+
                         depth1*weedy3[6,1]+
                         b.herbivore1*weedy3[7,1]+
                         year1*weedy3[8,1]+
                         diadema1*weedy3[9,1]+
                         P2*P1*weedy3[10,1]+
                         P2*P2*weedy3[11,1]+
                         P2*P2*weedy3[12,1]+
                         P2*P2*weedy3[13,1]+
                         P1*P2*weedy3[14,1]+
                         P1*P2*weedy3[15,1]+
                         P1*P2*weedy3[16,1]+
                         P2*P2*weedy3[17,1]+
                         P2*P2*weedy3[18,1]+
                         P2*P2*weedy3[19,1])),
  predict.mean %>% filter(Group == "Farmers") %>%
    mutate(weedy = exp(P2*weedy3[1,1]+
                         P2*weedy3[2,1]+
                         P1*weedy3[3,1]+
                         P2*weedy3[4,1]+
                         P2*weedy3[5,1]+
                         depth1*weedy3[6,1]+
                         b.herbivore1*weedy3[7,1]+
                         year1*weedy3[8,1]+
                         diadema1*weedy3[9,1]+
                         P2*P2*weedy3[10,1]+
                         P2*P1*weedy3[11,1]+
                         P2*P2*weedy3[12,1]+
                         P2*P2*weedy3[13,1]+
                         P2*P1*weedy3[14,1]+
                         P2*P2*weedy3[15,1]+
                         P2*P2*weedy3[16,1]+
                         P1*P2*weedy3[17,1]+
                         P1*P2*weedy3[18,1]+
                         P2*P2*weedy3[19,1])),
  predict.mean %>% filter(Group == "Scrapers") %>%
    mutate(weedy = exp(P2*weedy3[1,1]+
                         P2*weedy3[2,1]+
                         P2*weedy3[3,1]+
                         P1*weedy3[4,1]+
                         P2*weedy3[5,1]+
                         depth1*weedy3[6,1]+
                         b.herbivore1*weedy3[7,1]+
                         year1*weedy3[8,1]+
                         diadema1*weedy3[9,1]+
                         P2*P2*weedy3[10,1]+
                         P2*P2*weedy3[11,1]+
                         P2*P1*weedy3[12,1]+
                         P2*P2*weedy3[13,1]+
                         P2*P2*weedy3[14,1]+
                         P2*P1*weedy3[15,1]+
                         P2*P2*weedy3[16,1]+
                         P2*P1*weedy3[17,1]+
                         P2*P2*weedy3[18,1]+
                         P1*P2*weedy3[19,1])),
  predict.mean %>% filter(Group == "Excavators") %>%
    mutate(weedy = exp(P2*weedy3[1,1]+
                         P2*weedy3[2,1]+
                         P2*weedy3[3,1]+
                         P2*weedy3[4,1]+
                         P1*weedy3[5,1]+
                         depth1*weedy3[6,1]+
                         b.herbivore1*weedy3[7,1]+
                         year1*weedy3[8,1]+
                         diadema1*weedy3[9,1]+
                         P2*P2*weedy3[10,1]+
                         P2*P2*weedy3[11,1]+
                         P2*P2*weedy3[12,1]+
                         P2*P1*weedy3[13,1]+
                         P2*P2*weedy3[14,1]+
                         P2*P2*weedy3[15,1]+
                         P2*P1*weedy3[16,1]+
                         P2*P2*weedy3[17,1]+
                         P2*P1*weedy3[18,1]+
                         P2*P1*weedy3[19,1]))
)

predict.mean <- rbind(
  predict.mean %>% filter(Group == "Croppers") %>%
    mutate(stress = exp(P1*stress3[1,1]+
                         P2*stress3[2,1]+
                         P2*stress3[3,1]+
                         P2*stress3[4,1]+
                         P2*stress3[5,1]+
                         depth1*stress3[6,1]+
                         b.herbivore1*stress3[7,1]+
                         year1*stress3[8,1]+
                         diadema1*stress3[9,1]+
                         P1*P2*stress3[10,1]+
                         P1*P2*stress3[11,1]+
                         P1*P2*stress3[12,1]+
                         P1*P2*stress3[13,1]+
                         P2*P2*stress3[14,1]+
                         P2*P2*stress3[15,1]+
                         P2*P2*stress3[16,1]+
                         P2*P2*stress3[17,1]+
                         P2*P2*stress3[18,1]+
                         P2*P2*stress3[19,1])),
  predict.mean %>% filter(Group == "Browsers") %>%
    mutate(stress = exp(P2*stress3[1,1]+
                         P1*stress3[2,1]+
                         P2*stress3[3,1]+
                         P2*stress3[4,1]+
                         P2*stress3[5,1]+
                         depth1*stress3[6,1]+
                         b.herbivore1*stress3[7,1]+
                         year1*stress3[8,1]+
                         diadema1*stress3[9,1]+
                         P2*P1*stress3[10,1]+
                         P2*P2*stress3[11,1]+
                         P2*P2*stress3[12,1]+
                         P2*P2*stress3[13,1]+
                         P1*P2*stress3[14,1]+
                         P1*P2*stress3[15,1]+
                         P1*P2*stress3[16,1]+
                         P2*P2*stress3[17,1]+
                         P2*P2*stress3[18,1]+
                         P2*P2*stress3[19,1])),
  predict.mean %>% filter(Group == "Farmers") %>%
    mutate(stress = exp(P2*stress3[1,1]+
                         P2*stress3[2,1]+
                         P1*stress3[3,1]+
                         P2*stress3[4,1]+
                         P2*stress3[5,1]+
                         depth1*stress3[6,1]+
                         b.herbivore1*stress3[7,1]+
                         year1*stress3[8,1]+
                         diadema1*stress3[9,1]+
                         P2*P2*stress3[10,1]+
                         P2*P1*stress3[11,1]+
                         P2*P2*stress3[12,1]+
                         P2*P2*stress3[13,1]+
                         P2*P1*stress3[14,1]+
                         P2*P2*stress3[15,1]+
                         P2*P2*stress3[16,1]+
                         P1*P2*stress3[17,1]+
                         P1*P2*stress3[18,1]+
                         P2*P2*stress3[19,1])),
  predict.mean %>% filter(Group == "Scrapers") %>%
    mutate(stress = exp(P2*stress3[1,1]+
                         P2*stress3[2,1]+
                         P2*stress3[3,1]+
                         P1*stress3[4,1]+
                         P2*stress3[5,1]+
                         depth1*stress3[6,1]+
                         b.herbivore1*stress3[7,1]+
                         year1*stress3[8,1]+
                         diadema1*stress3[9,1]+
                         P2*P2*stress3[10,1]+
                         P2*P2*stress3[11,1]+
                         P2*P1*stress3[12,1]+
                         P2*P2*stress3[13,1]+
                         P2*P2*stress3[14,1]+
                         P2*P1*stress3[15,1]+
                         P2*P2*stress3[16,1]+
                         P2*P1*stress3[17,1]+
                         P2*P2*stress3[18,1]+
                         P1*P2*stress3[19,1])),
  predict.mean %>% filter(Group == "Excavators") %>%
    mutate(stress = exp(P2*stress3[1,1]+
                         P2*stress3[2,1]+
                         P2*stress3[3,1]+
                         P2*stress3[4,1]+
                         P1*stress3[5,1]+
                         depth1*stress3[6,1]+
                         b.herbivore1*stress3[7,1]+
                         year1*stress3[8,1]+
                         diadema1*stress3[9,1]+
                         P2*P2*stress3[10,1]+
                         P2*P2*stress3[11,1]+
                         P2*P2*stress3[12,1]+
                         P2*P1*stress3[13,1]+
                         P2*P2*stress3[14,1]+
                         P2*P2*stress3[15,1]+
                         P2*P1*stress3[16,1]+
                         P2*P2*stress3[17,1]+
                         P2*P1*stress3[18,1]+
                         P2*P1*stress3[19,1]))
)

predict.mean <- rbind(
  predict.mean %>% filter(Group == "Croppers") %>%
    mutate(comp = exp(P1*comp3[1,1]+
                         P2*comp3[2,1]+
                         P2*comp3[3,1]+
                         P2*comp3[4,1]+
                         P2*comp3[5,1]+
                         depth1*comp3[6,1]+
                         b.herbivore1*comp3[7,1]+
                         year1*comp3[8,1]+
                         diadema1*comp3[9,1]+
                         P1*P2*comp3[10,1]+
                         P1*P2*comp3[11,1]+
                         P1*P2*comp3[12,1]+
                         P1*P2*comp3[13,1]+
                         P2*P2*comp3[14,1]+
                         P2*P2*comp3[15,1]+
                         P2*P2*comp3[16,1]+
                         P2*P2*comp3[17,1]+
                         P2*P2*comp3[18,1]+
                         P2*P2*comp3[19,1])),
  predict.mean %>% filter(Group == "Browsers") %>%
    mutate(comp = exp(P2*comp3[1,1]+
                         P1*comp3[2,1]+
                         P2*comp3[3,1]+
                         P2*comp3[4,1]+
                         P2*comp3[5,1]+
                         depth1*comp3[6,1]+
                         b.herbivore1*comp3[7,1]+
                         year1*comp3[8,1]+
                         diadema1*comp3[9,1]+
                         P2*P1*comp3[10,1]+
                         P2*P2*comp3[11,1]+
                         P2*P2*comp3[12,1]+
                         P2*P2*comp3[13,1]+
                         P1*P2*comp3[14,1]+
                         P1*P2*comp3[15,1]+
                         P1*P2*comp3[16,1]+
                         P2*P2*comp3[17,1]+
                         P2*P2*comp3[18,1]+
                         P2*P2*comp3[19,1])),
  predict.mean %>% filter(Group == "Farmers") %>%
    mutate(comp = exp(P2*comp3[1,1]+
                         P2*comp3[2,1]+
                         P1*comp3[3,1]+
                         P2*comp3[4,1]+
                         P2*comp3[5,1]+
                         depth1*comp3[6,1]+
                         b.herbivore1*comp3[7,1]+
                         year1*comp3[8,1]+
                         diadema1*comp3[9,1]+
                         P2*P2*comp3[10,1]+
                         P2*P1*comp3[11,1]+
                         P2*P2*comp3[12,1]+
                         P2*P2*comp3[13,1]+
                         P2*P1*comp3[14,1]+
                         P2*P2*comp3[15,1]+
                         P2*P2*comp3[16,1]+
                         P1*P2*comp3[17,1]+
                         P1*P2*comp3[18,1]+
                         P2*P2*comp3[19,1])),
  predict.mean %>% filter(Group == "Scrapers") %>%
    mutate(comp = exp(P2*comp3[1,1]+
                         P2*comp3[2,1]+
                         P2*comp3[3,1]+
                         P1*comp3[4,1]+
                         P2*comp3[5,1]+
                         depth1*comp3[6,1]+
                         b.herbivore1*comp3[7,1]+
                         year1*comp3[8,1]+
                         diadema1*comp3[9,1]+
                         P2*P2*comp3[10,1]+
                         P2*P2*comp3[11,1]+
                         P2*P1*comp3[12,1]+
                         P2*P2*comp3[13,1]+
                         P2*P2*comp3[14,1]+
                         P2*P1*comp3[15,1]+
                         P2*P2*comp3[16,1]+
                         P2*P1*comp3[17,1]+
                         P2*P2*comp3[18,1]+
                         P1*P2*comp3[19,1])),
  predict.mean %>% filter(Group == "Excavators") %>%
    mutate(comp = exp(P2*comp3[1,1]+
                         P2*comp3[2,1]+
                         P2*comp3[3,1]+
                         P2*comp3[4,1]+
                         P1*comp3[5,1]+
                         depth1*comp3[6,1]+
                         b.herbivore1*comp3[7,1]+
                         year1*comp3[8,1]+
                         diadema1*comp3[9,1]+
                         P2*P2*comp3[10,1]+
                         P2*P2*comp3[11,1]+
                         P2*P2*comp3[12,1]+
                         P2*P1*comp3[13,1]+
                         P2*P2*comp3[14,1]+
                         P2*P2*comp3[15,1]+
                         P2*P1*comp3[16,1]+
                         P2*P2*comp3[17,1]+
                         P2*P1*comp3[18,1]+
                         P2*P1*comp3[19,1]))
)

predict.mean <- rbind(
  predict.mean %>% filter(Group == "Croppers") %>%
    mutate(gen = exp(P1*gen3[1,1]+
                         P2*gen3[2,1]+
                         P2*gen3[3,1]+
                         P2*gen3[4,1]+
                         P2*gen3[5,1]+
                         depth1*gen3[6,1]+
                         b.herbivore1*gen3[7,1]+
                         year1*gen3[8,1]+
                         diadema1*gen3[9,1]+
                         P1*P2*gen3[10,1]+
                         P1*P2*gen3[11,1]+
                         P1*P2*gen3[12,1]+
                         P1*P2*gen3[13,1]+
                         P2*P2*gen3[14,1]+
                         P2*P2*gen3[15,1]+
                         P2*P2*gen3[16,1]+
                         P2*P2*gen3[17,1]+
                         P2*P2*gen3[18,1]+
                         P2*P2*gen3[19,1])),
  predict.mean %>% filter(Group == "Browsers") %>%
    mutate(gen = exp(P2*gen3[1,1]+
                         P1*gen3[2,1]+
                         P2*gen3[3,1]+
                         P2*gen3[4,1]+
                         P2*gen3[5,1]+
                         depth1*gen3[6,1]+
                         b.herbivore1*gen3[7,1]+
                         year1*gen3[8,1]+
                         diadema1*gen3[9,1]+
                         P2*P1*gen3[10,1]+
                         P2*P2*gen3[11,1]+
                         P2*P2*gen3[12,1]+
                         P2*P2*gen3[13,1]+
                         P1*P2*gen3[14,1]+
                         P1*P2*gen3[15,1]+
                         P1*P2*gen3[16,1]+
                         P2*P2*gen3[17,1]+
                         P2*P2*gen3[18,1]+
                         P2*P2*gen3[19,1])),
  predict.mean %>% filter(Group == "Farmers") %>%
    mutate(gen = exp(P2*gen3[1,1]+
                         P2*gen3[2,1]+
                         P1*gen3[3,1]+
                         P2*gen3[4,1]+
                         P2*gen3[5,1]+
                         depth1*gen3[6,1]+
                         b.herbivore1*gen3[7,1]+
                         year1*gen3[8,1]+
                         diadema1*gen3[9,1]+
                         P2*P2*gen3[10,1]+
                         P2*P1*gen3[11,1]+
                         P2*P2*gen3[12,1]+
                         P2*P2*gen3[13,1]+
                         P2*P1*gen3[14,1]+
                         P2*P2*gen3[15,1]+
                         P2*P2*gen3[16,1]+
                         P1*P2*gen3[17,1]+
                         P1*P2*gen3[18,1]+
                         P2*P2*gen3[19,1])),
  predict.mean %>% filter(Group == "Scrapers") %>%
    mutate(gen = exp(P2*gen3[1,1]+
                         P2*gen3[2,1]+
                         P2*gen3[3,1]+
                         P1*gen3[4,1]+
                         P2*gen3[5,1]+
                         depth1*gen3[6,1]+
                         b.herbivore1*gen3[7,1]+
                         year1*gen3[8,1]+
                         diadema1*gen3[9,1]+
                         P2*P2*gen3[10,1]+
                         P2*P2*gen3[11,1]+
                         P2*P1*gen3[12,1]+
                         P2*P2*gen3[13,1]+
                         P2*P2*gen3[14,1]+
                         P2*P1*gen3[15,1]+
                         P2*P2*gen3[16,1]+
                         P2*P1*gen3[17,1]+
                         P2*P2*gen3[18,1]+
                         P1*P2*gen3[19,1])),
  predict.mean %>% filter(Group == "Excavators") %>%
    mutate(gen = exp(P2*gen3[1,1]+
                         P2*gen3[2,1]+
                         P2*gen3[3,1]+
                         P2*gen3[4,1]+
                         P1*gen3[5,1]+
                         depth1*gen3[6,1]+
                         b.herbivore1*gen3[7,1]+
                         year1*gen3[8,1]+
                         diadema1*gen3[9,1]+
                         P2*P2*gen3[10,1]+
                         P2*P2*gen3[11,1]+
                         P2*P2*gen3[12,1]+
                         P2*P1*gen3[13,1]+
                         P2*P2*gen3[14,1]+
                         P2*P2*gen3[15,1]+
                         P2*P1*gen3[16,1]+
                         P2*P2*gen3[17,1]+
                         P2*P1*gen3[18,1]+
                         P2*P1*gen3[19,1]))
)

predict.mean <- rbind(
  predict.mean %>% filter(Group == "Croppers") %>%
    mutate(fleshy = inv.logit(P1*fleshy3[1,1]+
                       P2*fleshy3[2,1]+
                       P2*fleshy3[3,1]+
                       P2*fleshy3[4,1]+
                       P2*fleshy3[5,1]+
                       depth3*fleshy3[6,1]+
                       b.herbivore3*fleshy3[7,1]+
                       year3*fleshy3[8,1]+
                       diadema3*fleshy3[9,1]+
                       P1*P2*fleshy3[10,1]+
                       P1*P2*fleshy3[11,1]+
                       P1*P2*fleshy3[12,1]+
                       P1*P2*fleshy3[13,1]+
                       P2*P2*fleshy3[14,1]+
                       P2*P2*fleshy3[15,1]+
                       P2*P2*fleshy3[16,1]+
                       P2*P2*fleshy3[17,1]+
                       P2*P2*fleshy3[18,1]+
                       P2*P2*fleshy3[19,1])),
  predict.mean %>% filter(Group == "Browsers") %>%
    mutate(fleshy = inv.logit(P2*fleshy3[1,1]+
                       P1*fleshy3[2,1]+
                       P2*fleshy3[3,1]+
                       P2*fleshy3[4,1]+
                       P2*fleshy3[5,1]+
                       depth3*fleshy3[6,1]+
                       b.herbivore3*fleshy3[7,1]+
                       year3*fleshy3[8,1]+
                       diadema3*fleshy3[9,1]+
                       P2*P1*fleshy3[10,1]+
                       P2*P2*fleshy3[11,1]+
                       P2*P2*fleshy3[12,1]+
                       P2*P2*fleshy3[13,1]+
                       P1*P2*fleshy3[14,1]+
                       P1*P2*fleshy3[15,1]+
                       P1*P2*fleshy3[16,1]+
                       P2*P2*fleshy3[17,1]+
                       P2*P2*fleshy3[18,1]+
                       P2*P2*fleshy3[19,1])),
  predict.mean %>% filter(Group == "Farmers") %>%
    mutate(fleshy = inv.logit(P2*fleshy3[1,1]+
                       P2*fleshy3[2,1]+
                       P1*fleshy3[3,1]+
                       P2*fleshy3[4,1]+
                       P2*fleshy3[5,1]+
                       depth3*fleshy3[6,1]+
                       b.herbivore3*fleshy3[7,1]+
                       year3*fleshy3[8,1]+
                       diadema3*fleshy3[9,1]+
                       P2*P2*fleshy3[10,1]+
                       P2*P1*fleshy3[11,1]+
                       P2*P2*fleshy3[12,1]+
                       P2*P2*fleshy3[13,1]+
                       P2*P1*fleshy3[14,1]+
                       P2*P2*fleshy3[15,1]+
                       P2*P2*fleshy3[16,1]+
                       P1*P2*fleshy3[17,1]+
                       P1*P2*fleshy3[18,1]+
                       P2*P2*fleshy3[19,1])),
  predict.mean %>% filter(Group == "Scrapers") %>%
    mutate(fleshy = inv.logit(P2*fleshy3[1,1]+
                       P2*fleshy3[2,1]+
                       P2*fleshy3[3,1]+
                       P1*fleshy3[4,1]+
                       P2*fleshy3[5,1]+
                       depth3*fleshy3[6,1]+
                       b.herbivore3*fleshy3[7,1]+
                       year3*fleshy3[8,1]+
                       diadema3*fleshy3[9,1]+
                       P2*P2*fleshy3[10,1]+
                       P2*P2*fleshy3[11,1]+
                       P2*P1*fleshy3[12,1]+
                       P2*P2*fleshy3[13,1]+
                       P2*P2*fleshy3[14,1]+
                       P2*P1*fleshy3[15,1]+
                       P2*P2*fleshy3[16,1]+
                       P2*P1*fleshy3[17,1]+
                       P2*P2*fleshy3[18,1]+
                       P1*P2*fleshy3[19,1])),
  predict.mean %>% filter(Group == "Excavators") %>%
    mutate(fleshy = inv.logit(P2*fleshy3[1,1]+
                       P2*fleshy3[2,1]+
                       P2*fleshy3[3,1]+
                       P2*fleshy3[4,1]+
                       P1*fleshy3[5,1]+
                       depth3*fleshy3[6,1]+
                       b.herbivore3*fleshy3[7,1]+
                       year3*fleshy3[8,1]+
                       diadema3*fleshy3[9,1]+
                       P2*P2*fleshy3[10,1]+
                       P2*P2*fleshy3[11,1]+
                       P2*P2*fleshy3[12,1]+
                       P2*P1*fleshy3[13,1]+
                       P2*P2*fleshy3[14,1]+
                       P2*P2*fleshy3[15,1]+
                       P2*P1*fleshy3[16,1]+
                       P2*P2*fleshy3[17,1]+
                       P2*P1*fleshy3[18,1]+
                       P2*P1*fleshy3[19,1]))
)

predict.mean <- rbind(
  predict.mean %>% filter(Group == "Croppers") %>%
    mutate(fleshyid = inv.logit(P1*fleshy3[1,1]+
                              P2*fleshy3[2,1]+
                              P2*fleshy3[3,1]+
                              P2*fleshy3[4,1]+
                              P2*fleshy3[5,1]+
                              depth3*fleshy3[6,1]+
                              b.herbivore3*fleshy3[7,1]+
                              year3*fleshy3[8,1]+
                              diadema3*fleshy3[9,1])),
  predict.mean %>% filter(Group == "Browsers") %>%
    mutate(fleshyid = inv.logit(P2*fleshy3[1,1]+
                              P1*fleshy3[2,1]+
                              P2*fleshy3[3,1]+
                              P2*fleshy3[4,1]+
                              P2*fleshy3[5,1]+
                              depth3*fleshy3[6,1]+
                              b.herbivore3*fleshy3[7,1]+
                              year3*fleshy3[8,1]+
                              diadema3*fleshy3[9,1])),
  predict.mean %>% filter(Group == "Farmers") %>%
    mutate(fleshyid = inv.logit(P2*fleshy3[1,1]+
                              P2*fleshy3[2,1]+
                              P1*fleshy3[3,1]+
                              P2*fleshy3[4,1]+
                              P2*fleshy3[5,1]+
                              depth3*fleshy3[6,1]+
                              b.herbivore3*fleshy3[7,1]+
                              year3*fleshy3[8,1]+
                              diadema3*fleshy3[9,1])),
  predict.mean %>% filter(Group == "Scrapers") %>%
    mutate(fleshyid = inv.logit(P2*fleshy3[1,1]+
                              P2*fleshy3[2,1]+
                              P2*fleshy3[3,1]+
                              P1*fleshy3[4,1]+
                              P2*fleshy3[5,1]+
                              depth3*fleshy3[6,1]+
                              b.herbivore3*fleshy3[7,1]+
                              year3*fleshy3[8,1]+
                              diadema3*fleshy3[9,1])),
  predict.mean %>% filter(Group == "Excavators") %>%
    mutate(fleshyid = inv.logit(P2*fleshy3[1,1]+
                              P2*fleshy3[2,1]+
                              P2*fleshy3[3,1]+
                              P2*fleshy3[4,1]+
                              P1*fleshy3[5,1]+
                              depth3*fleshy3[6,1]+
                              b.herbivore3*fleshy3[7,1]+
                              year3*fleshy3[8,1]+
                              diadema3*fleshy3[9,1]))
)

predict.mean <- rbind(
  predict.mean %>% filter(Group == "Croppers") %>%
    mutate(fleshy2 = inv.logit(P1*fleshy2[1,1]+
                                  P2*fleshy2[2,1]+
                                  P2*fleshy2[3,1]+
                                  P2*fleshy2[4,1]+
                                  P2*fleshy2[5,1]+
                                  depth3*fleshy2[6,1]+
                                  b.herbivore3*fleshy2[7,1]+
                                  year3*fleshy2[8,1]+
                                  diadema3*fleshy2[9,1])),
  predict.mean %>% filter(Group == "Browsers") %>%
    mutate(fleshy2 = inv.logit(P2*fleshy2[1,1]+
                                  P1*fleshy2[2,1]+
                                  P2*fleshy2[3,1]+
                                  P2*fleshy2[4,1]+
                                  P2*fleshy2[5,1]+
                                  depth3*fleshy2[6,1]+
                                  b.herbivore3*fleshy2[7,1]+
                                  year3*fleshy2[8,1]+
                                  diadema3*fleshy2[9,1])),
  predict.mean %>% filter(Group == "Farmers") %>%
    mutate(fleshy2 = inv.logit(P2*fleshy2[1,1]+
                                  P2*fleshy2[2,1]+
                                  P1*fleshy2[3,1]+
                                  P2*fleshy2[4,1]+
                                  P2*fleshy2[5,1]+
                                  depth3*fleshy2[6,1]+
                                  b.herbivore3*fleshy2[7,1]+
                                  year3*fleshy2[8,1]+
                                  diadema3*fleshy2[9,1])),
  predict.mean %>% filter(Group == "Scrapers") %>%
    mutate(fleshy2 = inv.logit(P2*fleshy2[1,1]+
                                  P2*fleshy2[2,1]+
                                  P2*fleshy2[3,1]+
                                  P1*fleshy2[4,1]+
                                  P2*fleshy2[5,1]+
                                  depth3*fleshy2[6,1]+
                                  b.herbivore3*fleshy2[7,1]+
                                  year3*fleshy2[8,1]+
                                  diadema3*fleshy2[9,1])),
  predict.mean %>% filter(Group == "Excavators") %>%
    mutate(fleshy2 = inv.logit(P2*fleshy2[1,1]+
                                  P2*fleshy2[2,1]+
                                  P2*fleshy2[3,1]+
                                  P2*fleshy2[4,1]+
                                  P1*fleshy2[5,1]+
                                  depth3*fleshy2[6,1]+
                                  b.herbivore3*fleshy2[7,1]+
                                  year3*fleshy2[8,1]+
                                  diadema3*fleshy2[9,1]))
)


predict.mean <- rbind(
  predict.mean %>% filter(Group == "Croppers") %>%
    mutate(calc = inv.logit(P1*calc3[1,1]+
                       P2*calc3[2,1]+
                       P2*calc3[3,1]+
                       P2*calc3[4,1]+
                       P2*calc3[5,1]+
                       depth3*calc3[6,1]+
                       b.herbivore3*calc3[7,1]+
                       year3*calc3[8,1]+
                       diadema3*calc3[9,1]+
                       P1*P2*calc3[10,1]+
                       P1*P2*calc3[11,1]+
                       P1*P2*calc3[12,1]+
                       P1*P2*calc3[13,1]+
                       P2*P2*calc3[14,1]+
                       P2*P2*calc3[15,1]+
                       P2*P2*calc3[16,1]+
                       P2*P2*calc3[17,1]+
                       P2*P2*calc3[18,1]+
                       P2*P2*calc3[19,1])),
  predict.mean %>% filter(Group == "Browsers") %>%
    mutate(calc = inv.logit(P2*calc3[1,1]+
                       P1*calc3[2,1]+
                       P2*calc3[3,1]+
                       P2*calc3[4,1]+
                       P2*calc3[5,1]+
                       depth3*calc3[6,1]+
                       b.herbivore3*calc3[7,1]+
                       year3*calc3[8,1]+
                       diadema3*calc3[9,1]+
                       P2*P1*calc3[10,1]+
                       P2*P2*calc3[11,1]+
                       P2*P2*calc3[12,1]+
                       P2*P2*calc3[13,1]+
                       P1*P2*calc3[14,1]+
                       P1*P2*calc3[15,1]+
                       P1*P2*calc3[16,1]+
                       P2*P2*calc3[17,1]+
                       P2*P2*calc3[18,1]+
                       P2*P2*calc3[19,1])),
  predict.mean %>% filter(Group == "Farmers") %>%
    mutate(calc = inv.logit(P2*calc3[1,1]+
                       P2*calc3[2,1]+
                       P1*calc3[3,1]+
                       P2*calc3[4,1]+
                       P2*calc3[5,1]+
                       depth3*calc3[6,1]+
                       b.herbivore3*calc3[7,1]+
                       year3*calc3[8,1]+
                       diadema3*calc3[9,1]+
                       P2*P2*calc3[10,1]+
                       P2*P1*calc3[11,1]+
                       P2*P2*calc3[12,1]+
                       P2*P2*calc3[13,1]+
                       P2*P1*calc3[14,1]+
                       P2*P2*calc3[15,1]+
                       P2*P2*calc3[16,1]+
                       P1*P2*calc3[17,1]+
                       P1*P2*calc3[18,1]+
                       P2*P2*calc3[19,1])),
  predict.mean %>% filter(Group == "Scrapers") %>%
    mutate(calc = inv.logit(P2*calc3[1,1]+
                       P2*calc3[2,1]+
                       P2*calc3[3,1]+
                       P1*calc3[4,1]+
                       P2*calc3[5,1]+
                       depth3*calc3[6,1]+
                       b.herbivore3*calc3[7,1]+
                       year3*calc3[8,1]+
                       diadema3*calc3[9,1]+
                       P2*P2*calc3[10,1]+
                       P2*P2*calc3[11,1]+
                       P2*P1*calc3[12,1]+
                       P2*P2*calc3[13,1]+
                       P2*P2*calc3[14,1]+
                       P2*P1*calc3[15,1]+
                       P2*P2*calc3[16,1]+
                       P2*P1*calc3[17,1]+
                       P2*P2*calc3[18,1]+
                       P1*P2*calc3[19,1])),
  predict.mean %>% filter(Group == "Excavators") %>%
    mutate(calc = inv.logit(P2*calc3[1,1]+
                       P2*calc3[2,1]+
                       P2*calc3[3,1]+
                       P2*calc3[4,1]+
                       P1*calc3[5,1]+
                       depth3*calc3[6,1]+
                       b.herbivore3*calc3[7,1]+
                       year3*calc3[8,1]+
                       diadema3*calc3[9,1]+
                       P2*P2*calc3[10,1]+
                       P2*P2*calc3[11,1]+
                       P2*P2*calc3[12,1]+
                       P2*P1*calc3[13,1]+
                       P2*P2*calc3[14,1]+
                       P2*P2*calc3[15,1]+
                       P2*P1*calc3[16,1]+
                       P2*P2*calc3[17,1]+
                       P2*P1*calc3[18,1]+
                       P2*P1*calc3[19,1]))
)

predict.mean <- rbind(
  predict.mean %>% filter(Group == "Croppers") %>%
    mutate(calcid = inv.logit(P1*calc3[1,1]+
                                  P2*calc3[2,1]+
                                  P2*calc3[3,1]+
                                  P2*calc3[4,1]+
                                  P2*calc3[5,1]+
                                  depth3*calc3[6,1]+
                                  b.herbivore3*calc3[7,1]+
                                  year3*calc3[8,1]+
                                  diadema3*calc3[9,1])),
  predict.mean %>% filter(Group == "Browsers") %>%
    mutate(calcid = inv.logit(P2*calc3[1,1]+
                                  P1*calc3[2,1]+
                                  P2*calc3[3,1]+
                                  P2*calc3[4,1]+
                                  P2*calc3[5,1]+
                                  depth3*calc3[6,1]+
                                  b.herbivore3*calc3[7,1]+
                                  year3*calc3[8,1]+
                                  diadema3*calc3[9,1])),
  predict.mean %>% filter(Group == "Farmers") %>%
    mutate(calcid = inv.logit(P2*calc3[1,1]+
                                  P2*calc3[2,1]+
                                  P1*calc3[3,1]+
                                  P2*calc3[4,1]+
                                  P2*calc3[5,1]+
                                  depth3*calc3[6,1]+
                                  b.herbivore3*calc3[7,1]+
                                  year3*calc3[8,1]+
                                  diadema3*calc3[9,1])),
  predict.mean %>% filter(Group == "Scrapers") %>%
    mutate(calcid = inv.logit(P2*calc3[1,1]+
                                  P2*calc3[2,1]+
                                  P2*calc3[3,1]+
                                  P1*calc3[4,1]+
                                  P2*calc3[5,1]+
                                  depth3*calc3[6,1]+
                                  b.herbivore3*calc3[7,1]+
                                  year3*calc3[8,1]+
                                  diadema3*calc3[9,1])),
  predict.mean %>% filter(Group == "Excavators") %>%
    mutate(calcid = inv.logit(P2*calc3[1,1]+
                                  P2*calc3[2,1]+
                                  P2*calc3[3,1]+
                                  P2*calc3[4,1]+
                                  P1*calc3[5,1]+
                                  depth3*calc3[6,1]+
                                  b.herbivore3*calc3[7,1]+
                                  year3*calc3[8,1]+
                                  diadema3*calc3[9,1]))
)

predict.mean <- rbind(
  predict.mean %>% filter(Group == "Croppers") %>%
    mutate(calc2 = inv.logit(P1*calc2[1,1]+
                                P2*calc2[2,1]+
                                P2*calc2[3,1]+
                                P2*calc2[4,1]+
                                P2*calc2[5,1]+
                                depth3*calc2[6,1]+
                                b.herbivore3*calc2[7,1]+
                                year3*calc2[8,1]+
                                diadema3*calc2[9,1])),
  predict.mean %>% filter(Group == "Browsers") %>%
    mutate(calc2 = inv.logit(P2*calc2[1,1]+
                                P1*calc2[2,1]+
                                P2*calc2[3,1]+
                                P2*calc2[4,1]+
                                P2*calc2[5,1]+
                                depth3*calc2[6,1]+
                                b.herbivore3*calc2[7,1]+
                                year3*calc2[8,1]+
                                diadema3*calc2[9,1])),
  predict.mean %>% filter(Group == "Farmers") %>%
    mutate(calc2 = inv.logit(P2*calc2[1,1]+
                                P2*calc2[2,1]+
                                P1*calc2[3,1]+
                                P2*calc2[4,1]+
                                P2*calc2[5,1]+
                                depth3*calc2[6,1]+
                                b.herbivore3*calc2[7,1]+
                                year3*calc2[8,1]+
                                diadema3*calc2[9,1])),
  predict.mean %>% filter(Group == "Scrapers") %>%
    mutate(calc2 = inv.logit(P2*calc2[1,1]+
                                P2*calc2[2,1]+
                                P2*calc2[3,1]+
                                P1*calc2[4,1]+
                                P2*calc2[5,1]+
                                depth3*calc2[6,1]+
                                b.herbivore3*calc2[7,1]+
                                year3*calc2[8,1]+
                                diadema3*calc2[9,1])),
  predict.mean %>% filter(Group == "Excavators") %>%
    mutate(calc2 = inv.logit(P2*calc2[1,1]+
                                P2*calc2[2,1]+
                                P2*calc2[3,1]+
                                P2*calc2[4,1]+
                                P1*calc2[5,1]+
                                depth3*calc2[6,1]+
                                b.herbivore3*calc2[7,1]+
                                year3*calc2[8,1]+
                                diadema3*calc2[9,1]))
)

predict.mean <- rbind(
  predict.mean %>% filter(Group == "Croppers") %>%
    mutate(turf = inv.logit(P1*turf3[1,1]+
                       P2*turf3[2,1]+
                       P2*turf3[3,1]+
                       P2*turf3[4,1]+
                       P2*turf3[5,1]+
                       depth3*turf3[6,1]+
                       b.herbivore3*turf3[7,1]+
                       year3*turf3[8,1]+
                       diadema3*turf3[9,1]+
                       P1*P2*turf3[10,1]+
                       P1*P2*turf3[11,1]+
                       P1*P2*turf3[12,1]+
                       P1*P2*turf3[13,1]+
                       P2*P2*turf3[14,1]+
                       P2*P2*turf3[15,1]+
                       P2*P2*turf3[16,1]+
                       P2*P2*turf3[17,1]+
                       P2*P2*turf3[18,1]+
                       P2*P2*turf3[19,1])),
  predict.mean %>% filter(Group == "Browsers") %>%
    mutate(turf = inv.logit(P2*turf3[1,1]+
                       P1*turf3[2,1]+
                       P2*turf3[3,1]+
                       P2*turf3[4,1]+
                       P2*turf3[5,1]+
                       depth3*turf3[6,1]+
                       b.herbivore3*turf3[7,1]+
                       year3*turf3[8,1]+
                       diadema3*turf3[9,1]+
                       P2*P1*turf3[10,1]+
                       P2*P2*turf3[11,1]+
                       P2*P2*turf3[12,1]+
                       P2*P2*turf3[13,1]+
                       P1*P2*turf3[14,1]+
                       P1*P2*turf3[15,1]+
                       P1*P2*turf3[16,1]+
                       P2*P2*turf3[17,1]+
                       P2*P2*turf3[18,1]+
                       P2*P2*turf3[19,1])),
  predict.mean %>% filter(Group == "Farmers") %>%
    mutate(turf = inv.logit(P2*turf3[1,1]+
                       P2*turf3[2,1]+
                       P1*turf3[3,1]+
                       P2*turf3[4,1]+
                       P2*turf3[5,1]+
                       depth3*turf3[6,1]+
                       b.herbivore3*turf3[7,1]+
                       year3*turf3[8,1]+
                       diadema3*turf3[9,1]+
                       P2*P2*turf3[10,1]+
                       P2*P1*turf3[11,1]+
                       P2*P2*turf3[12,1]+
                       P2*P2*turf3[13,1]+
                       P2*P1*turf3[14,1]+
                       P2*P2*turf3[15,1]+
                       P2*P2*turf3[16,1]+
                       P1*P2*turf3[17,1]+
                       P1*P2*turf3[18,1]+
                       P2*P2*turf3[19,1])),
  predict.mean %>% filter(Group == "Scrapers") %>%
    mutate(turf = inv.logit(P2*turf3[1,1]+
                       P2*turf3[2,1]+
                       P2*turf3[3,1]+
                       P1*turf3[4,1]+
                       P2*turf3[5,1]+
                       depth3*turf3[6,1]+
                       b.herbivore3*turf3[7,1]+
                       year3*turf3[8,1]+
                       diadema3*turf3[9,1]+
                       P2*P2*turf3[10,1]+
                       P2*P2*turf3[11,1]+
                       P2*P1*turf3[12,1]+
                       P2*P2*turf3[13,1]+
                       P2*P2*turf3[14,1]+
                       P2*P1*turf3[15,1]+
                       P2*P2*turf3[16,1]+
                       P2*P1*turf3[17,1]+
                       P2*P2*turf3[18,1]+
                       P1*P2*turf3[19,1])),
  predict.mean %>% filter(Group == "Excavators") %>%
    mutate(turf = inv.logit(P2*turf3[1,1]+
                       P2*turf3[2,1]+
                       P2*turf3[3,1]+
                       P2*turf3[4,1]+
                       P1*turf3[5,1]+
                       depth3*turf3[6,1]+
                       b.herbivore3*turf3[7,1]+
                       year3*turf3[8,1]+
                       diadema3*turf3[9,1]+
                       P2*P2*turf3[10,1]+
                       P2*P2*turf3[11,1]+
                       P2*P2*turf3[12,1]+
                       P2*P1*turf3[13,1]+
                       P2*P2*turf3[14,1]+
                       P2*P2*turf3[15,1]+
                       P2*P1*turf3[16,1]+
                       P2*P2*turf3[17,1]+
                       P2*P1*turf3[18,1]+
                       P2*P1*turf3[19,1]))
)

predict.mean <- rbind(
  predict.mean %>% filter(Group == "Croppers") %>%
    mutate(turfid = inv.logit(P1*turf3[1,1]+
                                P2*turf3[2,1]+
                                P2*turf3[3,1]+
                                P2*turf3[4,1]+
                                P2*turf3[5,1]+
                                depth3*turf3[6,1]+
                                b.herbivore3*turf3[7,1]+
                                year3*turf3[8,1]+
                                diadema3*turf3[9,1])),
  predict.mean %>% filter(Group == "Browsers") %>%
    mutate(turfid = inv.logit(P2*turf3[1,1]+
                                P1*turf3[2,1]+
                                P2*turf3[3,1]+
                                P2*turf3[4,1]+
                                P2*turf3[5,1]+
                                depth3*turf3[6,1]+
                                b.herbivore3*turf3[7,1]+
                                year3*turf3[8,1]+
                                diadema3*turf3[9,1])),
  predict.mean %>% filter(Group == "Farmers") %>%
    mutate(turfid = inv.logit(P2*turf3[1,1]+
                                P2*turf3[2,1]+
                                P1*turf3[3,1]+
                                P2*turf3[4,1]+
                                P2*turf3[5,1]+
                                depth3*turf3[6,1]+
                                b.herbivore3*turf3[7,1]+
                                year3*turf3[8,1]+
                                diadema3*turf3[9,1])),
  predict.mean %>% filter(Group == "Scrapers") %>%
    mutate(turfid = inv.logit(P2*turf3[1,1]+
                                P2*turf3[2,1]+
                                P2*turf3[3,1]+
                                P1*turf3[4,1]+
                                P2*turf3[5,1]+
                                depth3*turf3[6,1]+
                                b.herbivore3*turf3[7,1]+
                                year3*turf3[8,1]+
                                diadema3*turf3[9,1])),
  predict.mean %>% filter(Group == "Excavators") %>%
    mutate(turfid = inv.logit(P2*turf3[1,1]+
                                P2*turf3[2,1]+
                                P2*turf3[3,1]+
                                P2*turf3[4,1]+
                                P1*turf3[5,1]+
                                depth3*turf3[6,1]+
                                b.herbivore3*turf3[7,1]+
                                year3*turf3[8,1]+
                                diadema3*turf3[9,1]))
)

predict.mean <- rbind(
  predict.mean %>% filter(Group == "Croppers") %>%
    mutate(turf2 = inv.logit(P1*turf2[1,1]+
                                P2*turf2[2,1]+
                                P2*turf2[3,1]+
                                P2*turf2[4,1]+
                                P2*turf2[5,1]+
                                depth3*turf2[6,1]+
                                b.herbivore3*turf2[7,1]+
                                year3*turf2[8,1]+
                                diadema3*turf2[9,1])),
  predict.mean %>% filter(Group == "Browsers") %>%
    mutate(turf2 = inv.logit(P2*turf2[1,1]+
                                P1*turf2[2,1]+
                                P2*turf2[3,1]+
                                P2*turf2[4,1]+
                                P2*turf2[5,1]+
                                depth3*turf2[6,1]+
                                b.herbivore3*turf2[7,1]+
                                year3*turf2[8,1]+
                                diadema3*turf2[9,1])),
  predict.mean %>% filter(Group == "Farmers") %>%
    mutate(turf2 = inv.logit(P2*turf2[1,1]+
                                P2*turf2[2,1]+
                                P1*turf2[3,1]+
                                P2*turf2[4,1]+
                                P2*turf2[5,1]+
                                depth3*turf2[6,1]+
                                b.herbivore3*turf2[7,1]+
                                year3*turf2[8,1]+
                                diadema3*turf2[9,1])),
  predict.mean %>% filter(Group == "Scrapers") %>%
    mutate(turf2 = inv.logit(P2*turf2[1,1]+
                                P2*turf2[2,1]+
                                P2*turf2[3,1]+
                                P1*turf2[4,1]+
                                P2*turf2[5,1]+
                                depth3*turf2[6,1]+
                                b.herbivore3*turf2[7,1]+
                                year3*turf2[8,1]+
                                diadema3*turf2[9,1])),
  predict.mean %>% filter(Group == "Excavators") %>%
    mutate(turf2 = inv.logit(P2*turf2[1,1]+
                                P2*turf2[2,1]+
                                P2*turf2[3,1]+
                                P2*turf2[4,1]+
                                P1*turf2[5,1]+
                                depth3*turf2[6,1]+
                                b.herbivore3*turf2[7,1]+
                                year3*turf2[8,1]+
                                diadema3*turf2[9,1]))
)

predict.mean <- rbind(
  predict.mean %>% filter(Group == "Croppers") %>%
    mutate(cca = inv.logit(P1*cca3[1,1]+
                       P2*cca3[2,1]+
                       P2*cca3[3,1]+
                       P2*cca3[4,1]+
                       P2*cca3[5,1]+
                       depth3*cca3[6,1]+
                       b.herbivore3*cca3[7,1]+
                       year3*cca3[8,1]+
                       diadema3*cca3[9,1]+
                       P1*P2*cca3[10,1]+
                       P1*P2*cca3[11,1]+
                       P1*P2*cca3[12,1]+
                       P1*P2*cca3[13,1]+
                       P2*P2*cca3[14,1]+
                       P2*P2*cca3[15,1]+
                       P2*P2*cca3[16,1]+
                       P2*P2*cca3[17,1]+
                       P2*P2*cca3[18,1]+
                       P2*P2*cca3[19,1])),
  predict.mean %>% filter(Group == "Browsers") %>%
    mutate(cca = inv.logit(P2*cca3[1,1]+
                       P1*cca3[2,1]+
                       P2*cca3[3,1]+
                       P2*cca3[4,1]+
                       P2*cca3[5,1]+
                       depth3*cca3[6,1]+
                       b.herbivore3*cca3[7,1]+
                       year3*cca3[8,1]+
                       diadema3*cca3[9,1]+
                       P2*P1*cca3[10,1]+
                       P2*P2*cca3[11,1]+
                       P2*P2*cca3[12,1]+
                       P2*P2*cca3[13,1]+
                       P1*P2*cca3[14,1]+
                       P1*P2*cca3[15,1]+
                       P1*P2*cca3[16,1]+
                       P2*P2*cca3[17,1]+
                       P2*P2*cca3[18,1]+
                       P2*P2*cca3[19,1])),
  predict.mean %>% filter(Group == "Farmers") %>%
    mutate(cca = inv.logit(P2*cca3[1,1]+
                       P2*cca3[2,1]+
                       P1*cca3[3,1]+
                       P2*cca3[4,1]+
                       P2*cca3[5,1]+
                       depth3*cca3[6,1]+
                       b.herbivore3*cca3[7,1]+
                       year3*cca3[8,1]+
                       diadema3*cca3[9,1]+
                       P2*P2*cca3[10,1]+
                       P2*P1*cca3[11,1]+
                       P2*P2*cca3[12,1]+
                       P2*P2*cca3[13,1]+
                       P2*P1*cca3[14,1]+
                       P2*P2*cca3[15,1]+
                       P2*P2*cca3[16,1]+
                       P1*P2*cca3[17,1]+
                       P1*P2*cca3[18,1]+
                       P2*P2*cca3[19,1])),
  predict.mean %>% filter(Group == "Scrapers") %>%
    mutate(cca = inv.logit(P2*cca3[1,1]+
                       P2*cca3[2,1]+
                       P2*cca3[3,1]+
                       P1*cca3[4,1]+
                       P2*cca3[5,1]+
                       depth3*cca3[6,1]+
                       b.herbivore3*cca3[7,1]+
                       year3*cca3[8,1]+
                       diadema3*cca3[9,1]+
                       P2*P2*cca3[10,1]+
                       P2*P2*cca3[11,1]+
                       P2*P1*cca3[12,1]+
                       P2*P2*cca3[13,1]+
                       P2*P2*cca3[14,1]+
                       P2*P1*cca3[15,1]+
                       P2*P2*cca3[16,1]+
                       P2*P1*cca3[17,1]+
                       P2*P2*cca3[18,1]+
                       P1*P2*cca3[19,1])),
  predict.mean %>% filter(Group == "Excavators") %>%
    mutate(cca = inv.logit(P2*cca3[1,1]+
                       P2*cca3[2,1]+
                       P2*cca3[3,1]+
                       P2*cca3[4,1]+
                       P1*cca3[5,1]+
                       depth3*cca3[6,1]+
                       b.herbivore3*cca3[7,1]+
                       year3*cca3[8,1]+
                       diadema3*cca3[9,1]+
                       P2*P2*cca3[10,1]+
                       P2*P2*cca3[11,1]+
                       P2*P2*cca3[12,1]+
                       P2*P1*cca3[13,1]+
                       P2*P2*cca3[14,1]+
                       P2*P2*cca3[15,1]+
                       P2*P1*cca3[16,1]+
                       P2*P2*cca3[17,1]+
                       P2*P1*cca3[18,1]+
                       P2*P1*cca3[19,1]))
)

predict.mean <- rbind(
  predict.mean %>% filter(Group == "Croppers") %>%
    mutate(ccaid = inv.logit(P1*cca3[1,1]+
                                P2*cca3[2,1]+
                                P2*cca3[3,1]+
                                P2*cca3[4,1]+
                                P2*cca3[5,1]+
                                depth3*cca3[6,1]+
                                b.herbivore3*cca3[7,1]+
                                year3*cca3[8,1]+
                                diadema3*cca3[9,1])),
  predict.mean %>% filter(Group == "Browsers") %>%
    mutate(ccaid = inv.logit(P2*cca3[1,1]+
                                P1*cca3[2,1]+
                                P2*cca3[3,1]+
                                P2*cca3[4,1]+
                                P2*cca3[5,1]+
                                depth3*cca3[6,1]+
                                b.herbivore3*cca3[7,1]+
                                year3*cca3[8,1]+
                                diadema3*cca3[9,1])),
  predict.mean %>% filter(Group == "Farmers") %>%
    mutate(ccaid = inv.logit(P2*cca3[1,1]+
                                P2*cca3[2,1]+
                                P1*cca3[3,1]+
                                P2*cca3[4,1]+
                                P2*cca3[5,1]+
                                depth3*cca3[6,1]+
                                b.herbivore3*cca3[7,1]+
                                year3*cca3[8,1]+
                                diadema3*cca3[9,1])),
  predict.mean %>% filter(Group == "Scrapers") %>%
    mutate(ccaid = inv.logit(P2*cca3[1,1]+
                                P2*cca3[2,1]+
                                P2*cca3[3,1]+
                                P1*cca3[4,1]+
                                P2*cca3[5,1]+
                                depth3*cca3[6,1]+
                                b.herbivore3*cca3[7,1]+
                                year3*cca3[8,1]+
                                diadema3*cca3[9,1])),
  predict.mean %>% filter(Group == "Excavators") %>%
    mutate(ccaid = inv.logit(P2*cca3[1,1]+
                                P2*cca3[2,1]+
                                P2*cca3[3,1]+
                                P2*cca3[4,1]+
                                P1*cca3[5,1]+
                                depth3*cca3[6,1]+
                                b.herbivore3*cca3[7,1]+
                                year3*cca3[8,1]+
                                diadema3*cca3[9,1]))
)

predict.mean <- rbind(
  predict.mean %>% filter(Group == "Croppers") %>%
    mutate(cca2 = inv.logit(P1*cca2[1,1]+
                               P2*cca2[2,1]+
                               P2*cca2[3,1]+
                               P2*cca2[4,1]+
                               P2*cca2[5,1]+
                               depth3*cca2[6,1]+
                               b.herbivore3*cca2[7,1]+
                               year3*cca2[8,1]+
                               diadema3*cca2[9,1])),
  predict.mean %>% filter(Group == "Browsers") %>%
    mutate(cca2 = inv.logit(P2*cca2[1,1]+
                               P1*cca2[2,1]+
                               P2*cca2[3,1]+
                               P2*cca2[4,1]+
                               P2*cca2[5,1]+
                               depth3*cca2[6,1]+
                               b.herbivore3*cca2[7,1]+
                               year3*cca2[8,1]+
                               diadema3*cca2[9,1])),
  predict.mean %>% filter(Group == "Farmers") %>%
    mutate(cca2 = inv.logit(P2*cca2[1,1]+
                               P2*cca2[2,1]+
                               P1*cca2[3,1]+
                               P2*cca2[4,1]+
                               P2*cca2[5,1]+
                               depth3*cca2[6,1]+
                               b.herbivore3*cca2[7,1]+
                               year3*cca2[8,1]+
                               diadema3*cca2[9,1])),
  predict.mean %>% filter(Group == "Scrapers") %>%
    mutate(cca2 = inv.logit(P2*cca2[1,1]+
                               P2*cca2[2,1]+
                               P2*cca2[3,1]+
                               P1*cca2[4,1]+
                               P2*cca2[5,1]+
                               depth3*cca2[6,1]+
                               b.herbivore3*cca2[7,1]+
                               year3*cca2[8,1]+
                               diadema3*cca2[9,1])),
  predict.mean %>% filter(Group == "Excavators") %>%
    mutate(cca2 = inv.logit(P2*cca2[1,1]+
                               P2*cca2[2,1]+
                               P2*cca2[3,1]+
                               P2*cca2[4,1]+
                               P1*cca2[5,1]+
                               depth3*cca2[6,1]+
                               b.herbivore3*cca2[7,1]+
                               year3*cca2[8,1]+
                               diadema3*cca2[9,1]))
)

#calculate y for subset1 up to max observed P (coral metrics; richness, calc rate, total cover, trait-based cover)
ss1predict <- rbind(
  ss1predict %>% filter(Group == "Croppers") %>%
    mutate(richness = exp(P1*richness3[1,1]+
                            P2*richness3[2,1]+
                            P2*richness3[3,1]+
                            P2*richness3[4,1]+
                            P2*richness3[5,1]+
                            depth1*richness3[6,1]+
                            b.herbivore1*richness3[7,1]+
                            year1*richness3[8,1]+
                            diadema1*richness3[9,1]+
                            P1*P2*richness3[10,1]+
                            P1*P2*richness3[11,1]+
                            P1*P2*richness3[12,1]+
                            P1*P2*richness3[13,1]+
                            P2*P2*richness3[14,1]+
                            P2*P2*richness3[15,1]+
                            P2*P2*richness3[16,1]+
                            P2*P2*richness3[17,1]+
                            P2*P2*richness3[18,1]+
                            P2*P2*richness3[19,1])),
  ss1predict %>% filter(Group == "Browsers") %>%
    mutate(richness = exp(P2*richness3[1,1]+
                            P1*richness3[2,1]+
                            P2*richness3[3,1]+
                            P2*richness3[4,1]+
                            P2*richness3[5,1]+
                            depth1*richness3[6,1]+
                            b.herbivore1*richness3[7,1]+
                            year1*richness3[8,1]+
                            diadema1*richness3[9,1]+
                            P2*P1*richness3[10,1]+
                            P2*P2*richness3[11,1]+
                            P2*P2*richness3[12,1]+
                            P2*P2*richness3[13,1]+
                            P1*P2*richness3[14,1]+
                            P1*P2*richness3[15,1]+
                            P1*P2*richness3[16,1]+
                            P2*P2*richness3[17,1]+
                            P2*P2*richness3[18,1]+
                            P2*P2*richness3[19,1])),
  ss1predict %>% filter(Group == "Farmers") %>%
    mutate(richness = exp(P2*richness3[1,1]+
                            P2*richness3[2,1]+
                            P1*richness3[3,1]+
                            P2*richness3[4,1]+
                            P2*richness3[5,1]+
                            depth1*richness3[6,1]+
                            b.herbivore1*richness3[7,1]+
                            year1*richness3[8,1]+
                            diadema1*richness3[9,1]+
                            P2*P2*richness3[10,1]+
                            P2*P1*richness3[11,1]+
                            P2*P2*richness3[12,1]+
                            P2*P2*richness3[13,1]+
                            P2*P1*richness3[14,1]+
                            P2*P2*richness3[15,1]+
                            P2*P2*richness3[16,1]+
                            P1*P2*richness3[17,1]+
                            P1*P2*richness3[18,1]+
                            P2*P2*richness3[19,1])),
  ss1predict %>% filter(Group == "Scrapers") %>%
    mutate(richness = exp(P2*richness3[1,1]+
                            P2*richness3[2,1]+
                            P2*richness3[3,1]+
                            P1*richness3[4,1]+
                            P2*richness3[5,1]+
                            depth1*richness3[6,1]+
                            b.herbivore1*richness3[7,1]+
                            year1*richness3[8,1]+
                            diadema1*richness3[9,1]+
                            P2*P2*richness3[10,1]+
                            P2*P2*richness3[11,1]+
                            P2*P1*richness3[12,1]+
                            P2*P2*richness3[13,1]+
                            P2*P2*richness3[14,1]+
                            P2*P1*richness3[15,1]+
                            P2*P2*richness3[16,1]+
                            P2*P1*richness3[17,1]+
                            P2*P2*richness3[18,1]+
                            P1*P2*richness3[19,1])),
  ss1predict %>% filter(Group == "Excavators") %>%
    mutate(richness = exp(P2*richness3[1,1]+
                            P2*richness3[2,1]+
                            P2*richness3[3,1]+
                            P2*richness3[4,1]+
                            P1*richness3[5,1]+
                            depth1*richness3[6,1]+
                            b.herbivore1*richness3[7,1]+
                            year1*richness3[8,1]+
                            diadema1*richness3[9,1]+
                            P2*P2*richness3[10,1]+
                            P2*P2*richness3[11,1]+
                            P2*P2*richness3[12,1]+
                            P2*P1*richness3[13,1]+
                            P2*P2*richness3[14,1]+
                            P2*P2*richness3[15,1]+
                            P2*P1*richness3[16,1]+
                            P2*P2*richness3[17,1]+
                            P2*P1*richness3[18,1]+
                            P2*P1*richness3[19,1]))
)

ss1predict <- rbind(
  ss1predict %>% filter(Group == "Croppers") %>%
    mutate(calcrate = exp(P1*calcrate3[1,1]+
                            P2*calcrate3[2,1]+
                            P2*calcrate3[3,1]+
                            P2*calcrate3[4,1]+
                            P2*calcrate3[5,1]+
                            depth1*calcrate3[6,1]+
                            b.herbivore1*calcrate3[7,1]+
                            year1*calcrate3[8,1]+
                            diadema1*calcrate3[9,1]+
                            P1*P2*calcrate3[10,1]+
                            P1*P2*calcrate3[11,1]+
                            P1*P2*calcrate3[12,1]+
                            P1*P2*calcrate3[13,1]+
                            P2*P2*calcrate3[14,1]+
                            P2*P2*calcrate3[15,1]+
                            P2*P2*calcrate3[16,1]+
                            P2*P2*calcrate3[17,1]+
                            P2*P2*calcrate3[18,1]+
                            P2*P2*calcrate3[19,1])),
  ss1predict %>% filter(Group == "Browsers") %>%
    mutate(calcrate = exp(P2*calcrate3[1,1]+
                            P1*calcrate3[2,1]+
                            P2*calcrate3[3,1]+
                            P2*calcrate3[4,1]+
                            P2*calcrate3[5,1]+
                            depth1*calcrate3[6,1]+
                            b.herbivore1*calcrate3[7,1]+
                            year1*calcrate3[8,1]+
                            diadema1*calcrate3[9,1]+
                            P2*P1*calcrate3[10,1]+
                            P2*P2*calcrate3[11,1]+
                            P2*P2*calcrate3[12,1]+
                            P2*P2*calcrate3[13,1]+
                            P1*P2*calcrate3[14,1]+
                            P1*P2*calcrate3[15,1]+
                            P1*P2*calcrate3[16,1]+
                            P2*P2*calcrate3[17,1]+
                            P2*P2*calcrate3[18,1]+
                            P2*P2*calcrate3[19,1])),
  ss1predict %>% filter(Group == "Farmers") %>%
    mutate(calcrate = exp(P2*calcrate3[1,1]+
                            P2*calcrate3[2,1]+
                            P1*calcrate3[3,1]+
                            P2*calcrate3[4,1]+
                            P2*calcrate3[5,1]+
                            depth1*calcrate3[6,1]+
                            b.herbivore1*calcrate3[7,1]+
                            year1*calcrate3[8,1]+
                            diadema1*calcrate3[9,1]+
                            P2*P2*calcrate3[10,1]+
                            P2*P1*calcrate3[11,1]+
                            P2*P2*calcrate3[12,1]+
                            P2*P2*calcrate3[13,1]+
                            P2*P1*calcrate3[14,1]+
                            P2*P2*calcrate3[15,1]+
                            P2*P2*calcrate3[16,1]+
                            P1*P2*calcrate3[17,1]+
                            P1*P2*calcrate3[18,1]+
                            P2*P2*calcrate3[19,1])),
  ss1predict %>% filter(Group == "Scrapers") %>%
    mutate(calcrate = exp(P2*calcrate3[1,1]+
                            P2*calcrate3[2,1]+
                            P2*calcrate3[3,1]+
                            P1*calcrate3[4,1]+
                            P2*calcrate3[5,1]+
                            depth1*calcrate3[6,1]+
                            b.herbivore1*calcrate3[7,1]+
                            year1*calcrate3[8,1]+
                            diadema1*calcrate3[9,1]+
                            P2*P2*calcrate3[10,1]+
                            P2*P2*calcrate3[11,1]+
                            P2*P1*calcrate3[12,1]+
                            P2*P2*calcrate3[13,1]+
                            P2*P2*calcrate3[14,1]+
                            P2*P1*calcrate3[15,1]+
                            P2*P2*calcrate3[16,1]+
                            P2*P1*calcrate3[17,1]+
                            P2*P2*calcrate3[18,1]+
                            P1*P2*calcrate3[19,1])),
  ss1predict %>% filter(Group == "Excavators") %>%
    mutate(calcrate = exp(P2*calcrate3[1,1]+
                            P2*calcrate3[2,1]+
                            P2*calcrate3[3,1]+
                            P2*calcrate3[4,1]+
                            P1*calcrate3[5,1]+
                            depth1*calcrate3[6,1]+
                            b.herbivore1*calcrate3[7,1]+
                            year1*calcrate3[8,1]+
                            diadema1*calcrate3[9,1]+
                            P2*P2*calcrate3[10,1]+
                            P2*P2*calcrate3[11,1]+
                            P2*P2*calcrate3[12,1]+
                            P2*P1*calcrate3[13,1]+
                            P2*P2*calcrate3[14,1]+
                            P2*P2*calcrate3[15,1]+
                            P2*P1*calcrate3[16,1]+
                            P2*P2*calcrate3[17,1]+
                            P2*P1*calcrate3[18,1]+
                            P2*P1*calcrate3[19,1]))
)

ss1predict <- rbind(
  ss1predict %>% filter(Group == "Croppers") %>%
    mutate(total = exp(P1*total3[1,1]+
                            P2*total3[2,1]+
                            P2*total3[3,1]+
                            P2*total3[4,1]+
                            P2*total3[5,1]+
                            depth1*total3[6,1]+
                            b.herbivore1*total3[7,1]+
                            year1*total3[8,1]+
                            diadema1*total3[9,1]+
                            P1*P2*total3[10,1]+
                            P1*P2*total3[11,1]+
                            P1*P2*total3[12,1]+
                            P1*P2*total3[13,1]+
                            P2*P2*total3[14,1]+
                            P2*P2*total3[15,1]+
                            P2*P2*total3[16,1]+
                            P2*P2*total3[17,1]+
                            P2*P2*total3[18,1]+
                            P2*P2*total3[19,1])),
  ss1predict %>% filter(Group == "Browsers") %>%
    mutate(total = exp(P2*total3[1,1]+
                            P1*total3[2,1]+
                            P2*total3[3,1]+
                            P2*total3[4,1]+
                            P2*total3[5,1]+
                            depth1*total3[6,1]+
                            b.herbivore1*total3[7,1]+
                            year1*total3[8,1]+
                            diadema1*total3[9,1]+
                            P2*P1*total3[10,1]+
                            P2*P2*total3[11,1]+
                            P2*P2*total3[12,1]+
                            P2*P2*total3[13,1]+
                            P1*P2*total3[14,1]+
                            P1*P2*total3[15,1]+
                            P1*P2*total3[16,1]+
                            P2*P2*total3[17,1]+
                            P2*P2*total3[18,1]+
                            P2*P2*total3[19,1])),
  ss1predict %>% filter(Group == "Farmers") %>%
    mutate(total = exp(P2*total3[1,1]+
                            P2*total3[2,1]+
                            P1*total3[3,1]+
                            P2*total3[4,1]+
                            P2*total3[5,1]+
                            depth1*total3[6,1]+
                            b.herbivore1*total3[7,1]+
                            year1*total3[8,1]+
                            diadema1*total3[9,1]+
                            P2*P2*total3[10,1]+
                            P2*P1*total3[11,1]+
                            P2*P2*total3[12,1]+
                            P2*P2*total3[13,1]+
                            P2*P1*total3[14,1]+
                            P2*P2*total3[15,1]+
                            P2*P2*total3[16,1]+
                            P1*P2*total3[17,1]+
                            P1*P2*total3[18,1]+
                            P2*P2*total3[19,1])),
  ss1predict %>% filter(Group == "Scrapers") %>%
    mutate(total = exp(P2*total3[1,1]+
                            P2*total3[2,1]+
                            P2*total3[3,1]+
                            P1*total3[4,1]+
                            P2*total3[5,1]+
                            depth1*total3[6,1]+
                            b.herbivore1*total3[7,1]+
                            year1*total3[8,1]+
                            diadema1*total3[9,1]+
                            P2*P2*total3[10,1]+
                            P2*P2*total3[11,1]+
                            P2*P1*total3[12,1]+
                            P2*P2*total3[13,1]+
                            P2*P2*total3[14,1]+
                            P2*P1*total3[15,1]+
                            P2*P2*total3[16,1]+
                            P2*P1*total3[17,1]+
                            P2*P2*total3[18,1]+
                            P1*P2*total3[19,1])),
  ss1predict %>% filter(Group == "Excavators") %>%
    mutate(total = exp(P2*total3[1,1]+
                            P2*total3[2,1]+
                            P2*total3[3,1]+
                            P2*total3[4,1]+
                            P1*total3[5,1]+
                            depth1*total3[6,1]+
                            b.herbivore1*total3[7,1]+
                            year1*total3[8,1]+
                            diadema1*total3[9,1]+
                            P2*P2*total3[10,1]+
                            P2*P2*total3[11,1]+
                            P2*P2*total3[12,1]+
                            P2*P1*total3[13,1]+
                            P2*P2*total3[14,1]+
                            P2*P2*total3[15,1]+
                            P2*P1*total3[16,1]+
                            P2*P2*total3[17,1]+
                            P2*P1*total3[18,1]+
                            P2*P1*total3[19,1]))
)

ss1predict <- rbind(
  ss1predict %>% filter(Group == "Croppers") %>%
    mutate(weedy = exp(P1*weedy3[1,1]+
                         P2*weedy3[2,1]+
                         P2*weedy3[3,1]+
                         P2*weedy3[4,1]+
                         P2*weedy3[5,1]+
                         depth1*weedy3[6,1]+
                         b.herbivore1*weedy3[7,1]+
                         year1*weedy3[8,1]+
                         diadema1*weedy3[9,1]+
                         P1*P2*weedy3[10,1]+
                         P1*P2*weedy3[11,1]+
                         P1*P2*weedy3[12,1]+
                         P1*P2*weedy3[13,1]+
                         P2*P2*weedy3[14,1]+
                         P2*P2*weedy3[15,1]+
                         P2*P2*weedy3[16,1]+
                         P2*P2*weedy3[17,1]+
                         P2*P2*weedy3[18,1]+
                         P2*P2*weedy3[19,1])),
  ss1predict %>% filter(Group == "Browsers") %>%
    mutate(weedy = exp(P2*weedy3[1,1]+
                         P1*weedy3[2,1]+
                         P2*weedy3[3,1]+
                         P2*weedy3[4,1]+
                         P2*weedy3[5,1]+
                         depth1*weedy3[6,1]+
                         b.herbivore1*weedy3[7,1]+
                         year1*weedy3[8,1]+
                         diadema1*weedy3[9,1]+
                         P2*P1*weedy3[10,1]+
                         P2*P2*weedy3[11,1]+
                         P2*P2*weedy3[12,1]+
                         P2*P2*weedy3[13,1]+
                         P1*P2*weedy3[14,1]+
                         P1*P2*weedy3[15,1]+
                         P1*P2*weedy3[16,1]+
                         P2*P2*weedy3[17,1]+
                         P2*P2*weedy3[18,1]+
                         P2*P2*weedy3[19,1])),
  ss1predict %>% filter(Group == "Farmers") %>%
    mutate(weedy = exp(P2*weedy3[1,1]+
                         P2*weedy3[2,1]+
                         P1*weedy3[3,1]+
                         P2*weedy3[4,1]+
                         P2*weedy3[5,1]+
                         depth1*weedy3[6,1]+
                         b.herbivore1*weedy3[7,1]+
                         year1*weedy3[8,1]+
                         diadema1*weedy3[9,1]+
                         P2*P2*weedy3[10,1]+
                         P2*P1*weedy3[11,1]+
                         P2*P2*weedy3[12,1]+
                         P2*P2*weedy3[13,1]+
                         P2*P1*weedy3[14,1]+
                         P2*P2*weedy3[15,1]+
                         P2*P2*weedy3[16,1]+
                         P1*P2*weedy3[17,1]+
                         P1*P2*weedy3[18,1]+
                         P2*P2*weedy3[19,1])),
  ss1predict %>% filter(Group == "Scrapers") %>%
    mutate(weedy = exp(P2*weedy3[1,1]+
                         P2*weedy3[2,1]+
                         P2*weedy3[3,1]+
                         P1*weedy3[4,1]+
                         P2*weedy3[5,1]+
                         depth1*weedy3[6,1]+
                         b.herbivore1*weedy3[7,1]+
                         year1*weedy3[8,1]+
                         diadema1*weedy3[9,1]+
                         P2*P2*weedy3[10,1]+
                         P2*P2*weedy3[11,1]+
                         P2*P1*weedy3[12,1]+
                         P2*P2*weedy3[13,1]+
                         P2*P2*weedy3[14,1]+
                         P2*P1*weedy3[15,1]+
                         P2*P2*weedy3[16,1]+
                         P2*P1*weedy3[17,1]+
                         P2*P2*weedy3[18,1]+
                         P1*P2*weedy3[19,1])),
  ss1predict %>% filter(Group == "Excavators") %>%
    mutate(weedy = exp(P2*weedy3[1,1]+
                         P2*weedy3[2,1]+
                         P2*weedy3[3,1]+
                         P2*weedy3[4,1]+
                         P1*weedy3[5,1]+
                         depth1*weedy3[6,1]+
                         b.herbivore1*weedy3[7,1]+
                         year1*weedy3[8,1]+
                         diadema1*weedy3[9,1]+
                         P2*P2*weedy3[10,1]+
                         P2*P2*weedy3[11,1]+
                         P2*P2*weedy3[12,1]+
                         P2*P1*weedy3[13,1]+
                         P2*P2*weedy3[14,1]+
                         P2*P2*weedy3[15,1]+
                         P2*P1*weedy3[16,1]+
                         P2*P2*weedy3[17,1]+
                         P2*P1*weedy3[18,1]+
                         P2*P1*weedy3[19,1]))
)

ss1predict <- rbind(
  ss1predict %>% filter(Group == "Croppers") %>%
    mutate(stress = exp(P1*stress3[1,1]+
                         P2*stress3[2,1]+
                         P2*stress3[3,1]+
                         P2*stress3[4,1]+
                         P2*stress3[5,1]+
                         depth1*stress3[6,1]+
                         b.herbivore1*stress3[7,1]+
                         year1*stress3[8,1]+
                         diadema1*stress3[9,1]+
                         P1*P2*stress3[10,1]+
                         P1*P2*stress3[11,1]+
                         P1*P2*stress3[12,1]+
                         P1*P2*stress3[13,1]+
                         P2*P2*stress3[14,1]+
                         P2*P2*stress3[15,1]+
                         P2*P2*stress3[16,1]+
                         P2*P2*stress3[17,1]+
                         P2*P2*stress3[18,1]+
                         P2*P2*stress3[19,1])),
  ss1predict %>% filter(Group == "Browsers") %>%
    mutate(stress = exp(P2*stress3[1,1]+
                         P1*stress3[2,1]+
                         P2*stress3[3,1]+
                         P2*stress3[4,1]+
                         P2*stress3[5,1]+
                         depth1*stress3[6,1]+
                         b.herbivore1*stress3[7,1]+
                         year1*stress3[8,1]+
                         diadema1*stress3[9,1]+
                         P2*P1*stress3[10,1]+
                         P2*P2*stress3[11,1]+
                         P2*P2*stress3[12,1]+
                         P2*P2*stress3[13,1]+
                         P1*P2*stress3[14,1]+
                         P1*P2*stress3[15,1]+
                         P1*P2*stress3[16,1]+
                         P2*P2*stress3[17,1]+
                         P2*P2*stress3[18,1]+
                         P2*P2*stress3[19,1])),
  ss1predict %>% filter(Group == "Farmers") %>%
    mutate(stress = exp(P2*stress3[1,1]+
                         P2*stress3[2,1]+
                         P1*stress3[3,1]+
                         P2*stress3[4,1]+
                         P2*stress3[5,1]+
                         depth1*stress3[6,1]+
                         b.herbivore1*stress3[7,1]+
                         year1*stress3[8,1]+
                         diadema1*stress3[9,1]+
                         P2*P2*stress3[10,1]+
                         P2*P1*stress3[11,1]+
                         P2*P2*stress3[12,1]+
                         P2*P2*stress3[13,1]+
                         P2*P1*stress3[14,1]+
                         P2*P2*stress3[15,1]+
                         P2*P2*stress3[16,1]+
                         P1*P2*stress3[17,1]+
                         P1*P2*stress3[18,1]+
                         P2*P2*stress3[19,1])),
  ss1predict %>% filter(Group == "Scrapers") %>%
    mutate(stress = exp(P2*stress3[1,1]+
                         P2*stress3[2,1]+
                         P2*stress3[3,1]+
                         P1*stress3[4,1]+
                         P2*stress3[5,1]+
                         depth1*stress3[6,1]+
                         b.herbivore1*stress3[7,1]+
                         year1*stress3[8,1]+
                         diadema1*stress3[9,1]+
                         P2*P2*stress3[10,1]+
                         P2*P2*stress3[11,1]+
                         P2*P1*stress3[12,1]+
                         P2*P2*stress3[13,1]+
                         P2*P2*stress3[14,1]+
                         P2*P1*stress3[15,1]+
                         P2*P2*stress3[16,1]+
                         P2*P1*stress3[17,1]+
                         P2*P2*stress3[18,1]+
                         P1*P2*stress3[19,1])),
  ss1predict %>% filter(Group == "Excavators") %>%
    mutate(stress = exp(P2*stress3[1,1]+
                         P2*stress3[2,1]+
                         P2*stress3[3,1]+
                         P2*stress3[4,1]+
                         P1*stress3[5,1]+
                         depth1*stress3[6,1]+
                         b.herbivore1*stress3[7,1]+
                         year1*stress3[8,1]+
                         diadema1*stress3[9,1]+
                         P2*P2*stress3[10,1]+
                         P2*P2*stress3[11,1]+
                         P2*P2*stress3[12,1]+
                         P2*P1*stress3[13,1]+
                         P2*P2*stress3[14,1]+
                         P2*P2*stress3[15,1]+
                         P2*P1*stress3[16,1]+
                         P2*P2*stress3[17,1]+
                         P2*P1*stress3[18,1]+
                         P2*P1*stress3[19,1]))
)

ss1predict <- rbind(
  ss1predict %>% filter(Group == "Croppers") %>%
    mutate(comp = exp(P1*comp3[1,1]+
                         P2*comp3[2,1]+
                         P2*comp3[3,1]+
                         P2*comp3[4,1]+
                         P2*comp3[5,1]+
                         depth1*comp3[6,1]+
                         b.herbivore1*comp3[7,1]+
                         year1*comp3[8,1]+
                         diadema1*comp3[9,1]+
                         P1*P2*comp3[10,1]+
                         P1*P2*comp3[11,1]+
                         P1*P2*comp3[12,1]+
                         P1*P2*comp3[13,1]+
                         P2*P2*comp3[14,1]+
                         P2*P2*comp3[15,1]+
                         P2*P2*comp3[16,1]+
                         P2*P2*comp3[17,1]+
                         P2*P2*comp3[18,1]+
                         P2*P2*comp3[19,1])),
  ss1predict %>% filter(Group == "Browsers") %>%
    mutate(comp = exp(P2*comp3[1,1]+
                         P1*comp3[2,1]+
                         P2*comp3[3,1]+
                         P2*comp3[4,1]+
                         P2*comp3[5,1]+
                         depth1*comp3[6,1]+
                         b.herbivore1*comp3[7,1]+
                         year1*comp3[8,1]+
                         diadema1*comp3[9,1]+
                         P2*P1*comp3[10,1]+
                         P2*P2*comp3[11,1]+
                         P2*P2*comp3[12,1]+
                         P2*P2*comp3[13,1]+
                         P1*P2*comp3[14,1]+
                         P1*P2*comp3[15,1]+
                         P1*P2*comp3[16,1]+
                         P2*P2*comp3[17,1]+
                         P2*P2*comp3[18,1]+
                         P2*P2*comp3[19,1])),
  ss1predict %>% filter(Group == "Farmers") %>%
    mutate(comp = exp(P2*comp3[1,1]+
                         P2*comp3[2,1]+
                         P1*comp3[3,1]+
                         P2*comp3[4,1]+
                         P2*comp3[5,1]+
                         depth1*comp3[6,1]+
                         b.herbivore1*comp3[7,1]+
                         year1*comp3[8,1]+
                         diadema1*comp3[9,1]+
                         P2*P2*comp3[10,1]+
                         P2*P1*comp3[11,1]+
                         P2*P2*comp3[12,1]+
                         P2*P2*comp3[13,1]+
                         P2*P1*comp3[14,1]+
                         P2*P2*comp3[15,1]+
                         P2*P2*comp3[16,1]+
                         P1*P2*comp3[17,1]+
                         P1*P2*comp3[18,1]+
                         P2*P2*comp3[19,1])),
  ss1predict %>% filter(Group == "Scrapers") %>%
    mutate(comp = exp(P2*comp3[1,1]+
                         P2*comp3[2,1]+
                         P2*comp3[3,1]+
                         P1*comp3[4,1]+
                         P2*comp3[5,1]+
                         depth1*comp3[6,1]+
                         b.herbivore1*comp3[7,1]+
                         year1*comp3[8,1]+
                         diadema1*comp3[9,1]+
                         P2*P2*comp3[10,1]+
                         P2*P2*comp3[11,1]+
                         P2*P1*comp3[12,1]+
                         P2*P2*comp3[13,1]+
                         P2*P2*comp3[14,1]+
                         P2*P1*comp3[15,1]+
                         P2*P2*comp3[16,1]+
                         P2*P1*comp3[17,1]+
                         P2*P2*comp3[18,1]+
                         P1*P2*comp3[19,1])),
  ss1predict %>% filter(Group == "Excavators") %>%
    mutate(comp = exp(P2*comp3[1,1]+
                         P2*comp3[2,1]+
                         P2*comp3[3,1]+
                         P2*comp3[4,1]+
                         P1*comp3[5,1]+
                         depth1*comp3[6,1]+
                         b.herbivore1*comp3[7,1]+
                         year1*comp3[8,1]+
                         diadema1*comp3[9,1]+
                         P2*P2*comp3[10,1]+
                         P2*P2*comp3[11,1]+
                         P2*P2*comp3[12,1]+
                         P2*P1*comp3[13,1]+
                         P2*P2*comp3[14,1]+
                         P2*P2*comp3[15,1]+
                         P2*P1*comp3[16,1]+
                         P2*P2*comp3[17,1]+
                         P2*P1*comp3[18,1]+
                         P2*P1*comp3[19,1]))
)

ss1predict <- rbind(
  ss1predict %>% filter(Group == "Croppers") %>%
    mutate(gen = exp(P1*gen3[1,1]+
                         P2*gen3[2,1]+
                         P2*gen3[3,1]+
                         P2*gen3[4,1]+
                         P2*gen3[5,1]+
                         depth1*gen3[6,1]+
                         b.herbivore1*gen3[7,1]+
                         year1*gen3[8,1]+
                         diadema1*gen3[9,1]+
                         P1*P2*gen3[10,1]+
                         P1*P2*gen3[11,1]+
                         P1*P2*gen3[12,1]+
                         P1*P2*gen3[13,1]+
                         P2*P2*gen3[14,1]+
                         P2*P2*gen3[15,1]+
                         P2*P2*gen3[16,1]+
                         P2*P2*gen3[17,1]+
                         P2*P2*gen3[18,1]+
                         P2*P2*gen3[19,1])),
  ss1predict %>% filter(Group == "Browsers") %>%
    mutate(gen = exp(P2*gen3[1,1]+
                         P1*gen3[2,1]+
                         P2*gen3[3,1]+
                         P2*gen3[4,1]+
                         P2*gen3[5,1]+
                         depth1*gen3[6,1]+
                         b.herbivore1*gen3[7,1]+
                         year1*gen3[8,1]+
                         diadema1*gen3[9,1]+
                         P2*P1*gen3[10,1]+
                         P2*P2*gen3[11,1]+
                         P2*P2*gen3[12,1]+
                         P2*P2*gen3[13,1]+
                         P1*P2*gen3[14,1]+
                         P1*P2*gen3[15,1]+
                         P1*P2*gen3[16,1]+
                         P2*P2*gen3[17,1]+
                         P2*P2*gen3[18,1]+
                         P2*P2*gen3[19,1])),
  ss1predict %>% filter(Group == "Farmers") %>%
    mutate(gen = exp(P2*gen3[1,1]+
                         P2*gen3[2,1]+
                         P1*gen3[3,1]+
                         P2*gen3[4,1]+
                         P2*gen3[5,1]+
                         depth1*gen3[6,1]+
                         b.herbivore1*gen3[7,1]+
                         year1*gen3[8,1]+
                         diadema1*gen3[9,1]+
                         P2*P2*gen3[10,1]+
                         P2*P1*gen3[11,1]+
                         P2*P2*gen3[12,1]+
                         P2*P2*gen3[13,1]+
                         P2*P1*gen3[14,1]+
                         P2*P2*gen3[15,1]+
                         P2*P2*gen3[16,1]+
                         P1*P2*gen3[17,1]+
                         P1*P2*gen3[18,1]+
                         P2*P2*gen3[19,1])),
  ss1predict %>% filter(Group == "Scrapers") %>%
    mutate(gen = exp(P2*gen3[1,1]+
                         P2*gen3[2,1]+
                         P2*gen3[3,1]+
                         P1*gen3[4,1]+
                         P2*gen3[5,1]+
                         depth1*gen3[6,1]+
                         b.herbivore1*gen3[7,1]+
                         year1*gen3[8,1]+
                         diadema1*gen3[9,1]+
                         P2*P2*gen3[10,1]+
                         P2*P2*gen3[11,1]+
                         P2*P1*gen3[12,1]+
                         P2*P2*gen3[13,1]+
                         P2*P2*gen3[14,1]+
                         P2*P1*gen3[15,1]+
                         P2*P2*gen3[16,1]+
                         P2*P1*gen3[17,1]+
                         P2*P2*gen3[18,1]+
                         P1*P2*gen3[19,1])),
  ss1predict %>% filter(Group == "Excavators") %>%
    mutate(gen = exp(P2*gen3[1,1]+
                         P2*gen3[2,1]+
                         P2*gen3[3,1]+
                         P2*gen3[4,1]+
                         P1*gen3[5,1]+
                         depth1*gen3[6,1]+
                         b.herbivore1*gen3[7,1]+
                         year1*gen3[8,1]+
                         diadema1*gen3[9,1]+
                         P2*P2*gen3[10,1]+
                         P2*P2*gen3[11,1]+
                         P2*P2*gen3[12,1]+
                         P2*P1*gen3[13,1]+
                         P2*P2*gen3[14,1]+
                         P2*P2*gen3[15,1]+
                         P2*P1*gen3[16,1]+
                         P2*P2*gen3[17,1]+
                         P2*P1*gen3[18,1]+
                         P2*P1*gen3[19,1]))
)


#calculate y for subset2 up to max P observed (recruits)
ss2predict <- rbind(
  ss2predict %>% filter(Group == "Croppers") %>%
    mutate(recruits = exp(P1*recruits3[1,1]+
                         P2*recruits3[2,1]+
                         P2*recruits3[3,1]+
                         P2*recruits3[4,1]+
                         P2*recruits3[5,1]+
                         depth2*recruits3[6,1]+
                         b.herbivore2*recruits3[7,1]+
                         year2*recruits3[8,1]+
                         diadema2*recruits3[9,1]+
                         P1*P2*recruits3[10,1]+
                         P1*P2*recruits3[11,1]+
                         P1*P2*recruits3[12,1]+
                         P1*P2*recruits3[13,1]+
                         P2*P2*recruits3[14,1]+
                         P2*P2*recruits3[15,1]+
                         P2*P2*recruits3[16,1]+
                         P2*P2*recruits3[17,1]+
                         P2*P2*recruits3[18,1]+
                         P2*P2*recruits3[19,1])),
  ss2predict %>% filter(Group == "Browsers") %>%
    mutate(recruits = exp(P2*recruits3[1,1]+
                         P1*recruits3[2,1]+
                         P2*recruits3[3,1]+
                         P2*recruits3[4,1]+
                         P2*recruits3[5,1]+
                         depth2*recruits3[6,1]+
                         b.herbivore2*recruits3[7,1]+
                         year2*recruits3[8,1]+
                         diadema2*recruits3[9,1]+
                         P2*P1*recruits3[10,1]+
                         P2*P2*recruits3[11,1]+
                         P2*P2*recruits3[12,1]+
                         P2*P2*recruits3[13,1]+
                         P1*P2*recruits3[14,1]+
                         P1*P2*recruits3[15,1]+
                         P1*P2*recruits3[16,1]+
                         P2*P2*recruits3[17,1]+
                         P2*P2*recruits3[18,1]+
                         P2*P2*recruits3[19,1])),
  ss2predict %>% filter(Group == "Farmers") %>%
    mutate(recruits = exp(P2*recruits3[1,1]+
                         P2*recruits3[2,1]+
                         P1*recruits3[3,1]+
                         P2*recruits3[4,1]+
                         P2*recruits3[5,1]+
                         depth2*recruits3[6,1]+
                         b.herbivore2*recruits3[7,1]+
                         year2*recruits3[8,1]+
                         diadema2*recruits3[9,1]+
                         P2*P2*recruits3[10,1]+
                         P2*P1*recruits3[11,1]+
                         P2*P2*recruits3[12,1]+
                         P2*P2*recruits3[13,1]+
                         P2*P1*recruits3[14,1]+
                         P2*P2*recruits3[15,1]+
                         P2*P2*recruits3[16,1]+
                         P1*P2*recruits3[17,1]+
                         P1*P2*recruits3[18,1]+
                         P2*P2*recruits3[19,1])),
  ss2predict %>% filter(Group == "Scrapers") %>%
    mutate(recruits = exp(P2*recruits3[1,1]+
                         P2*recruits3[2,1]+
                         P2*recruits3[3,1]+
                         P1*recruits3[4,1]+
                         P2*recruits3[5,1]+
                         depth2*recruits3[6,1]+
                         b.herbivore2*recruits3[7,1]+
                         year2*recruits3[8,1]+
                         diadema2*recruits3[9,1]+
                         P2*P2*recruits3[10,1]+
                         P2*P2*recruits3[11,1]+
                         P2*P1*recruits3[12,1]+
                         P2*P2*recruits3[13,1]+
                         P2*P2*recruits3[14,1]+
                         P2*P1*recruits3[15,1]+
                         P2*P2*recruits3[16,1]+
                         P2*P1*recruits3[17,1]+
                         P2*P2*recruits3[18,1]+
                         P1*P2*recruits3[19,1])),
  ss2predict %>% filter(Group == "Excavators") %>%
    mutate(recruits = exp(P2*recruits3[1,1]+
                         P2*recruits3[2,1]+
                         P2*recruits3[3,1]+
                         P2*recruits3[4,1]+
                         P1*recruits3[5,1]+
                         depth2*recruits3[6,1]+
                         b.herbivore2*recruits3[7,1]+
                         year2*recruits3[8,1]+
                         diadema2*recruits3[9,1]+
                         P2*P2*recruits3[10,1]+
                         P2*P2*recruits3[11,1]+
                         P2*P2*recruits3[12,1]+
                         P2*P1*recruits3[13,1]+
                         P2*P2*recruits3[14,1]+
                         P2*P2*recruits3[15,1]+
                         P2*P1*recruits3[16,1]+
                         P2*P2*recruits3[17,1]+
                         P2*P1*recruits3[18,1]+
                         P2*P1*recruits3[19,1]))
)

#calculate y for subset3 up to max P observed (algal cover)
ss3predict <- rbind(
  ss3predict %>% filter(Group == "Croppers") %>%
    mutate(fleshy = inv.logit(P1*fleshy3[1,1]+
                            P2*fleshy3[2,1]+
                            P2*fleshy3[3,1]+
                            P2*fleshy3[4,1]+
                            P2*fleshy3[5,1]+
                            depth3*fleshy3[6,1]+
                            b.herbivore3*fleshy3[7,1]+
                            year3*fleshy3[8,1]+
                            diadema3*fleshy3[9,1]+
                            P1*P2*fleshy3[10,1]+
                            P1*P2*fleshy3[11,1]+
                            P1*P2*fleshy3[12,1]+
                            P1*P2*fleshy3[13,1]+
                            P2*P2*fleshy3[14,1]+
                            P2*P2*fleshy3[15,1]+
                            P2*P2*fleshy3[16,1]+
                            P2*P2*fleshy3[17,1]+
                            P2*P2*fleshy3[18,1]+
                            P2*P2*fleshy3[19,1])),
  ss3predict %>% filter(Group == "Browsers") %>%
    mutate(fleshy = inv.logit(P2*fleshy3[1,1]+
                            P1*fleshy3[2,1]+
                            P2*fleshy3[3,1]+
                            P2*fleshy3[4,1]+
                            P2*fleshy3[5,1]+
                            depth3*fleshy3[6,1]+
                            b.herbivore3*fleshy3[7,1]+
                            year3*fleshy3[8,1]+
                            diadema3*fleshy3[9,1]+
                            P2*P1*fleshy3[10,1]+
                            P2*P2*fleshy3[11,1]+
                            P2*P2*fleshy3[12,1]+
                            P2*P2*fleshy3[13,1]+
                            P1*P2*fleshy3[14,1]+
                            P1*P2*fleshy3[15,1]+
                            P1*P2*fleshy3[16,1]+
                            P2*P2*fleshy3[17,1]+
                            P2*P2*fleshy3[18,1]+
                            P2*P2*fleshy3[19,1])),
  ss3predict %>% filter(Group == "Farmers") %>%
    mutate(fleshy = inv.logit(P2*fleshy3[1,1]+
                            P2*fleshy3[2,1]+
                            P1*fleshy3[3,1]+
                            P2*fleshy3[4,1]+
                            P2*fleshy3[5,1]+
                            depth3*fleshy3[6,1]+
                            b.herbivore3*fleshy3[7,1]+
                            year3*fleshy3[8,1]+
                            diadema3*fleshy3[9,1]+
                            P2*P2*fleshy3[10,1]+
                            P2*P1*fleshy3[11,1]+
                            P2*P2*fleshy3[12,1]+
                            P2*P2*fleshy3[13,1]+
                            P2*P1*fleshy3[14,1]+
                            P2*P2*fleshy3[15,1]+
                            P2*P2*fleshy3[16,1]+
                            P1*P2*fleshy3[17,1]+
                            P1*P2*fleshy3[18,1]+
                            P2*P2*fleshy3[19,1])),
  ss3predict %>% filter(Group == "Scrapers") %>%
    mutate(fleshy = inv.logit(P2*fleshy3[1,1]+
                            P2*fleshy3[2,1]+
                            P2*fleshy3[3,1]+
                            P1*fleshy3[4,1]+
                            P2*fleshy3[5,1]+
                            depth3*fleshy3[6,1]+
                            b.herbivore3*fleshy3[7,1]+
                            year3*fleshy3[8,1]+
                            diadema3*fleshy3[9,1]+
                            P2*P2*fleshy3[10,1]+
                            P2*P2*fleshy3[11,1]+
                            P2*P1*fleshy3[12,1]+
                            P2*P2*fleshy3[13,1]+
                            P2*P2*fleshy3[14,1]+
                            P2*P1*fleshy3[15,1]+
                            P2*P2*fleshy3[16,1]+
                            P2*P1*fleshy3[17,1]+
                            P2*P2*fleshy3[18,1]+
                            P1*P2*fleshy3[19,1])),
  ss3predict %>% filter(Group == "Excavators") %>%
    mutate(fleshy = inv.logit(P2*fleshy3[1,1]+
                            P2*fleshy3[2,1]+
                            P2*fleshy3[3,1]+
                            P2*fleshy3[4,1]+
                            P1*fleshy3[5,1]+
                            depth3*fleshy3[6,1]+
                            b.herbivore3*fleshy3[7,1]+
                            year3*fleshy3[8,1]+
                            diadema3*fleshy3[9,1]+
                            P2*P2*fleshy3[10,1]+
                            P2*P2*fleshy3[11,1]+
                            P2*P2*fleshy3[12,1]+
                            P2*P1*fleshy3[13,1]+
                            P2*P2*fleshy3[14,1]+
                            P2*P2*fleshy3[15,1]+
                            P2*P1*fleshy3[16,1]+
                            P2*P2*fleshy3[17,1]+
                            P2*P1*fleshy3[18,1]+
                            P2*P1*fleshy3[19,1]))
)

ss3predict <- rbind(
  ss3predict %>% filter(Group == "Croppers") %>%
    mutate(calc = inv.logit(P1*calc3[1,1]+
                          P2*calc3[2,1]+
                          P2*calc3[3,1]+
                          P2*calc3[4,1]+
                          P2*calc3[5,1]+
                          depth3*calc3[6,1]+
                          b.herbivore3*calc3[7,1]+
                          year3*calc3[8,1]+
                          diadema3*calc3[9,1]+
                          P1*P2*calc3[10,1]+
                          P1*P2*calc3[11,1]+
                          P1*P2*calc3[12,1]+
                          P1*P2*calc3[13,1]+
                          P2*P2*calc3[14,1]+
                          P2*P2*calc3[15,1]+
                          P2*P2*calc3[16,1]+
                          P2*P2*calc3[17,1]+
                          P2*P2*calc3[18,1]+
                          P2*P2*calc3[19,1])),
  ss3predict %>% filter(Group == "Browsers") %>%
    mutate(calc = inv.logit(P2*calc3[1,1]+
                          P1*calc3[2,1]+
                          P2*calc3[3,1]+
                          P2*calc3[4,1]+
                          P2*calc3[5,1]+
                          depth3*calc3[6,1]+
                          b.herbivore3*calc3[7,1]+
                          year3*calc3[8,1]+
                          diadema3*calc3[9,1]+
                          P2*P1*calc3[10,1]+
                          P2*P2*calc3[11,1]+
                          P2*P2*calc3[12,1]+
                          P2*P2*calc3[13,1]+
                          P1*P2*calc3[14,1]+
                          P1*P2*calc3[15,1]+
                          P1*P2*calc3[16,1]+
                          P2*P2*calc3[17,1]+
                          P2*P2*calc3[18,1]+
                          P2*P2*calc3[19,1])),
  ss3predict %>% filter(Group == "Farmers") %>%
    mutate(calc = inv.logit(P2*calc3[1,1]+
                          P2*calc3[2,1]+
                          P1*calc3[3,1]+
                          P2*calc3[4,1]+
                          P2*calc3[5,1]+
                          depth3*calc3[6,1]+
                          b.herbivore3*calc3[7,1]+
                          year3*calc3[8,1]+
                          diadema3*calc3[9,1]+
                          P2*P2*calc3[10,1]+
                          P2*P1*calc3[11,1]+
                          P2*P2*calc3[12,1]+
                          P2*P2*calc3[13,1]+
                          P2*P1*calc3[14,1]+
                          P2*P2*calc3[15,1]+
                          P2*P2*calc3[16,1]+
                          P1*P2*calc3[17,1]+
                          P1*P2*calc3[18,1]+
                          P2*P2*calc3[19,1])),
  ss3predict %>% filter(Group == "Scrapers") %>%
    mutate(calc = inv.logit(P2*calc3[1,1]+
                          P2*calc3[2,1]+
                          P2*calc3[3,1]+
                          P1*calc3[4,1]+
                          P2*calc3[5,1]+
                          depth3*calc3[6,1]+
                          b.herbivore3*calc3[7,1]+
                          year3*calc3[8,1]+
                          diadema3*calc3[9,1]+
                          P2*P2*calc3[10,1]+
                          P2*P2*calc3[11,1]+
                          P2*P1*calc3[12,1]+
                          P2*P2*calc3[13,1]+
                          P2*P2*calc3[14,1]+
                          P2*P1*calc3[15,1]+
                          P2*P2*calc3[16,1]+
                          P2*P1*calc3[17,1]+
                          P2*P2*calc3[18,1]+
                          P1*P2*calc3[19,1])),
  ss3predict %>% filter(Group == "Excavators") %>%
    mutate(calc = inv.logit(P2*calc3[1,1]+
                          P2*calc3[2,1]+
                          P2*calc3[3,1]+
                          P2*calc3[4,1]+
                          P1*calc3[5,1]+
                          depth3*calc3[6,1]+
                          b.herbivore3*calc3[7,1]+
                          year3*calc3[8,1]+
                          diadema3*calc3[9,1]+
                          P2*P2*calc3[10,1]+
                          P2*P2*calc3[11,1]+
                          P2*P2*calc3[12,1]+
                          P2*P1*calc3[13,1]+
                          P2*P2*calc3[14,1]+
                          P2*P2*calc3[15,1]+
                          P2*P1*calc3[16,1]+
                          P2*P2*calc3[17,1]+
                          P2*P1*calc3[18,1]+
                          P2*P1*calc3[19,1]))
)

ss3predict <- rbind(
  ss3predict %>% filter(Group == "Croppers") %>%
    mutate(turf = inv.logit(P1*turf3[1,1]+
                          P2*turf3[2,1]+
                          P2*turf3[3,1]+
                          P2*turf3[4,1]+
                          P2*turf3[5,1]+
                          depth3*turf3[6,1]+
                          b.herbivore3*turf3[7,1]+
                          year3*turf3[8,1]+
                          diadema3*turf3[9,1]+
                          P1*P2*turf3[10,1]+
                          P1*P2*turf3[11,1]+
                          P1*P2*turf3[12,1]+
                          P1*P2*turf3[13,1]+
                          P2*P2*turf3[14,1]+
                          P2*P2*turf3[15,1]+
                          P2*P2*turf3[16,1]+
                          P2*P2*turf3[17,1]+
                          P2*P2*turf3[18,1]+
                          P2*P2*turf3[19,1])),
  ss3predict %>% filter(Group == "Browsers") %>%
    mutate(turf = inv.logit(P2*turf3[1,1]+
                          P1*turf3[2,1]+
                          P2*turf3[3,1]+
                          P2*turf3[4,1]+
                          P2*turf3[5,1]+
                          depth3*turf3[6,1]+
                          b.herbivore3*turf3[7,1]+
                          year3*turf3[8,1]+
                          diadema3*turf3[9,1]+
                          P2*P1*turf3[10,1]+
                          P2*P2*turf3[11,1]+
                          P2*P2*turf3[12,1]+
                          P2*P2*turf3[13,1]+
                          P1*P2*turf3[14,1]+
                          P1*P2*turf3[15,1]+
                          P1*P2*turf3[16,1]+
                          P2*P2*turf3[17,1]+
                          P2*P2*turf3[18,1]+
                          P2*P2*turf3[19,1])),
  ss3predict %>% filter(Group == "Farmers") %>%
    mutate(turf = inv.logit(P2*turf3[1,1]+
                          P2*turf3[2,1]+
                          P1*turf3[3,1]+
                          P2*turf3[4,1]+
                          P2*turf3[5,1]+
                          depth3*turf3[6,1]+
                          b.herbivore3*turf3[7,1]+
                          year3*turf3[8,1]+
                          diadema3*turf3[9,1]+
                          P2*P2*turf3[10,1]+
                          P2*P1*turf3[11,1]+
                          P2*P2*turf3[12,1]+
                          P2*P2*turf3[13,1]+
                          P2*P1*turf3[14,1]+
                          P2*P2*turf3[15,1]+
                          P2*P2*turf3[16,1]+
                          P1*P2*turf3[17,1]+
                          P1*P2*turf3[18,1]+
                          P2*P2*turf3[19,1])),
  ss3predict %>% filter(Group == "Scrapers") %>%
    mutate(turf = inv.logit(P2*turf3[1,1]+
                          P2*turf3[2,1]+
                          P2*turf3[3,1]+
                          P1*turf3[4,1]+
                          P2*turf3[5,1]+
                          depth3*turf3[6,1]+
                          b.herbivore3*turf3[7,1]+
                          year3*turf3[8,1]+
                          diadema3*turf3[9,1]+
                          P2*P2*turf3[10,1]+
                          P2*P2*turf3[11,1]+
                          P2*P1*turf3[12,1]+
                          P2*P2*turf3[13,1]+
                          P2*P2*turf3[14,1]+
                          P2*P1*turf3[15,1]+
                          P2*P2*turf3[16,1]+
                          P2*P1*turf3[17,1]+
                          P2*P2*turf3[18,1]+
                          P1*P2*turf3[19,1])),
  ss3predict %>% filter(Group == "Excavators") %>%
    mutate(turf = inv.logit(P2*turf3[1,1]+
                          P2*turf3[2,1]+
                          P2*turf3[3,1]+
                          P2*turf3[4,1]+
                          P1*turf3[5,1]+
                          depth3*turf3[6,1]+
                          b.herbivore3*turf3[7,1]+
                          year3*turf3[8,1]+
                          diadema3*turf3[9,1]+
                          P2*P2*turf3[10,1]+
                          P2*P2*turf3[11,1]+
                          P2*P2*turf3[12,1]+
                          P2*P1*turf3[13,1]+
                          P2*P2*turf3[14,1]+
                          P2*P2*turf3[15,1]+
                          P2*P1*turf3[16,1]+
                          P2*P2*turf3[17,1]+
                          P2*P1*turf3[18,1]+
                          P2*P1*turf3[19,1]))
)

ss3predict <- rbind(
  ss3predict %>% filter(Group == "Croppers") %>%
    mutate(cca = inv.logit(P1*cca3[1,1]+
                          P2*cca3[2,1]+
                          P2*cca3[3,1]+
                          P2*cca3[4,1]+
                          P2*cca3[5,1]+
                          depth3*cca3[6,1]+
                          b.herbivore3*cca3[7,1]+
                          year3*cca3[8,1]+
                          diadema3*cca3[9,1]+
                          P1*P2*cca3[10,1]+
                          P1*P2*cca3[11,1]+
                          P1*P2*cca3[12,1]+
                          P1*P2*cca3[13,1]+
                          P2*P2*cca3[14,1]+
                          P2*P2*cca3[15,1]+
                          P2*P2*cca3[16,1]+
                          P2*P2*cca3[17,1]+
                          P2*P2*cca3[18,1]+
                          P2*P2*cca3[19,1])),
  ss3predict %>% filter(Group == "Browsers") %>%
    mutate(cca = inv.logit(P2*cca3[1,1]+
                          P1*cca3[2,1]+
                          P2*cca3[3,1]+
                          P2*cca3[4,1]+
                          P2*cca3[5,1]+
                          depth3*cca3[6,1]+
                          b.herbivore3*cca3[7,1]+
                          year3*cca3[8,1]+
                          diadema3*cca3[9,1]+
                          P2*P1*cca3[10,1]+
                          P2*P2*cca3[11,1]+
                          P2*P2*cca3[12,1]+
                          P2*P2*cca3[13,1]+
                          P1*P2*cca3[14,1]+
                          P1*P2*cca3[15,1]+
                          P1*P2*cca3[16,1]+
                          P2*P2*cca3[17,1]+
                          P2*P2*cca3[18,1]+
                          P2*P2*cca3[19,1])),
  ss3predict %>% filter(Group == "Farmers") %>%
    mutate(cca = inv.logit(P2*cca3[1,1]+
                          P2*cca3[2,1]+
                          P1*cca3[3,1]+
                          P2*cca3[4,1]+
                          P2*cca3[5,1]+
                          depth3*cca3[6,1]+
                          b.herbivore3*cca3[7,1]+
                          year3*cca3[8,1]+
                          diadema3*cca3[9,1]+
                          P2*P2*cca3[10,1]+
                          P2*P1*cca3[11,1]+
                          P2*P2*cca3[12,1]+
                          P2*P2*cca3[13,1]+
                          P2*P1*cca3[14,1]+
                          P2*P2*cca3[15,1]+
                          P2*P2*cca3[16,1]+
                          P1*P2*cca3[17,1]+
                          P1*P2*cca3[18,1]+
                          P2*P2*cca3[19,1])),
  ss3predict %>% filter(Group == "Scrapers") %>%
    mutate(cca = inv.logit(P2*cca3[1,1]+
                          P2*cca3[2,1]+
                          P2*cca3[3,1]+
                          P1*cca3[4,1]+
                          P2*cca3[5,1]+
                          depth3*cca3[6,1]+
                          b.herbivore3*cca3[7,1]+
                          year3*cca3[8,1]+
                          diadema3*cca3[9,1]+
                          P2*P2*cca3[10,1]+
                          P2*P2*cca3[11,1]+
                          P2*P1*cca3[12,1]+
                          P2*P2*cca3[13,1]+
                          P2*P2*cca3[14,1]+
                          P2*P1*cca3[15,1]+
                          P2*P2*cca3[16,1]+
                          P2*P1*cca3[17,1]+
                          P2*P2*cca3[18,1]+
                          P1*P2*cca3[19,1])),
  ss3predict %>% filter(Group == "Excavators") %>%
    mutate(cca = inv.logit(P2*cca3[1,1]+
                          P2*cca3[2,1]+
                          P2*cca3[3,1]+
                          P2*cca3[4,1]+
                          P1*cca3[5,1]+
                          depth3*cca3[6,1]+
                          b.herbivore3*cca3[7,1]+
                          year3*cca3[8,1]+
                          diadema3*cca3[9,1]+
                          P2*P2*cca3[10,1]+
                          P2*P2*cca3[11,1]+
                          P2*P2*cca3[12,1]+
                          P2*P1*cca3[13,1]+
                          P2*P2*cca3[14,1]+
                          P2*P2*cca3[15,1]+
                          P2*P1*cca3[16,1]+
                          P2*P2*cca3[17,1]+
                          P2*P1*cca3[18,1]+
                          P2*P1*cca3[19,1]))
)

#calculate y for full predictions using coefficients from all 1000 bootstrapped iterations
#read in full coefficients (if applicable)
richness.coef <- read.csv("richness3coef.csv", header = T, stringsAsFactors = F)
richness.coef$iteration <- 1:1000 #add iteration number
richness.coef <- richness.coef %>% slice(rep(1:n(), each = 101)) #duplicate so that row number matches predict.full to calculate y for all functional groups

calcrate.coef <- read.csv("calcrate3coef.csv", header = T, stringsAsFactors = F)
calcrate.coef$iteration <- 1:1000 #add iteration number
calcrate.coef <- calcrate.coef %>% slice(rep(1:n(), each = 101))

total.coef <- read.csv("total3coef.csv", header = T, stringsAsFactors = F)
total.coef$iteration <- 1:1000 #add iteration number
total.coef <- total.coef %>% slice(rep(1:n(), each = 101))

recruits.coef <- read.csv("recruits3coef.csv", header = T, stringsAsFactors = F)
recruits.coef$iteration <- 1:1000 #add iteration number
recruits.coef <- recruits.coef %>% slice(rep(1:n(), each = 101))

weedy.coef <- read.csv("weedy3coef.csv", header = T, stringsAsFactors = F)
weedy.coef$iteration <- 1:1000 #add iteration number
weedy.coef <- weedy.coef %>% slice(rep(1:n(), each = 101))

stress.coef <- read.csv("stress3coef.csv", header = T, stringsAsFactors = F)
stress.coef$iteration <- 1:1000 #add iteration number
stress.coef <- stress.coef %>% slice(rep(1:n(), each = 101))

comp.coef <- read.csv("comp3coef.csv", header = T, stringsAsFactors = F)
comp.coef$iteration <- 1:1000 #add iteration number
comp.coef <- comp.coef %>% slice(rep(1:n(), each = 101))

gen.coef <- read.csv("gen3coef.csv", header = T, stringsAsFactors = F)
gen.coef$iteration <- 1:1000 #add iteration number
gen.coef <- gen.coef %>% slice(rep(1:n(), each = 101))

fleshy.coef <- read.csv("fleshy3coef.csv", header = T, stringsAsFactors = F)
fleshy.coef$iteration <- 1:1000 #add iteration number
fleshy.coef <- fleshy.coef %>% slice(rep(1:n(), each = 101))

calc.coef <- read.csv("calc3coef.csv", header = T, stringsAsFactors = F)
calc.coef$iteration <- 1:1000 #add iteration number
calc.coef <- calc.coef %>% slice(rep(1:n(), each = 101))

turf.coef <- read.csv("turf3coef.csv", header = T, stringsAsFactors = F)
turf.coef$iteration <- 1:1000 #add iteration number
turf.coef <- turf.coef %>% slice(rep(1:n(), each = 101))

cca.coef <- read.csv("cca3coef.csv", header = T, stringsAsFactors = F)
cca.coef$iteration <- 1:1000 #add iteration number
cca.coef <- cca.coef %>% slice(rep(1:n(), each = 101))

predict.full <- rbind(
  predict.full %>% filter(Group == "Croppers") %>%
    mutate(richness = exp(P1*richness.coef$c.v+
                            P2*richness.coef$b.v+
                            P2*richness.coef$f.v+
                            P2*richness.coef$s.v+
                            P2*richness.coef$e.v+
                            depth1*richness.coef$d.v+
                            b.herbivore1*richness.coef$h.v+
                            year1*richness.coef$y.v+
                            diadema1*richness.coef$dd.v+
                            P1*P2*richness.coef$cb.v+
                            P1*P2*richness.coef$cf.v+
                            P1*P2*richness.coef$cs.v+
                            P1*P2*richness.coef$ce.v+
                            P2*P2*richness.coef$bf.v+
                            P2*P2*richness.coef$bs.v+
                            P2*P2*richness.coef$be.v+
                            P2*P2*richness.coef$fs.v+
                            P2*P2*richness.coef$fe.v+
                            P2*P2*richness.coef$se.v)),
  predict.full %>% filter(Group == "Browsers") %>%
    mutate(richness = exp(P2*richness.coef$c.v+
                            P1*richness.coef$b.v+
                            P2*richness.coef$f.v+
                            P2*richness.coef$s.v+
                            P2*richness.coef$e.v+
                            depth1*richness.coef$d.v+
                            b.herbivore1*richness.coef$h.v+
                            year1*richness.coef$y.v+
                            diadema1*richness.coef$dd.v+
                            P2*P1*richness.coef$cb.v+
                            P2*P2*richness.coef$cf.v+
                            P2*P2*richness.coef$cs.v+
                            P2*P2*richness.coef$ce.v+
                            P1*P2*richness.coef$bf.v+
                            P1*P2*richness.coef$bs.v+
                            P1*P2*richness.coef$be.v+
                            P2*P2*richness.coef$fs.v+
                            P2*P2*richness.coef$fe.v+
                            P2*P2*richness.coef$se.v)),
  predict.full %>% filter(Group == "Farmers") %>%
    mutate(richness = exp(P2*richness.coef$c.v+
                            P2*richness.coef$b.v+
                            P1*richness.coef$f.v+
                            P2*richness.coef$s.v+
                            P2*richness.coef$e.v+
                            depth1*richness.coef$d.v+
                            b.herbivore1*richness.coef$h.v+
                            year1*richness.coef$y.v+
                            diadema1*richness.coef$dd.v+
                            P2*P2*richness.coef$cb.v+
                            P2*P1*richness.coef$cf.v+
                            P2*P2*richness.coef$cs.v+
                            P2*P2*richness.coef$ce.v+
                            P2*P1*richness.coef$bf.v+
                            P2*P2*richness.coef$bs.v+
                            P2*P2*richness.coef$be.v+
                            P1*P2*richness.coef$fs.v+
                            P1*P2*richness.coef$fe.v+
                            P2*P2*richness.coef$se.v)),
  predict.full %>% filter(Group == "Scrapers") %>%
    mutate(richness = exp(P2*richness.coef$c.v+
                            P2*richness.coef$b.v+
                            P2*richness.coef$f.v+
                            P1*richness.coef$s.v+
                            P2*richness.coef$e.v+
                            depth1*richness.coef$d.v+
                            b.herbivore1*richness.coef$h.v+
                            year1*richness.coef$y.v+
                            diadema1*richness.coef$dd.v+
                            P2*P2*richness.coef$cb.v+
                            P2*P2*richness.coef$cf.v+
                            P2*P1*richness.coef$cs.v+
                            P2*P2*richness.coef$ce.v+
                            P2*P2*richness.coef$bf.v+
                            P2*P1*richness.coef$bs.v+
                            P2*P2*richness.coef$be.v+
                            P2*P1*richness.coef$fs.v+
                            P2*P2*richness.coef$fe.v+
                            P1*P2*richness.coef$se.v)),
  predict.full %>% filter(Group == "Excavators") %>%
    mutate(richness = exp(P2*richness.coef$c.v+
                            P2*richness.coef$b.v+
                            P2*richness.coef$f.v+
                            P2*richness.coef$s.v+
                            P1*richness.coef$e.v+
                            depth1*richness.coef$d.v+
                            b.herbivore1*richness.coef$h.v+
                            year1*richness.coef$y.v+
                            diadema1*richness.coef$dd.v+
                            P2*P2*richness.coef$cb.v+
                            P2*P2*richness.coef$cf.v+
                            P2*P2*richness.coef$cs.v+
                            P2*P1*richness.coef$ce.v+
                            P2*P2*richness.coef$bf.v+
                            P2*P2*richness.coef$bs.v+
                            P2*P1*richness.coef$be.v+
                            P2*P2*richness.coef$fs.v+
                            P2*P1*richness.coef$fe.v+
                            P2*P1*richness.coef$se.v))
  )

predict.full <- rbind(
  predict.full %>% filter(Group == "Croppers") %>%
    mutate(calcrate = exp(P1*calcrate.coef$c.v+
                            P2*calcrate.coef$b.v+
                            P2*calcrate.coef$f.v+
                            P2*calcrate.coef$s.v+
                            P2*calcrate.coef$e.v+
                            depth1*calcrate.coef$d.v+
                            b.herbivore1*calcrate.coef$h.v+
                            year1*calcrate.coef$y.v+
                            diadema1*calcrate.coef$dd.v+
                            P1*P2*calcrate.coef$cb.v+
                            P1*P2*calcrate.coef$cf.v+
                            P1*P2*calcrate.coef$cs.v+
                            P1*P2*calcrate.coef$ce.v+
                            P2*P2*calcrate.coef$bf.v+
                            P2*P2*calcrate.coef$bs.v+
                            P2*P2*calcrate.coef$be.v+
                            P2*P2*calcrate.coef$fs.v+
                            P2*P2*calcrate.coef$fe.v+
                            P2*P2*calcrate.coef$se.v)),
  predict.full %>% filter(Group == "Browsers") %>%
    mutate(calcrate = exp(P2*calcrate.coef$c.v+
                            P1*calcrate.coef$b.v+
                            P2*calcrate.coef$f.v+
                            P2*calcrate.coef$s.v+
                            P2*calcrate.coef$e.v+
                            depth1*calcrate.coef$d.v+
                            b.herbivore1*calcrate.coef$h.v+
                            year1*calcrate.coef$y.v+
                            diadema1*calcrate.coef$dd.v+
                            P2*P1*calcrate.coef$cb.v+
                            P2*P2*calcrate.coef$cf.v+
                            P2*P2*calcrate.coef$cs.v+
                            P2*P2*calcrate.coef$ce.v+
                            P1*P2*calcrate.coef$bf.v+
                            P1*P2*calcrate.coef$bs.v+
                            P1*P2*calcrate.coef$be.v+
                            P2*P2*calcrate.coef$fs.v+
                            P2*P2*calcrate.coef$fe.v+
                            P2*P2*calcrate.coef$se.v)),
  predict.full %>% filter(Group == "Farmers") %>%
    mutate(calcrate = exp(P2*calcrate.coef$c.v+
                            P2*calcrate.coef$b.v+
                            P1*calcrate.coef$f.v+
                            P2*calcrate.coef$s.v+
                            P2*calcrate.coef$e.v+
                            depth1*calcrate.coef$d.v+
                            b.herbivore1*calcrate.coef$h.v+
                            year1*calcrate.coef$y.v+
                            diadema1*calcrate.coef$dd.v+
                            P2*P2*calcrate.coef$cb.v+
                            P2*P1*calcrate.coef$cf.v+
                            P2*P2*calcrate.coef$cs.v+
                            P2*P2*calcrate.coef$ce.v+
                            P2*P1*calcrate.coef$bf.v+
                            P2*P2*calcrate.coef$bs.v+
                            P2*P2*calcrate.coef$be.v+
                            P1*P2*calcrate.coef$fs.v+
                            P1*P2*calcrate.coef$fe.v+
                            P2*P2*calcrate.coef$se.v)),
  predict.full %>% filter(Group == "Scrapers") %>%
    mutate(calcrate = exp(P2*calcrate.coef$c.v+
                            P2*calcrate.coef$b.v+
                            P2*calcrate.coef$f.v+
                            P1*calcrate.coef$s.v+
                            P2*calcrate.coef$e.v+
                            depth1*calcrate.coef$d.v+
                            b.herbivore1*calcrate.coef$h.v+
                            year1*calcrate.coef$y.v+
                            diadema1*calcrate.coef$dd.v+
                            P2*P2*calcrate.coef$cb.v+
                            P2*P2*calcrate.coef$cf.v+
                            P2*P1*calcrate.coef$cs.v+
                            P2*P2*calcrate.coef$ce.v+
                            P2*P2*calcrate.coef$bf.v+
                            P2*P1*calcrate.coef$bs.v+
                            P2*P2*calcrate.coef$be.v+
                            P2*P1*calcrate.coef$fs.v+
                            P2*P2*calcrate.coef$fe.v+
                            P1*P2*calcrate.coef$se.v)),
  predict.full %>% filter(Group == "Excavators") %>%
    mutate(calcrate = exp(P2*calcrate.coef$c.v+
                            P2*calcrate.coef$b.v+
                            P2*calcrate.coef$f.v+
                            P2*calcrate.coef$s.v+
                            P1*calcrate.coef$e.v+
                            depth1*calcrate.coef$d.v+
                            b.herbivore1*calcrate.coef$h.v+
                            year1*calcrate.coef$y.v+
                            diadema1*calcrate.coef$dd.v+
                            P2*P2*calcrate.coef$cb.v+
                            P2*P2*calcrate.coef$cf.v+
                            P2*P2*calcrate.coef$cs.v+
                            P2*P1*calcrate.coef$ce.v+
                            P2*P2*calcrate.coef$bf.v+
                            P2*P2*calcrate.coef$bs.v+
                            P2*P1*calcrate.coef$be.v+
                            P2*P2*calcrate.coef$fs.v+
                            P2*P1*calcrate.coef$fe.v+
                            P2*P1*calcrate.coef$se.v))
)

predict.full <- rbind(
  predict.full %>% filter(Group == "Croppers") %>%
    mutate(total = exp(P1*total.coef$c.v+
                         P2*total.coef$b.v+
                         P2*total.coef$f.v+
                         P2*total.coef$s.v+
                         P2*total.coef$e.v+
                         depth1*total.coef$d.v+
                         b.herbivore1*total.coef$h.v+
                         year1*total.coef$y.v+
                         diadema1*total.coef$dd.v+
                         P1*P2*total.coef$cb.v+
                         P1*P2*total.coef$cf.v+
                         P1*P2*total.coef$cs.v+
                         P1*P2*total.coef$ce.v+
                         P2*P2*total.coef$bf.v+
                         P2*P2*total.coef$bs.v+
                         P2*P2*total.coef$be.v+
                         P2*P2*total.coef$fs.v+
                         P2*P2*total.coef$fe.v+
                         P2*P2*total.coef$se.v)),
  predict.full %>% filter(Group == "Browsers") %>%
    mutate(total = exp(P2*total.coef$c.v+
                         P1*total.coef$b.v+
                         P2*total.coef$f.v+
                         P2*total.coef$s.v+
                         P2*total.coef$e.v+
                         depth1*total.coef$d.v+
                         b.herbivore1*total.coef$h.v+
                         year1*total.coef$y.v+
                         diadema1*total.coef$dd.v+
                         P2*P1*total.coef$cb.v+
                         P2*P2*total.coef$cf.v+
                         P2*P2*total.coef$cs.v+
                         P2*P2*total.coef$ce.v+
                         P1*P2*total.coef$bf.v+
                         P1*P2*total.coef$bs.v+
                         P1*P2*total.coef$be.v+
                         P2*P2*total.coef$fs.v+
                         P2*P2*total.coef$fe.v+
                         P2*P2*total.coef$se.v)),
  predict.full %>% filter(Group == "Farmers") %>%
    mutate(total = exp(P2*total.coef$c.v+
                         P2*total.coef$b.v+
                         P1*total.coef$f.v+
                         P2*total.coef$s.v+
                         P2*total.coef$e.v+
                         depth1*total.coef$d.v+
                         b.herbivore1*total.coef$h.v+
                         year1*total.coef$y.v+
                         diadema1*total.coef$dd.v+
                         P2*P2*total.coef$cb.v+
                         P2*P1*total.coef$cf.v+
                         P2*P2*total.coef$cs.v+
                         P2*P2*total.coef$ce.v+
                         P2*P1*total.coef$bf.v+
                         P2*P2*total.coef$bs.v+
                         P2*P2*total.coef$be.v+
                         P1*P2*total.coef$fs.v+
                         P1*P2*total.coef$fe.v+
                         P2*P2*total.coef$se.v)),
  predict.full %>% filter(Group == "Scrapers") %>%
    mutate(total = exp(P2*total.coef$c.v+
                         P2*total.coef$b.v+
                         P2*total.coef$f.v+
                         P1*total.coef$s.v+
                         P2*total.coef$e.v+
                         depth1*total.coef$d.v+
                         b.herbivore1*total.coef$h.v+
                         year1*total.coef$y.v+
                         diadema1*total.coef$dd.v+
                         P2*P2*total.coef$cb.v+
                         P2*P2*total.coef$cf.v+
                         P2*P1*total.coef$cs.v+
                         P2*P2*total.coef$ce.v+
                         P2*P2*total.coef$bf.v+
                         P2*P1*total.coef$bs.v+
                         P2*P2*total.coef$be.v+
                         P2*P1*total.coef$fs.v+
                         P2*P2*total.coef$fe.v+
                         P1*P2*total.coef$se.v)),
  predict.full %>% filter(Group == "Excavators") %>%
    mutate(total = exp(P2*total.coef$c.v+
                         P2*total.coef$b.v+
                         P2*total.coef$f.v+
                         P2*total.coef$s.v+
                         P1*total.coef$e.v+
                         depth1*total.coef$d.v+
                         b.herbivore1*total.coef$h.v+
                         year1*total.coef$y.v+
                         diadema1*total.coef$dd.v+
                         P2*P2*total.coef$cb.v+
                         P2*P2*total.coef$cf.v+
                         P2*P2*total.coef$cs.v+
                         P2*P1*total.coef$ce.v+
                         P2*P2*total.coef$bf.v+
                         P2*P2*total.coef$bs.v+
                         P2*P1*total.coef$be.v+
                         P2*P2*total.coef$fs.v+
                         P2*P1*total.coef$fe.v+
                         P2*P1*total.coef$se.v))
)

predict.full <- rbind(
  predict.full %>% filter(Group == "Croppers") %>%
    mutate(recruits = exp(P1*recruits.coef$c.v+
                            P2*recruits.coef$b.v+
                            P2*recruits.coef$f.v+
                            P2*recruits.coef$s.v+
                            P2*recruits.coef$e.v+
                            depth2*recruits.coef$d.v+
                            b.herbivore2*recruits.coef$h.v+
                            year2*recruits.coef$y.v+
                            diadema2*recruits.coef$dd.v+
                            P1*P2*recruits.coef$cb.v+
                            P1*P2*recruits.coef$cf.v+
                            P1*P2*recruits.coef$cs.v+
                            P1*P2*recruits.coef$ce.v+
                            P2*P2*recruits.coef$bf.v+
                            P2*P2*recruits.coef$bs.v+
                            P2*P2*recruits.coef$be.v+
                            P2*P2*recruits.coef$fs.v+
                            P2*P2*recruits.coef$fe.v+
                            P2*P2*recruits.coef$se.v)),
  predict.full %>% filter(Group == "Browsers") %>%
    mutate(recruits = exp(P2*recruits.coef$c.v+
                            P1*recruits.coef$b.v+
                            P2*recruits.coef$f.v+
                            P2*recruits.coef$s.v+
                            P2*recruits.coef$e.v+
                            depth2*recruits.coef$d.v+
                            b.herbivore2*recruits.coef$h.v+
                            year2*recruits.coef$y.v+
                            diadema2*recruits.coef$dd.v+
                            P2*P1*recruits.coef$cb.v+
                            P2*P2*recruits.coef$cf.v+
                            P2*P2*recruits.coef$cs.v+
                            P2*P2*recruits.coef$ce.v+
                            P1*P2*recruits.coef$bf.v+
                            P1*P2*recruits.coef$bs.v+
                            P1*P2*recruits.coef$be.v+
                            P2*P2*recruits.coef$fs.v+
                            P2*P2*recruits.coef$fe.v+
                            P2*P2*recruits.coef$se.v)),
  predict.full %>% filter(Group == "Farmers") %>%
    mutate(recruits = exp(P2*recruits.coef$c.v+
                            P2*recruits.coef$b.v+
                            P1*recruits.coef$f.v+
                            P2*recruits.coef$s.v+
                            P2*recruits.coef$e.v+
                            depth2*recruits.coef$d.v+
                            b.herbivore2*recruits.coef$h.v+
                            year2*recruits.coef$y.v+
                            diadema2*recruits.coef$dd.v+
                            P2*P2*recruits.coef$cb.v+
                            P2*P1*recruits.coef$cf.v+
                            P2*P2*recruits.coef$cs.v+
                            P2*P2*recruits.coef$ce.v+
                            P2*P1*recruits.coef$bf.v+
                            P2*P2*recruits.coef$bs.v+
                            P2*P2*recruits.coef$be.v+
                            P1*P2*recruits.coef$fs.v+
                            P1*P2*recruits.coef$fe.v+
                            P2*P2*recruits.coef$se.v)),
  predict.full %>% filter(Group == "Scrapers") %>%
    mutate(recruits = exp(P2*recruits.coef$c.v+
                            P2*recruits.coef$b.v+
                            P2*recruits.coef$f.v+
                            P1*recruits.coef$s.v+
                            P2*recruits.coef$e.v+
                            depth2*recruits.coef$d.v+
                            b.herbivore2*recruits.coef$h.v+
                            year2*recruits.coef$y.v+
                            diadema2*recruits.coef$dd.v+
                            P2*P2*recruits.coef$cb.v+
                            P2*P2*recruits.coef$cf.v+
                            P2*P1*recruits.coef$cs.v+
                            P2*P2*recruits.coef$ce.v+
                            P2*P2*recruits.coef$bf.v+
                            P2*P1*recruits.coef$bs.v+
                            P2*P2*recruits.coef$be.v+
                            P2*P1*recruits.coef$fs.v+
                            P2*P2*recruits.coef$fe.v+
                            P1*P2*recruits.coef$se.v)),
  predict.full %>% filter(Group == "Excavators") %>%
    mutate(recruits = exp(P2*recruits.coef$c.v+
                            P2*recruits.coef$b.v+
                            P2*recruits.coef$f.v+
                            P2*recruits.coef$s.v+
                            P1*recruits.coef$e.v+
                            depth2*recruits.coef$d.v+
                            b.herbivore2*recruits.coef$h.v+
                            year2*recruits.coef$y.v+
                            diadema2*recruits.coef$dd.v+
                            P2*P2*recruits.coef$cb.v+
                            P2*P2*recruits.coef$cf.v+
                            P2*P2*recruits.coef$cs.v+
                            P2*P1*recruits.coef$ce.v+
                            P2*P2*recruits.coef$bf.v+
                            P2*P2*recruits.coef$bs.v+
                            P2*P1*recruits.coef$be.v+
                            P2*P2*recruits.coef$fs.v+
                            P2*P1*recruits.coef$fe.v+
                            P2*P1*recruits.coef$se.v))
)

predict.full <- rbind(
  predict.full %>% filter(Group == "Croppers") %>%
    mutate(weedy = exp(P1*weedy.coef$c.v+
                         P2*weedy.coef$b.v+
                         P2*weedy.coef$f.v+
                         P2*weedy.coef$s.v+
                         P2*weedy.coef$e.v+
                         depth1*weedy.coef$d.v+
                         b.herbivore1*weedy.coef$h.v+
                         year1*weedy.coef$y.v+
                         diadema1*weedy.coef$dd.v+
                         P1*P2*weedy.coef$cb.v+
                         P1*P2*weedy.coef$cf.v+
                         P1*P2*weedy.coef$cs.v+
                         P1*P2*weedy.coef$ce.v+
                         P2*P2*weedy.coef$bf.v+
                         P2*P2*weedy.coef$bs.v+
                         P2*P2*weedy.coef$be.v+
                         P2*P2*weedy.coef$fs.v+
                         P2*P2*weedy.coef$fe.v+
                         P2*P2*weedy.coef$se.v)),
  predict.full %>% filter(Group == "Browsers") %>%
    mutate(weedy = exp(P2*weedy.coef$c.v+
                         P1*weedy.coef$b.v+
                         P2*weedy.coef$f.v+
                         P2*weedy.coef$s.v+
                         P2*weedy.coef$e.v+
                         depth1*weedy.coef$d.v+
                         b.herbivore1*weedy.coef$h.v+
                         year1*weedy.coef$y.v+
                         diadema1*weedy.coef$dd.v+
                         P2*P1*weedy.coef$cb.v+
                         P2*P2*weedy.coef$cf.v+
                         P2*P2*weedy.coef$cs.v+
                         P2*P2*weedy.coef$ce.v+
                         P1*P2*weedy.coef$bf.v+
                         P1*P2*weedy.coef$bs.v+
                         P1*P2*weedy.coef$be.v+
                         P2*P2*weedy.coef$fs.v+
                         P2*P2*weedy.coef$fe.v+
                         P2*P2*weedy.coef$se.v)),
  predict.full %>% filter(Group == "Farmers") %>%
    mutate(weedy = exp(P2*weedy.coef$c.v+
                         P2*weedy.coef$b.v+
                         P1*weedy.coef$f.v+
                         P2*weedy.coef$s.v+
                         P2*weedy.coef$e.v+
                         depth1*weedy.coef$d.v+
                         b.herbivore1*weedy.coef$h.v+
                         year1*weedy.coef$y.v+
                         diadema1*weedy.coef$dd.v+
                         P2*P2*weedy.coef$cb.v+
                         P2*P1*weedy.coef$cf.v+
                         P2*P2*weedy.coef$cs.v+
                         P2*P2*weedy.coef$ce.v+
                         P2*P1*weedy.coef$bf.v+
                         P2*P2*weedy.coef$bs.v+
                         P2*P2*weedy.coef$be.v+
                         P1*P2*weedy.coef$fs.v+
                         P1*P2*weedy.coef$fe.v+
                         P2*P2*weedy.coef$se.v)),
  predict.full %>% filter(Group == "Scrapers") %>%
    mutate(weedy = exp(P2*weedy.coef$c.v+
                         P2*weedy.coef$b.v+
                         P2*weedy.coef$f.v+
                         P1*weedy.coef$s.v+
                         P2*weedy.coef$e.v+
                         depth1*weedy.coef$d.v+
                         b.herbivore1*weedy.coef$h.v+
                         year1*weedy.coef$y.v+
                         diadema1*weedy.coef$dd.v+
                         P2*P2*weedy.coef$cb.v+
                         P2*P2*weedy.coef$cf.v+
                         P2*P1*weedy.coef$cs.v+
                         P2*P2*weedy.coef$ce.v+
                         P2*P2*weedy.coef$bf.v+
                         P2*P1*weedy.coef$bs.v+
                         P2*P2*weedy.coef$be.v+
                         P2*P1*weedy.coef$fs.v+
                         P2*P2*weedy.coef$fe.v+
                         P1*P2*weedy.coef$se.v)),
  predict.full %>% filter(Group == "Excavators") %>%
    mutate(weedy = exp(P2*weedy.coef$c.v+
                         P2*weedy.coef$b.v+
                         P2*weedy.coef$f.v+
                         P2*weedy.coef$s.v+
                         P1*weedy.coef$e.v+
                         depth1*weedy.coef$d.v+
                         b.herbivore1*weedy.coef$h.v+
                         year1*weedy.coef$y.v+
                         diadema1*weedy.coef$dd.v+
                         P2*P2*weedy.coef$cb.v+
                         P2*P2*weedy.coef$cf.v+
                         P2*P2*weedy.coef$cs.v+
                         P2*P1*weedy.coef$ce.v+
                         P2*P2*weedy.coef$bf.v+
                         P2*P2*weedy.coef$bs.v+
                         P2*P1*weedy.coef$be.v+
                         P2*P2*weedy.coef$fs.v+
                         P2*P1*weedy.coef$fe.v+
                         P2*P1*weedy.coef$se.v))
)

predict.full <- rbind(
  predict.full %>% filter(Group == "Croppers") %>%
    mutate(stress = exp(P1*stress.coef$c.v+
                         P2*stress.coef$b.v+
                         P2*stress.coef$f.v+
                         P2*stress.coef$s.v+
                         P2*stress.coef$e.v+
                         depth1*stress.coef$d.v+
                         b.herbivore1*stress.coef$h.v+
                         year1*stress.coef$y.v+
                         diadema1*stress.coef$dd.v+
                         P1*P2*stress.coef$cb.v+
                         P1*P2*stress.coef$cf.v+
                         P1*P2*stress.coef$cs.v+
                         P1*P2*stress.coef$ce.v+
                         P2*P2*stress.coef$bf.v+
                         P2*P2*stress.coef$bs.v+
                         P2*P2*stress.coef$be.v+
                         P2*P2*stress.coef$fs.v+
                         P2*P2*stress.coef$fe.v+
                         P2*P2*stress.coef$se.v)),
  predict.full %>% filter(Group == "Browsers") %>%
    mutate(stress = exp(P2*stress.coef$c.v+
                         P1*stress.coef$b.v+
                         P2*stress.coef$f.v+
                         P2*stress.coef$s.v+
                         P2*stress.coef$e.v+
                         depth1*stress.coef$d.v+
                         b.herbivore1*stress.coef$h.v+
                         year1*stress.coef$y.v+
                         diadema1*stress.coef$dd.v+
                         P2*P1*stress.coef$cb.v+
                         P2*P2*stress.coef$cf.v+
                         P2*P2*stress.coef$cs.v+
                         P2*P2*stress.coef$ce.v+
                         P1*P2*stress.coef$bf.v+
                         P1*P2*stress.coef$bs.v+
                         P1*P2*stress.coef$be.v+
                         P2*P2*stress.coef$fs.v+
                         P2*P2*stress.coef$fe.v+
                         P2*P2*stress.coef$se.v)),
  predict.full %>% filter(Group == "Farmers") %>%
    mutate(stress = exp(P2*stress.coef$c.v+
                         P2*stress.coef$b.v+
                         P1*stress.coef$f.v+
                         P2*stress.coef$s.v+
                         P2*stress.coef$e.v+
                         depth1*stress.coef$d.v+
                         b.herbivore1*stress.coef$h.v+
                         year1*stress.coef$y.v+
                         diadema1*stress.coef$dd.v+
                         P2*P2*stress.coef$cb.v+
                         P2*P1*stress.coef$cf.v+
                         P2*P2*stress.coef$cs.v+
                         P2*P2*stress.coef$ce.v+
                         P2*P1*stress.coef$bf.v+
                         P2*P2*stress.coef$bs.v+
                         P2*P2*stress.coef$be.v+
                         P1*P2*stress.coef$fs.v+
                         P1*P2*stress.coef$fe.v+
                         P2*P2*stress.coef$se.v)),
  predict.full %>% filter(Group == "Scrapers") %>%
    mutate(stress = exp(P2*stress.coef$c.v+
                         P2*stress.coef$b.v+
                         P2*stress.coef$f.v+
                         P1*stress.coef$s.v+
                         P2*stress.coef$e.v+
                         depth1*stress.coef$d.v+
                         b.herbivore1*stress.coef$h.v+
                         year1*stress.coef$y.v+
                         diadema1*stress.coef$dd.v+
                         P2*P2*stress.coef$cb.v+
                         P2*P2*stress.coef$cf.v+
                         P2*P1*stress.coef$cs.v+
                         P2*P2*stress.coef$ce.v+
                         P2*P2*stress.coef$bf.v+
                         P2*P1*stress.coef$bs.v+
                         P2*P2*stress.coef$be.v+
                         P2*P1*stress.coef$fs.v+
                         P2*P2*stress.coef$fe.v+
                         P1*P2*stress.coef$se.v)),
  predict.full %>% filter(Group == "Excavators") %>%
    mutate(stress = exp(P2*stress.coef$c.v+
                         P2*stress.coef$b.v+
                         P2*stress.coef$f.v+
                         P2*stress.coef$s.v+
                         P1*stress.coef$e.v+
                         depth1*stress.coef$d.v+
                         b.herbivore1*stress.coef$h.v+
                         year1*stress.coef$y.v+
                         diadema1*stress.coef$dd.v+
                         P2*P2*stress.coef$cb.v+
                         P2*P2*stress.coef$cf.v+
                         P2*P2*stress.coef$cs.v+
                         P2*P1*stress.coef$ce.v+
                         P2*P2*stress.coef$bf.v+
                         P2*P2*stress.coef$bs.v+
                         P2*P1*stress.coef$be.v+
                         P2*P2*stress.coef$fs.v+
                         P2*P1*stress.coef$fe.v+
                         P2*P1*stress.coef$se.v))
)

predict.full <- rbind(
  predict.full %>% filter(Group == "Croppers") %>%
    mutate(comp = exp(P1*comp.coef$c.v+
                         P2*comp.coef$b.v+
                         P2*comp.coef$f.v+
                         P2*comp.coef$s.v+
                         P2*comp.coef$e.v+
                         depth1*comp.coef$d.v+
                         b.herbivore1*comp.coef$h.v+
                         year1*comp.coef$y.v+
                         diadema1*comp.coef$dd.v+
                         P1*P2*comp.coef$cb.v+
                         P1*P2*comp.coef$cf.v+
                         P1*P2*comp.coef$cs.v+
                         P1*P2*comp.coef$ce.v+
                         P2*P2*comp.coef$bf.v+
                         P2*P2*comp.coef$bs.v+
                         P2*P2*comp.coef$be.v+
                         P2*P2*comp.coef$fs.v+
                         P2*P2*comp.coef$fe.v+
                         P2*P2*comp.coef$se.v)),
  predict.full %>% filter(Group == "Browsers") %>%
    mutate(comp = exp(P2*comp.coef$c.v+
                         P1*comp.coef$b.v+
                         P2*comp.coef$f.v+
                         P2*comp.coef$s.v+
                         P2*comp.coef$e.v+
                         depth1*comp.coef$d.v+
                         b.herbivore1*comp.coef$h.v+
                         year1*comp.coef$y.v+
                         diadema1*comp.coef$dd.v+
                         P2*P1*comp.coef$cb.v+
                         P2*P2*comp.coef$cf.v+
                         P2*P2*comp.coef$cs.v+
                         P2*P2*comp.coef$ce.v+
                         P1*P2*comp.coef$bf.v+
                         P1*P2*comp.coef$bs.v+
                         P1*P2*comp.coef$be.v+
                         P2*P2*comp.coef$fs.v+
                         P2*P2*comp.coef$fe.v+
                         P2*P2*comp.coef$se.v)),
  predict.full %>% filter(Group == "Farmers") %>%
    mutate(comp = exp(P2*comp.coef$c.v+
                         P2*comp.coef$b.v+
                         P1*comp.coef$f.v+
                         P2*comp.coef$s.v+
                         P2*comp.coef$e.v+
                         depth1*comp.coef$d.v+
                         b.herbivore1*comp.coef$h.v+
                         year1*comp.coef$y.v+
                         diadema1*comp.coef$dd.v+
                         P2*P2*comp.coef$cb.v+
                         P2*P1*comp.coef$cf.v+
                         P2*P2*comp.coef$cs.v+
                         P2*P2*comp.coef$ce.v+
                         P2*P1*comp.coef$bf.v+
                         P2*P2*comp.coef$bs.v+
                         P2*P2*comp.coef$be.v+
                         P1*P2*comp.coef$fs.v+
                         P1*P2*comp.coef$fe.v+
                         P2*P2*comp.coef$se.v)),
  predict.full %>% filter(Group == "Scrapers") %>%
    mutate(comp = exp(P2*comp.coef$c.v+
                         P2*comp.coef$b.v+
                         P2*comp.coef$f.v+
                         P1*comp.coef$s.v+
                         P2*comp.coef$e.v+
                         depth1*comp.coef$d.v+
                         b.herbivore1*comp.coef$h.v+
                         year1*comp.coef$y.v+
                         diadema1*comp.coef$dd.v+
                         P2*P2*comp.coef$cb.v+
                         P2*P2*comp.coef$cf.v+
                         P2*P1*comp.coef$cs.v+
                         P2*P2*comp.coef$ce.v+
                         P2*P2*comp.coef$bf.v+
                         P2*P1*comp.coef$bs.v+
                         P2*P2*comp.coef$be.v+
                         P2*P1*comp.coef$fs.v+
                         P2*P2*comp.coef$fe.v+
                         P1*P2*comp.coef$se.v)),
  predict.full %>% filter(Group == "Excavators") %>%
    mutate(comp = exp(P2*comp.coef$c.v+
                         P2*comp.coef$b.v+
                         P2*comp.coef$f.v+
                         P2*comp.coef$s.v+
                         P1*comp.coef$e.v+
                         depth1*comp.coef$d.v+
                         b.herbivore1*comp.coef$h.v+
                         year1*comp.coef$y.v+
                         diadema1*comp.coef$dd.v+
                         P2*P2*comp.coef$cb.v+
                         P2*P2*comp.coef$cf.v+
                         P2*P2*comp.coef$cs.v+
                         P2*P1*comp.coef$ce.v+
                         P2*P2*comp.coef$bf.v+
                         P2*P2*comp.coef$bs.v+
                         P2*P1*comp.coef$be.v+
                         P2*P2*comp.coef$fs.v+
                         P2*P1*comp.coef$fe.v+
                         P2*P1*comp.coef$se.v))
)

predict.full <- rbind(
  predict.full %>% filter(Group == "Croppers") %>%
    mutate(gen = exp(P1*gen.coef$c.v+
                         P2*gen.coef$b.v+
                         P2*gen.coef$f.v+
                         P2*gen.coef$s.v+
                         P2*gen.coef$e.v+
                         depth1*gen.coef$d.v+
                         b.herbivore1*gen.coef$h.v+
                         year1*gen.coef$y.v+
                         diadema1*gen.coef$dd.v+
                         P1*P2*gen.coef$cb.v+
                         P1*P2*gen.coef$cf.v+
                         P1*P2*gen.coef$cs.v+
                         P1*P2*gen.coef$ce.v+
                         P2*P2*gen.coef$bf.v+
                         P2*P2*gen.coef$bs.v+
                         P2*P2*gen.coef$be.v+
                         P2*P2*gen.coef$fs.v+
                         P2*P2*gen.coef$fe.v+
                         P2*P2*gen.coef$se.v)),
  predict.full %>% filter(Group == "Browsers") %>%
    mutate(gen = exp(P2*gen.coef$c.v+
                         P1*gen.coef$b.v+
                         P2*gen.coef$f.v+
                         P2*gen.coef$s.v+
                         P2*gen.coef$e.v+
                         depth1*gen.coef$d.v+
                         b.herbivore1*gen.coef$h.v+
                         year1*gen.coef$y.v+
                         diadema1*gen.coef$dd.v+
                         P2*P1*gen.coef$cb.v+
                         P2*P2*gen.coef$cf.v+
                         P2*P2*gen.coef$cs.v+
                         P2*P2*gen.coef$ce.v+
                         P1*P2*gen.coef$bf.v+
                         P1*P2*gen.coef$bs.v+
                         P1*P2*gen.coef$be.v+
                         P2*P2*gen.coef$fs.v+
                         P2*P2*gen.coef$fe.v+
                         P2*P2*gen.coef$se.v)),
  predict.full %>% filter(Group == "Farmers") %>%
    mutate(gen = exp(P2*gen.coef$c.v+
                         P2*gen.coef$b.v+
                         P1*gen.coef$f.v+
                         P2*gen.coef$s.v+
                         P2*gen.coef$e.v+
                         depth1*gen.coef$d.v+
                         b.herbivore1*gen.coef$h.v+
                         year1*gen.coef$y.v+
                         diadema1*gen.coef$dd.v+
                         P2*P2*gen.coef$cb.v+
                         P2*P1*gen.coef$cf.v+
                         P2*P2*gen.coef$cs.v+
                         P2*P2*gen.coef$ce.v+
                         P2*P1*gen.coef$bf.v+
                         P2*P2*gen.coef$bs.v+
                         P2*P2*gen.coef$be.v+
                         P1*P2*gen.coef$fs.v+
                         P1*P2*gen.coef$fe.v+
                         P2*P2*gen.coef$se.v)),
  predict.full %>% filter(Group == "Scrapers") %>%
    mutate(gen = exp(P2*gen.coef$c.v+
                         P2*gen.coef$b.v+
                         P2*gen.coef$f.v+
                         P1*gen.coef$s.v+
                         P2*gen.coef$e.v+
                         depth1*gen.coef$d.v+
                         b.herbivore1*gen.coef$h.v+
                         year1*gen.coef$y.v+
                         diadema1*gen.coef$dd.v+
                         P2*P2*gen.coef$cb.v+
                         P2*P2*gen.coef$cf.v+
                         P2*P1*gen.coef$cs.v+
                         P2*P2*gen.coef$ce.v+
                         P2*P2*gen.coef$bf.v+
                         P2*P1*gen.coef$bs.v+
                         P2*P2*gen.coef$be.v+
                         P2*P1*gen.coef$fs.v+
                         P2*P2*gen.coef$fe.v+
                         P1*P2*gen.coef$se.v)),
  predict.full %>% filter(Group == "Excavators") %>%
    mutate(gen = exp(P2*gen.coef$c.v+
                         P2*gen.coef$b.v+
                         P2*gen.coef$f.v+
                         P2*gen.coef$s.v+
                         P1*gen.coef$e.v+
                         depth1*gen.coef$d.v+
                         b.herbivore1*gen.coef$h.v+
                         year1*gen.coef$y.v+
                         diadema1*gen.coef$dd.v+
                         P2*P2*gen.coef$cb.v+
                         P2*P2*gen.coef$cf.v+
                         P2*P2*gen.coef$cs.v+
                         P2*P1*gen.coef$ce.v+
                         P2*P2*gen.coef$bf.v+
                         P2*P2*gen.coef$bs.v+
                         P2*P1*gen.coef$be.v+
                         P2*P2*gen.coef$fs.v+
                         P2*P1*gen.coef$fe.v+
                         P2*P1*gen.coef$se.v))
)

predict.full <- rbind(
  predict.full %>% filter(Group == "Croppers") %>%
    mutate(fleshy = inv.logit(P1*fleshy.coef$c.v+
                       P2*fleshy.coef$b.v+
                       P2*fleshy.coef$f.v+
                       P2*fleshy.coef$s.v+
                       P2*fleshy.coef$e.v+
                       depth3*fleshy.coef$d.v+
                       b.herbivore3*fleshy.coef$h.v+
                       year3*fleshy.coef$y.v+
                       diadema3*fleshy.coef$dd.v+
                       P1*P2*fleshy.coef$cb.v+
                       P1*P2*fleshy.coef$cf.v+
                       P1*P2*fleshy.coef$cs.v+
                       P1*P2*fleshy.coef$ce.v+
                       P2*P2*fleshy.coef$bf.v+
                       P2*P2*fleshy.coef$bs.v+
                       P2*P2*fleshy.coef$be.v+
                       P2*P2*fleshy.coef$fs.v+
                       P2*P2*fleshy.coef$fe.v+
                       P2*P2*fleshy.coef$se.v)),
  predict.full %>% filter(Group == "Browsers") %>%
    mutate(fleshy = inv.logit(P2*fleshy.coef$c.v+
                       P1*fleshy.coef$b.v+
                       P2*fleshy.coef$f.v+
                       P2*fleshy.coef$s.v+
                       P2*fleshy.coef$e.v+
                       depth3*fleshy.coef$d.v+
                       b.herbivore3*fleshy.coef$h.v+
                       year3*fleshy.coef$y.v+
                       diadema3*fleshy.coef$dd.v+
                       P2*P1*fleshy.coef$cb.v+
                       P2*P2*fleshy.coef$cf.v+
                       P2*P2*fleshy.coef$cs.v+
                       P2*P2*fleshy.coef$ce.v+
                       P1*P2*fleshy.coef$bf.v+
                       P1*P2*fleshy.coef$bs.v+
                       P1*P2*fleshy.coef$be.v+
                       P2*P2*fleshy.coef$fs.v+
                       P2*P2*fleshy.coef$fe.v+
                       P2*P2*fleshy.coef$se.v)),
  predict.full %>% filter(Group == "Farmers") %>%
    mutate(fleshy = inv.logit(P2*fleshy.coef$c.v+
                       P2*fleshy.coef$b.v+
                       P1*fleshy.coef$f.v+
                       P2*fleshy.coef$s.v+
                       P2*fleshy.coef$e.v+
                       depth3*fleshy.coef$d.v+
                       b.herbivore3*fleshy.coef$h.v+
                       year3*fleshy.coef$y.v+
                       diadema3*fleshy.coef$dd.v+
                       P2*P2*fleshy.coef$cb.v+
                       P2*P1*fleshy.coef$cf.v+
                       P2*P2*fleshy.coef$cs.v+
                       P2*P2*fleshy.coef$ce.v+
                       P2*P1*fleshy.coef$bf.v+
                       P2*P2*fleshy.coef$bs.v+
                       P2*P2*fleshy.coef$be.v+
                       P1*P2*fleshy.coef$fs.v+
                       P1*P2*fleshy.coef$fe.v+
                       P2*P2*fleshy.coef$se.v)),
  predict.full %>% filter(Group == "Scrapers") %>%
    mutate(fleshy = inv.logit(P2*fleshy.coef$c.v+
                       P2*fleshy.coef$b.v+
                       P2*fleshy.coef$f.v+
                       P1*fleshy.coef$s.v+
                       P2*fleshy.coef$e.v+
                       depth3*fleshy.coef$d.v+
                       b.herbivore3*fleshy.coef$h.v+
                       year3*fleshy.coef$y.v+
                       diadema3*fleshy.coef$dd.v+
                       P2*P2*fleshy.coef$cb.v+
                       P2*P2*fleshy.coef$cf.v+
                       P2*P1*fleshy.coef$cs.v+
                       P2*P2*fleshy.coef$ce.v+
                       P2*P2*fleshy.coef$bf.v+
                       P2*P1*fleshy.coef$bs.v+
                       P2*P2*fleshy.coef$be.v+
                       P2*P1*fleshy.coef$fs.v+
                       P2*P2*fleshy.coef$fe.v+
                       P1*P2*fleshy.coef$se.v)),
  predict.full %>% filter(Group == "Excavators") %>%
    mutate(fleshy = inv.logit(P2*fleshy.coef$c.v+
                       P2*fleshy.coef$b.v+
                       P2*fleshy.coef$f.v+
                       P2*fleshy.coef$s.v+
                       P1*fleshy.coef$e.v+
                       depth3*fleshy.coef$d.v+
                       b.herbivore3*fleshy.coef$h.v+
                       year3*fleshy.coef$y.v+
                       diadema3*fleshy.coef$dd.v+
                       P2*P2*fleshy.coef$cb.v+
                       P2*P2*fleshy.coef$cf.v+
                       P2*P2*fleshy.coef$cs.v+
                       P2*P1*fleshy.coef$ce.v+
                       P2*P2*fleshy.coef$bf.v+
                       P2*P2*fleshy.coef$bs.v+
                       P2*P1*fleshy.coef$be.v+
                       P2*P2*fleshy.coef$fs.v+
                       P2*P1*fleshy.coef$fe.v+
                       P2*P1*fleshy.coef$se.v))
)

predict.full <- rbind(
  predict.full %>% filter(Group == "Croppers") %>%
    mutate(calc = inv.logit(P1*calc.coef$c.v+
                          P2*calc.coef$b.v+
                          P2*calc.coef$f.v+
                          P2*calc.coef$s.v+
                          P2*calc.coef$e.v+
                          depth3*calc.coef$d.v+
                          b.herbivore3*calc.coef$h.v+
                          year3*calc.coef$y.v+
                          diadema3*calc.coef$dd.v+
                          P1*P2*calc.coef$cb.v+
                          P1*P2*calc.coef$cf.v+
                          P1*P2*calc.coef$cs.v+
                          P1*P2*calc.coef$ce.v+
                          P2*P2*calc.coef$bf.v+
                          P2*P2*calc.coef$bs.v+
                          P2*P2*calc.coef$be.v+
                          P2*P2*calc.coef$fs.v+
                          P2*P2*calc.coef$fe.v+
                          P2*P2*calc.coef$se.v)),
  predict.full %>% filter(Group == "Browsers") %>%
    mutate(calc = inv.logit(P2*calc.coef$c.v+
                          P1*calc.coef$b.v+
                          P2*calc.coef$f.v+
                          P2*calc.coef$s.v+
                          P2*calc.coef$e.v+
                          depth3*calc.coef$d.v+
                          b.herbivore3*calc.coef$h.v+
                          year3*calc.coef$y.v+
                          diadema3*calc.coef$dd.v+
                          P2*P1*calc.coef$cb.v+
                          P2*P2*calc.coef$cf.v+
                          P2*P2*calc.coef$cs.v+
                          P2*P2*calc.coef$ce.v+
                          P1*P2*calc.coef$bf.v+
                          P1*P2*calc.coef$bs.v+
                          P1*P2*calc.coef$be.v+
                          P2*P2*calc.coef$fs.v+
                          P2*P2*calc.coef$fe.v+
                          P2*P2*calc.coef$se.v)),
  predict.full %>% filter(Group == "Farmers") %>%
    mutate(calc = inv.logit(P2*calc.coef$c.v+
                          P2*calc.coef$b.v+
                          P1*calc.coef$f.v+
                          P2*calc.coef$s.v+
                          P2*calc.coef$e.v+
                          depth3*calc.coef$d.v+
                          b.herbivore3*calc.coef$h.v+
                          year3*calc.coef$y.v+
                          diadema3*calc.coef$dd.v+
                          P2*P2*calc.coef$cb.v+
                          P2*P1*calc.coef$cf.v+
                          P2*P2*calc.coef$cs.v+
                          P2*P2*calc.coef$ce.v+
                          P2*P1*calc.coef$bf.v+
                          P2*P2*calc.coef$bs.v+
                          P2*P2*calc.coef$be.v+
                          P1*P2*calc.coef$fs.v+
                          P1*P2*calc.coef$fe.v+
                          P2*P2*calc.coef$se.v)),
  predict.full %>% filter(Group == "Scrapers") %>%
    mutate(calc = inv.logit(P2*calc.coef$c.v+
                          P2*calc.coef$b.v+
                          P2*calc.coef$f.v+
                          P1*calc.coef$s.v+
                          P2*calc.coef$e.v+
                          depth3*calc.coef$d.v+
                          b.herbivore3*calc.coef$h.v+
                          year3*calc.coef$y.v+
                          diadema3*calc.coef$dd.v+
                          P2*P2*calc.coef$cb.v+
                          P2*P2*calc.coef$cf.v+
                          P2*P1*calc.coef$cs.v+
                          P2*P2*calc.coef$ce.v+
                          P2*P2*calc.coef$bf.v+
                          P2*P1*calc.coef$bs.v+
                          P2*P2*calc.coef$be.v+
                          P2*P1*calc.coef$fs.v+
                          P2*P2*calc.coef$fe.v+
                          P1*P2*calc.coef$se.v)),
  predict.full %>% filter(Group == "Excavators") %>%
    mutate(calc = inv.logit(P2*calc.coef$c.v+
                          P2*calc.coef$b.v+
                          P2*calc.coef$f.v+
                          P2*calc.coef$s.v+
                          P1*calc.coef$e.v+
                          depth3*calc.coef$d.v+
                          b.herbivore3*calc.coef$h.v+
                          year3*calc.coef$y.v+
                          diadema3*calc.coef$dd.v+
                          P2*P2*calc.coef$cb.v+
                          P2*P2*calc.coef$cf.v+
                          P2*P2*calc.coef$cs.v+
                          P2*P1*calc.coef$ce.v+
                          P2*P2*calc.coef$bf.v+
                          P2*P2*calc.coef$bs.v+
                          P2*P1*calc.coef$be.v+
                          P2*P2*calc.coef$fs.v+
                          P2*P1*calc.coef$fe.v+
                          P2*P1*calc.coef$se.v))
)

predict.full <- rbind(
  predict.full %>% filter(Group == "Croppers") %>%
    mutate(turf = inv.logit(P1*turf.coef$c.v+
                          P2*turf.coef$b.v+
                          P2*turf.coef$f.v+
                          P2*turf.coef$s.v+
                          P2*turf.coef$e.v+
                          depth3*turf.coef$d.v+
                          b.herbivore3*turf.coef$h.v+
                          year3*turf.coef$y.v+
                          diadema3*turf.coef$dd.v+
                          P1*P2*turf.coef$cb.v+
                          P1*P2*turf.coef$cf.v+
                          P1*P2*turf.coef$cs.v+
                          P1*P2*turf.coef$ce.v+
                          P2*P2*turf.coef$bf.v+
                          P2*P2*turf.coef$bs.v+
                          P2*P2*turf.coef$be.v+
                          P2*P2*turf.coef$fs.v+
                          P2*P2*turf.coef$fe.v+
                          P2*P2*turf.coef$se.v)),
  predict.full %>% filter(Group == "Browsers") %>%
    mutate(turf = inv.logit(P2*turf.coef$c.v+
                          P1*turf.coef$b.v+
                          P2*turf.coef$f.v+
                          P2*turf.coef$s.v+
                          P2*turf.coef$e.v+
                          depth3*turf.coef$d.v+
                          b.herbivore3*turf.coef$h.v+
                          year3*turf.coef$y.v+
                          diadema3*turf.coef$dd.v+
                          P2*P1*turf.coef$cb.v+
                          P2*P2*turf.coef$cf.v+
                          P2*P2*turf.coef$cs.v+
                          P2*P2*turf.coef$ce.v+
                          P1*P2*turf.coef$bf.v+
                          P1*P2*turf.coef$bs.v+
                          P1*P2*turf.coef$be.v+
                          P2*P2*turf.coef$fs.v+
                          P2*P2*turf.coef$fe.v+
                          P2*P2*turf.coef$se.v)),
  predict.full %>% filter(Group == "Farmers") %>%
    mutate(turf = inv.logit(P2*turf.coef$c.v+
                          P2*turf.coef$b.v+
                          P1*turf.coef$f.v+
                          P2*turf.coef$s.v+
                          P2*turf.coef$e.v+
                          depth3*turf.coef$d.v+
                          b.herbivore3*turf.coef$h.v+
                          year3*turf.coef$y.v+
                          diadema3*turf.coef$dd.v+
                          P2*P2*turf.coef$cb.v+
                          P2*P1*turf.coef$cf.v+
                          P2*P2*turf.coef$cs.v+
                          P2*P2*turf.coef$ce.v+
                          P2*P1*turf.coef$bf.v+
                          P2*P2*turf.coef$bs.v+
                          P2*P2*turf.coef$be.v+
                          P1*P2*turf.coef$fs.v+
                          P1*P2*turf.coef$fe.v+
                          P2*P2*turf.coef$se.v)),
  predict.full %>% filter(Group == "Scrapers") %>%
    mutate(turf = inv.logit(P2*turf.coef$c.v+
                          P2*turf.coef$b.v+
                          P2*turf.coef$f.v+
                          P1*turf.coef$s.v+
                          P2*turf.coef$e.v+
                          depth3*turf.coef$d.v+
                          b.herbivore3*turf.coef$h.v+
                          year3*turf.coef$y.v+
                          diadema3*turf.coef$dd.v+
                          P2*P2*turf.coef$cb.v+
                          P2*P2*turf.coef$cf.v+
                          P2*P1*turf.coef$cs.v+
                          P2*P2*turf.coef$ce.v+
                          P2*P2*turf.coef$bf.v+
                          P2*P1*turf.coef$bs.v+
                          P2*P2*turf.coef$be.v+
                          P2*P1*turf.coef$fs.v+
                          P2*P2*turf.coef$fe.v+
                          P1*P2*turf.coef$se.v)),
  predict.full %>% filter(Group == "Excavators") %>%
    mutate(turf = inv.logit(P2*turf.coef$c.v+
                          P2*turf.coef$b.v+
                          P2*turf.coef$f.v+
                          P2*turf.coef$s.v+
                          P1*turf.coef$e.v+
                          depth3*turf.coef$d.v+
                          b.herbivore3*turf.coef$h.v+
                          year3*turf.coef$y.v+
                          diadema3*turf.coef$dd.v+
                          P2*P2*turf.coef$cb.v+
                          P2*P2*turf.coef$cf.v+
                          P2*P2*turf.coef$cs.v+
                          P2*P1*turf.coef$ce.v+
                          P2*P2*turf.coef$bf.v+
                          P2*P2*turf.coef$bs.v+
                          P2*P1*turf.coef$be.v+
                          P2*P2*turf.coef$fs.v+
                          P2*P1*turf.coef$fe.v+
                          P2*P1*turf.coef$se.v))
)

predict.full <- rbind(
  predict.full %>% filter(Group == "Croppers") %>%
    mutate(cca = inv.logit(P1*cca.coef$c.v+
                          P2*cca.coef$b.v+
                          P2*cca.coef$f.v+
                          P2*cca.coef$s.v+
                          P2*cca.coef$e.v+
                          depth3*cca.coef$d.v+
                          b.herbivore3*cca.coef$h.v+
                          year3*cca.coef$y.v+
                          diadema3*cca.coef$dd.v+
                          P1*P2*cca.coef$cb.v+
                          P1*P2*cca.coef$cf.v+
                          P1*P2*cca.coef$cs.v+
                          P1*P2*cca.coef$ce.v+
                          P2*P2*cca.coef$bf.v+
                          P2*P2*cca.coef$bs.v+
                          P2*P2*cca.coef$be.v+
                          P2*P2*cca.coef$fs.v+
                          P2*P2*cca.coef$fe.v+
                          P2*P2*cca.coef$se.v)),
  predict.full %>% filter(Group == "Browsers") %>%
    mutate(cca = inv.logit(P2*cca.coef$c.v+
                          P1*cca.coef$b.v+
                          P2*cca.coef$f.v+
                          P2*cca.coef$s.v+
                          P2*cca.coef$e.v+
                          depth3*cca.coef$d.v+
                          b.herbivore3*cca.coef$h.v+
                          year3*cca.coef$y.v+
                          diadema3*cca.coef$dd.v+
                          P2*P1*cca.coef$cb.v+
                          P2*P2*cca.coef$cf.v+
                          P2*P2*cca.coef$cs.v+
                          P2*P2*cca.coef$ce.v+
                          P1*P2*cca.coef$bf.v+
                          P1*P2*cca.coef$bs.v+
                          P1*P2*cca.coef$be.v+
                          P2*P2*cca.coef$fs.v+
                          P2*P2*cca.coef$fe.v+
                          P2*P2*cca.coef$se.v)),
  predict.full %>% filter(Group == "Farmers") %>%
    mutate(cca = inv.logit(P2*cca.coef$c.v+
                          P2*cca.coef$b.v+
                          P1*cca.coef$f.v+
                          P2*cca.coef$s.v+
                          P2*cca.coef$e.v+
                          depth3*cca.coef$d.v+
                          b.herbivore3*cca.coef$h.v+
                          year3*cca.coef$y.v+
                          diadema3*cca.coef$dd.v+
                          P2*P2*cca.coef$cb.v+
                          P2*P1*cca.coef$cf.v+
                          P2*P2*cca.coef$cs.v+
                          P2*P2*cca.coef$ce.v+
                          P2*P1*cca.coef$bf.v+
                          P2*P2*cca.coef$bs.v+
                          P2*P2*cca.coef$be.v+
                          P1*P2*cca.coef$fs.v+
                          P1*P2*cca.coef$fe.v+
                          P2*P2*cca.coef$se.v)),
  predict.full %>% filter(Group == "Scrapers") %>%
    mutate(cca = inv.logit(P2*cca.coef$c.v+
                          P2*cca.coef$b.v+
                          P2*cca.coef$f.v+
                          P1*cca.coef$s.v+
                          P2*cca.coef$e.v+
                          depth3*cca.coef$d.v+
                          b.herbivore3*cca.coef$h.v+
                          year3*cca.coef$y.v+
                          diadema3*cca.coef$dd.v+
                          P2*P2*cca.coef$cb.v+
                          P2*P2*cca.coef$cf.v+
                          P2*P1*cca.coef$cs.v+
                          P2*P2*cca.coef$ce.v+
                          P2*P2*cca.coef$bf.v+
                          P2*P1*cca.coef$bs.v+
                          P2*P2*cca.coef$be.v+
                          P2*P1*cca.coef$fs.v+
                          P2*P2*cca.coef$fe.v+
                          P1*P2*cca.coef$se.v)),
  predict.full %>% filter(Group == "Excavators") %>%
    mutate(cca = inv.logit(P2*cca.coef$c.v+
                          P2*cca.coef$b.v+
                          P2*cca.coef$f.v+
                          P2*cca.coef$s.v+
                          P1*cca.coef$e.v+
                          depth3*cca.coef$d.v+
                          b.herbivore3*cca.coef$h.v+
                          year3*cca.coef$y.v+
                          diadema3*cca.coef$dd.v+
                          P2*P2*cca.coef$cb.v+
                          P2*P2*cca.coef$cf.v+
                          P2*P2*cca.coef$cs.v+
                          P2*P1*cca.coef$ce.v+
                          P2*P2*cca.coef$bf.v+
                          P2*P2*cca.coef$bs.v+
                          P2*P1*cca.coef$be.v+
                          P2*P2*cca.coef$fs.v+
                          P2*P1*cca.coef$fe.v+
                          P2*P1*cca.coef$se.v))
)


###OPTIONAL: save predictions###
write.csv(predict.mean, "predict.mean.csv")
write.csv(ss1predict, "ss1predict.csv")
write.csv(ss2predict, "ss2predict.csv")
write.csv(ss3predict, "ss3predict.csv")
write.csv(predict.full, "predict.full.csv")