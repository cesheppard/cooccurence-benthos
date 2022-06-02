###IMPORTANT PLEASE READ###
###CHECK-LIST###
#subset dataset to remove surveys with NA response variable, denoted by <<input response variable here>>
#insert response variable into bootstrap, denoted by <<input response variable here>>
#check correct distribution in model: poisson or binomial (binomial for algal cover)

library(tidyverse)
library(geepack)

###SET WORKING DIRECTORY###
setwd()

###READ IN DATA (if applicable)###
survey_data <- read.csv("survey_data.csv", header = T, stringsAsFactors = F)

###PREPARE DATA FOR ANALYSIS###
subsetA <- survey_data %>% filter(diadema<150) %>% #remove diadema outlier 25sd above mean
  drop_na(diadema) %>% #drop surveys with NA diadema
  drop_na(coral.richness) %>% #<<input response variable here>> drop surveys with NA in response variable
  arrange(subregionID) #arrange by subregion for geeglm corr structure (see geepack for further details)

for (i in 1:length(subsetA$cor.site)) {
  subsetA$freq[i] <- length(subsetA$siteID[subsetA$cor.site == subsetA$cor.site[i]])
} #how many times site surveyed?

uni <- which(subsetA$freq==1) #vector of row numbers for unique sites (surveyed once)
uni <- as.vector(uni)

dupes <- subsetA %>% filter(freq > 1) %>% select(c("cor.site")) %>% distinct() #vector of replicated corrected siteID (surveyed multiple)


###CREATE RESULT DATAFRAMES###

#dataframes for all model coefficients from 1000 bootstrapped iterations
coef1 <- data.frame(matrix(ncol = 20, nrow = 1000)) #coefficients from Null model (Model 1)
colnames(coef1) <- c("i.v","d.v","h.v","y.v","dd.v",
                     "i.se","d.se","h.se","y.se","dd.se",
                     "i.w","d.w","h.w","y.w","dd.w",
                     "i.p","d.p","h.p","y.p","dd.p") 

coef2 <- data.frame(matrix(ncol = 36, nrow = 1000)) #coefficients from Identity model (Model 2)
colnames(coef2) <- c("c.v","b.v","f.v","s.v","e.v","d.v","h.v","y.v","dd.v",
                     "c.se","b.se","f.se","s.se","e.se","d.se","h.se","y.se","dd.se",
                     "c.w","b.w","f.w","s.w","e.w","d.w","h.w","y.w","dd.w",
                     "c.p","b.p","f.p","s.p","e.p","d.p","h.p","y.p","dd.p")

coef3 <- data.frame(matrix(ncol = 76, nrow = 1000)) #coefficients from Pairwise Interactions model (Model 3)
colnames(coef3) <- c("c.v","b.v","f.v","s.v","e.v","d.v","h.v","y.v","dd.v","cb.v","cf.v","cs.v","ce.v","bf.v","bs.v","be.v","fs.v","fe.v","se.v",
                    "c.se","b.se","f.se","s.se","e.se","d.se","h.se","y.se","dd.se","cb.se","cf.se","cs.se","ce.se","bf.se","bs.se","be.se","fs.se","fe.se","se.se",
                    "c.w","b.w","f.w","s.w","e.w","d.w","h.w","y.w","dd.w","cb.w","cf.w","cs.w","ce.w","bf.w","bs.w","be.w","fs.w","fe.w","se.w",
                    "c.p","b.p","f.p","s.p","e.p","d.p","h.p","y.p","dd.p","cb.p","cf.p","cs.p","ce.p","bf.p","bs.p","be.p","fs.p","fe.p","se.p") 

test.all <- data.frame(matrix(ncol = 6, nrow = 1000)) #coefficients from comparison of Wald statistics tests
colnames(test.all) <- c("test1df", "test1x", "test1p",
                        "test2df", "test2x", "test2p")

alpha.all <- data.frame(matrix(ncol = 12, nrow=1000)) #spatial correlation (alpha) for all models
colnames(alpha.all) <- c("alpha1", "alpha1se", "alpha1wald", "alpha1p",
                         "alpha2", "alpha2se", "alpha2wald", "alpha2p",
                         "alpha3", "alpha3se", "alpha3wald", "alpha3p")

#dataframe for coefficient means
results1 <- data.frame(matrix(ncol = 4, nrow = 5)) #mean coefficients Model 1
colnames(results1) <- c("coef", "sd", "wald", "p")
rownames(results1) <- c("Intercept", "Depth", "Biomass", "Year", "Diadema")

results2 <- data.frame(matrix(ncol = 4, nrow = 9)) #mean coefficients Model 2
colnames(results2) <- c("coef", "sd", "wald", "p")
rownames(results2) <- c("PC", "PB", "PF", "PS", "PE", "Depth", "Biomass", "Year", "Diadema")

results3 <- data.frame(matrix(ncol = 4, nrow = 19)) #mean coefficients Model 3
colnames(results3) <- c("coef", "sd", "wald", "p")
rownames(results3) <- c("PC", "PB", "PF", "PS", "PE", "Depth", "Biomass", "Year", "Diadema", 
                       "PCB", "PCF", "PCS", "PCE", "PBF", "PBS", "PBE", "PFS", "PFE", "PSE")


###BOOTSTRAPPED DIVERSITY-INTERACTIONS MODELS###
for (i in 1:1000) {
  for (j in 1:length(dupes$cor.site)) {
    dupes$ss[j] <- sample(which(subsetA$cor.site == dupes$cor.site[j]), 1, replace = T) #list of random row numbers for each duplicated site; select random survey for each duplicate
  }
  ss <- sort(c(uni, dupes$ss), decreasing = F) #combine random row numbers from duplicated sites with unique surveys. Sort by row to keep subregions in order
  y <- subsetA$coral.richness[ss] #<<input response variable here>>
  x1 <- subsetA$Pn.croppers[ss]
  x2 <- subsetA$Pn.browsers[ss]
  x3 <- subsetA$Pn.farmers[ss]
  x4 <- subsetA$Pn.scrapers[ss]
  x5 <- subsetA$Pn.excavators[ss]
  x6 <- subsetA$depth[ss]
  x7 <- subsetA$b.herbivore[ss]
  x8 <- subsetA$year[ss]
  x9 <- subsetA$diadema[ss]
  x10 <- subsetA$subregionID[ss]
  model1 <- geeglm(y ~x6+x7+x8+x9, 
                   na.action = na.exclude, id = x10,
                   family = poisson, corstr = "exchangeable") #check distribution in model: poisson or binomial?
  model2 <- geeglm(y ~x1+x2+x3+x4+x5+x6+x7+x8+x9-1, 
                   na.action = na.exclude, id = x10,
                   family = poisson, corstr = "exchangeable") #check distribution in model: poisson or binomial?
  model3 <- geeglm(y ~(x1+x2+x3+x4+x5)^2+x6+x7+x8+x9-1, 
                  na.action = na.exclude, id = x10,
                  family = poisson, corstr = "exchangeable") #check distribution in model: poisson or binomial?
  test.all$test1df[i] <- anova(model1, model2)[1,1]
  test.all$test1x[i] <- anova(model1, model2)[1,2]
  test.all$test1p[i] <- anova(model1, model2)[1,3]
  test.all$test2df[i] <- anova(model2, model3)[1,1]
  test.all$test2x[i] <- anova(model2, model3)[1,2]
  test.all$test2p[i] <- anova(model2, model3)[1,3]
  alpha.all$alpha1[i] <- as.numeric(summary(model1)$geese$correlation) [1]
  alpha.all$alpha1se[i] <- as.numeric(summary(model1)$geese$correlation [2])
  alpha.all$alpha1wald[i] <- summary(model1)$geese$correlation [3]
  alpha.all$alpha1p[i] <- summary(model1)$geese$correlation [4]
  alpha.all$alpha2[i] <- summary(model2)$geese$correlation [1]
  alpha.all$alpha2se[i] <- summary(model2)$geese$correlation [2]
  alpha.all$alpha2wald[i] <- summary(model2)$geese$correlation [3]
  alpha.all$alpha2p[i] <- summary(model2)$geese$correlation [4]
  alpha.all$alpha3[i] <- summary(model3)$geese$correlation [1]
  alpha.all$alpha3se[i] <- summary(model3)$geese$correlation [2]
  alpha.all$alpha3wald[i] <- summary(model3)$geese$correlation [3]
  alpha.all$alpha3p[i] <- summary(model3)$geese$correlation [4]
  coef1$i.v [i] <- coef(model1) [1]
  coef1$d.v[i] <- coef(model1) [2]
  coef1$h.v[i] <- coef(model1) [3]
  coef1$y.v[i] <- coef(model1) [4]
  coef1$dd.v[i] <- coef(model1) [5]
  coef1$i.se[i] <- summary(model1)$coefficients [1,2]
  coef1$d.se[i] <- summary(model1)$coefficients [2,2]
  coef1$h.se[i] <- summary(model1)$coefficients [3,2]
  coef1$y.se[i] <- summary(model1)$coefficients [4,2]
  coef1$dd.se[i] <- summary(model1)$coefficients [5,2]
  coef1$i.w[i] <- summary(model1)$coefficients [1,3]
  coef1$d.w[i] <- summary(model1)$coefficients [2,3]
  coef1$h.w[i] <- summary(model1)$coefficients [3,3]
  coef1$y.w[i] <- summary(model1)$coefficients [4,3]
  coef1$dd.w[i] <- summary(model1)$coefficients [5,3]
  coef1$i.p[i] <- summary(model1)$coefficients [1,4]
  coef1$d.p[i] <- summary(model1)$coefficients [2,4]
  coef1$h.p[i] <- summary(model1)$coefficients [3,4]
  coef1$y.p[i] <- summary(model1)$coefficients [4,4]
  coef1$dd.p[i] <- summary(model1)$coefficients [5,4]
  results1$coef <- colMeans(coef1[ , 1:5])
  results1$sd <- colMeans(coef1[ , 6:10])
  results1$wald <- colMeans(coef1[ , 11:15])
  results1$p <- colMeans(coef1[ , 16:20])
  coef2$c.v[i] <- coef(model2) [1]
  coef2$b.v[i] <- coef(model2) [2]
  coef2$f.v[i] <- coef(model2) [3]
  coef2$s.v[i] <- coef(model2) [4]
  coef2$e.v[i] <- coef(model2) [5]
  coef2$d.v[i] <- coef(model2) [6]
  coef2$h.v[i] <- coef(model2) [7]
  coef2$y.v[i] <- coef(model2) [8]
  coef2$dd.v[i] <- coef(model2) [9]
  coef2$c.se[i] <- summary(model2)$coefficients [1,2]
  coef2$b.se[i] <- summary(model2)$coefficients [2,2]
  coef2$f.se[i] <- summary(model2)$coefficients [3,2]
  coef2$s.se[i] <- summary(model2)$coefficients [4,2]
  coef2$e.se[i] <- summary(model2)$coefficients [5,2]
  coef2$d.se[i] <- summary(model2)$coefficients [6,2]
  coef2$h.se[i] <- summary(model2)$coefficients [7,2]
  coef2$y.se[i] <- summary(model2)$coefficients [8,2]
  coef2$dd.se[i] <- summary(model2)$coefficients [9,2]
  coef2$c.w[i] <- summary(model2)$coefficients [1,3]
  coef2$b.w[i] <- summary(model2)$coefficients [2,3]
  coef2$f.w[i] <- summary(model2)$coefficients [3,3]
  coef2$s.w[i] <- summary(model2)$coefficients [4,3]
  coef2$e.w[i] <- summary(model2)$coefficients [5,3]
  coef2$d.w[i] <- summary(model2)$coefficients [6,3]
  coef2$h.w[i] <- summary(model2)$coefficients [7,3]
  coef2$y.w[i] <- summary(model2)$coefficients [8,3]
  coef2$dd.w[i] <- summary(model2)$coefficients [9,3]
  coef2$c.p[i] <- summary(model2)$coefficients [1,4]
  coef2$b.p[i] <- summary(model2)$coefficients [2,4]
  coef2$f.p[i] <- summary(model2)$coefficients [3,4]
  coef2$s.p[i] <- summary(model2)$coefficients [4,4]
  coef2$e.p[i] <- summary(model2)$coefficients [5,4]
  coef2$d.p[i] <- summary(model2)$coefficients [6,4]
  coef2$h.p[i] <- summary(model2)$coefficients [7,4]
  coef2$y.p[i] <- summary(model2)$coefficients [8,4]
  coef2$dd.p[i] <- summary(model2)$coefficients [9,4]
  results2$coef <- colMeans(coef2[ , 1:9])
  results2$sd <- colMeans(coef2[ , 10:18])
  results2$wald <- colMeans(coef2[ , 19:27])
  results2$p <- colMeans(coef2[ , 28:36])
  coef3$c.v[i] <- coef(model3) [1]
  coef3$b.v[i] <- coef(model3) [2]
  coef3$f.v[i] <- coef(model3) [3]
  coef3$s.v[i] <- coef(model3) [4]
  coef3$e.v[i] <- coef(model3) [5]
  coef3$d.v[i] <- coef(model3) [6]
  coef3$h.v[i] <- coef(model3) [7]
  coef3$y.v[i] <- coef(model3) [8]
  coef3$dd.v[i] <- coef(model3) [9]
  coef3$cb.v[i] <- coef(model3) [10]
  coef3$cf.v[i] <- coef(model3) [11]
  coef3$cs.v[i] <- coef(model3) [12]
  coef3$ce.v[i] <- coef(model3) [13]
  coef3$bf.v[i] <- coef(model3) [14]
  coef3$bs.v[i] <- coef(model3) [15]
  coef3$be.v[i] <- coef(model3) [16]
  coef3$fs.v[i] <- coef(model3) [17]
  coef3$fe.v[i] <- coef(model3) [18]
  coef3$se.v[i] <- coef(model3) [19]
  coef3$c.se[i] <- summary(model3)$coefficients [1,2]
  coef3$b.se[i] <- summary(model3)$coefficients [2,2]
  coef3$f.se[i] <- summary(model3)$coefficients [3,2]
  coef3$s.se[i] <- summary(model3)$coefficients [4,2]
  coef3$e.se[i] <- summary(model3)$coefficients [5,2]
  coef3$d.se[i] <- summary(model3)$coefficients [6,2]
  coef3$h.se[i] <- summary(model3)$coefficients [7,2]
  coef3$y.se[i] <- summary(model3)$coefficients [8,2]
  coef3$dd.se[i] <- summary(model3)$coefficients [9,2]
  coef3$cb.se[i] <- summary(model3)$coefficients [10,2]
  coef3$cf.se[i] <- summary(model3)$coefficients [11,2]
  coef3$cs.se[i] <- summary(model3)$coefficients [12,2]
  coef3$ce.se[i] <- summary(model3)$coefficients [13,2]
  coef3$bf.se[i] <- summary(model3)$coefficients [14,2]
  coef3$bs.se[i] <- summary(model3)$coefficients [15,2]
  coef3$be.se[i] <- summary(model3)$coefficients [16,2]
  coef3$fs.se[i] <- summary(model3)$coefficients [17,2]
  coef3$fe.se[i] <- summary(model3)$coefficients [18,2]
  coef3$se.se[i] <- summary(model3)$coefficients [19,2]
  coef3$c.w[i] <- summary(model3)$coefficients [1,3]
  coef3$b.w[i] <- summary(model3)$coefficients [2,3]
  coef3$f.w[i] <- summary(model3)$coefficients [3,3]
  coef3$s.w[i] <- summary(model3)$coefficients [4,3]
  coef3$e.w[i] <- summary(model3)$coefficients [5,3]
  coef3$d.w[i] <- summary(model3)$coefficients [6,3]
  coef3$h.w[i] <- summary(model3)$coefficients [7,3]
  coef3$y.w[i] <- summary(model3)$coefficients [8,3]
  coef3$dd.w[i] <- summary(model3)$coefficients [9,3]
  coef3$cb.w[i] <- summary(model3)$coefficients [10,3]
  coef3$cf.w[i] <- summary(model3)$coefficients [11,3]
  coef3$cs.w[i] <- summary(model3)$coefficients [12,3]
  coef3$ce.w[i] <- summary(model3)$coefficients [13,3]
  coef3$bf.w[i] <- summary(model3)$coefficients [14,3]
  coef3$bs.w[i] <- summary(model3)$coefficients [15,3]
  coef3$be.w[i] <- summary(model3)$coefficients [16,3]
  coef3$fs.w[i] <- summary(model3)$coefficients [17,3]
  coef3$fe.w[i] <- summary(model3)$coefficients [18,3]
  coef3$se.w[i] <- summary(model3)$coefficients [19,3]
  coef3$c.p[i] <- summary(model3)$coefficients [1,4]
  coef3$b.p[i] <- summary(model3)$coefficients [2,4]
  coef3$f.p[i] <- summary(model3)$coefficients [3,4]
  coef3$s.p[i] <- summary(model3)$coefficients [4,4]
  coef3$e.p[i] <- summary(model3)$coefficients [5,4]
  coef3$d.p[i] <- summary(model3)$coefficients [6,4]
  coef3$h.p[i] <- summary(model3)$coefficients [7,4]
  coef3$y.p[i] <- summary(model3)$coefficients [8,4]
  coef3$dd.p[i] <- summary(model3)$coefficients [9,4]
  coef3$cb.p[i] <- summary(model3)$coefficients [10,4]
  coef3$cf.p[i] <- summary(model3)$coefficients [11,4]
  coef3$cs.p[i] <- summary(model3)$coefficients [12,4]
  coef3$ce.p[i] <- summary(model3)$coefficients [13,4]
  coef3$bf.p[i] <- summary(model3)$coefficients [14,4]
  coef3$bs.p[i] <- summary(model3)$coefficients [15,4]
  coef3$be.p[i] <- summary(model3)$coefficients [16,4]
  coef3$fs.p[i] <- summary(model3)$coefficients [17,4]
  coef3$fe.p[i] <- summary(model3)$coefficients [18,4]
  coef3$se.p[i] <- summary(model3)$coefficients [19,4]
  results3$coef <- colMeans(coef3[ , 1:19])
  results3$sd <- colMeans(coef3[ , 20:38])
  results3$wald <- colMeans(coef3[ , 39:57])
  results3$p <- colMeans(coef3[ , 58:76])
}

k <- c(1:12) #specify column numbers
alpha.all[ ,k] <- apply(alpha.all[ ,k], 2,            
                    function(x) as.numeric(as.character(x))) #convert alpha to numeric

###OPTIONAL: save results###
#input response variable into file names
write.csv(alpha.all, file = "richness.alpha.csv", row.names = F) #save vectors of alpha
write.csv(test.all, file = "richness.test.csv", row.names = F) #save test results
write.csv(results1, file = "richness1.csv", row.names = T)
write.csv(coef1, file = "richness1coef.csv", row.names = F)
write.csv(results2, file = "richness2.csv", row.names = T)
write.csv(coef2, file = "richness2coef.csv", row.names = F)
write.csv(results3, file = "richness3.csv", row.names = T)
write.csv(coef3, file = "richness3coef.csv", row.names =F) #save model coefficients and results

#test 1 (model1 vs model2): Do herbivore functional groups vary in their identity effect?
length(subset(test.all$test1p, test.all$test1p<=0.05))/10 #percentage p-values under 0.05

#test 2 (model2 vs model3): Are statistical interactions between functional groups associated with coral benthic state?
length(subset(test.all$test2p, test.all$test2p<=0.05))/10 #percentage p-values under 0.05