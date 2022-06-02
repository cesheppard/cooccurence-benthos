#Data must be downloaded from AGRRA and saved as separate csv files of the types listed below
#NOTE: column headings manually changed before reading in data for consistency

#site: site data
#fish: raw fish observation data
#species_fish: cross ref data on fish species
#coral: raw coral observation data
#species_coral: cross ref data on coral species
#recruit: raw coral recruit observation data
#quadrat: quadrat data (e.g.algal cover, relief)
#point: point intercept data
#cover: cross ref data on cover type
#transect_fish: fish transect data (e.g. date, site, depth)
#transect_coral: coral transect data (e.g. date, site, coral cover)
#transect_benthic: benthic point intercept transect data (e.g. date, site)

#In addition data must be downloaded from
#x.calc_rates: mean calc rates from González-Barrios and Álvarez-Filip (2018)

#The following data sheets are created using the code below
#collated_fish: collated fish data per fish transect (e.g. fish species richness, abundance)
#collated_coral: collated coral data per coral transect (e.g. coral species richness, coral recruits)
#collated_benthic: collated benthic data per benthic transect (e.g. coral recruits)
#survey_data: all collated data per survey

library(tidyverse)

###SET WORKING DIRECTORY###
setwd()

###READ IN DATA###
site <- read.csv("Site.csv", header = T, stringsAsFactors = F)
subregion <- read.csv("Subregion.csv", header = T, stringsAsFactors = F)
country <- read.csv("Country.csv", header = T, stringsAsFactors = F)

fish <- read.csv("fish.csv", header = T, stringsAsFactors = F)
transect_fish <- read.csv("TransectFish.csv", header = T, stringsAsFactors = F)
species_fish <- read.csv("SpeciesFish.csv", header = T, stringsAsFactors = F)

transect_coral <- read.csv("TransectCoral.csv", header = T, stringsAsFactors = F)
coral <- read.csv("Coral.csv", header = T, stringsAsFactors = F)
species_coral <- read.csv("SpeciesCoral.csv", header = T, stringsAsFactors = F)
calc_rates <- read.csv("CalcRates.csv", header = T, stringsAsFactors = F)

quadrat <- read.csv("Quadrat.csv", header = T, stringsAsFactors = F)
recruit <- read.csv("Recruit.csv", header = T, stringsAsFactors = F)
point <- read.csv("Point.csv", header = T, stringsAsFactors = F)
cover <- read.csv("Cover.csv", header = T, stringsAsFactors = F)
transect_benthic <- read.csv("TransectBenthic.csv", header = T, stringsAsFactors = F)

###JOIN RAW DATA TABLES###

#raw fish data
fish <- fish %>% left_join(transect_fish, by = c("transectID"), suffix = c("",".y")) %>% filter(year >= 2007) #filter by year
fish <- fish %>% left_join(site, by = c("siteID"), suffix=c("",".y"))
fish <- fish %>% left_join(species_fish, by = c("fishID"), suffix = c("",".y"))
fish <- select(fish, -c(benthic.depth,coral.depth,fish.depth,date.y, year.y, dateN.y, CSort,SSort,TL,version)) #remove unwanted columns

#assigning herbivore functional groups

v.b <- c(77,85,86,87,88,89) #vector of browsing parrotfish species ID
v.e <- c(79,81,90) #vector of excavating parrotfish species ID
v.s <- c(82,83,84) #vector of scraping parrotfish species ID

for (i in 1:length(fish$transectID)) {
  if (fish$family[i] == 1) {
    fish$group[i] <- "Cropper"
  } else {
    if (fish$family[i] == 8) {
      fish$group[i] <- "Browser"
    } else {
      if (fish$family[i] == 15) {
        fish$group[i]<-"Farmer"
      } else {
        if (fish$fishID[i] %in% v.b) {
          fish$group[i] <- "Browser"
        } else {
          if (fish$fishID[i] %in% v.s) {
            fish$group[i] <- "Scraper"
          } else {
            if (fish$fishID[i] %in% v.e) {
              fish$group[i] <- "Excavator"
            } else {
              if (fish$fishID[i]==80) {
                fish$group[i] <- "Cropper"
              } else fish$group[i] <- "none"
            }
          }
        }
      } 
    } 
  }
} #assigns all fish observations to herbivore functional group. NOTE: Species ID 80 is only cropping parrotfish

                      
#raw coral data
coral <- coral %>% left_join(transect_coral, by = c("transectID"), suffix = c("",".y")) %>% filter(year >= 2007) #filter by year
coral <- coral %>% left_join(species_coral, by = c("speciesID"), suffix = c("",".y"))
coral$genus <- coral %>% mutate(genus = gsub(" .*$", "", coral$name)) %>% pull(genus)

#raw quadrat data
for (i in 1:length(quadrat$quadratID)) {
  if (is.na(quadrat$bentransectID[i])) {
    quadrat$siteID[i] <- transect_coral$siteID[transect_coral$transectID == quadrat$cortransectID[i]]
    } else {
    if (is.na(quadrat$cortransectID[i])) {
      quadrat$siteID[i] <- transect_benthic$siteID[transect_benthic$transectID == quadrat$bentransectID[i]]
    } else {
      quadrat$siteID[i] <- NA
  }}
} #pulls siteID for all quadrats. NOTE: AGRRA protocol changes mean quadrats are aligned with either coral or benthic transects

for (i in 1:length(quadrat$quadratID)) {
  if (is.na(quadrat$bentransectID[i])) {
    quadrat$dateN[i] <- transect_coral$dateN[transect_coral$transectID == quadrat$cortransectID[i]]
    } else {
    if (is.na(quadrat$cortransectID[i])) {
      quadrat$dateN[i] <- transect_benthic$dateN[transect_benthic$transectID == quadrat$bentransectID[i]]
    } else {
    quadrat$dateN[i] <- NA
  }}
} #pulls date for all quadrats based on coral/benthic transect


#raw recruit data (measured by quadrats)
recruit <- recruit %>% group_by(quadratID) %>% summarise(count=sum(count)) #total coral recruits per quadrat
recruit <- recruit %>% left_join(quadrat %>% select(c(quadratID, cortransectID, bentransectID)), by = c("quadratID"), suffix = c("",".y"))

for (i in 1:length(recruit$quadratID)) {
  if (is.na(recruit$bentransectID[i])) {
    recruit$siteID[i] <- transect_coral$siteID[transect_coral$transectID == recruit$cortransectID[i]]
  } else {
    if (is.na(recruit$cortransectID[i])) {
    recruit$siteID[i] <- transect_benthic$siteID[transect_benthic$transectID == recruit$bentransectID[i]]
  } else {
    recruit$siteID[i] <- NA
  }}
} #pulls siteID for recruit data based on coral/benthic transect

for (i in 1:length(recruit$quadratID)) {
  if (is.na(recruit$bentransectID[i])) {
    recruit$dateN[i] <- transect_coral$dateN[transect_coral$transectID == recruit$cortransectID[i]]
  } else {
    if (is.na(recruit$cortransectID[i])) {
    recruit$dateN[i] <- transect_benthic$dateN[transect_benthic$transectID == recruit$bentransectID[i]]
  } else {
    recruit$dateN[i] <- NA
  }}
} #pulls date for recruit data based on coral/benthic transect

recruit <- recruit %>% filter(dateN >= 39083) #filter to 01/01/2007


#raw point intercept data
point <- point %>% mutate(code = str_remove(code, "P-")) %>% #change pale coral codes to live coral
  left_join(cover, by = c("code"), suffix = c("",".y")) %>% 
  distinct()

point <- point %>% left_join(transect_benthic, by = c("transectID"))


#raw site/region/country data#
site <- site %>% left_join(subregion, by = c("subregionID"), suffix = c("",".y")) %>% 
  left_join(country, by = c("countryID"), suffix = c("",".y"))


###COLLATING FISH DATA PER TRANSECT###

#functional group abundances and biomass
count_fg <- fish %>% group_by(group, transectID) %>% summarise(sum = sum(count))
biomass_fg <- fish %>% group_by(group, transectID) %>% summarise(sum = sum(biomass))

#unique fish transects
collated_fish <- fish %>% select(c(transectID, siteID, date, dateN, year, month)) %>% distinct()

#functional group abundances
for (i in 1:length(collated_fish$transectID)) {
  if (collated_fish$transectID[i] %in% count_fg$transectID[count_fg$group == "Cropper"]) {
    collated_fish$n.croppers[i] <- count_fg$sum[count_fg$transectID == collated_fish$transectID[i] 
                                               & count_fg$group == "Cropper"]
  } else {
    collated_fish$n.croppers[i] <- 0
  }
}

for (i in 1:length(collated_fish$transectID)) {
  if (collated_fish$transectID[i] %in% count_fg$transectID[count_fg$group == "Browser"]) {
    collated_fish$n.browsers[i] <- count_fg$sum[count_fg$transectID == collated_fish$transectID[i] 
                                                & count_fg$group == "Browser"]
  } else {
    collated_fish$n.browsers[i] <- 0
  }
}

for (i in 1:length(collated_fish$transectID)) {
  if (collated_fish$transectID[i] %in% count_fg$transectID[count_fg$group == "Farmer"]) {
    collated_fish$n.farmers[i] <- count_fg$sum[count_fg$transectID == collated_fish$transectID[i] 
                                               & count_fg$group == "Farmer"]
  } else {
    collated_fish$n.farmers[i] <- 0
  }
}

for (i in 1:length(collated_fish$transectID)) {
  if (collated_fish$transectID[i] %in% count_fg$transectID[count_fg$group == "Scraper"]) {
    collated_fish$n.scrapers[i] <- count_fg$sum[count_fg$transectID == collated_fish$transectID[i] 
                                                & count_fg$group == "Scraper"]
  } else {
    collated_fish$n.scrapers[i] <- 0
  }
}

for (i in 1:length(collated_fish$transectID)) {
  if (collated_fish$transectID[i] %in% count_fg$transectID[count_fg$group == "Excavator"]) {
    collated_fish$n.excavators[i] <- count_fg$sum[count_fg$transectID == collated_fish$transectID[i] 
                                                & count_fg$group == "Excavator"]
  } else {
    collated_fish$n.excavators[i] <- 0
  }
}


#total herbivore abundance
collated_fish$n.herbivore <- rowSums(collated_fish[ , c("n.croppers", "n.browsers", "n.farmers", "n.scrapers", "n.excavators")])

#functional group biomass
for (i in 1:length(collated_fish$transectID)) {
  if (collated_fish$transectID[i] %in% biomass_fg$transectID[biomass_fg$group == "Cropper"]) {
    collated_fish$b.croppers[i] <- biomass_fg$sum[biomass_fg$transectID == collated_fish$transectID[i] 
                                                   & biomass_fg$group == "Cropper"]
  } else {
    collated_fish$b.croppers[i] <- 0
  }
}

for (i in 1:length(collated_fish$transectID)) {
  if (collated_fish$transectID[i] %in% biomass_fg$transectID[biomass_fg$group == "Browser"]) {
    collated_fish$b.browsers[i] <- biomass_fg$sum[biomass_fg$transectID == collated_fish$transectID[i] 
                                                    & biomass_fg$group == "Browser"]
  } else {
    collated_fish$b.browsers[i] <- 0
  }
}

for (i in 1:length(collated_fish$transectID)) {
  if (collated_fish$transectID[i] %in% biomass_fg$transectID[biomass_fg$group == "Farmer"]) {
    collated_fish$b.farmers[i] <- biomass_fg$sum[biomass_fg$transectID == collated_fish$transectID[i] 
                                                   & biomass_fg$group == "Farmer"]
  } else {
    collated_fish$b.farmers[i] <- 0
  }
}

for (i in 1:length(collated_fish$transectID)) {
  if (collated_fish$transectID[i] %in% biomass_fg$transectID[biomass_fg$group == "Scraper"]) {
    collated_fish$b.scrapers[i] <- biomass_fg$sum[biomass_fg$transectID == collated_fish$transectID[i] 
                                                    & biomass_fg$group == "Scraper"]
  } else {
    collated_fish$b.scrapers[i] <- 0
  }
}

for (i in 1:length(collated_fish$transectID)) {
  if (collated_fish$transectID[i] %in% biomass_fg$transectID[biomass_fg$group == "Excavator"]) {
    collated_fish$b.excavators[i] <- biomass_fg$sum[biomass_fg$transectID == collated_fish$transectID[i] 
                                                    & biomass_fg$group == "Excavator"]
  } else {
    collated_fish$b.excavators[i] <- 0
  }
}

#total herbivore biomass
collated_fish$b.herbivore <- rowSums(collated_fish[ , c("b.croppers", "b.browsers", "b.farmers", "b.scrapers", "b.excavators")])

#fish species richness
collated_fish$fish.richness <- fish %>% group_by(transectID) %>% summarise(n = n_distinct(fishID)) %>% pull(c(n))


###COLLATING CORAL DATA PER CORAL TRANSECT###

#unique coral transects
collated_coral <- coral %>% select(c(transectID, siteID, date, dateN, year)) %>% distinct()

#coral species richness
collated_coral$coral.richness <- coral %>% group_by(transectID) %>% summarise(n=n_distinct(speciesID)) %>% pull(c(n))

#total coral area (m2)
collated_coral$total.area <- coral %>% group_by(transectID) %>% summarise(n=sum(total.area, na.rm=T)) %>% mutate(n=n*0.0001) %>% pull(c(n))

#coral area by trait-based group (m2)
#NOTE: coral groups added to coral dataset prior to reading in based on Darling et al. (2012). See manuscript for further details.
collated_coral <- collated_coral %>% left_join((coral %>% filter(group=="weedy") %>% group_by(transectID) %>% 
                                                  summarise(weedy=sum(total.area, na.rm=T)) %>% mutate(weedy=weedy*0.0001)), 
                                               by = c("transectID"), suffix = c("",".y"))
collated_coral <- collated_coral %>% left_join((coral %>% filter(group=="competitive") %>% group_by(transectID) %>%
                                                  summarise(competitive=sum(total.area, na.rm=T)) %>% mutate(competitive=competitive*0.0001)), 
                                               by = c("transectID"), suffix = c("",".y"))
collated_coral <- collated_coral %>% left_join((coral %>% filter(group=="stress-tolerant") %>% group_by(transectID) %>%
                                                  summarise(stress.tol=sum(total.area, na.rm=T)) %>% mutate(stress.tol=stress.tol*0.0001)), 
                                               by = c("transectID"), suffix = c("",".y"))
collated_coral <- collated_coral %>% left_join((coral %>% filter(group=="generalist") %>% group_by(transectID) %>%
                                                  summarise(generalist=sum(total.area, na.rm=T)) %>% mutate(generalist=generalist*0.0001)), 
                                               by = c("transectID"), suffix = c("",".y"))

collated_coral <- collated_coral %>% mutate(weedy = ifelse(is.na(weedy) & total.area>0, 0, weedy)) %>%
  mutate(competitive = ifelse(is.na(competitive) & total.area>0, 0, competitive)) %>%
  mutate(stress.tol = ifelse(is.na(stress.tol) & total.area>0, 0, stress.tol)) %>%
  mutate(generalist = ifelse(is.na(generalist) & total.area>0, 0, generalist)) #change NA to 0

#coral calcification rate (kg CaCO3 m2 year1)
#calcification rate for each coral observation
calc_rates$genus <- calc_rates %>% mutate(genus = gsub(" .*$", "", calc_rates$species)) %>% pull(genus)

for (i in 1:length(coral$name)) {
  if (coral$name[i] %in% calc_rates$species) {
    coral$calc.rate[i] <- calc_rates$calc.rate[calc_rates$species == coral$name[i]]
  } else {
    if (coral$genus[i] %in% calc_rates$genus) {
      coral$calc.rate[i] <- mean(calc_rates$calc.rate[calc_rates$genus == coral$genus[i]])
    } else {
      coral$calc.rate[i] <- NA
    }
  }
} #mean species-specific calc rate based on González-Barrios and Álvarez-Filip (2018)

coral$total.rate <- coral %>% mutate(total.rate=calc.rate*(total.area*0.0001)) %>% pull(total.rate) #calc rate per observation

sum(coral$total.area[is.na(coral$calc.rate)], na.rm=TRUE)/sum(coral$total.area, na.rm=TRUE) #proportion of coral cover where calc rate not available

#total calcification rate per transect
collated_coral$calc.rate <- coral %>% group_by(transectID) %>% summarise(sum = sum(total.rate, na.rm = TRUE)) %>% pull(sum)


###PERCENTAGE ALGAL COVER PER TRANSECT###

#Fleshy macroalgal cover (%)
fleshy_point <- point %>% filter(grepl("Fleshy", category)) %>% distinct() #select all fleshy macroalgal point data

for (i in 1:length(transect_benthic$transectID)) {
  transect_benthic$fleshy.cover[i] <- length(unique(fleshy_point$pointID[fleshy_point$transectID == transect_benthic$transectID[i]]))
}

#Calcareous macroalgal cover (%)
calc_point <- point %>% filter(grepl("Calcareous", category)) %>% distinct() #select all calcareous macroalgal point data
  
for (i in 1:length(transect_benthic$transectID)) {
  transect_benthic$calc.cover[i] <- length(unique(calc_point$pointID[calc_point$transectID == transect_benthic$transectID[i]]))
}

#Turf algal cover (%)
turf_point <- point %>% filter(grepl("Turf", category) & !grepl("Crustose", category)) %>% distinct() #select all turf algal point data

for (i in 1:length(transect_benthic$transectID)) {
  transect_benthic$turf.cover[i] <- length(unique(turf_point$pointID[turf_point$transectID == transect_benthic$transectID[i]]))
}

#CCA cover (%)
cca_point <- point %>% filter(grepl("Crustose", category)) %>% distinct() #select all CCA point data

for (i in 1:length(transect_benthic$transectID)) {
  transect_benthic$cca.cover[i] <- length(unique(cca_point$pointID[cca_point$transectID == transect_benthic$transectID[i]]))
}

#total Diadema abundance
transect_benthic <- transect_benthic %>% mutate(dia.all = dia.juv + dia.adult)
  

###SURVEY AVERAGES###

#get unique surveys
survey_data <- bind_rows(collated_fish %>% 
            group_by(siteID) %>% 
            summarise(min.date = min(dateN), max.date = max(dateN)) %>% 
            mutate(date.dif = max.date - min.date) %>%
            filter(date.dif <= 28) %>% 
            select(c(siteID, min.date)), #site ID and date for surveys where all transects conducted within 28 days
          collated_fish %>% 
            group_by(siteID) %>% 
            summarise(min.date = min(dateN), max.date = max(dateN)) %>% 
            mutate(date.dif = max.date - min.date) %>%
            filter(date.dif > 28) %>%
            select(c(siteID, min.date)), #site ID and first date for surveys where transects conducted over >28 days
          collated_fish %>% 
            group_by(siteID) %>% 
            summarise(min.date = min(dateN), max.date = max(dateN)) %>% 
            mutate(date.dif = max.date - min.date) %>%
            filter(date.dif > 28) %>%
            mutate(min.date = max.date - 28) %>%
            select(c(siteID, min.date))
) #site ID and last date -28 days (to give earliest possible date) for surveys where transects conducted over >28 days

#middle date and year of fish surveys; dateN gives reference when selecting corresponding coral and benthic transects
for (i in 1:length(survey_data$siteID)){
  survey_data$dateN[i] <- 0.5*(max(collated_fish$dateN[collated_fish$siteID == survey_data$siteID[i] &
                                                       collated_fish$dateN <= survey_data$min.date[i] + 28]) + 
                               min(collated_fish$dateN[collated_fish$siteID == survey_data$siteID[i] &
                                                         collated_fish$dateN >= survey_data$min.date[i]]))
}

#first date of fish survey
for (i in 1:length(survey_data$siteID)){
  survey_data$first.date[i] <- min(collated_fish$dateN[collated_fish$siteID == survey_data$siteID[i] &
                                                       collated_fish$dateN >= survey_data$min.date[i]])
}

#last date of fish survey
for (i in 1:length(survey_data$siteID)){
  survey_data$last.date[i] <- max(collated_fish$dateN[collated_fish$siteID == survey_data$siteID[i] &
                                                        collated_fish$dateN <= survey_data$min.date[i] + 28])
}

#difference between first date and last date of fish survey (i.e. how many days was the fish survey conducted over?)
survey_data$date.dif <- survey_data$last.date-survey_data$first.date

for (i in 1:length(survey_data$siteID)){
  survey_data$year[i] <- collated_fish$year[collated_fish$siteID == survey_data$siteID[i] &
                                            collated_fish$dateN <= survey_data$dateN[i]+14 &
                                            collated_fish$dateN >= survey_data$dateN[i]-14]
}

#fetch site data and identify duplicate sites
survey_data <- survey_data %>% 
  left_join(site, by = c("siteID"), suffix = c("",".y")) %>% 
  select(c(siteID, min.date, dateN, first.date, last.date, date.dif, year, duplicate, depth, zone, subregionID, subregion, countryID, country, ecoregionID, lat, long)) %>%
  mutate(cor.site = ifelse(is.na(duplicate), siteID, duplicate)) %>% #corrected siteID
  mutate(x = lat, y = long)

#number of transects included in survey
for (i in 1:length(survey_data$siteID)) {
  survey_data$n.fishtransects[i] <- length(unique(collated_fish$transectID[collated_fish$siteID == survey_data$siteID[i] 
                                                                           & collated_fish$dateN <= survey_data$dateN[i] + 14
                                                                           & collated_fish$dateN >= survey_data$dateN[i] - 14]))
}

for (i in 1:length(survey_data$siteID)) {
  survey_data$n.coraltransects[i] <- length(unique(collated_coral$transectID[collated_coral$siteID == survey_data$siteID[i] 
                                                                             & collated_coral$dateN <= survey_data$dateN[i] + 14
                                                                             & collated_coral$dateN >= survey_data$dateN[i] - 14]))
}

for (i in 1:length(survey_data$siteID)) {
  survey_data$n.bentransects[i] <- length(unique(transect_benthic$transectID[transect_benthic$siteID == survey_data$siteID[i] 
                                                                             & transect_benthic$dateN <= survey_data$dateN[i] + 14
                                                                             & transect_benthic$dateN >= survey_data$dateN[i] - 14]))
}

#mean coral richness
for (i in 1:length(survey_data$siteID)) {
  survey_data$coral.richness[i] <- mean(collated_coral$coral.richness[collated_coral$siteID == survey_data$siteID[i] 
                                                   & collated_coral$dateN <= survey_data$dateN[i] + 14
                                                   & collated_coral$dateN >= survey_data$dateN[i] - 14], na.rm=T)
}

#mean total coral area (m2)
for (i in 1:length(survey_data$siteID)) {
  survey_data$coral.area[i] <- mean(collated_coral$total.area[collated_coral$siteID == survey_data$siteID[i] 
                                                                      & collated_coral$dateN <= survey_data$dateN[i] + 14
                                                                      & collated_coral$dateN >= survey_data$dateN[i] - 14], na.rm=T)
}

#mean coral area by trait-based group
for (i in 1:length(survey_data$siteID)) {
  survey_data$weedy.area[i] <- mean(collated_coral$weedy[collated_coral$siteID == survey_data$siteID[i] 
                                                                     & collated_coral$dateN <= survey_data$dateN[i] + 14
                                                                     & collated_coral$dateN >= survey_data$dateN[i] - 14], na.rm=T)
}

for (i in 1:length(survey_data$siteID)) {
  survey_data$competitive.area[i] <- mean(collated_coral$competitive[collated_coral$siteID == survey_data$siteID[i] 
                                                              & collated_coral$dateN <= survey_data$dateN[i] + 14
                                                              & collated_coral$dateN >= survey_data$dateN[i] - 14], na.rm=T)
}

for (i in 1:length(survey_data$siteID)) {
  survey_data$stress.tol.area[i] <- mean(collated_coral$stress.tol[collated_coral$siteID == survey_data$siteID[i] 
                                                              & collated_coral$dateN <= survey_data$dateN[i] + 14
                                                              & collated_coral$dateN >= survey_data$dateN[i] - 14], na.rm=T)
}

for (i in 1:length(survey_data$siteID)) {
  survey_data$generalist.area[i] <- mean(collated_coral$generalist[collated_coral$siteID == survey_data$siteID[i] 
                                                              & collated_coral$dateN <= survey_data$dateN[i] + 14
                                                              & collated_coral$dateN >= survey_data$dateN[i] - 14], na.rm=T)
}

#mean coral recruits
#sum recruits over whole survey divided by number of quadrats taken
#NOTE: quadrats 25cm2 therefore multiplied by 4 to give individuals/m^2
for (i in 1:length(survey_data$siteID)) {
  if (survey_data$siteID[i] %in% recruit$siteID) {
    survey_data$recruits[i] <- (sum(recruit$count[recruit$siteID == survey_data$siteID[i]
                                                 & recruit$dateN <= survey_data$dateN[i] + 14
                                                 & recruit$dateN >= survey_data$dateN[i] - 14], na.rm = T)/(survey_data$n.bentransects[i]*5))*4
  } else {
    survey_data$recruits[i] <- NA
  }
}

#mean calcification rate (kg CaCO3/m-2/year1)
for (i in 1:length(survey_data$siteID)) {
  survey_data$calc.rate[i] <- mean(collated_coral$calc.rate[collated_coral$siteID == survey_data$siteID[i] 
                                                                      & collated_coral$dateN <= survey_data$dateN[i] + 14
                                                                      & collated_coral$dateN >= survey_data$dateN[i] - 14], na.rm = T) /10 #per transect-per m2
}

#mean algal covers (%)
for (i in 1:length(survey_data$siteID)) {
  survey_data$fleshy.cover[i] <- mean(transect_benthic$fleshy.cover[transect_benthic$siteID == survey_data$siteID[i] 
                                                                & transect_benthic$dateN <= survey_data$dateN[i] + 14
                                                                & transect_benthic$dateN >= survey_data$dateN[i] - 14], na.rm = T)
}

for (i in 1:length(survey_data$siteID)) {
  survey_data$calc.cover[i] <- mean(transect_benthic$calc.cover[transect_benthic$siteID == survey_data$siteID[i] 
                                                                & transect_benthic$dateN <= survey_data$dateN[i] + 14
                                                                & transect_benthic$dateN >= survey_data$dateN[i] - 14], na.rm = T)
}

for (i in 1:length(survey_data$siteID)) {
  survey_data$turf.cover[i] <- mean(transect_benthic$turf.cover[transect_benthic$siteID == survey_data$siteID[i] 
                                                                  & transect_benthic$dateN <= survey_data$dateN[i] + 14
                                                                  & transect_benthic$dateN >= survey_data$dateN[i] - 14], na.rm = T)
}

for (i in 1:length(survey_data$siteID)) {
  survey_data$cca.cover[i] <- mean(transect_benthic$cca.cover[transect_benthic$siteID == survey_data$siteID[i] 
                                                                  & transect_benthic$dateN <= survey_data$dateN[i] + 14
                                                                  & transect_benthic$dateN >= survey_data$dateN[i] - 14], na.rm = T)
}

#mean Diadema
for (i in 1:length(survey_data$siteID)) {
  survey_data$diadema[i] <- mean(transect_benthic$dia.all[transect_benthic$siteID == survey_data$siteID[i] 
                                                                & transect_benthic$dateN <= survey_data$dateN[i] + 14
                                                                & transect_benthic$dateN >= survey_data$dateN[i] - 14], na.rm = T)
}

#mean functional group abundances
for (i in 1:length(survey_data$siteID)) {
  survey_data$n.croppers[i] <- mean(collated_fish$n.croppers[collated_fish$siteID == survey_data$siteID[i] 
                                                             & collated_fish$dateN <= survey_data$dateN[i] + 14
                                                             & collated_fish$dateN >= survey_data$dateN[i] - 14])
}

for (i in 1:length(survey_data$siteID)) {
  survey_data$n.browsers[i] <- mean(collated_fish$n.browsers[collated_fish$siteID == survey_data$siteID[i] 
                                                             & collated_fish$dateN <= survey_data$dateN[i] + 14
                                                             & collated_fish$dateN >= survey_data$dateN[i] - 14])
}

for (i in 1:length(survey_data$siteID)) {
  survey_data$n.farmers[i] <- mean(collated_fish$n.farmers[collated_fish$siteID == survey_data$siteID[i] 
                                                           & collated_fish$dateN <= survey_data$dateN[i] + 14
                                                           & collated_fish$dateN >= survey_data$dateN[i] - 14])
}

for (i in 1:length(survey_data$siteID)) {
  survey_data$n.scrapers[i] <- mean(collated_fish$n.scrapers[collated_fish$siteID == survey_data$siteID[i] 
                                                             & collated_fish$dateN <= survey_data$dateN[i] + 14
                                                             & collated_fish$dateN >= survey_data$dateN[i] - 14])
}

for (i in 1:length(survey_data$siteID)) {
  survey_data$n.excavators[i] <- mean(collated_fish$n.excavators[collated_fish$siteID == survey_data$siteID[i] 
                                                                 & collated_fish$dateN <= survey_data$dateN[i] + 14
                                                                 & collated_fish$dateN >= survey_data$dateN[i] - 14])
}

#mean total herbivore abundance
for (i in 1:length(survey_data$siteID)) {
  survey_data$n.herbivore[i] <- mean(collated_fish$n.herbivore[collated_fish$siteID == survey_data$siteID[i] 
                                                              & collated_fish$dateN <= survey_data$dateN[i] + 14
                                                              & collated_fish$dateN >= survey_data$dateN[i] - 14])
}


#mean total herbivore biomass
for (i in 1:length(survey_data$siteID)) {
  survey_data$b.herbivore[i] <- mean(collated_fish$b.herbivore[collated_fish$siteID == survey_data$siteID[i] 
                                                               & collated_fish$dateN <= survey_data$dateN[i] + 14
                                                               & collated_fish$dateN >= survey_data$dateN[i] - 14])
}

#herbivore functional group proportional abundances
survey_data <- survey_data %>% mutate(
  Pn.croppers = n.croppers/n.herbivore,
  Pn.browsers = n.browsers/n.herbivore,
  Pn.farmers = n.farmers/n.herbivore,
  Pn.scrapers = n.scrapers/n.herbivore,
  Pn.excavators = n.excavators/n.herbivore,
)

survey_data$coral.richness[is.nan(survey_data$coral.richness)]<-NA #fix NA glitch

###OPTIONAL: save survey data###
write.csv(survey_data, "survey_data.csv")