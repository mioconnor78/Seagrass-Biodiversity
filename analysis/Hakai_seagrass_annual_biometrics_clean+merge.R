###################################################################
###  Hakai + O'Connor Seagrass Data From Calvert Island 
###  Dataset begins in 2014 
###  Samples from several sites (not always the same)
###
###  This code will clean and merge raw data from different years 
###  code by Matt Whalen
###  started on   22 January 2018
###  
###################################################################

## Script goals and notes
# read, clean, and merge biometric data across years

# libraries
library(tidyverse)
library(lubridate)
library(reshape2)

#### Read data-------------------------------------------------------------------------------
### 
# person of record: Nicole Knight
d14 <- NULL
##
##
##
## 2015
##
##
##
# quadrats
q15 <- read.csv( "../Data/Seagrass+Epiphytes/Hakai_2015_eelgrass_quadrat.csv", 
                 stringsAsFactors = FALSE )
q15 <- q15 %>%
  select( site, id=sample.ID, shoot.count=X..of.shoots.in.quadrat, biomass.quad=total.dry.weight, 
          unknown.weight.shoots=weight..shoots,
          microepiphyte.bulk=microepiphyte, macroepiphyte.quad=macroepiphyte, drift=drift.seaweed, anchored=rooted.seaweed )
q15$site <- gsub( "_", " ", q15$site )
q15$site[ q15$site=="mcmullins nroth"] <- "mcmullins north"

# single shoots
s15 <- read.csv( "../Data/Seagrass+Epiphytes/Hakai_2015_eelgrass_single_shoots.csv", 
                 stringsAsFactors = FALSE )
s15 <- s15 %>%
  mutate( date=dmy(date) ) %>%
  select( site, id=sample.ID, shoot.length, shoot.width, blades, biomass.shoot=total.dry.weight,
          microepiphyte.filter=microepiphyte, macroepiphyte.shoot=macroepiphyte, smithora )  # microepiphytes were measured after filtering through GFC filters?
# remove underscores
s15$site <- gsub( "_"," ", s15$site )
s16$site[ s16$site=="choked sandspit"] <- "sandspit"
# merge 2015 data
d15 <- left_join( s15, q15 )
# add year
d15$year <- 2015
##
##
##
## 2016
##
##
##
# quadrat biomass
b16 <- read.csv( "../Data/Seagrass+Epiphytes/Hakai_2016_eelgrass_quadrat_biomass.csv", 
                 stringsAsFactors = FALSE )
b16 <- b16 %>%
  mutate( date=dmy(date),
          biomass = foil..dry.weight - eelgrass.foil.weight,
          detritus = detritus.foil..dry.weight - detritus.foil.weight,
          epibiont = epiphyte.bag.wt. - epiphyte.bag..dry.wt. ) %>%
  select( date, site, id=sample.ID, shoot.count=X..of.shoots.in.quadrat,  
          biomass, detritus, epibiont.taxon=epiphyte.species, epibiont )
# make all site names lowercase
b16$site <- tolower(b16$site)
# quadrat shoot measurements
q16 <- read.csv( "../Data/Seagrass+Epiphytes/Hakai_2016_eelgrass_quadrat_shoots.csv", 
                 stringsAsFactors = FALSE )
q16 <- q16 %>%
  mutate( date=dmy(date) ) %>%
  select( date, site, id=sample.ID, sheath.length=mean.sheath.length, 
          shoot.lengthX1=shoot.length.1, shoot.lengthX2=shoot.length.2,
          shoot.widthX1=shoot.width..cm..1, shoot.widthX2=shoot.width..cm..2,
          bladesX1=X..blades.shoot.1, bladesX2=X..blades.shoot.2  )
q16.melt <- melt( q16, id.vars = 1:4 )
shoot.split <- as.data.frame( do.call( rbind, strsplit( as.character(q16.melt$variable), split = "X" ) ) )
  names( shoot.split ) <- c( "metric","shoot.rep" )
q16.split <- cbind( q16.melt, shoot.split )
q16 <- dcast( q16.split, date+site+id+sheath.length+shoot.rep~metric )
# make all site names lowercase
q16$site <- tolower(q16$site)
# single shoot measurements
s16 <- read.csv( "../Data/Seagrass+Epiphytes/Hakai_2016_eelgrass_single.shoots.csv", 
                 stringsAsFactors = FALSE )
s16 <- s16 %>%
  mutate( date=dmy(?..date),
          microepiphyte.filter = GF.C.with.dry.sample - GF.C.filter.weight,  # microepiphyte has negative values
          microepiphyte = foil.dry.weight - foil.weight, 
          bryozoan = bryozoan.dry.weight - bryozoan.foil.wt., 
          smithora = smithora.dry.weight - smithora.foil.weight ,
          bulk.diatom = bulk.diatoms.dry.weight - bulk.diatoms.foil.weight ) %>%
  select( date, site, id=sample.ID, blades, shoot.length, shoot.width, microepiphyte,
          microepiphyte.filter, bryozoan, smithora )  # bulk diatoms removed because only appears once and calculated dry mass is negative
# make all site names lowercase
s16$site <- tolower(s16$site)
# think of shoot lengths and widths here as a third replicate from each quadrat
s16$shoot.rep <- factor(3)
# FOIL WEIGHT == EELGRASS

# drift seaweed
d16 <- read.csv(  "../Data/Seagrass+Epiphytes/Hakai_2016_eelgrass_quadrat_driftseaweed.csv", 
                  stringsAsFactors = FALSE )
d16 <- d16 %>%
  mutate( date=dmy(date), dry=as.numeric(drift.algae.dry.wt...bag.wt.),
          bryozoan.drift=as.numeric(factor(notes))-1 ) %>%
  mutate( drift.mass = dry-drift.algae.foil.wt. ) %>%
  select( date, site, id=sample.ID, taxon=drift.algae.species, drift.mass, bryozoan.drift )
d16$drift.mass[ is.na(d16$drift.mass) ] <- 0.02   
# make all site names lowercase
d16$site <- tolower(d16$site)
# merge 2016 data
bd16 <- full_join( b16, d16 )
qs16 <- full_join(q16,s16)
bds16 <- left_join( bd16, s16  )
##
##
##
## 2017
##
##
##
# quadrat level data
q17 <- read.csv( "../Data/Seagrass+Epiphytes/Hakai_2017_Eelgrass_Biometrics.csv", 
                 stringsAsFactors = FALSE ) 
q17 <- q17 %>%
  mutate( date=dmy(date), shoot.count=regular.shoot..+flowering.shoot..,
          biomass = live.dry.weight..g. - live.paper.bag.weight..g.,
          detritus = detritus.dry.weight..g. - detritus.tin.weight..g.,
          smithora = epiphyte.bag.weight..g. - epi.algae.dry.weight..g. ) %>%
  select( date, site, id=quadrat.., shoot.count, veg.count=regular.shoot.., flowering.count=flowering.shoot.., 
          biomass, detritus, smithora,
          shoot.lengthX1=shoot.1.length, shoot.lengthX2=shoot.2.length, shoot.lengthX3=shoot.3.length, 
          shoot.lengthX4=shoot.4.length, shoot.lengthX5=shoot.5.length,
          sheath.lengthX1=shoot.1.sheath.length, sheath.lengthX2=shoot.2.sheath.length, sheath.lengthX3=shoot.3.sheath.length,
          sheath.lengthX4=shoot.4.sheath.length, sheath.lengthX5=shoot.5.sheath.length,
          shoot.widthX1=shoot.1.width..cm., shoot.widthX2=shoot.2.width..cm., shoot.widthX3=shoot.3.width..cm.,
          shoot.widthX4=shoot.4.width..cm., shoot.widthX5=shoot.5.width..cm.,
          bladesX1=shoot.1...blades, bladesX2=shoot.2...blades, bladesX3=shoot.3...blades,
          bladesX4=shoot.4...blades, bladesX5=shoot.5...blades )
q17.melt <- melt( q17, id.vars = 1:9 )
shoot.split <- as.data.frame( do.call( rbind, strsplit( as.character(q17.melt$variable), split = "X" ) ) )
names( shoot.split ) <- c( "metric","shoot.rep" )
q17.split <- cbind( q17.melt, shoot.split )
q17 <- dcast( q17.split, date+site+id+shoot.count+veg.count+flowering.count+biomass+detritus+smithora+shoot.rep~metric )
# shoot level epiphyte data
s17 <- read.csv( "../Data/Seagrass+Epiphytes/Hakai_2017_Epiphyte_Data.csv", 
                 stringsAsFactors = FALSE ) 
s17 <- s17 %>%
  mutate( date=dmy(date), 
          microepiphyte=epiphyte.dry.weight..g.-epi.foil.weight..g., 
          ) %>%
  select( date, site, id=quadrat.., blade.length=epi.blade.length..cm., blade.width=epi.blade.width..cm.,
          sheath.length=sheath.length..cm.,  )
# 
# # perhaps deal with 'drift' algae separately
# c(taxonX1=macroalgae.species.1, taxonX2=macroalgae.species.2, taxonX3=macroalgae.species.3, taxonX4=macroalgae.species.4,
#   macroalgae.1.wet.weight..g.,
# macroalgae.1.foil..g.,
# macroalgae.1.dry..g.,
# 
# macroalgae.2.wet.weight..g.,
# macroalgae.2.foil..g.,
# macroalgae.2.dry..g.,
# 
# macroalgae.3.wet.weight..g.,
# macroalgae.3.foil..g.,
# macroalgae.3.dry..g.,
# 
# macroalgae.4.wet.weight..g.,
# macroalgae.4.foil..g.,
# macroalgae.4.dry..g. )
##
##
##
## 2018
##
##
##
# quadrat-level biomass, morphology, drift algae
q18 <- read.csv( "../Data/Seagrass+Epiphytes/Hakai_2018_Eelgrass_Biometrics.csv", 
                 stringsAsFactors = FALSE ) 

q18 <- q18 %>%
  mutate( date=dmy(date), shoot.count=regular.shoot..+flowering.shoot..,
          biomass = live.dry.weight..g. - live.paper.bag.weight..g.,
          detritus = detritus.dry.weight..g. - detritus.tin.weight..g. ) %>%
  select( date, site, id=quadrat.., shoot.count, veg.count=regular.shoot.., flowering.count=flowering.shoot.., 
          biomass, detritus, 
          shoot.lengthX1=shoot.1.length, shoot.lengthX2=shoot.2.length, shoot.lengthX3=shoot.3.length, 
          shoot.lengthX4=shoot.4.length, shoot.lengthX5=shoot.5.length,
          sheath.lengthX1=shoot.1.sheath.length, sheath.lengthX2=shoot.2.sheath.length, sheath.lengthX3=shoot.3.sheath.length,
          sheath.lengthX4=shoot.4.sheath.length, sheath.lengthX5=shoot.5.sheath.length,
          shoot.widthX1=shoot.1.width..cm., shoot.widthX2=shoot.2.width..cm., shoot.widthX3=shoot.3.width..cm.,
          shoot.widthX4=shoot.4.width..cm., shoot.widthX5=shoot.5.width..cm.,
          bladesX1=shoot.1...blades, bladesX2=shoot.2...blades, bladesX3=shoot.3...blades,
          bladesX4=shoot.4...blades, bladesX5=shoot.5...blades )
q18.melt <- melt( q18, id.vars = 1:8 )
shoot.split <- as.data.frame( do.call( rbind, strsplit( as.character(q18.melt$variable), split = "X" ) ) )
names( shoot.split ) <- c( "metric","shoot.rep" )
q18.split <- cbind( q18.melt, shoot.split )
q18 <- dcast( q18.split, date+site+id+shoot.count+veg.count+flowering.count+biomass+detritus+shoot.rep~metric )
# shoot level epiphyte data
s18 <- read.csv( "../Data/Seagrass+Epiphytes/Hakai_2018_Epiphyte_Data.csv", 
                 stringsAsFactors = FALSE ) 
s18 <- s18 %>%
  mutate( date=dmy(date) ) %>%
  select( date, site, id=quadrat.., blade.length=epi.blade.length..cm., blade.width=epi.blade.width..cm.,
          sheath.length=sheath.length..cm., microepiphyte=epiphyte.dry.weight..g.-epi.foil.weight..g. )
# epiphyte.dry.weight refers to the dry mass of the eelgrass blade from which epiphytes were scrapped and quantified
# GF/C with dry sample is the mass of filtered microepiphytes

