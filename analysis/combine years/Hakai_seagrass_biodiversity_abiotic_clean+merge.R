###################################################################
###  Hakai + O'Connor Seagrass Data From Calvert Island 
###  Dataset begins in 2014 
###  Samples from several sites (not always the same)
###
###  This code will clean and merge raw data from different years 
###  code by Matt Whalen
###  started on   22 January 2018
###  last updated 27 September 2018
###  
###################################################################

## Script goals and notes
# read, clean, and merge abiotic data across years

# libraries
library(tidyverse)
library(lubridate)
library(reshape2)

#### Read data -------------------------------------------------------------------------------
### 
# person of record: Nicole Knight
d14 <- NULL
##
##
## 2015
##
##
##
d15 <- read.csv( "../Data/Abiotic Data/hakai abiotic 2015.csv",
                 stringsAsFactors = FALSE)
a15 <- d15 %>% 
  mutate( date=mdy(date) ) %>%
  select( date, site, lat=latitude, long=longitude, depth=mean_depth, do=dissolved_oxygen_concentration, salinity, 
          temperature, pressure, par, fluorometry_chlorophyll, turbidity, chl_a, phaeo,
          nitrate_nitrite_ugL, phosphate_ugL, silicate_ugL, bed_area_m2 ) %>%
  distinct()
a15$site <- gsub( x=a15$site, pattern= "_", replacement=" "  )
# rename sites so they are constent across time
a15$site[ a15$site=="choked sandspit" ] <- "sandspit"
a15$site[ a15$site=="triquet bay south" ] <- "triquet bay"
a15$site[ a15$site=="choked south pigu" ] <- "choked interior"
# NO INFORMATION FOR PRUTH BAY FROM 2015
#
##
##
## 2016
##
##
##
d16 <- read.csv( "../Data/Abiotic Data/Hakai YSI data 2016 (compiled) - Sheet1.csv",
                 stringsAsFactors = FALSE )
geo <- as.data.frame( do.call(rbind, strsplit( d16$coordinates,split="N, ", fixed=TRUE )) )
names(geo) <- c("lat","long")
lat <- gsub( "[(]", "", geo$lat)
latsplit <- strsplit( lat, split="[.]" )
geo$lat <- unlist(lapply( latsplit, function(z) as.numeric(paste(z[2],z[3],sep="."))/60 + as.numeric(z[1]) ))
long <- gsub( "W)", "", geo$long)
longsplit <- strsplit( long, split="[.]" )
geo$long <- unlist(lapply( longsplit, function(z) -( as.numeric(paste(z[2],z[3],sep="."))/60 + as.numeric(z[1]) ) ))
d16.geo <- cbind( d16, geo )
a16 <- d16.geo %>% 
  mutate( date=dmy(date) ) %>%
  select( date, site, lat, long, depth=depth..m., do=do..mg.l., salinity=salinity..ppt., 
          temperature = temperature..c.,
          site.depth = max.depth.at.site..m.)
# rename sites so they are constent across time
a16$site[ a16$site=="flat island" ] <- "choked interior"
# pick the deepest sample for each site
u16 <- list()
for(i in 1:length(unique(a16$site)) ){
  tmp <- a16[ a16$site == unique(a16$site)[i], ]
  u16[[i]] <- if( all(!is.na(tmp$depth)) ) {
    tmp[ tmp$depth == max(tmp$depth,na.rm=T), ]
  } else {
  tmp
  }}
# combine list
a16 <- do.call( rbind, u16 )
# looks like two YSI over different depths in a bed (maybe at different parts of the tide?) - average these
library(plyr)
a16 <- ddply( a16, .(date,site), summarize, lat=mean(lat,na.rm=T), long=mean(long,na.rm=T),
              depth=mean(depth,na.rm=T), site.depth = mean(site.depth, na.rm=T), 
              do=mean(do,na.rm=T), salinity=mean(salinity,na.rm=T), temperature=mean(temperature,na.rm=T)  )
##
##
## 2017
##
##
d17 <- read.csv( "../Data/Abiotic Data/Hakai abiotic data 2017 - Sheet1.csv",
                 stringsAsFactors = FALSE )
a17 <- d17 %>%
  mutate( date=dmy(date) ) %>%
  select( date, site, depth=depth.m., temperature=temp.c., salinity=salinity.ppt., ph )
# rename sites so they are constent across time
a17$site <- gsub( x=a17$site, pattern= "_", replacement=" "  )
a17$site[ a17$site=="choked interior i5" ] <- "choked interior"
a17$site[ a17$site=="sand spit" ] <- "sandspit"
# pick the deepest sample for each site
u17 <- list()
for(i in 1:length(unique(a17$site)) ){
  tmp <- a17[ a17$site == unique(a17$site)[i], ]
  u17[[i]] <- if( all(!is.na(tmp$depth)) ) {
    tmp[ tmp$depth == max(tmp$depth,na.rm=T), ]
  } else {
    tmp
  }}
# combine list
a17 <- do.call( rbind, u17 )
##
##
## 2018
##
##
d18 <- read.csv( "../Data/Abiotic Data/YSI data Hakai June-July 2018.csv",
                 stringsAsFactors = FALSE, strip.white = TRUE )
# remove blank rows
d18 <- d18[ !is.na(d18$temp..c.), ]
a18 <- d18 %>%
  mutate( date=dmy(date) ) %>%
  select( date, site, depth=sampling.depth..m., temperature=temp..c., salinity=salinity..ppt.,
          site.depth=bottom.depth..m., ph=pH )
# pick the deepest sample for each site
u18 <- list()
for(i in 1:length(unique(a18$site)) ){
  tmp <- a18[ a18$site == unique(a18$site)[i], ]
  u18[[i]] <- if( all(!is.na(tmp$depth)) ) {
    tmp[ tmp$depth == max(tmp$depth,na.rm=T), ]
  } else {
    tmp
  }}
# combine list
a18 <- do.call( rbind, u18 )
# rename sites so they are constent across time
a18$site[ a18$site=="choked I5" ] <- "choked interior"
##
##
# compare site names and columns names across years
a  <- list( a15, a16, a17, a18 )
lapply( a, function(z) unique(z$site) )
lapply( a, names )
##
##
#### MERGE THE DATA ----------------------------------------------------------------------------
a1516 <- full_join( a15, a16 )
a567 <- full_join( a1516, a17 )
abiotic <- full_join( a567, a18 )
##
##
#### WRITE THE MASTER DATA TO FILE -------------------------------------------------------------
write.csv( abiotic, "../Data/Abiotic Data/O'Connor_hakai_seagrass_MASTER_abiotic.csv", row.names = FALSE )
