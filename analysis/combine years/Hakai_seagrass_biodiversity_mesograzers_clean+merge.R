####################################################################
###  Hakai + O'Connor Seagrass Mesograzer Data From Calvert Island 
###  Dataset begins in 2014 
###  Samples from several sites (not always the same)
###
###  This code will clean and merge raw data from different years 
###  code by Matt Whalen
###  started on   22 January 2018
###  last updated sometime in 2018
###  
####################################################################

## Script goals and notes
# handle each year separately
# determine which taxa are present in each dataset (year), generate taxonomy to deal with tax uncertainty
# determine which sites were sampled in each year, and standardize names across years
# merge data into single long and wide formats and save the output

# libraries
library(tidyverse)
library(reshape2)

#### 2014 DATA--------------------------------------------------------------------------------------------------
### 
# person of record: Nicole Knight

# read data
d14 <- read.csv( "../Data/Grazers/OConnor_Hakai_2014_raw_data.csv", stringsAsFactors = FALSE )

# several extra columns labelled "X","X.1","X.2",etc.
# get rid of these columns
d14 <- d14[,-grep("[X*]",names(d14))]

# several extraneous rows in the dataset
# one shows column sums for each taxon
# others are tacked on NA columns
d14 <- d14[ d14$Sample.number != "", ]

# site "Lower" is actually "Lower Choked"
d14$Site[ d14$Site=="Lower" ] <- "Lower Choked"


# convert to long form for easier manipulation
d14long <- melt( d14, id.vars=1:4, variable.name="taxon", value.name="count" )

# get rid of zeros
l14 <- d14long[ d14long$count!=0, ] 

# add a column for year
l14$year <- 2014

# rename and organize columns
l14 <- l14 %>%
  select( year, date=Date, site=Site, sample=Sample.number, sieve=Sieve.size..mm., taxon, count )
#### END OF 2014 




#### 2015 DATA----------------------------------------------------------------------------------------------------
### 
# person of record: Allison Dennert

# read data
d15 <- read.csv( "../Data/Grazers/Hakai 2015 Data.allison.dennert.ID.csv", stringsAsFactors = FALSE )

# lots of NA values in this wide format
# convert these all to zeros
d15[ is.na(d15) ] <- 0

# convert to long form for easier manipulation
d15long <- melt( d15, id.vars=1:4, variable.name="taxon", value.name="count" )

# get rid of zeros
l15 <- d15long[ d15long$count!=0, ] 

# add a column for year
l15$year <- 2015

# rename and organize columns
l15 <- l15 %>%
  select( year, date=Date, site=Site, sample=Sample, sieve=Sieve.size..mm., taxon, count )
#### END OF 2015



#### 2016 DATA----------------------------------------------------------------------------------------------------
### 
# person of record: Tanya Prinzig

# read data
d16 <- read.csv( "../Data/Grazers/2016_OConnor_Grazer_Data-July21_Whalen.csv", stringsAsFactors = FALSE )
# data is already in long format


# add a column for year
d16$year <- 2016

# rename and organize columns
# Note that sieves were not used after 2015
l16 <- d16 %>%
  select( year, date=date_collected, site, sample=quadrat, 
          taxon=final_id, size=total_length_mm )

str(l16)
# body size (total_length_mm) is not numeric. Coerce to numeric
l16$size <- as.numeric( l16$size )

# store taxonomy in a different place than the count and size data
t16 <- d16 %>%
  select( class, order, suborder, family, genus, taxon=final_id ) %>%
  distinct( )
#### END OF 2016



#### 2017 DATA----------------------------------------------------------------------------------------------------
### 
# person of record: Tanya Prinzig

# read data
d17 <- read.csv( "../Data/Grazers/2017_OConnor_Grazer_Data-July27_Whalen.csv", stringsAsFactors = FALSE )
# data is already in long format

# add a column for year
d17$year <- 2017

# rename and organize columns
# Note that sieves were not used after 2015
l17 <- d17 %>%
  select( year, date=date_collected, site, sample=quadrat, 
          taxon=final_id, size=total_length_mm ) 

# store taxonomy in a different place than the count and size data
t17 <- d17 %>%
  select( class, order, suborder, family, genus, taxon=final_id ) %>%
  distinct( )
#### END OF 2017





# NOTES: Both 2014 and 2015 data started in wide format. These are now in long format
# 2015 currently does not have date information
# 2014 data listed sample numbers as integers, 2015 listed samples as letters (capitalized)
# Site names are likely to be different between 2014 and 2015


# site names
sort(unique(l14$site))
sort(unique(l15$site))
sort(unique(l16$site))
sort(unique(l17$site))

with( l14, table(site,sample) )
sort(unique(l14$sample))


with( l15, table(site,sample) )
with( l16, table(site,sample) )
with( l17, table(site,sample) )


# notes about sites from Coreen Forbes and from looking at data sources
# site Goose (2014) is called Goose West (2015)
# site McMullin (2014) is McMullin S (2015) 
# Triquet and Triquet Bay are difficult to pin down
# 2014: Lower and Choked Lower are the same


# Data starting in 2016 are a little different than previous years
#   because we have length measurements for each individual
# We can either summarize these data, or expand the 2014 and 2015 data to repeat 
#   rows based on the count in each sieve size


#### expand rows for 2014, 2015----------------

## 2014
l14
# this is slow, but works. loop over all rows and replicate them
expand14 <- list()
for( i in 1:nrow(l14) ){
  expand14[[i]] <- l14[i,] %>% slice(rep(1:n(), each = count))  # slice will return a tibble
}
# put list elements back together
expand14 <- do.call( rbind, expand14 )
# we can get rid of the count column now
f14 <- expand14 %>%
  select( year, date, site, sample, size=sieve, taxon )

## 2015
l15
# this is slow, but works. loop over all rows and replicate them
expand15 <- list()
for( i in 1:nrow(l15) ){
  expand15[[i]] <- l15[i,] %>% slice(rep(1:n(), each = count))  # slice will return a tibble
}
# put list elements back together
expand15 <- do.call( rbind, expand15 )
# we can get rid of the count column now
f15 <- expand15 %>%
  select( year, date, site, sample, size=sieve, taxon )

## 2016 + 2017, nothing to do, but go ahead and rename it
f16 <- l16
f17 <- l17

## histograms
windows(4,4)
par( mfrow=c(2,2), mar=c(2,4,2,0)+0.1 )
ylimit <- c(0,1)
hist( f14$size, breaks=8, freq = FALSE, main=2014, ylim=ylimit, col="moccasin" )
hist( f15$size, breaks=8, freq = FALSE, main=2015, ylim=ylimit, col="moccasin" )
# filter out the biggest stuff, which makes the histogram look crazy
hist( l16$size[ l16$size<=8 ], breaks = 8, freq = FALSE, main=2016, ylim=ylimit, col="dodgerblue" ) 
hist( l17$size[ l17$size<=8 ], breaks = 8, freq = FALSE, main=2017, ylim=ylimit, col="dodgerblue" ) 
# measuring size in 2016 on seems to pull things into bigger size classes than those captured by sieves
par( mfrow=c(2,1) )
hist( log(l16$size), breaks = 50, freq = FALSE, main=2016, ylim=ylimit, col="dodgerblue" ) 
hist( log(l17$size), breaks = 50, freq = FALSE, main=2017, ylim=ylimit, col="dodgerblue" ) 


# How many individuals total?
nrow(f14)
nrow(f15)
nrow(f16)
nrow(f17)

# how many quadrats total?
( nq <- c(
length(unique(with( f14, paste(site,sample,sep=".") ))),
length(unique(with( f15, paste(site,sample,sep=".") ))),
length(unique(with( f16, paste(site,sample,sep=".") ))),
length(unique(with( f17, paste(site,sample,sep=".") )))  ) )

# individuals per quadrat
( ipq <- c(
nrow(f14) / length(unique(with( f14, paste(site,sample,sep=".") ))),
nrow(f15) / length(unique(with( f15, paste(site,sample,sep=".") ))),
nrow(f16) / length(unique(with( f16, paste(site,sample,sep=".") ))),
nrow(f17) / length(unique(with( f17, paste(site,sample,sep=".") )))  ) )

data.frame( year=2014:2017, quad.num=nq,ind.per.quad=ipq )

windows(4,4)
par( mar=c(3,4,3,1)+0.1 )
plot( x=factor(2014:2017), y=ipq, las=1, ylab="Number of individuals per quadrat",
      main= "O'Connor Hakai Mesograzers")
text( x=factor(2014:2017), y=ipq, paste( "n =", nq ), pos=c(1,3,1,1) )
# text( x=factor(2014:2017), y=ipq, labels = c("Nicole\nKnight", "Allison\nDennert", 
#                                              "Tanya\nPrinzig","Tanya\nPrinzig"), 
#       pos=c(1,3,1,1))


############ Sort those darn sites out ------------------




#################################################
### strategies
# beta diversity within sites? across times
# is it membership or dominance that defines 
# normalize to biomass
#################################################
