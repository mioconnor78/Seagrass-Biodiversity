library(MuMIn)
library(plyr)
library(tidyverse)
library(ggplot2)

### Shoot data
shoot.density2014 <- read.csv("./data/2014_Shoot_density_plot.csv")
shoot.biomass2014 <- read.csv("./data/2014_macro_plot_biomass_MV.csv")
grazers2014 <- read.csv("./data/2014hakaigrazer.csv")
shoots2015 <- read.csv("./data/biodiversitysurvey.2015.eelgrass.quadrat.csv")
grazers2015 <- read.csv("./data/biodiversitysurvey.2015.grazers.quadrat.csv")

View(shoot.density2014)
View(shoot.biomass2014)

### data processing
## standardize site codes
shoot.density2014$site_id <- revalue(shoot.density2014$site_id, c("GE"="GsE", "GW"="GsW", "MN" = "MMn", "MS" = "MMs", "TB" = "TqB", "TN"= "Tqn", "CL" = "Clow"))
shoot.biomass2014$site_id <- revalue(shoot.biomass2014$site_id, c("GE"="GsE", "GW"="GsW", "MN" = "MMn", "MS" = "MMs", "TB" = "TqB", "NT"= "TqN", "LC" = "Clow", "SC" = "Csan"))
grazers2014$Site <- revalue(grazers2014$Site, c("choked"="Csan", "goose"="GsW", "goose.east"="GsE","mcmullin" = "MMs", "mcmullin.north" = "MMn", "triquet.noname.cove" = "TqB", "triquet"= "TqN", "lower.choked" = "Clow"))

View(shoot.biomass2014)
View(shoot.density2014)
View(grazers2014)

shoots2015 <- shoots2015 %>%
  filter(Region == "Hakai") %>%
  rename(density = X..of.shoots.in.quadrat) %>%
  rename(Zdrywt = total.dry.weight) %>%
  replace(is.na(.), 0) %>%
  mutate(meZ = macroepiphyte / Zdrywt) %>%
  mutate(microZ = microepiphyte / Zdrywt)

shoots2014 <- shoot.density2014 %>%
  select(site_id, sample, density, epi_bag_no)

shoots2014bio <- shoot.biomass2014 %>%
  select(site_id, sample_loc, sample_id, final_dry_wgt_kg)

shoots2014total <- shoots2014bio %>%
  mutate(site_sample = paste(site_id, sample_loc, sep = ".")) %>%
  group_by(site_sample) %>%
  select(-c(sample_id, site_id, sample_loc)) %>%
  summarise_each(funs(sum)) %>%
  separate(site_sample, c("site_id", "sample_loc"), sep = "_") ## not working quite yet but close
  
## we want a total macro biomass row for each plot
## then, we want grazers / macro and grazers / zostera; so that will require merging these two files with density and diversity of grazers from 2014.

View(grazers2015)
grazers2015 <- grazers2015 %>%
  mutate(site_sample = paste(Site_short, Sample, sep = ".")) %>%
  group_by(site_sample) %>%
  select(-(Site:size.class)) %>%
  summarise_each(funs(sum)) %>%
  mutate(total.grazers = rowSums(.[2:67])) %>%
  separate(site_sample, c("Site_short", "sample.ID"))

View(grazers)

pooled2015 <- inner_join(grazers, Hakai)
View(pooled2015)

pooled2015 <- pooled2015 %>%
  mutate(grazers.zmshoot = total.grazers / density) %>%
  mutate(grazers.zmbio = total.grazers / Zdrywt)


## FIGURES
shoot.density <- ggplot(Hakai, aes(x = Site_short, y = density)) + 
  geom_point(size = 6, colour = "gray") +
  geom_boxplot(size = 1, fill = "transparent") + 
  scale_y_continuous(name = "Zostera Shoots (N/0.0625 m^2)") +
  theme_bw() + 
  theme(panel.grid = element_blank()) + 
  xlab(expression("Location")) 

shoot.density
ggsave("shoot.density2015.jpg", plot = shoot.density, width = 7, height = 3)


Zm.biomass <- ggplot(Hakai, aes(x = Site_short, y = Zdrywt)) + 
  geom_point(size = 6, colour = "gray") +
  geom_boxplot(size = 1, fill = "transparent") + 
  scale_y_continuous(name = "Zostera Dry Wt (g/0.0625 m^2)") +
  theme_bw() + 
  theme(panel.grid = element_blank()) + 
  xlab(expression("Location")) 

Zm.biomass
ggsave("Zm.biomass2015.jpg", plot = Zm.biomass, width = 7, height = 3)


Macro.epi.Z <- ggplot(Hakai, aes(x = Site_short, y = meZ)) + 
  geom_point(size = 6, colour = "gray") +
  geom_boxplot(size = 1, fill = "transparent") + 
  scale_y_continuous(name = "Macro Epi / Zg (g/g)") +
  theme_bw() + 
  theme(panel.grid = element_blank()) + 
  xlab(expression("Location")) 

Macro.epi.Z
ggsave("Macro.epi.Z2015.jpg", plot = Macro.epi.Z, width = 7, height = 3)


Micro.epi.Z <- ggplot(Hakai, aes(x = Site_short, y = log(microZ))) + 
  geom_point(size = 6, colour = "gray") +
  geom_boxplot(size = 1, fill = "transparent") + 
  scale_y_continuous(name = "Micro Epi / Zm (g/g)") +
  theme_bw() + 
  theme(panel.grid = element_blank()) + 
  xlab(expression("Location")) 

Micro.epi.Z
ggsave("Micro.epi.Z2015.jpg", plot = Micro.epi.Z, width = 7, height = 3)


Grazer.density <- ggplot(pooled2015, aes(x = Site_short, y = total.grazers)) + 
  geom_point(size = 6, colour = "gray") +
  geom_boxplot(size = 1, fill = "transparent") + 
  scale_y_continuous(name = "Grazers (N/0.0625 m^2)") +
  theme_bw() + 
  theme(panel.grid = element_blank()) + 
  xlab(expression("Location")) 

Grazer.density
ggsave("Grazer.density2015.jpg", plot = Grazer.density, width = 7, height = 3)


grazers.zmshoot <- ggplot(pooled2015, aes(x = Site_short, y = grazers.zmshoot)) + 
  geom_point(size = 6, colour = "gray") +
  geom_boxplot(size = 1, fill = "transparent") + 
  scale_y_continuous(name = "Grazers/shoot (N/shoot)") +
  theme_bw() + 
  theme(panel.grid = element_blank()) + 
  xlab(expression("Location")) 

grazers.zmshoot
ggsave("grazers.zmshoot2015.jpg", plot = grazers.zmshoot, width = 7, height = 3)



grazers.zmbio <- ggplot(pooled2015, aes(x = Site_short, y = grazers.zmbio)) + 
  geom_point(size = 6, colour = "gray") +
  geom_boxplot(size = 1, fill = "transparent") + 
  scale_y_continuous(name = "Grazers/Zm biomass (N/g)") +
  theme_bw() + 
  theme(panel.grid = element_blank()) + 
  xlab(expression("Location")) 

grazers.zmbio
ggsave("grazers.zmbio2015.jpg", plot = grazers.zmbio, width = 7, height = 3)

View(Hakai)



#### below here is some analysis from god knows when (old), but I didn't delete it because it does relate the data to abiotic variables.
#Read in datafile
seagrass=read.csv("./data/hakaigrazer.csv")
seagrass <- seagrass[,-(53:62)]
seagrass$Abundance <- sum(seagrass[,5], seagrass[,6])

class(seagrass[,(3:50)])
dim(seagrass)
#identify working datafile
data<- seagrass

dim(seagrass)

## collapse size classes
data1 <- ddply(data, .(Site, Sample.number), summarize, sum(data[,(5:52)]))



hist(log(data$Abundance))

plot(log(data$Abundance) ~ data$average_sal, col = c(data$Time), pch = 19)
plot(log(data$Abundance) ~ data$average_temp, col = c(data$Time), pch = 19)
plot(log(data$Abundance) ~ data$dens, col = c(data$Time), pch = 19)
plot(data$average_sal ~ data$average_temp, col = c(data$Time), pch = 19)
plot(data$average_sal ~ data$Time)
plot(data$average_temp ~ data$Time)
plot(data$average_sal ~ data$Site)
plot(data$average_temp ~ data$Site)
plot(data$dens ~ data$Site, col = c(data$Time), pch = 19)

## the plots of temperature and salinity suggest that salinity is a better site-level variable, and temperature varies more through time than among sites. if you want to say that in the talk, do stats and say we only have point sampling data (not continuous datalogger data)

#### Modeling effects of different predictors of abundance
# I chose these models each based on a hypothesis that was influenced by looking at each of the plots above
# the AIC results in model.sel (below) suggest that mod4 is the best, telling us that abundance varies by site and that variation is explained in part by salinity, and time. we dind't retain a model with a time*temp interaction. this isn't incredibly conclusive, but I'd move forward with this conclusion: salinity explains some of the site differences in abundance. Temperature and other factors might too...
mod0 <- lm(log(data$Abundance+0.1) ~ 1, data = data)
mod1 <- lm(log(data$Abundance+0.1) ~ Site, data = data)
mod2 <- lm(log(data$Abundance+0.1) ~ Site + Time, data = data)
mod3 <- lm(log(data$Abundance+0.1) ~ Site*Time, data = data)
mod4 <- lm(log(data$Abundance+0.1) ~ Site*average_sal + Time, data = data)
mod5 <- lm(log(data$Abundance+0.1) ~ Site*average_sal + Time*average_temp, data = data)
mod6 <- lm(log(data$Abundance+0.1) ~ Site*average_sal*Time, data = data) 

model.sel(mod0, mod1, mod2, mod3, mod4, mod5, mod6)

##### Considering shoot density. 
##### if you had meadow area or something, you could add that here too
##### we can't include shoot density in the above model set b/c we don't have shoot density for all those sites. 

sites.dens <- sg.time[-which(sg.time$dens == 'NA'),]
data <- sites.dens

mod0.1 <- lm(log(data$Abundance+0.1) ~ 1, data = data)
mod1.1 <- lm(log(data$Abundance+0.1) ~ Site, data = data)
mod2.1 <- lm(log(data$Abundance+0.1) ~ Site + Time, data = data)
mod3.1 <- lm(log(data$Abundance+0.1) ~ Site*Time, data = data)
mod4.1 <- lm(log(data$Abundance+0.1) ~ Site*average_sal + Time, data = data)
mod5 <- lm(log(data$Abundance+0.1) ~ Site*average_sal + Time*average_temp, data = data)
mod6 <- lm(log(data$Abundance+0.1) ~ Site*average_sal*Time, data = data) 
