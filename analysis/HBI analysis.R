library(MuMIn)
library(plyr)
library(tidyverse)
library(ggplot2)

### Shoot data
shoots2015 <- read.csv("./data/biodiversitysurvey.2015.eelgrass.quadrat.csv")
grazers2015 <- read.csv("./data/biodiversitysurvey.2015.grazers.quadrat.csv")
View(shoots2015)

### data processing
Hakai <- shoots2015 %>%
  filter(Region == "Hakai") %>%
  rename(density = X..of.shoots.in.quadrat) %>%
  rename(Zdrywt = total.dry.weight) %>%
  replace(is.na(.), 0) %>%
  mutate(meZ = macroepiphyte / Zdrywt) %>%
  mutate(microZ = microepiphyte / Zdrywt)

View(Hakai)


View(grazers2015)
grazers <- grazers2015 %>%
  mutate(site_sample = paste(Site_short, Sample, sep = ".")) %>%
  group_by(site_sample) %>%
  select(-(Site:size.class)) %>%
  summarise_each(funs(sum)) %>%
  mutate(total.grazers = rowSums(.[2:67])) %>%
  separate(site_sample, c("Site_short", "sample.ID"))

View(grazers)

pooled <- inner_join(grazers, Hakai)
View(pooled)

pooled <- pooled %>%
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


Grazer.density <- ggplot(pooled, aes(x = Site_short, y = total.grazers)) + 
  geom_point(size = 6, colour = "gray") +
  geom_boxplot(size = 1, fill = "transparent") + 
  scale_y_continuous(name = "Grazers (N/0.0625 m^2)") +
  theme_bw() + 
  theme(panel.grid = element_blank()) + 
  xlab(expression("Location")) 

Grazer.density
ggsave("Grazer.density2015.jpg", plot = Grazer.density, width = 7, height = 3)


grazers.zmshoot <- ggplot(pooled, aes(x = Site_short, y = grazers.zmshoot)) + 
  geom_point(size = 6, colour = "gray") +
  geom_boxplot(size = 1, fill = "transparent") + 
  scale_y_continuous(name = "Grazers/shoot (N/shoot)") +
  theme_bw() + 
  theme(panel.grid = element_blank()) + 
  xlab(expression("Location")) 

grazers.zmshoot
ggsave("grazers.zmshoot2015.jpg", plot = grazers.zmshoot, width = 7, height = 3)



grazers.zmbio <- ggplot(pooled, aes(x = Site_short, y = grazers.zmbio)) + 
  geom_point(size = 6, colour = "gray") +
  geom_boxplot(size = 1, fill = "transparent") + 
  scale_y_continuous(name = "Grazers/Zm biomass (N/g)") +
  theme_bw() + 
  theme(panel.grid = element_blank()) + 
  xlab(expression("Location")) 

grazers.zmbio
ggsave("grazers.zmbio2015.jpg", plot = grazers.zmbio, width = 7, height = 3)

View(Hakai)

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
