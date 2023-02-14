############################################################################
### Summary Statistics Figures                                           ###
### Code written by Lucas Epstein                                        ###
### Economic Theory credited to Erich Muehlegger                         ###
### Last updated 4/26/2022                                               ###
############################################################################ 

library(maps)
library(sp)
library(tigris)
library(acs)
library(stringr)
library(tidycensus)
library(sf)
library(geosphere)
library(tidyverse)
library(dplyr)
library(haven)
library(reshape2)
library(haven)
library(readr)
library(stargazer)
library(xtable)
library(fabricatr)
library(vtable)

dir <- "/Users/Epste/Dropbox/Gas Tax Repeal Draft/"
setwd(dir)

DT <- readRDS("Data/tracts/tract_clean.RDS")

## Histogram
ggplot(DT, aes(x = avg.gas.price)) + geom_histogram(position = "stack", bins = 100)
ggsave("Results/Figures/histogram_gas_price.png", width = 6.18, height = 3.54)


## Bivariate Correlations
ggplot(DT, aes(x= commute.minutes, y= annual.incidence.dollars)) + geom_point(alpha = .2)

## Liberal index on y axis

ggplot(DT, aes(x= gov.dem.pct.18.gene., y= liberal.index.pct.18.gene.)) + geom_point(alpha = .2) + labs(y="Gas Tax Support", x="Liberal Index") +
  geom_smooth(method=lm) 
ggsave("Results/Figures/scat_gov_lib_index.png", width = 6.18, height = 3.54)

ggplot(DT, aes(x= liberal.reg, y= liberal.index.pct.18.gene.)) + geom_point(alpha = .2) + labs( y="Gas Tax Support", x="Liberal Index") +
  geom_smooth(method=lm) 
ggsave("Results/Figures/scat_reg_lib_index.png", width = 6.18, height = 3.54)


#####
## Gas tax support on y axis
ggplot(DT, aes(y= p6n.pct.18.gene., x= liberal.index.pct.18.gene.)) + geom_point(alpha = .2) + labs(title = "Gas Tax Support by Liberal Index", y="Gas Tax Support", x="Liberal Index") +
  geom_smooth(method=lm) 
ggsave("Results/Figures/scat_p6_lib_index.png", width = 6.18, height = 3.54)

ggplot(DT, aes(y= p6n.pct.18.gene., x= gov.dem.pct.18.gene.)) + geom_point(alpha = .2) + labs(title = "Gas Tax Support by Support for \nDemocrat Governor", y="Gas Tax Support", x="Vote Share for Democrat Governor") +
  geom_smooth(method=lm) 
ggsave("Results/Figures/scat_p6_govdem.png", width = 6.18, height = 3.54)

ggplot(DT, aes(y= p6n.pct.18.gene., x= prs.dem.pct.16.gene.)) + geom_point(alpha = .2) + labs(title = "Gas Tax Support by Support for \nDecomocrat President", y="Gas Tax Support", x="Vote Share for Democrat President") +
  geom_smooth(method=lm) 
ggsave("Results/Figures/scat_p6_prsdem.png", width = 6.18, height = 3.54)

ggplot(DT, aes(y= p6n.pct.18.gene., x= weekday.gallons)) + geom_point(alpha = .2) + labs(title = "Gas Tax Support by Weekly Gallons", y="Gas Tax Support", x="Weekly Gallons of Gasoline") +
  geom_smooth(method=lm) 
ggsave("Results/Figures/scat_p6_gallons.png", width = 6.18, height = 3.54)

ggplot(DT, aes(y= p6n.pct.18.gene., x= avg.gas.price)) + geom_point(alpha = .2) + labs(title = "Gas Tax Support by Gas Price", y="Gas Tax Support", x="Average Local Gas Price") +
  geom_smooth(method=lm) 
ggsave("Results/Figures/scat_p6_gasprice.png", width = 6.18, height = 3.54)

ggplot(DT, aes(y= p6n.pct.18.gene., x= pct.vmiles)) + geom_point(alpha = .2) + labs(title = "Gas Tax Support by Share of \nMiles Travelled in Vehilce", y="Gas Tax Support", x="Share of Distance Travelled in Vehicles") +
  geom_smooth(method=lm) 
ggsave("Results/Figures/scat_p6_pctvmiles.png", width = 6.18, height = 3.54)

ggplot(DT, aes(y= p6n.pct.18.gene., x= white)) + geom_point(alpha = .2) + labs(title = "Gas Tax Support by %white", y="Gas Tax Support", x="Race (share of population that is white)") +
  geom_smooth(method=lm) 
ggsave("Results/Figures/scat_p6_white.png", width = 6.18, height = 3.54)

ggplot(DT, aes(y= p6n.pct.18.gene., x= male)) + geom_point(alpha = .2) + labs(title = "Gas Tax Support by %Male", y="Gas Tax Support", x="Male, Share of Population") +
  geom_smooth(method=lm) 
ggsave("Results/Figures/scat_p6_male.png", width = 6.18, height = 3.54)

ggplot(DT, aes(y= p6n.pct.18.gene., x= bachelor)) + geom_point(alpha = .2) + labs(title = "Gas Tax Support by % w/ 4-year Degree", y="Gas Tax Support", x="4-Year Degree") +
  geom_smooth(method=lm) 
ggsave("Results/Figures/scat_p6_4year.png", width = 6.18, height = 3.54)

ggplot(DT, aes(y= p6n.pct.18.gene., x= income)) + geom_point(alpha = .2) + labs(title = "Gas Tax Support by Income", y="Gas Tax Support", x="Log Income") +
  geom_smooth(method=lm) 
ggsave("Results/Figures/scat_p6_income.png", width = 6.18, height = 3.54)

ggplot(DT, aes(y= p6n.pct.18.gene., x= density)) + geom_point(alpha = .2) + labs(title = "Gas Tax Support by Density", y="Gas Tax Support", x="Density") +
  geom_smooth(method=lm) 
ggsave("Results/Figures/scat_p6_density.png", width = 6.18, height = 3.54)


## Voter turnout on y axis

ggplot(DT, aes(y= turnout.pct.18.gene., x= p6n.pct.18.gene.)) + geom_point(alpha = .2) + labs(title = "Turnout by Gas Tax Support", y="Voter Turnout", x="Support for Gasoline Tax") +
  geom_smooth(method=lm) 
ggsave("Results/Figures/scat_turnout_p6.png", width = 6.18, height = 3.54)

ggplot(DT, aes(y= turnout.pct.18.gene., x= liberal.index.pct.18.gene.)) + geom_point(alpha = .2) + labs(title = "Turnout by Liberal Index", y="Voter Turnout", x="Liberal Index") +
  geom_smooth(method=lm) 
ggsave("Results/Figures/scat_turnout_lib_index.png", width = 6.18, height = 3.54)

ggplot(DT, aes(y= turnout.pct.18.gene., x= gov.dem.pct.18.gene.)) + geom_point(alpha = .2) + labs(title = "Turnout by Support for \nDemocrat Governor", y="Voter Turnout", x="Vote Share for Democrat Governor") +
  geom_smooth(method=lm) 
ggsave("Results/Figures/scat_turnout_govdem.png", width = 6.18, height = 3.54)

ggplot(DT, aes(y= turnout.pct.18.gene., x= prs.dem.pct.16.gene.)) + geom_point(alpha = .2) + labs(title = "Turnout by Support for \nDecomocrat President", y="Voter Turnout", x="Vote Share for Democrat President") +
  geom_smooth(method=lm) 
ggsave("Results/Figures/scat_turnout_prsdem.png", width = 6.18, height = 3.54)

ggplot(DT, aes(y= turnout.pct.18.gene., x= weekday.gallons)) + geom_point(alpha = .2) + labs(title = "Turnout by Weekly Gallons", y="Voter Turnout", x="Weekly Gallons of Gasoline Consumed") +
  geom_smooth(method=lm) 
ggsave("Results/Figures/scat_turnout_gallons.png", width = 6.18, height = 3.54)

ggplot(DT, aes(y= turnout.pct.18.gene., x= avg.gas.price)) + geom_point(alpha = .2) + labs(title = "Turnout by Gas Price", y="Voter Turnout", x="Average Gas Price") +
  geom_smooth(method=lm) 
ggsave("Results/Figures/scat_turnout_gasprice.png", width = 6.18, height = 3.54)

ggplot(DT, aes(y= turnout.pct.18.gene., x= pct.vmiles)) + geom_point(alpha = .2) + labs(title = "Turnout by Share of \nMiles Travelled in Vehilce", y="Voter Turnout", x="Share of Miles Travelled by Vehicle") +
  geom_smooth(method=lm) 
ggsave("Results/Figures/scat_turnout_pctvmiles.png", width = 6.18, height = 3.54)

ggplot(DT, aes(y= turnout.pct.18.gene., x= white)) + geom_point(alpha = .2) + labs(title = "Turnout by %white", y="Voter Turnout", x="Race (share of population that is white)") +
  geom_smooth(method=lm) 
ggsave("Results/Figures/scat_turnout_white.png", width = 6.18, height = 3.54)

ggplot(DT, aes(y= turnout.pct.18.gene., x= male)) + geom_point(alpha = .2) + labs(title = "Turnout by %Male", y="Voter Turnout", x="Share of Population that is Male") +
  geom_smooth(method=lm) 
ggsave("Results/Figures/scat_turnout_male.png", width = 6.18, height = 3.54)

ggplot(DT, aes(y= turnout.pct.18.gene., x= bachelor)) + geom_point(alpha = .2) + labs(title = "Turnout by % w/ 4-year Degree", y="Voter Turnout", x="Share of population with 4-year degree") +
  geom_smooth(method=lm) 
ggsave("Results/Figures/scat_turnout_4year.png", width = 6.18, height = 3.54)

ggplot(DT, aes(y= turnout.pct.18.gene., x= income)) + geom_point(alpha = .2) + labs(title = "Turnout by Income", y="Voter Turnout", x="Log Income") +
  geom_smooth(method=lm) 
ggsave("Results/Figures/scat_turnout_income.png", width = 6.18, height = 3.54)

ggplot(DT, aes(y= turnout.pct.18.gene., x= density)) + geom_point(alpha = .2) + labs(title = "Turnout by Density", y="Voter Turnout", x="Population Density") +
  geom_smooth(method=lm) 
ggsave("Results/Figures/scat_turnout_density.png", width = 6.18, height = 3.54)


#####


## Compare different measures of gasoline usage
ggplot(DT, aes(x = weekday.gallons, y = GSYRGAL.prediction, color = as.factor(urban.group))) + 
  geom_point(alpha = .2) + 
  labs(x = "Weeday Gallons (est.vmiles*mpg*1-EV)", y = "Yearly Gallons (prediction based on NHTS)")
ggsave("Results/Figures/compare_gallons1.png", width = 6.18, height = 3.54)





## Maps
#####
# 
# download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="test/world_shape_file.zip")
# # system("unzip test/world_shape_file.zip")
# 
# library(rgdal)
# my_spdf <- readOGR( 
#   dsn= "test/world_shape_file/" , 
#   # layer="TM_WORLD_BORDERS_SIMPL-0.3.dbf",
#   verbose=FALSE
# )
# 
# plot(DT)
# 
# ggplot() + geom_polygon()
