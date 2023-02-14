############################################################################
### Main Rscript to recompile all data and Materials for Gas Tax Project ###
### Code written by Lucas Epstein                                        ###
### Last updated 2/8/2023                                                ###
############################################################################ 

## Import libaries and set working directory
library(readxl)
library(plm)
library(shiny)
library(leaflet)
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
# setwd("C:/Users/Epste/OneDrive/Documents")
dir <- "/Users/Epste/Dropbox/Gas Tax Repeal Draft/"
setwd(dir)

###################
## Data Cleaning ##
###################
## Import and Process voting data from SWBD
source("Code/tract_votes_and_reg.R")
tract_elections <- read_csv("Data/tracts/tract_elections.csv")

## Import and process census data
source("Code/tract_census.R")
tract_acs <- read_csv("Data/tracts/tract_acs.csv")

## Import and process gas price data
# source("Code/tract_gas.R")
tract_gas_price <- read_csv("Data/tracts/tract_gas_price.csv")

## Import and process BTS data

tract_bts <- read.csv("/Users/Epste/Dropbox/Gas Tax Repeal Draft/Data/data/raw/latch_2017_vmt.csv") %>% 
  mutate(geocode = as.character(geocode)) %>% 
  filter(substr(geocode, 1, 1) == "6") %>% 
  select(GEOID = geocode, est_vmiles, urban_group, est_pmiles) %>% # Urban group 1:urban 2:suburban 3:rural
  mutate(GEOID = paste0(0, GEOID),
         est_vmiles = est_vmiles,
         est_pmiles = est_pmiles,
         pct_vmiles = est_vmiles/est_pmiles)


## Import and process vehicle data
source("Code/tract_vehicles.R")
tract_vehicles <- read_csv("Data/tracts/tract_vehicles.csv")

# Import and process caltrans data
source("Code/clean_caltrans.R")
tract_caltrans <- read_csv("Data/tracts/tract_caltrans.csv")

## Merge data on census tracts
tract_full <- inner_join(tract_elections, tract_acs, by = "GEOID") %>% 
              inner_join(tract_gas_price, by = "GEOID")%>% 
              inner_join(tract_bts, by = "GEOID") %>% 
              inner_join(tract_vehicles, by = "GEOID") %>% 
              inner_join(tract_caltrans, by = "GEOID") %>% 
  rename(EV_old = EV)

# Load Vehicle data from DMV
vehicle_stats_zip <- read_csv("Data/vehicle-fuel-type-count-by-zip-code.csv") 

vehicle_stats_zip <- vehicle_stats_zip %>% 
  rename(myear = `Model Year`) %>% 
  mutate(myear = as.integer(str_replace(myear, "<2006", "2005"))) %>% 
  group_by(`Zip Code`) %>% 
  summarise(EV = sum(Vehicles *(Fuel=="Battery Electric"))/sum(Vehicles),
            tif = (25*sum(Vehicles*(myear<2007))+50*sum(Vehicles*between(myear, 2007, 2016))+100*sum(Vehicles*(myear>2016)))/sum(Vehicles))
library(readxl)
zip_tract_cw <- read_excel("Data/TRACT_ZIP_122018.xlsx") %>% 
  filter(substr(tract, 1, 2)=="06")

dmv <- inner_join(vehicle_stats_zip, zip_tract_cw, by = c("Zip Code" = "zip")) %>% 
  group_by(tract) %>% 
  summarise(EV = sum(EV*tot_ratio),
            tif = sum(tif*tot_ratio)) %>% 
  rename(GEOID = tract)

tract_full <- inner_join(tract_full, dmv, by = "GEOID")

## Combine and transform some data
tract_full <- tract_full %>% 
  mutate(weekday_gallons= est_vmiles/mpg*(1-EV),
         county = substr(GEOID, 3, 5),
         log_income = log(income),
         density = population/area,
         log_density = log(density),
         county = as.factor(county),
         gas_tax_dollars = 365 * weekday_gallons*.12,
         old_annual_fee_dollars = vehicle_hh_count*48 + EV*vehicle_hh_count*100,
         annual_fee_dollars = EV*vehicle_hh_count*100 +vehicle_hh_count*tif,
         annual_incidence_dollars = gas_tax_dollars + annual_fee_dollars,
         acs_vehicle_count = vehicle_hh_count * household_count,
         experian_share = experian_vehicle_count/acs_vehicle_count,
         avg_gas_price = as.numeric(avg_gas_price))



## Incorpate other measures of gasoline conusmption based on regressions of NHTS data
source("Code/nhts_reanalysis.R")


tract_full <- mutate(tract_full,
  GSYRGAL_prediction = (ca_m3$coefficients[names(ca_m3$coefficients)=="(Intercept)"] + ca_m3$coefficients[names(ca_m3$coefficients)=="URBAN"]*tract_full$urban_areas + ca_m3$coefficients[names(ca_m3$coefficients)=="cartime_avg"]*tract_full$commute_minutes + ca_m3$coefficients[names(ca_m3$coefficients)=="HHSIZE"]*household_size))
summary(ca_m3)
summary(tract_full$GSYRGAL_prediction)
quantile(tract_full$GSYRGAL_prediction, c(.0005, .9995), na.rm=T) #FIXME: Suggest this level of winsorization

## use periods instead of underscore (needed for regression recipes)
colnames(tract_full) <- str_replace_all(colnames(tract_full), "_", ".")

tract_full <- tract_full %>% 
  mutate(across(c(gas.tax.dollars, annual.incidence.dollars, old.annual.fee.dollars, annual.fee.dollars, 
                  ad.per.household.FederalFunds, ad.per.household.SB1Funds, ad.per.household.TotalCost),
                .fns = ~.x/100),
         rraa.net.benefits = ad.per.household.SB1Funds - annual.incidence.dollars)

## Save clean data
write_csv(select(tract_full, -geometry), "Data/tracts/tract_clean1.csv")
write_csv(select(tract_full, -geometry), "tract_clean.csv")
saveRDS(tract_full, "Data/tracts/tract_clean.RDS")
DT <- tract_full
#############

##############
## Analysis ##
##############

## Create Summary Statistics
source("Code/tract_summary_tables.R")
source("Code/tract_summary_tables_new.R")

## Tract summary figures
source("Code/tract_summary_figures.R")

## Create regressions
source("Code/tract_regressions.R")

## WTP
source("Code/wtp.R")



#############
## Scratch ##
#############

# test <- mutate(DT,
#        test = conservative.reg + liberal.reg+dcl.reg.pct.18.gene.) %>% 
#   select(test, conservative.reg, liberal.reg, contains("reg.pct.18.gene"))
