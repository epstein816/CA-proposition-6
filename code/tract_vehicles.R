############################################################################
### Download and Vehicle data                                            ###
### Code written by Lucas Epstein                                        ###
### Relies heavily on prior work by __________________________           ###
### Economic Theory credited to Erich Muehlegger                         ###
### Last updated 4/21/2022                                               ###
############################################################################ 

library(maps)
library(sp)
library(tigris)
library(acs)
library(stringr)
library(tidycensus)
library(sf)
library(tidyverse)
library(dplyr)
library(haven)
library(reshape2)
library(readr)



vehicle_stats <- read_dta("/Users/Epste/Dropbox/Gas Tax Repeal Draft/Data/Fuel Economy Data/fueleffMMY.dta") %>% 
  mutate(model = toupper(model),
         make = toupper(make))

vehicle_purchases <- read_dta("/Users/Epste/Dropbox/Gas Tax Repeal Draft/Data/Experian/VehiclePurchases2017q4_2018q3.dta") %>% 
  mutate(model = toupper(model),
         make = toupper(make),
         GEOID = paste0("0", as.character(censustract))) %>% 
  filter(substr(GEOID, 1, 2) == "06")

vehicles <- left_join(vehicle_purchases, vehicle_stats, by = c("model", "make", "modelyr")) %>% 
  filter(year == 2018 & quarter <= 2 | year < 2018) %>% 
  mutate(mpg = replace(mpg, .$EV == 1, NA)) %>% 
  group_by(GEOID) %>% 
  summarise(across(c(mpg, EV, Diesel, Hybrid), ~ weighted.mean(.x, w = cnt_total, na.rm = T)),
            experian_vehicle_count = sum(cnt_total, na.rm = T))

write_csv(vehicles, "Data/tracts/tract_vehicles.csv")
