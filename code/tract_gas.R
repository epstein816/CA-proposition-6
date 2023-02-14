############################################################################
### Download and Gas Price Data                                          ###
### Code written by Lucas Epstein                                        ###
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



# Import shapefiles from computer for Tract Areas
tract_area <- st_read(paste0(dir, "Data/data/raw/tract_shapes/tl_2018_06_tract.shp")) %>% 
  mutate(area = ALAND/2589988) %>% 
  select(GEOID, area)

# Import tract population centroids
tract_centroid <- read_dta(paste0(dir, "Data/data/raw/gas_prices/tract_pop_centroid.dta")) %>% 
  rename(GEOID = geoid2)

## For each census tract set a radius proportional to the area of the census tract
tract_geo <- inner_join(tract_centroid, tract_area, by = "GEOID") %>% 
  mutate(radius = 1.2*(area/3.14)^.5,
         radius = case_when(radius < 1.5 ~ 1.5,
                            # radius < 1 ~ area,
                            TRUE ~ radius))

## Import gas price data, multiple observations per station
raw_gas <- read_dta(paste0(dir, "Data/data/raw/gas_prices/gas_prices_oct_nov_2018.dta"))

## Create average price per station
sum_gas <- raw_gas %>% 
  filter(rpname == "Unleaded Gas") %>% 
  group_by(fuellocationid) %>% 
  summarise(retailaverage)

## Import list of gas station locations
station_list <- read_dta(paste0(dir, "Data/data/raw/gas_prices/station_list.dta")) %>% 
  mutate(longitude = as.numeric(longitude),
         latitude = as.numeric(latitude)) %>% 
  select(fuellocationid, longitude, latitude)

## Merge Average price with station location
gas_loop <- inner_join(station_list, sum_gas, by = "fuellocationid")

## Prepare for loop
tract_geo$avg_gas_price <- as.numeric(NA)
count <- 0

## This loop draws a circle around the population centroid of each tract and calculates the average price that gas stations within the circle charged
for (tract in unique(tract_geo$GEOID)){
  ## pull latitutde, longitude, and desired radius
  tract_lon <- as.numeric(filter(tract_geo, GEOID == tract)$tract_centroid_longitude)
  tract_lat <- as.numeric(filter(tract_geo, GEOID == tract)$tract_centroid_latitude)
  radius <- filter(tract_geo, GEOID == tract)$radius
  
  ## For each gas station, determine the distance away from the tract centroid
  gas_loop$l_distance_from_tract <- as.vector(distm(matrix(c(tract_lon, tract_lat), nrow = 1), as.matrix(select(gas_loop, longitude, latitude)), fun = distCosine)/1609)
  
  ## Find the average price at nearby gas stations
  nearby_prices <- gas_loop[gas_loop$l_distance_from_tract < radius,]$retailaverage
  avg_tract_price <- sum(nearby_prices)/length(nearby_prices)
  
  ## assign average to census tract
  tract_geo[tract_geo$GEOID == tract,]$avg_gas_price <- avg_tract_price
  
  count <- count + 1
  print(paste("Processing Geodata for tract",count, "out of", length(tract_geo$GEOID)))
}

write_csv(tract_geo, "Data/tracts/tract_gas_price.csv")