############################################################################
### Download and Vehicle data                                            ###
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




vmt <- read.csv("/Users/Epste/Dropbox/Gas Tax Repeal Draft/Data/data/raw/latch_2017_vmt.csv") %>% 
  mutate(geocode = as.character(geocode)) %>% 
  filter(substr(geocode, 1, 1) == "6") %>% 
  select(GEOID = geocode, est_vmiles, urban_group, est_pmiles) %>% # Urban group 1:urban 2:suburban 3:rural
  mutate(GEOID = paste0(0, GEOID),
         est_vmiles = est_vmiles,
         est_pmiles = est_pmiles,
         pct_vmiles = est_vmiles/est_pmiles)

tracts_full <- inner_join(tracts_full, vmt, by = "GEOID")

class(tracts_full)

