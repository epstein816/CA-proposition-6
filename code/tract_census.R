############################################################################
### Download and process American Communities Survey data                ###
### Code written by Lucas Epstein                                        ###
### Economic Theory credited to Erich Erich Muehlegger                   ###
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

# Import tract shapes using census API
census_api_key("f0c4c1a6d16d4326629b5779d4c6b7cf18a8d7e3", install = T) # use `install = TRUE` to install the key
options(tigris_use_cache = TRUE) # optional - to cache the Census shapefile

codebook_acs <- load_variables(2018, "acs5", cache = TRUE)

codebook_2010 <- load_variables(2010, "sf1", cache = TRUE)


tract_census_long <- get_decennial(geography = "tract",
                          survey = "sf1",
                          variables = c("H002001", "H002003"
                          ),
                          state = "CA",
                          geometry = FALSE,
                          year = 2010)

tract_urban_areas <- pivot_wider(tract_census_long,
                                 names_from = variable,
                                 values_from = value) %>% 
  mutate(urban_areas = H002003/H002001) %>% 
  select(GEOID, urban_areas) 




## Import variables
tract_acs_long <- get_acs(geography = "tract",
                           variables = c("B19001_001", "B19001_002", "B19001_003", "B19001_004", "B19001_005", "B19001_006", "B19001_007", "B19001_008", "B19001_009", "B19001_010", "B19001_011", "B19001_001", "B19001_012", "B19001_013", "B19001_014", "B19001_015", "B19001_016", "B19001_017", #income
                                         "B25075_001", "B25075_002", "B25075_003", "B25075_004", "B25075_005", "B25075_006", "B25075_007", "B25075_008", "B25075_009", "B25075_010", "B25075_011", "B25075_012", "B25075_013", "B25075_014", "B25075_015", 
                                         "B25075_016", "B25075_017", "B25075_018", "B25075_019", "B25075_020", "B25075_021", "B25075_022", "B25075_023", "B25075_024", "B25075_025", "B25075_026", "B25075_027", # Home Value
                                         "B25017_001", "B25017_002", "B25017_003", "B25017_004", "B25017_005", "B25017_006", "B25017_007", "B25017_008", "B25017_009", "B25017_010", # Rooms
                                         "B06009_001", "B06009_002", "B06009_003", "B06009_004", "B06009_005", "B06009_006", # Edu
                                         "C02003_001", "C02003_003", "C02003_004", "C02003_005", "C02003_006", "C02003_007", "C02003_008", "C02003_009", #Race
                                         "B03001_001", "B03001_003", #latino
                                         "B01003_001", # Total Population
                                         "B01001_001", "B01001_002", # Male
                                         "B08014_001", "B08014_002", "B08014_003", "B08014_004", "B08014_005", "B08014_006", "B08014_007", # vehicles available by worker
                                         "B08303_001", "B08303_002", "B08303_003", "B08303_004", "B08303_005", "B08303_006", "B08303_007", "B08303_008", "B08303_009", "B08303_010", "B08303_011", "B08303_012", "B08303_013", # Commute
                                         "B08201_001", "B08201_002", "B08201_003", "B08201_004", "B08201_005", "B08201_006", # Vehicle available by household
                                         "B25033_001", "B25033_002", # Owner Occupied
                                         # "B25010_001", #Household Size
                                         "B01002_001", #Age
                                         "B11001_001", "B11001_002", "B11001_007", #Households
                                         # "B08201_001", "B08201_001", "B08201_001",  # Household size
                                         "B11005_001", "B11005_002", "B11007_001", "B11007_008", "B11007_007", "B11007_004", "B11007_003", "B11007_009",
                                         "B08201_001", "B08201_003", "B08201_002", "B08202_001", "B08202_002", "B08202_003", "B08202_004", "B08202_005",
                                         "B06011_001", "B06009_005", "C02003_003", "B01003_001", "B06009_002", "B01001_002", "B19013_001", "B29001_001"
                                         
                           ),
                           state = "CA",
                           geometry = FALSE,
                           year = 2018) %>%
  ## Change names
  mutate(variable = str_replace(variable, "B19001_001", "income.tot"),
         variable = str_replace(variable, "B19001_002", "income_1"),
         variable = str_replace(variable, "B19001_003", "income_2"),
         variable = str_replace(variable, "B19001_004", "income_3"),
         variable = str_replace(variable, "B19001_005", "income_4"),
         variable = str_replace(variable, "B19001_006", "income_5"),
         variable = str_replace(variable, "B19001_007", "income_6"),
         variable = str_replace(variable, "B19001_008", "income_7"),
         variable = str_replace(variable, "B19001_009", "income_8"),
         variable = str_replace(variable, "B19001_010", "income_9"),
         variable = str_replace(variable, "B19001_011", "income_10"),
         variable = str_replace(variable, "B19001_012", "income_11"),
         variable = str_replace(variable, "B19001_013", "income_12"),
         variable = str_replace(variable, "B19001_014", "income_13"),
         variable = str_replace(variable, "B19001_015", "income_14"),
         variable = str_replace(variable, "B19001_016", "income_15"),
         variable = str_replace(variable, "B19001_017", "income_16"),
         
         variable = str_replace(variable, "B25017_001", "rooms.tot"),
         variable = str_replace(variable, "B25017_002", "rooms_1"),
         variable = str_replace(variable, "B25017_003", "rooms_2"),
         variable = str_replace(variable, "B25017_004", "rooms_3"),
         variable = str_replace(variable, "B25017_005", "rooms_4"),
         variable = str_replace(variable, "B25017_006", "rooms_5"),
         variable = str_replace(variable, "B25017_007", "rooms_6"),
         variable = str_replace(variable, "B25017_008", "rooms_7"),
         variable = str_replace(variable, "B25017_009", "rooms_8"),
         variable = str_replace(variable, "B25017_010", "rooms_9"),
         
         variable = str_replace(variable, "B08014_001", "vehicle_p.tot"),
         variable = str_replace(variable, "B08014_002", "vehicle_p_1"),
         variable = str_replace(variable, "B08014_003", "vehicle_p_2"),
         variable = str_replace(variable, "B08014_004", "vehicle_p_3"),
         variable = str_replace(variable, "B08014_005", "vehicle_p_4"),
         variable = str_replace(variable, "B08014_006", "vehicle_p_5"),
         variable = str_replace(variable, "B08014_007", "vehicle_p_6"),  
         
         variable = str_replace(variable, "B08201_001", "vehicle_hh.tot"),
         variable = str_replace(variable, "B08201_002", "vehicle_hh_1"),
         variable = str_replace(variable, "B08201_003", "vehicle_hh_2"),
         variable = str_replace(variable, "B08201_004", "vehicle_hh_3"),
         variable = str_replace(variable, "B08201_005", "vehicle_hh_4"),
         variable = str_replace(variable, "B08201_006", "vehicle_hh_5"),
         
         variable = str_replace(variable, "B08303_001", "commute.tot"),
         variable = str_replace(variable, "B08303_002", "commute_1"),
         variable = str_replace(variable, "B08303_003", "commute_2"),
         variable = str_replace(variable, "B08303_004", "commute_3"),
         variable = str_replace(variable, "B08303_005", "commute_4"),
         variable = str_replace(variable, "B08303_006", "commute_5"),
         variable = str_replace(variable, "B08303_007", "commute_6"),
         variable = str_replace(variable, "B08303_008", "commute_7"),
         variable = str_replace(variable, "B08303_009", "commute_8"),
         variable = str_replace(variable, "B08303_010", "commute_9"),
         variable = str_replace(variable, "B08303_011", "commute_10"),
         variable = str_replace(variable, "B08303_012", "commute_11"),
         variable = str_replace(variable, "B08303_013", "commute_12"),
         
         
         variable = str_replace(variable, "B25010_001", "household_size"),
         
         variable = str_replace(variable, "B25075_001", "value.tot"),
         variable = str_replace(variable, "B25075_002", "value_1"),
         variable = str_replace(variable, "B25075_003", "value_2"),
         variable = str_replace(variable, "B25075_004", "value_3"),
         variable = str_replace(variable, "B25075_005", "value_4"),
         variable = str_replace(variable, "B25075_006", "value_5"),
         variable = str_replace(variable, "B25075_007", "value_6"),
         variable = str_replace(variable, "B25075_008", "value_7"),
         variable = str_replace(variable, "B25075_009", "value_8"),
         variable = str_replace(variable, "B25075_010", "value_9"),
         variable = str_replace(variable, "B25075_011", "value_10"),
         variable = str_replace(variable, "B25075_012", "value_11"),
         variable = str_replace(variable, "B25075_013", "value_12"),
         variable = str_replace(variable, "B25075_014", "value_13"),
         variable = str_replace(variable, "B25075_015", "value_14"),
         variable = str_replace(variable, "B25075_016", "value_15"),
         variable = str_replace(variable, "B25075_017", "value_16"),
         variable = str_replace(variable, "B25075_018", "value_17"),
         variable = str_replace(variable, "B25075_019", "value_18"),
         variable = str_replace(variable, "B25075_020", "value_19"),
         variable = str_replace(variable, "B25075_021", "value_20"),
         variable = str_replace(variable, "B25075_022", "value_21"),
         variable = str_replace(variable, "B25075_023", "value_22"),
         variable = str_replace(variable, "B25075_024", "value_23"),
         variable = str_replace(variable, "B25075_025", "value_24"),
         variable = str_replace(variable, "B25075_026", "value_25"),
         variable = str_replace(variable, "B25075_027", "value_26"),
         
         variable = str_replace(variable, "C02003_001", "race.tot"),
         variable = str_replace(variable, "C02003_003", "race_1"),
         variable = str_replace(variable, "C02003_004", "race_2"),
         variable = str_replace(variable, "C02003_005", "race_3"),
         variable = str_replace(variable, "C02003_006", "race_4"),
         variable = str_replace(variable, "C02003_007", "race_5"),
         variable = str_replace(variable, "C02003_008", "race_6"),
         variable = str_replace(variable, "C02003_009", "race_7"),
         
         variable = str_replace(variable, "B03001_001", "latino.tot"),
         variable = str_replace(variable, "B03001_003","latino_1"),
         
         
         variable = str_replace(variable, "B01001_001", "male.tot"),
         variable = str_replace(variable, "B01001_002", "male_1"),
         
         variable = str_replace(variable, "B06009_001","edu.tot"),
         variable = str_replace(variable, "B06009_002","edu_1"),
         variable = str_replace(variable, "B06009_003","edu_2"),
         variable = str_replace(variable, "B06009_004","edu_3"),
         variable = str_replace(variable, "B06009_005","edu_4"),
         variable = str_replace(variable, "B06009_006","edu_5")
  ) %>% select(-moe)
  


## Turn values into shares
tract_acs <- pivot_wider(filter(tract_acs_long, !is.na(estimate)), names_from = variable, values_from = estimate) %>% 
  filter(!is.na(B01003_001)) %>% 
  mutate(household_count = vehicle_hh.tot,
         across(contains("income_"), .fns = ~.x/income.tot),
         across(contains("rooms_"), .fns = ~.x/rooms.tot),
         across(contains("value_"), .fns = ~.x/value.tot),
         across(contains("commute_"), .fns = ~.x/commute.tot),
         across(contains("vehicle_hh"), .fns = ~.x/vehicle_hh.tot),
         across(contains("vehicle_p"), .fns = ~.x/vehicle_p.tot),
         across(contains("race"), .fns = ~.x/race.tot),
         across(contains("edu_"), .fns = ~.x/edu.tot),
         latino = latino_1/latino.tot,
         male = male_1/male.tot,
         
         owner_occupied = B25033_002/B25033_001,
         household_size = B25033_001/B11001_001, #FIXME: Check appendix D to see what census variables were actually used
         hh_count = B11001_001, #FIXME: Check if this is correct
         pct_lhcd = B11005_002/B11005_001,# Share of housholds with a child under 18
         pct_lhd1 = B11007_008/(B11007_008+B11007_003), # Share of 1 person households under 65
         pct_lhd2 = B11007_009/(B11007_009+B11007_004), # Share of 2 person households under 65
         pct_lhd3 = B11007_003/(B11007_008+B11007_003), # Share of 1 person households 65+ **Omit this one**
         pct_lhd4 = B11007_004/(B11007_009+B11007_004), # Share of 2 person households 65+
         # veh1 = B08201_003/B08201_001,
         # veh2p = 1 - (B08201_003+B08201_002)/B08201_001,
         worker1 = B08202_003/B08202_001,
         worker2p = (B08202_005+B08202_004)/B08202_001,
         income = B19013_001,
         bachelor = edu_4,
         white = race_1,
         age = B01002_001,
         population = B01003_001,
         hs_dropout = edu_1,
         eligible_voters = B29001_001
         ) %>% 
  inner_join(tract_urban_areas, by = "GEOID")





## Transform discrete shares into average value
tract_acs <- tract_acs %>% 
  mutate(commute_minutes = commute_1 * 2.5 + commute_2 *7  + commute_3 *12  + commute_4 *17  + commute_5 *22  + commute_6 *27 
         + commute_7 *32   + commute_8 *37   + commute_9 *42   + commute_10 *52   + commute_11 *75   + commute_12 *100,
         vehicle_hh_count = vehicle_hh_1*0 + vehicle_hh_2*1 + vehicle_hh_3*2 + vehicle_hh_4*3 + vehicle_hh_5*4,
         vehicle_p_count = vehicle_p_1*0 + vehicle_p_2*1 + vehicle_p_3*2 + vehicle_p_4*3 + vehicle_p_5*4 + vehicle_p_6*5,
         home_value = value_1*5000 + value_2*12500 + value_3*17500 + value_4*22500 + value_5*27500 + value_6*32500 + value_7*37500 + value_8*45000 + value_9*55000 +
           value_10*65000 + value_11*75000 + value_12*85000 + value_13*95000 + value_14*112500 + value_15*137500 + value_16*162500 + value_17*187500 + value_18*225000 + value_19*275000 + 
           value_20*350000 + value_21*450000 + value_22*625000 + value_23*875000 + value_24*1250000 + value_25*1750000 + value_26*2250000)


tract_acs
write_csv(tract_acs, "Data/tracts/tract_acs.csv")

## DO NOT DELETE: Generate spreadsheet descibing variables
## DO NOT UNCOMMENT
# DT <- inner_join(codebook_acs, census_variables, by = c("name" = "Census variable name")) %>% 
#   mutate(label = str_replace(label, "Estimate!!", ""),
#          label = str_replace(label, "Total!!", ""),
#          label = str_replace(label, "!!", ", "),
#          Definition = if_else(is.na(Definition), paste0(label, " - ", concept), Definition)) %>% 
#   select(-label, -concept)

