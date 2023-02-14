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
library(lmtest)
library(sandwich)
library(plm)
library(recipes)
library(readxl)
library(leaflet)

dir <- "/Users/Epste/Dropbox/Gas Tax Repeal Draft/"
# dir <- "/stfm/dev2/m1lxe00/Projects/gas_tax"
setwd(dir)

# DT_full <- readRDS("tract_clean.RDS") %>% 
#   mutate(across(c(gas.tax.dollars, annual.incidence.dollars, annual.fee.dollars), .fns = ~.x/100)) %>% 
#   select(GEOID, tract.centroid.latitude, tract.centroid.longitude, hh.count)

#####
# 
# atp <- read_xlsx("Data/caltrans/Lat_and_long_SB1_Projects_05.27.2022.xlsx", sheet = "V032022_ATP") 
# fm <- read_xlsx("Data/caltrans/Lat_and_long_SB1_Projects_05.27.2022.xlsx", sheet = "V032022_FM") 
# hm_pt <- read_xlsx("Data/caltrans/Lat_and_long_SB1_Projects_05.27.2022.xlsx", sheet = "V032022_HM_pt") 
# hm_ln <- read_xlsx("Data/caltrans/Lat_and_long_SB1_Projects_05.27.2022.xlsx", sheet = "V032022_HM_ln") 
# lppc_pt <- read_xlsx("Data/caltrans/Lat_and_long_SB1_Projects_05.27.2022.xlsx", sheet = "V032022_LPPC_pt") 
# lppc_ln <- read_xlsx("Data/caltrans/Lat_and_long_SB1_Projects_05.27.2022.xlsx", sheet = "V032022_LPPC_ln") 
# lppf <- read_xlsx("Data/caltrans/Lat_and_long_SB1_Projects_05.27.2022.xlsx", sheet = "V032022_LPPF") 
# lsr <- read_xlsx("Data/caltrans/Lat_and_long_SB1_Projects_05.27.2022.xlsx", sheet = "V032022_LSR") 
# sccp_pt <- read_xlsx("Data/caltrans/Lat_and_long_SB1_Projects_05.27.2022.xlsx", sheet = "V032022_SCCP_pt") 
# sccp_ln <- read_xlsx("Data/caltrans/Lat_and_long_SB1_Projects_05.27.2022.xlsx", sheet = "V032022_SCCP_ln") 
# sgr <- read_xlsx("Data/caltrans/Lat_and_long_SB1_Projects_05.27.2022.xlsx", sheet = "V032022_SGR") 
# shopa_pt <- read_xlsx("Data/caltrans/Lat_and_long_SB1_Projects_05.27.2022.xlsx", sheet = "V032022_SHOPA_pt") 
# shopa_ln <- read_xlsx("Data/caltrans/Lat_and_long_SB1_Projects_05.27.2022.xlsx", sheet = "V032022_SHOPA_ln") 
# shopp_pt <- read_xlsx("Data/caltrans/Lat_and_long_SB1_Projects_05.27.2022.xlsx", sheet = "V032022_SHOPP_pt") 
# shopp_ln <- read_xlsx("Data/caltrans/Lat_and_long_SB1_Projects_05.27.2022.xlsx", sheet = "V032022_SHOPP_ln") 
# sra <- read_xlsx("Data/caltrans/Lat_and_long_SB1_Projects_05.27.2022.xlsx", sheet = "V032022_SRA") 
# sta <- read_xlsx("Data/caltrans/Lat_and_long_SB1_Projects_05.27.2022.xlsx", sheet = "V032022_STA") 
# stip_pt <- read_xlsx("Data/caltrans/Lat_and_long_SB1_Projects_05.27.2022.xlsx", sheet = "V032022_STIP_pt") 
# stip_ln <- read_xlsx("Data/caltrans/Lat_and_long_SB1_Projects_05.27.2022.xlsx", sheet = "V032022_STIP_ln") 
# tcep_pt <- read_xlsx("Data/caltrans/Lat_and_long_SB1_Projects_05.27.2022.xlsx", sheet = "V032022_TCEP_pt") 
# tcep_ln <- read_xlsx("Data/caltrans/Lat_and_long_SB1_Projects_05.27.2022.xlsx", sheet = "V032022_TCEP_ln")
# tircp <- read_xlsx("Data/caltrans/Lat_and_long_SB1_Projects_05.27.2022.xlsx", sheet = "V032022_TIRCP") 
# 
# 
# 
# clean <- function(DT) {
# DT <- DT %>%
#   # filter(ProjectStatuses != "Planned") %>% 
#   mutate(IIJAFunds = as.numeric(str_replace(IIJAFunds, "<Null>", "0")),
#          SB1Funds = as.numeric(str_replace(SB1Funds, "<Null>", "0")),
#          AssemblyDistricts = paste0(" ", AssemblyDistricts, ",") %>% 
#            str_replace(" 1,", " 01,") %>% 
#            str_replace(" 2,", " 02,") %>% 
#            str_replace(" 3,", " 03,") %>% 
#            str_replace(" 4,", " 04,") %>% 
#            str_replace(" 5,", " 05,") %>% 
#            str_replace(" 6,", " 06,") %>% 
#            str_replace(" 7,", " 07,") %>% 
#            str_replace(" 8,", " 08,") %>% 
#            str_replace(" 9,", " 09,"),
#          category = "FIXME",
#          location = "FIXME")
# 
# if ("ProjectTitle" %in% names(DT)){
#   DT <- mutate(DT,
#                ProjectTitle = tolower(ProjectTitle),
#                ProjectDescription = tolower(ProjectDescription),
#                # location = if_else(ProjectTitle == "<null>", "assembly_districts", "una"),
#                category = case_when(
#                  grepl("bridge", ProjectTitle) ~ "bridge",
#                  grepl("pavement", ProjectTitle) ~ "pavement",
#                  grepl("striping", ProjectTitle) ~ "striping",
#                  grepl("guardrail", ProjectTitle) ~ "pavement",
#                  grepl("drainage", ProjectTitle) ~ "drainage",
#                  grepl("traffic", ProjectTitle) ~ "traffic",
#                  grepl("safety", ProjectTitle) ~ "safety",
#                  grepl("signal", ProjectTitle) ~ "safety",
#                  grepl("roadside", ProjectTitle) ~ "roadside",
#                  grepl("mitigation", ProjectTitle) ~ "mitigation",
#                  grepl("western placerville interchanges", ProjectTitle) | grepl("intersection improvements", ProjectTitle) ~ "misc.roads",
#                  grepl("americans with disabilities", ProjectTitle) | grepl("bike", ProjectTitle) ~ "pedestrian.bike.ada",
#                  grepl("storm water mitigation", ProjectTitle) ~ "storm.water.mitigation",
#                  grepl("pier g and j", ProjectTitle) ~ "shipping.port",
#                  grepl("- tms", ProjectTitle) | grepl("intelligent transportation system", ProjectTitle) | grepl("ramp meter", ProjectTitle) ~ "transportation.management.systems",
#                  grepl("- ada", ProjectTitle) | grepl("sidewalk", ProjectTitle) ~ "pedestrian.bike.ada",
#                  grepl("buses", ProjectTitle) | grepl("metrolink", ProjectTitle) | grepl("passenger rail", ProjectTitle) | grepl("transit station", ProjectTitle) ~ "public.transit",
#                  grepl("active transportation project", ProjectTitle) | grepl("cycle track", ProjectTitle) ~ "pedestrian.bike.ada",
#                  grepl("us 1", ProjectTitle) | grepl("i-", ProjectTitle) | grepl("streets improvements", ProjectTitle) | grepl("street improvements", ProjectTitle) | 
#                    grepl("widen", ProjectTitle) | grepl("roadway", ProjectTitle) | grepl("hov lanes", ProjectTitle) | grepl("express lanes", ProjectTitle)   ~ "misc.road",
#                  grepl(" bus ", ProjectTitle) ~ "public.transit",
#                  grepl("buses", ProjectDescription) ~ "public.transit",
#                  grepl("roadside", ProjectDescription) ~ "roadside",
#                  grepl("zero-emission vehicle charging", ProjectDescription) ~ "ev.charging",
#                  grepl("sidewalk", ProjectDescription) | grepl("pedestrian improvement", ProjectDescription)~ "pedestrian.bike.ada",
#                  grepl("widen", ProjectDescription) & grepl("road", ProjectDescription) ~ "misc.road",
#                  grepl("roadway", ProjectDescription) | grepl("offramp", ProjectDescription) ~ "misc.road",
#                  grepl("maintenance station", ProjectDescription) |grepl("facilities", ProjectTitle)  ~ "maintenance.station",
#                  TRUE ~ "FIXME"
#                ))
# } else {
#  DT$ProjectTitle = "no title"
#  DT$ProjectDescription = "no description"
# }
# if ("ProjectID" %in% names(DT)) {DT<-distinct(DT, ProjectID, .keep_all = T)}
# DT <- select(DT, ProjectTitle, ProjectDescription, TotalCost, SB1Funds, IIJAFunds, AssemblyDistricts, FiscalYears, ProjectStatuses, category, location, Longitude, Latitude)
# DT
# }
# 
# atp <- clean(atp) %>% 
#   mutate(category = "pedestrian.bike.ada",
#          location = "assembly.districts",
#          program = "atp")
# 
# fm <- clean(fm) %>% 
#   mutate(category = "unassigned.FM",
#          location = "assembly.districts",
#          program = "fm")
# 
# hm_pt <- clean(hm_pt) %>% 
#   mutate(location = "lat.lon.pt",
#          category = if_else(category == "FIXME", "unassigned HM", category),
#          program = "hm")
# 
# hm_ln <- clean(hm_ln) %>% 
#   mutate(location = "lat.lon.ln",
#          program = "hm")
# 
# lppc_pt <- clean(lppc_pt) %>% 
#   mutate(location = "unassigned",
#          category = if_else(category == "FIXME", "unassigned.LPPC", category),
#          program = "lppc")
# 
# 
# lppc_ln <- clean(lppc_ln) %>% 
#   mutate(location = "lat.lon.ln",
#          category = if_else(category=="FIXME", "unassigned.LPPC", category),
#          program = "lppc")
# 
# 
# lppf <- clean(lppf) %>% 
#   mutate(category = "unassigned.LPPF",
#          location = "assembly.districts",
#          program = "lppf")
# 
# lsr <- clean(lsr) %>% 
#   mutate(category = "roads.lsr",
#          location = "assembly.districts",
#          program = "lsr")
# 
# sccp_pt <- clean(sccp_pt) %>% 
#   mutate(location = "assembly.districts",
#          category = if_else(category=="FIXME", "unassigned.SCCP", category),
#          program = "sccp")
# 
# sccp_ln <- clean(sccp_ln) %>% 
#   mutate(location = "lat.lon.ln",
#          category = if_else(category=="FIXME", "unassigned.SCCP", category),
#          program = "sccp")
# 
# sgr <- clean(sgr) %>% 
#   mutate(location = "assembly.districts",
#          program = "sgr",
#          category = "public.transit")
# 
# shopa_pt <- clean(shopa_pt) %>% 
#   mutate(location = "lat.lon.pt",
#          category = if_else(category=="FIXME", "roads.shopa", category),
#          program = "shopa")
# 
# shopa_ln <- clean(shopa_ln) %>% 
#   mutate(location = "lat.lon.ln",
#          category = if_else(category=="FIXME", "roads.shopa", category),
#          program = "shopa")
# 
# shopp_pt <- clean(shopp_pt) %>% 
#   mutate(location = "lat.lon.pt",
#          category = if_else(category=="FIXME", "misc.shopp", category),
#          program = "shopp")
# 
# shopp_ln <- clean(shopp_ln) %>% 
#   mutate(location = "lat.lon.ln",
#          category = if_else(category=="FIXME", "misc.shopp", category),
#          program = "shopp")
# 
# sta <- clean(sta) %>% 
#   mutate(location = "assembly.districts",
#          category = if_else(category=="FIXME", "transit.sta", category),
#          program = "sta")
# 
# sra <- clean(sra) %>% 
#   mutate(location = "assembly.districts",
#          category = if_else(category=="FIXME", "transit.sra", category),
#          program = "sra")
# 
# stip_pt <- clean(stip_pt) %>% 
#   mutate(location = "lat.lon.pt",
#          category = if_else(category=="FIXME", "roads.stip", category),
#          program = "stip")
# 
# stip_ln <- clean(stip_ln)  %>% 
#   mutate(location = "lat.lon.ln",
#          category = if_else(category=="FIXME", "roads.stip", category),
#          program = "stip")
# 
# tcep_pt <- clean(tcep_pt) %>% 
#   mutate(location = "lat.lon.pt",
#          category = if_else(category=="FIXME", "roads.tcep", category),
#          program = "tcep")
# 
# tcep_ln <- clean(tcep_ln) %>% 
#   mutate(location = "lat.lon.ln",
#          category = if_else(category=="FIXME", "roads.tcep", category),
#          program = "tcep")
# 
# 
# 
# 
# tircp <- clean(tircp) %>% 
#   mutate(location = "assembly.districts",
#          category = "public.transit",
#          program = "tircp")
# 
# 
#            
# 
# 
# DT <- rbind(atp, fm) %>% 
#   rbind(hm_ln) %>% 
#   rbind(hm_pt) %>% 
#   rbind(lppc_pt)%>% 
#   rbind(lppc_ln) %>% 
#   rbind(lppf) %>% 
#   rbind(lsr) %>% 
#   rbind(sccp_pt) %>% 
#   rbind(sccp_ln)%>% 
#   rbind(sgr)  %>% 
#   rbind(shopa_pt) %>% 
#   rbind(shopa_ln) %>% 
#   rbind(shopp_pt) %>% 
#   rbind(shopp_ln) %>% 
#   rbind(sra) %>% 
#   rbind(sta) %>% 
#   rbind(stip_pt) %>% 
#   rbind(stip_ln) %>% 
#   rbind(tcep_pt) %>% 
#   rbind(tcep_ln) %>% 
#   rbind(tircp) 
#   
# unique(DT$category)
# 
# write_csv(DT, "caltrans_data_combined_categorized.csv")
# 
##################################

DT <- read_csv("Data/caltrans/SB1_Table.csv")

names(DT) <- str_replace_all(names(DT), " ", "")

annual_funds <- DT %>% 
  group_by(FiscalYear) %>% 
  summarise(funds = sum(SB1Funds, na.rm = T))

ggplot(annual_funds) + geom_bar(stat = "identity", aes(x = FiscalYear, y = funds))

years <- c("'17/18" ,"'18/19", "'19/20",  "'20/21", "'21/22", "'22/23")
# years <- c("'17/18" ,"'18/19", "'20/21", "'21/22") old version used this

DT <- dplyr::mutate(DT, 
             SB1Funds = if_else(is.na(SB1Funds), 0, SB1Funds)/length(years),
             FederalFunds = if_else(is.na(FederalFunds), 0, FederalFunds)/length(years))





DT <- filter(DT, FiscalYear %in% years)

##################################

assembly_districts_raw <- read.csv("Data/data/raw/elections/assembly_districts.txt") %>% 
  mutate(BLOCKID = as.character(BLOCKID)) %>% 
  filter(substr(BLOCKID, 1, 1)=="6")

assembly_districts <- assembly_districts_raw %>% 
  mutate(TRACTID = paste0("0", substr(BLOCKID, 1, 10)),
         blockgroupID = paste0("0", substr(BLOCKID, 1, 10))
  ) %>%
  distinct(TRACTID, DISTRICT) %>% 
  # ungroup() %>% 
  group_by(TRACTID) %>% 
  mutate(n = n_distinct(DISTRICT),
         district = paste0("ad", substr(DISTRICT, 2, 3)),
         GEOID = TRACTID)

geom <- readRDS("Data/tract_geom.RDS") %>% 
  # mutate(population = estimate) %>% 
  select(GEOID, NAME, hh.count, geometry)

ad_hh.count <- inner_join(assembly_districts, geom, by = "GEOID") %>%
  group_by(district) %>% 
  mutate(ad_hh.count = sum(hh.count, na.rm = T)) %>% 
  select(district, ad_hh.count) %>% 
  distinct(district, ad_hh.count) %>% 
  arrange(district)


###############################


districts <- paste0("ad", as.character(1:80)) 

districts[1:9] <- paste0("ad0", 1:9)


for (n in districts){
  needle <- substr(n, 3, 4)
  DT[, ncol(DT)+1] <- if_else(grepl(needle, DT$AssemblyDistricts), 1, 0)
  colnames(DT)[ncol(DT)] <- n
}

DT$spread_funds <- 0

for (n in districts){
  needle <- substr(n, 3, 4)
  DT[grepl(needle, DT$AssemblyDistricts),]$spread_funds <- DT[grepl(needle, DT$AssemblyDistricts),]$spread_funds + ad_hh.count[needle,]$ad_hh.count
}

# view(select(DT ,AssemblyDistricts,spread_funds))

# DT$spread_funds <- rowSums(DT[,14:93])


for (n in districts){
  needle <- substr(n, 3, 4)
  DT[, ncol(DT)+1] <- if_else(grepl(needle, DT$AssemblyDistricts), 1, 0)*DT$TotalCost/DT$spread_funds
  colnames(DT)[ncol(DT)] <- paste0("TotalCost_", n)
}

for (n in districts){
  needle <- substr(n, 3, 4)
  DT[, ncol(DT)+1] <- if_else(grepl(needle, DT$AssemblyDistricts), 1, 0)*DT$FederalFunds/DT$spread_funds
  colnames(DT)[ncol(DT)] <- paste0("FederalFunds_", n)
}

for (n in districts){
  needle <- substr(n, 3, 4)
  DT[, ncol(DT)+1] <- if_else(grepl(needle, DT$AssemblyDistricts), 1, 0)*DT$SB1Funds/DT$spread_funds
  colnames(DT)[ncol(DT)] <- paste0("SB1Funds_", n)
}

# These are per captia costs
funds_by_ad <- tibble(district = districts,
                      TotalCost = colSums(DT[103:182], na.rm = T),
                      FederalFunds = colSums(DT[183:262], na.rm = T),
                      SB1Funds = colSums(DT[263:342], na.rm = T))


per_household_funds <- inner_join(funds_by_ad, assembly_districts, by = "district") %>% 
  inner_join(geom, by = "GEOID") %>%
#   group_by(district) 
# %>% 
#   mutate(ad_hh.count = sum(hh.count, na.rm = T),
#          ad_per_household_TotalCost = TotalCost/ad_hh.count,
#          ad_per_household_IIJAFunds = IIJAFunds/ad_hh.count,
#          ad_per_household_SB1Funds = SB1Funds/ad_hh.count) %>% 
  group_by(GEOID) %>% 
  summarise(ad_per_household_TotalCost = mean(TotalCost),
            ad_per_household_FederalFunds = mean(FederalFunds),
            ad_per_household_SB1Funds = mean(SB1Funds),
            assembly_districts = list(district),
            # ad_hh.count = mean(ad_hh.count),
            hh.count = mean(hh.count)
            )

write_csv(select(per_household_funds, -hh.count), "Data/tracts/tract_caltrans.csv")

per_household_funds$ad_per_household_SB1Funds %>% mean()

(sum(per_household_funds$ad_per_household_TotalCost*per_household_funds$hh.count)-sum(DT$TotalCost))/(sum(per_household_funds$ad_per_household_TotalCost*per_household_funds$hh.count)+sum(DT$TotalCost))/2



############
### Maps ###
############


complete <- inner_join(select(geom, -hh.count), per_household_funds, by="GEOID")

pal <- colorNumeric(palette = "viridis", domain = subset(complete, select = ad_per_household_TotalCost, drop = TRUE), n = 8)

st_crs(complete) <- 4326

complete %>%
  st_transform(crs = 4326) %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(popup = ~ paste0(NAME, "<br>",
                               "Assembly District: ", assembly_districts, "<br>",
                               "Total Funds: $", round(ad_per_household_TotalCost), "<br>",
                               "SB1 Funds: $", round(ad_per_household_SB1Funds), "<br>",
                               "Federal Funds: $", round(ad_per_household_FederalFunds), "<br>",
                               "Tract hh.count: ", hh.count, "<br>"
  ),
  stroke = FALSE,
  smoothFactor = 0,
  fillOpacity = 0.7,
  color = ~ pal(ad_per_household_TotalCost)) %>%
  addLegend("topright",
            pal = pal,
            values = ~ ad_per_household_TotalCost,
            title = "Total Fund per household",
            opacity = 1)

# 
# pal <- colorNumeric(palette = "viridis", domain = subset(complete, select = ad_per_household_SB1Funds, drop = TRUE), n = 8)
# 
# 
# complete %>% 
#   st_transform(crs = 4326) %>%
#   leaflet() %>% 
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addPolygons(popup = ~ paste0(NAME, "<br>",
#                                "Assembly District: ", assembly_districts, "<br>",
#                                "Total Funds: $", round(ad_per_household_TotalCost), "<br>",
#                                "SB1 Funds: $", round(ad_per_household_SB1Funds), "<br>",
#                                "Federal Funds: $", round(ad_per_household_FederalFunds), "<br>",
#                                "Tract hh.count: ", hh.count, "<br>"
#   ),
#   stroke = FALSE,
#   smoothFactor = 0,
#   fillOpacity = 0.7,
#   color = ~ pal(ad_per_household_SB1Funds)) %>%
#   addLegend("bottomright", 
#             pal = pal, 
#             values = ~ ad_per_household_SB1Funds,
#             title = "SB1 Funds per household",
#             opacity = 1)
# 
# 
# pal <- colorNumeric(palette = "viridis", domain = subset(complete, select = ad_per_household_FederalFunds, drop = TRUE), n = 8)
# 
# 
# complete %>% 
#   st_transform(crs = 4326) %>%
#   leaflet() %>% 
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addPolygons(popup = ~ paste0(NAME, "<br>",
#                                "Assembly District: ", assembly_districts, "<br>",
#                                "Total Funds: $", round(ad_per_household_TotalCost), "<br>",
#                                "SB1 Funds: $", round(ad_per_household_SB1Funds), "<br>",
#                                "Federal Funds: $", round(ad_per_household_FederalFunds), "<br>",
#                                "Tract hh.count: ", hh.count, "<br>"
#   ),
#   stroke = FALSE,
#   smoothFactor = 0,
#   fillOpacity = 0.7,
#   color = ~ pal(ad_per_household_FederalFunds)) %>%
#   addLegend("bottomright", 
#             pal = pal, 
#             values = ~ ad_per_household_FederalFunds,
#             title = "Federal Funds per household",
#             opacity = 1)
# 
# 
# 
# 
# 
