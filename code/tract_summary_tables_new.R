############################################################################
### Summary Statistics Tables                                            ###
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
library(haven)
library(reshape2)
library(haven)
library(readr)
library(stargazer)
library(xtable)
library(fabricatr)
library(vtable)
unloadNamespace("dplyr")
library(dplyr)

dir <- "/Users/Epste/Dropbox/Gas Tax Repeal Draft/"
setwd(dir)

DT <- readRDS("Data/tracts/tract_clean.RDS")




## Summary stats table (main)
#####

vars <- c("p6n.pct.18.gene.",  "p6.undervote.pct.18.gene.", "liberal.reg",
          "turnout.pct.18.gene.", "gov.dem.pct.18.gene.", "prs.dem.pct.20.gene.", "liberal.index.pct.18.gene.",
          "income", "weekday.gallons", "est.vmiles",
          "pct.vmiles", "avg.gas.price", "mpg",
          "white", "male", "bachelor",
          "hs.dropout", "population", "density",
          "conservative.reg", "dcl.reg.pct.18.gene.", "gas.tax.dollars",
          "annual.fee.dollars", "annual.incidence.dollars",
          "ad.per.household.SB1Funds", "rraa.net.benefits",
          "vehicle.hh.count", "commute.minutes")

labels <-  c("Gas Tax Support (voting no on Prop 6)", "Gas Tax Abstentions", "Liberal Party Registration",
             "Turnout (2018 General Election)", "Vote Share for Democrat Governor (2018 General Election)", "Liberal Index (2018 Propositions)", "Vote Share for Democrat President (2020 General Election)",
             "Household Income", "Household Weekday Gallons", "Household Vehicle Miles (LATCH)",
             "Share of Total Miles in Vehicles", "Average Gas Price", "Miles per Gallon",
             "White", "Male", "4 year degrees",
             "Highschool dropouts", "Population", "Population Density", 
             "Conservative Party Registration", "Decline to State Party Registration", "RRAA Annual Gas Tax Increase ($100s per Household)",
             "RRAA Annual Fee Increase ($100s per Household)", "RRAA Total Annual Costs ($100s per Household)",
             "RRAA Gross Annual Benefits ($100s per Household)", "RRAA Net Annual Benefits ($100s per Household)",
             "Vehicles per Household", "Commute Minutes")

var_labels <- data.frame(vars, labels)

digits <- function(x) {
  if (x>1000){x <- round(x)}
  if (x>100){x <- round(x, digits = 1)}
  if (x>10){x <- round(x, digits = 2)}
  
  x
}

st(select(DT, p6n.pct.18.gene., liberal.index.pct.18.gene., liberal.reg, conservative.reg, dcl.reg.pct.18.gene.,
          turnout.pct.18.gene., p6.undervote.pct.18.gene., gov.dem.pct.18.gene., prs.dem.pct.20.gene.,
          rraa.net.benefits, ad.per.household.SB1Funds, annual.incidence.dollars, gas.tax.dollars, annual.fee.dollars,
          avg.gas.price, weekday.gallons, est.vmiles, pct.vmiles, mpg,vehicle.hh.count, commute.minutes,
          income, white, male, bachelor, hs.dropout, population, density), 
   summ = c('notNA(x)','digits(mean(x))','digits(sd(x))','digits(min(x))','digits(max(x))'),
   summ.names = c('N', 'Mean', "Std. Dev.", 'Min', 'Max'),
   labels = var_labels,
   out = "latex",
   file = "Results/Tables/summary_stats.tex")

output0 <- readLines("Results/Tables/summary_stats.tex")

output <- output0[2:(length(output0)-3)]
output[5] <- paste0(output[5], "\\\\[-1.5ex]")
output[14] <- paste0(output[14], "[5pt]")
output[19] <- paste0(output[19], "[5pt]")
output[33] <- paste0(output[33], "[5pt]")
output[26] <- paste0(output[26], "[5pt]")


write(output, "Results/Tables/summary_stats_stub.tex")
#####


