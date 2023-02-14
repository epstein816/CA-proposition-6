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

dir <- "/Users/Epste/Dropbox/Gas Tax Repeal Draft/"
# dir <- "/stfm/dev2/m1lxe00/Projects/gas_tax/"
setwd(dir)

DT <- readRDS("Data/tracts/tract_clean.RDS")

#####


#################
## New Table 1 ##
#################

m1 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene., 
         weights = TOTVOTE.18.gene.,
         data = DT)

m2 <- lm(p6n.pct.18.gene. ~ annual.incidence.dollars, 
         weights = TOTVOTE.18.gene.,
         data = DT)

m3 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + annual.incidence.dollars, 
         weights = TOTVOTE.18.gene.,
         data = DT)

m4 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + annual.incidence.dollars + as.factor(county), 
         weights = TOTVOTE.18.gene.,
         data = DT)


m5 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + annual.incidence.dollars + 
           white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
         weights = TOTVOTE.18.gene.,
         data = DT)



vars <- c("race.1", "race.2", "race.3", "race.4", "race.5", "race.6", "race.7",
          "income.1", "income.2", "income.3", "income.4", "income.5", "income.6", "income.7", "income.8", "income.9", "income.10", "income.11", "income.12", "income.13", "income.14", "income.15", "income.16",
          "value.1", "value.2", "value.3", "value.4", "value.5", "value.6", "value.7", "value.8", "value.9", "value.10", "value.11", "value.12", "value.13", "value.14", "value.15", "value.16", "value.17", "value.18", "value.19", "value.20", "value.21", "value.22", "value.23", "value.24", "value.25", "value.26",
          # "rooms.1", "rooms.2", "rooms.3", "rooms.4", "rooms.5", "rooms.6", "rooms.7", "rooms.8", "rooms.9",
          "edu.1", "edu.2", "edu.3", "edu.4", "edu.5", 
          "latino",
          "male",
          "owner.occupied",
          "household.size",
          "log.density",
          "commute.1", "commute.2", "commute.3", "commute.4", "commute.5", "commute.6", "commute.7", "commute.8", "commute.9", "commute.10", "commute.11", "commute.12",
          "vehicle.hh.1", "vehicle.hh.2", "vehicle.hh.3", "vehicle.hh.4", "vehicle.hh.5")


m6 <- DT %>% recipe() %>% 
  update_role(p6n.pct.18.gene. ,new_role = "outcome") %>%
  update_role(c(annual.incidence.dollars, liberal.index.pct.18.gene., vars, log.density, county), new_role = "predictor") %>% 
  prep() %>% 
  formula() %>% 
  lm(DT,
     weights = TOTVOTE.18.gene.)



m1_robust_se <- sqrt(diag(vcovHC(m1, type="HC1")))
m2_robust_se <- sqrt(diag(vcovHC(m2, type="HC1")))
m3_robust_se <- sqrt(diag(vcovHC(m3, type="HC1")))
m4_robust_se <- sqrt(diag(vcovHC(m4, type="HC1")))
m5_robust_se <- sqrt(diag(vcovHC(m5, type="HC1")))
m6_robust_se <- sqrt(diag(vcovHC(m6, type="HC1")))



new_reg1 <- stargazer(m1, m2, m3, m4, m5, m6,
                      type = "latex",
                      # type = "text",
                      omit.stat = c("adj.rsq", "f", "ser"),
                      omit = c("county", vars, "white", "male", "bachelor", "log.income", "log.density", "home.value", "commute.minutes", "vehicle.hh.count"),
                      se = list(m1_robust_se, m2_robust_se,  m3_robust_se,  m4_robust_se,  m5_robust_se, m6_robust_se),
                      title = "Simple Regressions",
                      # column.labels = c("Politics", "Incidence", "+ Politics", "+ Census", "+ County FEs", "+ \\% miles in Vehicle", "(5) + Gas Price"),
                      order = c("liberal.index.pct.18.gene.", "annual.incidence.dollars"),
                      covariate.labels  = c("Liberal Index", "RRAA Annual Tax Incidence"),
                      # out = "Results/Tables/simple_regression.tex",
                      column.sep.width = "1pt",
                      font.size = "normalsize",
                      dep.var.labels = "Percent 0:1 voting no on Prop 6 (Supportive of Gas Tax)",
                      add.lines = list(c("County Fixed Effects", "No", "No", "No", "Yes", "Yes", "Yes"),
                                       c("Demographics", "No", "No", "No", "No", "Linear", "Binned"))
)

new_reg1 <- new_reg1[8:length(new_reg1)-1]
new_reg1[length(new_reg1)-1] <- ""
new_reg1[11] <- paste0("   (percent 0:1)", new_reg1[11])
new_reg1[14] <- paste0("   (\\$00s per household)", new_reg1[14])

write(new_reg1, file = "Results/Tables/new_reg1.tex")



##################
## New Table 1b ##
##################

m1 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + ad.per.household.SB1Funds, 
         weights = TOTVOTE.18.gene.,
         data = DT)

m2 <- lm(p6n.pct.18.gene. ~ annual.incidence.dollars + ad.per.household.SB1Funds, 
         weights = TOTVOTE.18.gene.,
         data = DT)

m3 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + annual.incidence.dollars + ad.per.household.SB1Funds, 
         weights = TOTVOTE.18.gene.,
         data = DT)

m4 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + annual.incidence.dollars + ad.per.household.SB1Funds + as.factor(county), 
         weights = TOTVOTE.18.gene.,
         data = DT)


m5 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + annual.incidence.dollars + ad.per.household.SB1Funds + 
           white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
         weights = TOTVOTE.18.gene.,
         data = DT)



vars <- c("race.1", "race.2", "race.3", "race.4", "race.5", "race.6", "race.7",
          "income.1", "income.2", "income.3", "income.4", "income.5", "income.6", "income.7", "income.8", "income.9", "income.10", "income.11", "income.12", "income.13", "income.14", "income.15", "income.16",
          "value.1", "value.2", "value.3", "value.4", "value.5", "value.6", "value.7", "value.8", "value.9", "value.10", "value.11", "value.12", "value.13", "value.14", "value.15", "value.16", "value.17", "value.18", "value.19", "value.20", "value.21", "value.22", "value.23", "value.24", "value.25", "value.26",
          "rooms.1", "rooms.2", "rooms.3", "rooms.4", "rooms.5", "rooms.6", "rooms.7", "rooms.8", "rooms.9",
          "edu.1", "edu.2", "edu.3", "edu.4", "edu.5", 
          "latino",
          "male",
          "owner.occupied",
          "household.size",
          "commute.1", "commute.2", "commute.3", "commute.4", "commute.5", "commute.6", "commute.7", "commute.8", "commute.9", "commute.10", "commute.11", "commute.12",
          "vehicle.hh.1", "vehicle.hh.2", "vehicle.hh.3", "vehicle.hh.4", "vehicle.hh.5")


m6 <- DT %>% recipe() %>% 
  update_role(p6n.pct.18.gene. ,new_role = "outcome") %>%
  update_role(c(annual.incidence.dollars, liberal.index.pct.18.gene., ad.per.household.SB1Funds, vars, log.density, county), new_role = "predictor") %>% 
  prep() %>% 
  formula() %>% 
  lm(DT,
     weights = TOTVOTE.18.gene.)



m1_robust_se <- sqrt(diag(vcovHC(m1, type="HC1")))
m2_robust_se <- sqrt(diag(vcovHC(m2, type="HC1")))
m3_robust_se <- sqrt(diag(vcovHC(m3, type="HC1")))
m4_robust_se <- sqrt(diag(vcovHC(m4, type="HC1")))
m5_robust_se <- sqrt(diag(vcovHC(m5, type="HC1")))
m6_robust_se <- sqrt(diag(vcovHC(m6, type="HC1")))



new_reg1 <- stargazer(m1, m2, m3, m4, m5, m6,
                      type = "latex",
                      # type = "text",
                      omit.stat = c("adj.rsq", "f", "ser"),
                      omit = c("county", vars, "white", "male", "bachelor", "log.income", "log.density", "home.value", "commute.minutes", "vehicle.hh.count"),
                      se = list(m1_robust_se, m2_robust_se,  m3_robust_se,  m4_robust_se,  m5_robust_se, m6_robust_se),
                      title = "Simple Regressions",
                      # column.labels = c("Politics", "Incidence", "+ Politics", "+ Census", "+ County FEs", "+ \\% miles in Vehicle", "(5) + Gas Price"),
                      order = c("ad.per.household.SB1Funds" ,"liberal.index.pct.18.gene.", "annual.incidence.dollars"),
                      covariate.labels  = c("RRAA Annual Funds" ,"Liberal Index", "RRAA Annual Tax Incidence"),
                      # out = "Results/Tables/simple_regression.tex",
                      column.sep.width = "1pt",
                      font.size = "normalsize",
                      dep.var.labels = "Percent 0:1 voting no on Prop 6 (Supportive of Gas Tax)",
                      add.lines = list(c("County Fixed Effects", "No", "No", "No", "Yes", "Yes", "Yes"),
                                       c("Demographics", "No", "No", "No", "No", "Linear", "Binned"))
)

new_reg1 <- new_reg1[8:length(new_reg1)-1]
new_reg1[length(new_reg1)-1] <- ""
new_reg1[14] <- paste0("   (percent 0:1)", new_reg1[14])
new_reg1[11] <- paste0("   (\\$00s per household)", new_reg1[11])
new_reg1[17] <- paste0("   (\\$00s per household)", new_reg1[17])

write(new_reg1, file = "Results/Tables/new_reg1b.tex")





#################
## New Table 2 ##
#################

m1 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene., 
         weights = TOTVOTE.18.gene.,
         data = DT)

m2 <- lm(p6n.pct.18.gene. ~ annual.fee.dollars + gas.tax.dollars, 
         weights = TOTVOTE.18.gene.,
         data = DT)

m3 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + annual.fee.dollars + gas.tax.dollars, 
         weights = TOTVOTE.18.gene.,
         data = DT)

m4 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + annual.fee.dollars + gas.tax.dollars + as.factor(county), 
         weights = TOTVOTE.18.gene.,
         data = DT)


m5 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + annual.fee.dollars + gas.tax.dollars + 
           white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
         weights = TOTVOTE.18.gene.,
         data = DT)



vars <- c("race.1", "race.2", "race.3", "race.4", "race.5", "race.6", "race.7",
          "income.1", "income.2", "income.3", "income.4", "income.5", "income.6", "income.7", "income.8", "income.9", "income.10", "income.11", "income.12", "income.13", "income.14", "income.15", "income.16",
          "value.1", "value.2", "value.3", "value.4", "value.5", "value.6", "value.7", "value.8", "value.9", "value.10", "value.11", "value.12", "value.13", "value.14", "value.15", "value.16", "value.17", "value.18", "value.19", "value.20", "value.21", "value.22", "value.23", "value.24", "value.25", "value.26",
          "rooms.1", "rooms.2", "rooms.3", "rooms.4", "rooms.5", "rooms.6", "rooms.7", "rooms.8", "rooms.9",
          "edu.1", "edu.2", "edu.3", "edu.4", "edu.5", 
          "latino",
          "male",
          "owner.occupied",
          "household.size",
          "commute.1", "commute.2", "commute.3", "commute.4", "commute.5", "commute.6", "commute.7", "commute.8", "commute.9", "commute.10", "commute.11", "commute.12",
          "vehicle.hh.1", "vehicle.hh.2", "vehicle.hh.3", "vehicle.hh.4", "vehicle.hh.5")


m6 <- DT %>% recipe() %>% 
  update_role(p6n.pct.18.gene. ,new_role = "outcome") %>%
  update_role(c(annual.fee.dollars, gas.tax.dollars, liberal.index.pct.18.gene., vars, log.density, county), new_role = "predictor") %>% 
  prep() %>% 
  formula() %>% 
  lm(DT,
     weights = TOTVOTE.18.gene.)



m1_robust_se <- sqrt(diag(vcovHC(m1, type="HC1")))
m2_robust_se <- sqrt(diag(vcovHC(m2, type="HC1")))
m3_robust_se <- sqrt(diag(vcovHC(m3, type="HC1")))
m4_robust_se <- sqrt(diag(vcovHC(m4, type="HC1")))
m5_robust_se <- sqrt(diag(vcovHC(m5, type="HC1")))
m6_robust_se <- sqrt(diag(vcovHC(m6, type="HC1")))



new_reg2 <- stargazer(m1, m2, m3, m4, m5, m6,
                      type = "latex",
                      # type = "text",
                      omit.stat = c("adj.rsq", "f", "ser"),
                      omit = c("county", vars, "white", "male", "bachelor", "log.income", "log.density", "home.value", "commute.minutes", "vehicle.hh.count"),
                      se = list(m1_robust_se, m2_robust_se,  m3_robust_se,  m4_robust_se,  m5_robust_se, m6_robust_se),
                      title = "Simple Regressions",
                      # column.labels = c("Politics", "Incidence", "+ Politics", "+ Census", "+ County FEs", "+ \\% miles in Vehicle", "(5) + Gas Price"),
                      order = c("liberal.index.pct.18.gene.", "gas.tax.dollars", "annual.fee.dollars"),
                      covariate.labels  = c("Liberal Index", "Annual Gas Tax", "Annual Fee Increase"),
                      # out = "Results/Tables/simple_regression.tex",
                      column.sep.width = "1pt",
                      font.size = "normalsize",
                      dep.var.labels = "Percent 0:1 voting no on Prop 6 (Supportive of Gas Tax)",
                      add.lines = list(c("County Fixed Effects", "No", "No", "No", "Yes", "Yes", "Yes"),
                                       c("Demographics", "No", "No", "No", "No", "Linear", "Binned"))
)

new_reg2 <- new_reg2[8:length(new_reg2)-1]
new_reg2[length(new_reg2)-1] <- ""
new_reg2[11] <- paste0("   (percent 0:1)", new_reg2[11])
new_reg2[14] <- paste0("   (\\$00s per household)", new_reg2[14])
new_reg2[17] <- paste0("   (\\$00s per household)", new_reg2[17])

write(new_reg2, file = "Results/Tables/new_reg2.tex")









##################
## New Table 2b ##
##################

m1 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + ad.per.household.SB1Funds, 
         weights = TOTVOTE.18.gene.,
         data = DT)

m2 <- lm(p6n.pct.18.gene. ~ annual.fee.dollars + gas.tax.dollars + ad.per.household.SB1Funds, 
         weights = TOTVOTE.18.gene.,
         data = DT)

m3 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + annual.fee.dollars + gas.tax.dollars + ad.per.household.SB1Funds, 
         weights = TOTVOTE.18.gene.,
         data = DT)

m4 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + annual.fee.dollars + gas.tax.dollars + as.factor(county) + ad.per.household.SB1Funds, 
         weights = TOTVOTE.18.gene.,
         data = DT)


m5 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + annual.fee.dollars + gas.tax.dollars + ad.per.household.SB1Funds + 
           white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
         weights = TOTVOTE.18.gene.,
         data = DT)



vars <- c("race.1", "race.2", "race.3", "race.4", "race.5", "race.6", "race.7",
          "income.1", "income.2", "income.3", "income.4", "income.5", "income.6", "income.7", "income.8", "income.9", "income.10", "income.11", "income.12", "income.13", "income.14", "income.15", "income.16",
          "value.1", "value.2", "value.3", "value.4", "value.5", "value.6", "value.7", "value.8", "value.9", "value.10", "value.11", "value.12", "value.13", "value.14", "value.15", "value.16", "value.17", "value.18", "value.19", "value.20", "value.21", "value.22", "value.23", "value.24", "value.25", "value.26",
          "rooms.1", "rooms.2", "rooms.3", "rooms.4", "rooms.5", "rooms.6", "rooms.7", "rooms.8", "rooms.9",
          "edu.1", "edu.2", "edu.3", "edu.4", "edu.5", 
          "latino",
          "male",
          "owner.occupied",
          "household.size",
          "commute.1", "commute.2", "commute.3", "commute.4", "commute.5", "commute.6", "commute.7", "commute.8", "commute.9", "commute.10", "commute.11", "commute.12",
          "vehicle.hh.1", "vehicle.hh.2", "vehicle.hh.3", "vehicle.hh.4", "vehicle.hh.5")


m6 <- DT %>% recipe() %>% 
  update_role(p6n.pct.18.gene. ,new_role = "outcome") %>%
  update_role(c(annual.fee.dollars, gas.tax.dollars, liberal.index.pct.18.gene., vars, log.density, ad.per.household.SB1Funds, county), new_role = "predictor") %>% 
  prep() %>% 
  formula() %>% 
  lm(DT,
     weights = TOTVOTE.18.gene.)



m1_robust_se <- sqrt(diag(vcovHC(m1, type="HC1")))
m2_robust_se <- sqrt(diag(vcovHC(m2, type="HC1")))
m3_robust_se <- sqrt(diag(vcovHC(m3, type="HC1")))
m4_robust_se <- sqrt(diag(vcovHC(m4, type="HC1")))
m5_robust_se <- sqrt(diag(vcovHC(m5, type="HC1")))
m6_robust_se <- sqrt(diag(vcovHC(m6, type="HC1")))



new_reg2 <- stargazer(m1, m2, m3, m4, m5, m6,
                      type = "latex",
                      # type = "text",
                      omit.stat = c("adj.rsq", "f", "ser"),
                      omit = c("county", vars, "white", "male", "bachelor", "log.income", "log.density", "home.value", "commute.minutes", "vehicle.hh.count"),
                      se = list(m1_robust_se, m2_robust_se,  m3_robust_se,  m4_robust_se,  m5_robust_se, m6_robust_se),
                      title = "Simple Regressions",
                      # column.labels = c("Politics", "Incidence", "+ Politics", "+ Census", "+ County FEs", "+ \\% miles in Vehicle", "(5) + Gas Price"),
                      order = c("ad.per.household.SB1Funds", "liberal.index.pct.18.gene.", "gas.tax.dollars", "annual.fee.dollars"),
                      covariate.labels  = c("RRAA Annual Funds", "Liberal Index", "Annual Gas Tax", "Annual Fee Increase"),
                      # out = "Results/Tables/simple_regression.tex",
                      column.sep.width = "1pt",
                      font.size = "normalsize",
                      dep.var.labels = "Percent 0:1 voting no on Prop 6 (Supportive of Gas Tax)",
                      add.lines = list(c("County Fixed Effects", "No", "No", "No", "Yes", "Yes", "Yes"),
                                       c("Demographics", "No", "No", "No", "No", "Linear", "Binned"))
)

new_reg2 <- new_reg2[8:length(new_reg2)-1]
new_reg2[length(new_reg2)-1] <- ""

new_reg2[11] <- paste0("   (\\$00s per household)", new_reg2[11])
new_reg2[14] <- paste0("   (percent 0:1)", new_reg2[14])
new_reg2[20] <- paste0("   (\\$00s per household)", new_reg2[20])
new_reg2[17] <- paste0("   (\\$00s per household)", new_reg2[17])


write(new_reg2, file = "Results/Tables/new_reg2b.tex")






#################
## New Table 3 ##
#################

m1 <- lm(p6n.pct.18.gene. ~ liberal.reg, 
         weights = TOTVOTE.18.gene.,
         data = DT)

m2 <- lm(p6n.pct.18.gene. ~ annual.incidence.dollars, 
         weights = TOTVOTE.18.gene.,
         data = DT)

m3 <- lm(p6n.pct.18.gene. ~ liberal.reg + annual.incidence.dollars, 
         weights = TOTVOTE.18.gene.,
         data = DT)

m4 <- lm(p6n.pct.18.gene. ~ liberal.reg + annual.incidence.dollars + as.factor(county), 
         weights = TOTVOTE.18.gene.,
         data = DT)


m5 <- lm(p6n.pct.18.gene. ~ liberal.reg + annual.incidence.dollars + 
           white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
         weights = TOTVOTE.18.gene.,
         data = DT)



vars <- c("race.1", "race.2", "race.3", "race.4", "race.5", "race.6", "race.7",
          "income.1", "income.2", "income.3", "income.4", "income.5", "income.6", "income.7", "income.8", "income.9", "income.10", "income.11", "income.12", "income.13", "income.14", "income.15", "income.16",
          "value.1", "value.2", "value.3", "value.4", "value.5", "value.6", "value.7", "value.8", "value.9", "value.10", "value.11", "value.12", "value.13", "value.14", "value.15", "value.16", "value.17", "value.18", "value.19", "value.20", "value.21", "value.22", "value.23", "value.24", "value.25", "value.26",
          "rooms.1", "rooms.2", "rooms.3", "rooms.4", "rooms.5", "rooms.6", "rooms.7", "rooms.8", "rooms.9",
          "edu.1", "edu.2", "edu.3", "edu.4", "edu.5", 
          "latino",
          "male",
          "owner.occupied",
          "household.size",
          "commute.1", "commute.2", "commute.3", "commute.4", "commute.5", "commute.6", "commute.7", "commute.8", "commute.9", "commute.10", "commute.11", "commute.12",
          "vehicle.hh.1", "vehicle.hh.2", "vehicle.hh.3", "vehicle.hh.4", "vehicle.hh.5")


m6 <- DT %>% recipe() %>% 
  update_role(p6n.pct.18.gene. ,new_role = "outcome") %>%
  update_role(c(annual.incidence.dollars, liberal.reg, vars, log.density, county), new_role = "predictor") %>% 
  prep() %>% 
  formula() %>% 
  lm(DT,
     weights = TOTVOTE.18.gene.)



m1_robust_se <- sqrt(diag(vcovHC(m1, type="HC1")))
m2_robust_se <- sqrt(diag(vcovHC(m2, type="HC1")))
m3_robust_se <- sqrt(diag(vcovHC(m3, type="HC1")))
m4_robust_se <- sqrt(diag(vcovHC(m4, type="HC1")))
m5_robust_se <- sqrt(diag(vcovHC(m5, type="HC1")))
m6_robust_se <- sqrt(diag(vcovHC(m6, type="HC1")))



new_reg3 <- stargazer(m1, m2, m3, m4, m5, m6,
                      type = "latex",
                      # type = "text",
                      omit.stat = c("adj.rsq", "f", "ser"),
                      omit = c("county", vars, "white", "male", "bachelor", "log.income", "log.density"),
                      se = list(m1_robust_se, m2_robust_se,  m3_robust_se,  m4_robust_se,  m5_robust_se, m6_robust_se),
                      title = "Simple Regressions",
                      # column.labels = c("Politics", "Incidence", "+ Politics", "+ Census", "+ County FEs", "+ \\% miles in Vehicle", "(5) + Gas Price"),
                      order = c("liberal.reg", "annual.incidence.dollars"),
                      covariate.labels  = c("Liberal Registration", "RRAA Annual Tax Incidence"),
                      # out = "Results/Tables/simple_regression.tex",
                      column.sep.width = "1pt",
                      font.size = "normalsize",
                      dep.var.labels = "Percent 0:1 voting no on Prop 6 (Supportive of Gas Tax)",
                      add.lines = list(c("County Fixed Effects", "No", "No", "No", "Yes", "Yes", "Yes"),
                                       c("Demographics", "No", "No", "No", "No", "Linear", "Binned"))
)

new_reg3 <- new_reg3[8:length(new_reg3)-1]

write(new_reg3, file = "Results/Tables/new_reg3.tex")





#################
## New Table 4 ##
#################

m1 <- lm(p6n.pct.18.gene. ~ liberal.reg, 
         weights = TOTVOTE.18.gene.,
         data = DT)

m2 <- lm(p6n.pct.18.gene. ~ annual.fee.dollars + gas.tax.dollars, 
         weights = TOTVOTE.18.gene.,
         data = DT)

m3 <- lm(p6n.pct.18.gene. ~ liberal.reg + annual.fee.dollars + gas.tax.dollars, 
         weights = TOTVOTE.18.gene.,
         data = DT)

m4 <- lm(p6n.pct.18.gene. ~ liberal.reg + annual.fee.dollars + gas.tax.dollars + as.factor(county), 
         weights = TOTVOTE.18.gene.,
         data = DT)


m5 <- lm(p6n.pct.18.gene. ~ liberal.reg + annual.fee.dollars + gas.tax.dollars + 
           white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
         weights = TOTVOTE.18.gene.,
         data = DT)



vars <- c("race.1", "race.2", "race.3", "race.4", "race.5", "race.6", "race.7",
          "income.1", "income.2", "income.3", "income.4", "income.5", "income.6", "income.7", "income.8", "income.9", "income.10", "income.11", "income.12", "income.13", "income.14", "income.15", "income.16",
          "value.1", "value.2", "value.3", "value.4", "value.5", "value.6", "value.7", "value.8", "value.9", "value.10", "value.11", "value.12", "value.13", "value.14", "value.15", "value.16", "value.17", "value.18", "value.19", "value.20", "value.21", "value.22", "value.23", "value.24", "value.25", "value.26",
          "rooms.1", "rooms.2", "rooms.3", "rooms.4", "rooms.5", "rooms.6", "rooms.7", "rooms.8", "rooms.9",
          "edu.1", "edu.2", "edu.3", "edu.4", "edu.5", 
          "latino",
          "male",
          "owner.occupied",
          "household.size",
          "commute.1", "commute.2", "commute.3", "commute.4", "commute.5", "commute.6", "commute.7", "commute.8", "commute.9", "commute.10", "commute.11", "commute.12",
          "vehicle.hh.1", "vehicle.hh.2", "vehicle.hh.3", "vehicle.hh.4", "vehicle.hh.5")


m6 <- DT %>% recipe() %>% 
  update_role(p6n.pct.18.gene. ,new_role = "outcome") %>%
  update_role(c(annual.fee.dollars, gas.tax.dollars, liberal.reg, vars, log.density, county), new_role = "predictor") %>% 
  prep() %>% 
  formula() %>% 
  lm(DT,
     weights = TOTVOTE.18.gene.)



m1_robust_se <- sqrt(diag(vcovHC(m1, type="HC1")))
m2_robust_se <- sqrt(diag(vcovHC(m2, type="HC1")))
m3_robust_se <- sqrt(diag(vcovHC(m3, type="HC1")))
m4_robust_se <- sqrt(diag(vcovHC(m4, type="HC1")))
m5_robust_se <- sqrt(diag(vcovHC(m5, type="HC1")))
m6_robust_se <- sqrt(diag(vcovHC(m6, type="HC1")))



new_reg4 <- stargazer(m1, m2, m3, m4, m5, m6,
                      type = "latex",
                      # type = "text",
                      omit.stat = c("adj.rsq", "f", "ser"),
                      omit = c("county", vars, "white", "male", "bachelor", "log.income", "log.density"),
                      se = list(m1_robust_se, m2_robust_se,  m3_robust_se,  m4_robust_se,  m5_robust_se, m6_robust_se),
                      title = "Simple Regressions",
                      # column.labels = c("Politics", "Incidence", "+ Politics", "+ Census", "+ County FEs", "+ \\% miles in Vehicle", "(5) + Gas Price"),
                      order = c("liberal.reg", "gas.tax.dollars", "annual.fee.dollars"),
                      covariate.labels  = c("Liberal Registration", "Annual Gas Tax", "Annual Fee Increase"),
                      # out = "Results/Tables/simple_regression.tex",
                      column.sep.width = "1pt",
                      font.size = "normalsize",
                      dep.var.labels = "Percent 0:1 voting no on Prop 6 (Supportive of Gas Tax)",
                      add.lines = list(c("County Fixed Effects", "No", "No", "No", "Yes", "Yes", "Yes"),
                                       c("Demographics", "No", "No", "No", "No", "Linear", "Binned"))
)

new_reg4 <- new_reg4[8:length(new_reg4)-1]

write(new_reg4, file = "Results/Tables/new_reg4.tex")






#################
## New Table 5 ##
#################

m1 <- lm(p6n.pct.18.gene. ~ gov.dem.pct.18.gene., 
         weights = TOTVOTE.18.gene.,
         data = DT)

m2 <- lm(p6n.pct.18.gene. ~ annual.incidence.dollars, 
         weights = TOTVOTE.18.gene.,
         data = DT)

m3 <- lm(p6n.pct.18.gene. ~ gov.dem.pct.18.gene. + annual.incidence.dollars, 
         weights = TOTVOTE.18.gene.,
         data = DT)

m4 <- lm(p6n.pct.18.gene. ~ gov.dem.pct.18.gene. + annual.incidence.dollars + as.factor(county), 
         weights = TOTVOTE.18.gene.,
         data = DT)


m5 <- lm(p6n.pct.18.gene. ~ gov.dem.pct.18.gene. + annual.incidence.dollars + 
           white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
         weights = TOTVOTE.18.gene.,
         data = DT)



vars <- c("race.1", "race.2", "race.3", "race.4", "race.5", "race.6", "race.7",
          "income.1", "income.2", "income.3", "income.4", "income.5", "income.6", "income.7", "income.8", "income.9", "income.10", "income.11", "income.12", "income.13", "income.14", "income.15", "income.16",
          "value.1", "value.2", "value.3", "value.4", "value.5", "value.6", "value.7", "value.8", "value.9", "value.10", "value.11", "value.12", "value.13", "value.14", "value.15", "value.16", "value.17", "value.18", "value.19", "value.20", "value.21", "value.22", "value.23", "value.24", "value.25", "value.26",
          "rooms.1", "rooms.2", "rooms.3", "rooms.4", "rooms.5", "rooms.6", "rooms.7", "rooms.8", "rooms.9",
          "edu.1", "edu.2", "edu.3", "edu.4", "edu.5", 
          "latino",
          "male",
          "owner.occupied",
          "household.size",
          "commute.1", "commute.2", "commute.3", "commute.4", "commute.5", "commute.6", "commute.7", "commute.8", "commute.9", "commute.10", "commute.11", "commute.12",
          "vehicle.hh.1", "vehicle.hh.2", "vehicle.hh.3", "vehicle.hh.4", "vehicle.hh.5")


m6 <- DT %>% recipe() %>% 
  update_role(p6n.pct.18.gene., new_role = "outcome") %>%
  update_role(c(annual.incidence.dollars, gov.dem.pct.18.gene., vars, log.density, county), new_role = "predictor") %>% 
  prep() %>% 
  formula() %>% 
  lm(DT,
     weights = TOTVOTE.18.gene.)



m1_robust_se <- sqrt(diag(vcovHC(m1, type="HC1")))
m2_robust_se <- sqrt(diag(vcovHC(m2, type="HC1")))
m3_robust_se <- sqrt(diag(vcovHC(m3, type="HC1")))
m4_robust_se <- sqrt(diag(vcovHC(m4, type="HC1")))
m5_robust_se <- sqrt(diag(vcovHC(m5, type="HC1")))
m6_robust_se <- sqrt(diag(vcovHC(m6, type="HC1")))



new_reg5 <- stargazer(m1, m2, m3, m4, m5, m6,
                      type = "latex",
                      # type = "text",
                      omit.stat = c("adj.rsq", "f", "ser"),
                      omit = c("county", vars, "white", "male", "bachelor", "log.income", "log.density"),
                      se = list(m1_robust_se, m2_robust_se,  m3_robust_se,  m4_robust_se,  m5_robust_se, m6_robust_se),
                      title = "Simple Regressions",
                      # column.labels = c("Politics", "Incidence", "+ Politics", "+ Census", "+ County FEs", "+ \\% miles in Vehicle", "(5) + Gas Price"),
                      order = c("gov.dem.pct.18.gene.", "annual.incidence.dollars"),
                      covariate.labels  = c("Democrat Governor Vote Share 2018", "RRAA Annual Tax Incidence"),
                      # out = "Results/Tables/simple_regression.tex",
                      column.sep.width = "1pt",
                      font.size = "normalsize",
                      dep.var.labels = "Percent 0:1 voting no on Prop 6 (Supportive of Gas Tax)",
                      add.lines = list(c("County Fixed Effects", "No", "No", "No", "Yes", "Yes", "Yes"),
                                       c("Demographics", "No", "No", "No", "No", "Linear", "Binned"))
)

new_reg5 <- new_reg5[8:length(new_reg5)-1]

write(new_reg5, file = "Results/Tables/new_reg5.tex")





#################
## New Table 6 ##
#################

m1 <- lm(p6n.pct.18.gene. ~ gov.dem.pct.18.gene., 
         weights = TOTVOTE.18.gene.,
         data = DT)

m2 <- lm(p6n.pct.18.gene. ~ annual.fee.dollars + gas.tax.dollars, 
         weights = TOTVOTE.18.gene.,
         data = DT)

m3 <- lm(p6n.pct.18.gene. ~ gov.dem.pct.18.gene. + annual.fee.dollars + gas.tax.dollars, 
         weights = TOTVOTE.18.gene.,
         data = DT)

m4 <- lm(p6n.pct.18.gene. ~ gov.dem.pct.18.gene. + annual.fee.dollars + gas.tax.dollars + as.factor(county), 
         weights = TOTVOTE.18.gene.,
         data = DT)


m5 <- lm(p6n.pct.18.gene. ~ gov.dem.pct.18.gene. + annual.fee.dollars + gas.tax.dollars + 
           white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
         weights = TOTVOTE.18.gene.,
         data = DT)



vars <- c("race.1", "race.2", "race.3", "race.4", "race.5", "race.6", "race.7",
          "income.1", "income.2", "income.3", "income.4", "income.5", "income.6", "income.7", "income.8", "income.9", "income.10", "income.11", "income.12", "income.13", "income.14", "income.15", "income.16",
          "value.1", "value.2", "value.3", "value.4", "value.5", "value.6", "value.7", "value.8", "value.9", "value.10", "value.11", "value.12", "value.13", "value.14", "value.15", "value.16", "value.17", "value.18", "value.19", "value.20", "value.21", "value.22", "value.23", "value.24", "value.25", "value.26",
          "rooms.1", "rooms.2", "rooms.3", "rooms.4", "rooms.5", "rooms.6", "rooms.7", "rooms.8", "rooms.9",
          "edu.1", "edu.2", "edu.3", "edu.4", "edu.5", 
          "latino",
          "male",
          "owner.occupied",
          "household.size",
          "commute.1", "commute.2", "commute.3", "commute.4", "commute.5", "commute.6", "commute.7", "commute.8", "commute.9", "commute.10", "commute.11", "commute.12",
          "vehicle.hh.1", "vehicle.hh.2", "vehicle.hh.3", "vehicle.hh.4", "vehicle.hh.5")


m6 <- DT %>% recipe() %>% 
  update_role(p6n.pct.18.gene. ,new_role = "outcome") %>%
  update_role(c(annual.fee.dollars, gas.tax.dollars, gov.dem.pct.18.gene., vars, log.density, county), new_role = "predictor") %>% 
  prep() %>% 
  formula() %>% 
  lm(DT,
     weights = TOTVOTE.18.gene.)



m1_robust_se <- sqrt(diag(vcovHC(m1, type="HC1")))
m2_robust_se <- sqrt(diag(vcovHC(m2, type="HC1")))
m3_robust_se <- sqrt(diag(vcovHC(m3, type="HC1")))
m4_robust_se <- sqrt(diag(vcovHC(m4, type="HC1")))
m5_robust_se <- sqrt(diag(vcovHC(m5, type="HC1")))
m6_robust_se <- sqrt(diag(vcovHC(m6, type="HC1")))



new_reg6 <- stargazer(m1, m2, m3, m4, m5, m6,
                      type = "latex",
                      # type = "text",
                      omit.stat = c("adj.rsq", "f", "ser"),
                      omit = c("county", vars, "white", "male", "bachelor", "log.income", "log.density"),
                      se = list(m1_robust_se, m2_robust_se,  m3_robust_se,  m4_robust_se,  m5_robust_se, m6_robust_se),
                      title = "Simple Regressions",
                      # column.labels = c("Politics", "Incidence", "+ Politics", "+ Census", "+ County FEs", "+ \\% miles in Vehicle", "(5) + Gas Price"),
                      order = c("gov.dem.pct.18.gene.", "gas.tax.dollars", "annual.fee.dollars"),
                      covariate.labels  = c("Democrat Governor Vote Share 2018", "Annual Gas Tax", "Annual Fee Increase"),
                      # out = "Results/Tables/simple_regression.tex",
                      column.sep.width = "1pt",
                      font.size = "normalsize",
                      dep.var.labels = "Percent 0:1 voting no on Prop 6 (Supportive of Gas Tax)",
                      add.lines = list(c("County Fixed Effects", "No", "No", "No", "Yes", "Yes", "Yes"),
                                       c("Demographics", "No", "No", "No", "No", "Linear", "Binned"))
)

new_reg6 <- new_reg6[8:length(new_reg6)-1]

write(new_reg6, file = "Results/Tables/new_reg6.tex")






#################
## New Table 7 ##
#################

m1 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene., 
         weights = TOTVOTE.18.gene.,
         data = DT)

m2 <- lm(p6n.pct.18.gene. ~ annual.incidence.dollars, 
         weights = TOTVOTE.18.gene.,
         data = DT)

m3 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + annual.incidence.dollars, 
         weights = TOTVOTE.18.gene.,
         data = DT)

m4 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + annual.incidence.dollars + as.factor(county), 
         weights = TOTVOTE.18.gene.,
         data = DT)


m5 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + annual.incidence.dollars + 
           white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
         weights = TOTVOTE.18.gene.,
         data = DT)



vars <- c("race.1", "race.2", "race.3", "race.4", "race.5", "race.6", "race.7",
          "income.1", "income.2", "income.3", "income.4", "income.5", "income.6", "income.7", "income.8", "income.9", "income.10", "income.11", "income.12", "income.13", "income.14", "income.15", "income.16",
          "value.1", "value.2", "value.3", "value.4", "value.5", "value.6", "value.7", "value.8", "value.9", "value.10", "value.11", "value.12", "value.13", "value.14", "value.15", "value.16", "value.17", "value.18", "value.19", "value.20", "value.21", "value.22", "value.23", "value.24", "value.25", "value.26",
          "rooms.1", "rooms.2", "rooms.3", "rooms.4", "rooms.5", "rooms.6", "rooms.7", "rooms.8", "rooms.9",
          "edu.1", "edu.2", "edu.3", "edu.4", "edu.5", 
          "latino",
          "male",
          "owner.occupied",
          "household.size",
          "commute.1", "commute.2", "commute.3", "commute.4", "commute.5", "commute.6", "commute.7", "commute.8", "commute.9", "commute.10", "commute.11", "commute.12",
          "vehicle.hh.1", "vehicle.hh.2", "vehicle.hh.3", "vehicle.hh.4", "vehicle.hh.5")


m6 <- DT %>% recipe() %>% 
  update_role(p6n.pct.18.gene. ,new_role = "outcome") %>%
  update_role(c(annual.incidence.dollars, liberal.index.pct.18.gene., vars, log.density, county), new_role = "predictor") %>% 
  prep() %>% 
  formula() %>% 
  lm(DT,
     weights = TOTVOTE.18.gene.)



m1_robust_se <- sqrt(diag(vcovHC(m1, type="HC1")))
m2_robust_se <- sqrt(diag(vcovHC(m2, type="HC1")))
m3_robust_se <- sqrt(diag(vcovHC(m3, type="HC1")))
m4_robust_se <- sqrt(diag(vcovHC(m4, type="HC1")))
m5_robust_se <- sqrt(diag(vcovHC(m5, type="HC1")))
m6_robust_se <- sqrt(diag(vcovHC(m6, type="HC1")))



new_reg7 <- stargazer(m1, m2, m3, m4, m5, m6,
                      type = "latex",
                      # type = "text",
                      omit.stat = c("adj.rsq", "f", "ser"),
                      omit = c("county", vars, "white", "male", "bachelor", "log.income", "log.density"),
                      se = list(m1_robust_se, m2_robust_se,  m3_robust_se,  m4_robust_se,  m5_robust_se, m6_robust_se),
                      title = "Simple Regressions",
                      # column.labels = c("Politics", "Incidence", "+ Politics", "+ Census", "+ County FEs", "+ \\% miles in Vehicle", "(5) + Gas Price"),
                      order = c("liberal.index.pct.18.gene.", "annual.incidence.dollars"),
                      covariate.labels  = c("Liberal Index", "RRAA Annual Tax Incidence"),
                      # out = "Results/Tables/simple_regression.tex",
                      column.sep.width = "1pt",
                      font.size = "normalsize",
                      dep.var.labels = "Percent 0:1 voting no on Prop 6 (Supportive of Gas Tax)",
                      add.lines = list(c("County Fixed Effects", "No", "No", "No", "Yes", "Yes", "Yes"),
                                       c("Demographics", "No", "No", "No", "No", "Linear", "Binned"))
)

new_reg7 <- new_reg7[8:length(new_reg7)-1]

write(new_reg7, file = "Results/Tables/new_reg7.tex")





#################
## New Table 8 ##
#################

m1 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene., 
         weights = TOTVOTE.18.gene.,
         data = DT)

m2 <- lm(p6n.pct.18.gene. ~ annual.fee.dollars + gas.tax.dollars, 
         weights = TOTVOTE.18.gene.,
         data = DT)

m3 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + annual.fee.dollars + gas.tax.dollars, 
         weights = TOTVOTE.18.gene.,
         data = DT)

m4 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + annual.fee.dollars + gas.tax.dollars + as.factor(county), 
         weights = TOTVOTE.18.gene.,
         data = DT)


m5 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + annual.fee.dollars + gas.tax.dollars + 
           white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
         weights = TOTVOTE.18.gene.,
         data = DT)



vars <- c("race.1", "race.2", "race.3", "race.4", "race.5", "race.6", "race.7",
          "income.1", "income.2", "income.3", "income.4", "income.5", "income.6", "income.7", "income.8", "income.9", "income.10", "income.11", "income.12", "income.13", "income.14", "income.15", "income.16",
          "value.1", "value.2", "value.3", "value.4", "value.5", "value.6", "value.7", "value.8", "value.9", "value.10", "value.11", "value.12", "value.13", "value.14", "value.15", "value.16", "value.17", "value.18", "value.19", "value.20", "value.21", "value.22", "value.23", "value.24", "value.25", "value.26",
          "rooms.1", "rooms.2", "rooms.3", "rooms.4", "rooms.5", "rooms.6", "rooms.7", "rooms.8", "rooms.9",
          "edu.1", "edu.2", "edu.3", "edu.4", "edu.5", 
          "latino",
          "male",
          "owner.occupied",
          "household.size",
          "commute.1", "commute.2", "commute.3", "commute.4", "commute.5", "commute.6", "commute.7", "commute.8", "commute.9", "commute.10", "commute.11", "commute.12",
          "vehicle.hh.1", "vehicle.hh.2", "vehicle.hh.3", "vehicle.hh.4", "vehicle.hh.5")


m6 <- DT %>% recipe() %>% 
  update_role(p6n.pct.18.gene. ,new_role = "outcome") %>%
  update_role(c(annual.fee.dollars, gas.tax.dollars, liberal.index.pct.18.gene., vars, log.density, county), new_role = "predictor") %>% 
  prep() %>% 
  formula() %>% 
  lm(DT,
     weights = TOTVOTE.18.gene.)



m1_robust_se <- sqrt(diag(vcovHC(m1, type="HC1")))
m2_robust_se <- sqrt(diag(vcovHC(m2, type="HC1")))
m3_robust_se <- sqrt(diag(vcovHC(m3, type="HC1")))
m4_robust_se <- sqrt(diag(vcovHC(m4, type="HC1")))
m5_robust_se <- sqrt(diag(vcovHC(m5, type="HC1")))
m6_robust_se <- sqrt(diag(vcovHC(m6, type="HC1")))



new_reg8 <- stargazer(m1, m2, m3, m4, m5, m6,
                      type = "latex",
                      # type = "text",
                      omit.stat = c("adj.rsq", "f", "ser"),
                      omit = c("county", vars, "white", "male", "bachelor", "log.income", "log.density"),
                      se = list(m1_robust_se, m2_robust_se,  m3_robust_se,  m4_robust_se,  m5_robust_se, m6_robust_se),
                      title = "Simple Regressions",
                      # column.labels = c("Politics", "Incidence", "+ Politics", "+ Census", "+ County FEs", "+ \\% miles in Vehicle", "(5) + Gas Price"),
                      order = c("liberal.index.pct.18.gene.", "gas.tax.dollars", "annual.fee.dollars"),
                      covariate.labels  = c("Liberal Index", "Annual Gas Tax", "Annual Fee Increase"),
                      # out = "Results/Tables/simple_regression.tex",
                      column.sep.width = "1pt",
                      font.size = "normalsize",
                      dep.var.labels = "Percent 0:1 voting no on Prop 6 (Supportive of Gas Tax)",
                      add.lines = list(c("County Fixed Effects", "No", "No", "No", "Yes", "Yes", "Yes"),
                                       c("Demographics", "No", "No", "No", "No", "Linear", "Binned"))
)

new_reg8 <- new_reg8[8:length(new_reg8)-1]

write(new_reg8, file = "Results/Tables/new_reg8.tex")






##############################
## Regression 1a - Main OLS ##
##############################
m1 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene., 
         weights = TOTVOTE.18.gene.,
         data = DT)

m2 <- lm(p6n.pct.18.gene. ~ gas.tax.dollars + avg.gas.price, 
         weights = TOTVOTE.18.gene.,
         data = DT)

m3 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + gas.tax.dollars + avg.gas.price, 
         weights = TOTVOTE.18.gene.,
         data = DT)

m4 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + gas.tax.dollars + 
           avg.gas.price + white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density, 
         weights = TOTVOTE.18.gene.,
         data = DT)

m5 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + gas.tax.dollars + 
           avg.gas.price + white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
         weights = TOTVOTE.18.gene.,
         data = DT)


m6 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + gas.tax.dollars + pct.vmiles +
           avg.gas.price + white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
         weights = TOTVOTE.18.gene.,
         data = DT)


## The following two methods are identical and should result in the same standard errors as VCE(robust) in STATA
m1_robust_se <- sqrt(diag(vcovHC(m1, type="HC1")))
m2_robust_se <- sqrt(diag(vcovHC(m2, type="HC1")))
m3_robust_se <- sqrt(diag(vcovHC(m3, type="HC1")))
m4_robust_se <- sqrt(diag(vcovHC(m4, type="HC1")))
m5_robust_se <- sqrt(diag(vcovHC(m5, type="HC1")))
m6_robust_se <- sqrt(diag(vcovHC(m6, type="HC1")))


m1_robust_se <- coeftest(m1, vcov =vcovHC(m1, type = "HC1"))[,2]
m2_robust_se <- coeftest(m2, vcov =vcovHC(m2, type = "HC1"))[,2]
m3_robust_se <- coeftest(m3, vcov =vcovHC(m3, type = "HC1"))[,2]
m4_robust_se <- coeftest(m4, vcov =vcovHC(m4, type = "HC1"))[,2]
m5_robust_se <- coeftest(m5, vcov =vcovHC(m5, type = "HC1"))[,2]
m6_robust_se <- coeftest(m6, vcov =vcovHC(m6, type = "HC1"))[,2]

setwd("/Users/Epste/Dropbox/Gas Tax Repeal Draft/")

reg_table1 <- stargazer(m1, m2, m3, m4, m5, m6,
                        type = "latex",
                        omit.stat = c("adj.rsq", "f", "ser"),
                        # add.lines=list(c('County fixed effects', 'Yes','No')
                        omit = "county",
                        se = list(m1_robust_se, m2_robust_se,  m3_robust_se,  m4_robust_se,  m5_robust_se, m6_robust_se),
                        title = "Simple Regressions",
                        column.labels = c("Politics", "Incidence", "+ Politics", "+ Census", "+ County FEs", "+ \\% vehicle miles"),
                        order = c("liberal.index.pct.18.gene.", "gas.tax.dollars", "pct.vmiles", "avg.gas.price", "white", "male", "bachelor", "log.income", "log.density"),
                        covariate.labels  = c("Liberal Index", "Annual Gas Tax (weekdays)", "\\% of weekly miles in vehicle", "average gas price",
                                              "white", "Male", "4-yr degree", "Log Income", "Log Density"),
                        column.sep.width = "1pt",
                        font.size = "normalsize",
                        dep.var.labels = "Percent 0:1 voting no on Prop 6 (Supportive of Gas Tax)"
)

reg_table1 <- reg_table1[8:length(reg_table1)-1]

write(reg_table1, file = "Results/Tables/main_regression_a.tex")

rm(m, m1, m2, m3, m4, m5, m6, m1_robust_se, m2_robust_se, m3_robust_se, m4_robust_se, m5_robust_se, m6_robust_se, m_robust_se)
#############################



##############################
## Regression 1b - Main OLS ##
##############################
m1 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene., 
         weights = TOTVOTE.18.gene.,
         data = DT)

m2 <- lm(p6n.pct.18.gene. ~ annual.incidence.dollars + avg.gas.price, 
         weights = TOTVOTE.18.gene.,
         data = DT)

m3 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + annual.incidence.dollars + avg.gas.price, 
         weights = TOTVOTE.18.gene.,
         data = DT)

m4 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + annual.incidence.dollars + 
           avg.gas.price + white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density, 
         weights = TOTVOTE.18.gene.,
         data = DT)

m5 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + annual.incidence.dollars + 
           avg.gas.price + white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
         weights = TOTVOTE.18.gene.,
         data = DT)


m6 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + annual.incidence.dollars + pct.vmiles +
           avg.gas.price + white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
         weights = TOTVOTE.18.gene.,
         data = DT)



m1_robust_se <- sqrt(diag(vcovHC(m1, type="HC1")))
m2_robust_se <- sqrt(diag(vcovHC(m2, type="HC1")))
m3_robust_se <- sqrt(diag(vcovHC(m3, type="HC1")))
m4_robust_se <- sqrt(diag(vcovHC(m4, type="HC1")))
m5_robust_se <- sqrt(diag(vcovHC(m5, type="HC1")))
m6_robust_se <- sqrt(diag(vcovHC(m6, type="HC1")))



setwd("/Users/Epste/Dropbox/Gas Tax Repeal Draft/")

reg_table1 <- stargazer(m1, m2, m3, m4, m5, m6,
                        type = "latex", omit.stat = c("adj.rsq", "f", "ser"), 
                        omit = "county",
                        se = list(m1_robust_se, m2_robust_se,  m3_robust_se,  m4_robust_se,  m5_robust_se, m6_robust_se),
                        title = "Simple Regressions",
                        column.labels = c("Politics", "Incidence", "+ Politics", "+ Census", "+ County FEs", "+ \\% vehicle miles"),
                        order = c("liberal.index.pct.18.gene.", "annual.incidence.dollars", "pct.vmiles", "avg.gas.price", "white", "male", "bachelor", "log.income", "log.density"),
                        covariate.labels  = c("Liberal Index", "RRAA Annual Tax Incidence", "\\% of weekly miles in vehicle", "average gas price",
                                              "white", "Male", "4-yr degree", "Log Income", "Log Density"),
                        column.sep.width = "1pt",
                        font.size = "normalsize",
                        dep.var.labels = "Percent 0:1 voting no on Prop 6 (Supportive of Gas Tax)"
)

reg_table1 <- reg_table1[8:length(reg_table1)-1]

write(reg_table1, file = "Results/Tables/main_regression_b.tex")

rm(m, m1, m2, m3, m4, m5, m6, m1_robust_se, m2_robust_se, m3_robust_se, m4_robust_se, m5_robust_se, m6_robust_se, m_robust_se)
#############################



##############################
## Regression 1c - Main OLS ##
##############################
m1 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene., 
         weights = TOTVOTE.18.gene.,
         data = DT)

m2 <- lm(p6n.pct.18.gene. ~ annual.fee.dollars + gas.tax.dollars + avg.gas.price, 
         weights = TOTVOTE.18.gene.,
         data = DT)

m3 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + annual.fee.dollars + gas.tax.dollars + avg.gas.price, 
         weights = TOTVOTE.18.gene.,
         data = DT)

m4 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + annual.fee.dollars + gas.tax.dollars + 
           avg.gas.price + white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density, 
         weights = TOTVOTE.18.gene.,
         data = DT)

m5 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + annual.fee.dollars + gas.tax.dollars + 
           avg.gas.price + white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
         weights = TOTVOTE.18.gene.,
         data = DT)


m6 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + annual.fee.dollars + gas.tax.dollars + pct.vmiles +
           avg.gas.price + white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
         weights = TOTVOTE.18.gene.,
         data = DT)



m1_robust_se <- sqrt(diag(vcovHC(m1, type="HC1")))
m2_robust_se <- sqrt(diag(vcovHC(m2, type="HC1")))
m3_robust_se <- sqrt(diag(vcovHC(m3, type="HC1")))
m4_robust_se <- sqrt(diag(vcovHC(m4, type="HC1")))
m5_robust_se <- sqrt(diag(vcovHC(m5, type="HC1")))
m6_robust_se <- sqrt(diag(vcovHC(m6, type="HC1")))



setwd("/Users/Epste/Dropbox/Gas Tax Repeal Draft/")

reg_table1 <- stargazer(m1, m2, m3, m4, m5, m6,
                        type = "latex", omit.stat = c("adj.rsq", "f", "ser"), 
                        omit = "county",
                        se = list(m1_robust_se, m2_robust_se,  m3_robust_se,  m4_robust_se,  m5_robust_se, m6_robust_se),
                        title = "Simple Regressions",
                        column.labels = c("Politics", "Incidence", "+ Politics", "+ Census", "+ County FEs", "+ \\% vehicle miles"),
                        order = c("liberal.index.pct.18.gene.", "annual.fee.dollars", "gas.tax.dollars", "pct.vmiles", "avg.gas.price", "white", "male", "bachelor", "log.income", "log.density"),
                        covariate.labels  = c("Liberal Index", "Annual Fee Increase", "Annual Gas Tax (weekdays)", "\\% of weekly miles in vehicle", "average gas price",
                                              "white", "Male", "4-yr degree", "Log Income", "Log Density"),
                        column.sep.width = "1pt",
                        font.size = "normalsize",
                        dep.var.labels = "Percent 0:1 voting no on Prop 6 (Supportive of Gas Tax)"
)

reg_table1 <- reg_table1[8:length(reg_table1)-1]

write(reg_table1, file = "Results/Tables/main_regression_c.tex")

rm(m, m1, m2, m3, m4, m5, m6, m1_robust_se, m2_robust_se, m3_robust_se, m4_robust_se, m5_robust_se, m6_robust_se, m_robust_se)
#############################





###########################
##OLS w/o Gas prices - A ##
###########################

m1 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene., 
         weights = TOTVOTE.18.gene.,
         data = DT)

m2 <- lm(p6n.pct.18.gene. ~ gas.tax.dollars, 
         weights = TOTVOTE.18.gene.,
         data = DT)

m3 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + gas.tax.dollars, 
         weights = TOTVOTE.18.gene.,
         data = DT)


m4 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + gas.tax.dollars + 
           white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density,
         weights = TOTVOTE.18.gene.,
         data = DT)

m5 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + gas.tax.dollars + 
           white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
         weights = TOTVOTE.18.gene.,
         data = DT)

m6 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + gas.tax.dollars + pct.vmiles +
           white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
         weights = TOTVOTE.18.gene.,
         data = DT)

m7 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + gas.tax.dollars +
           white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + avg.gas.price + as.factor(county),
         weights = TOTVOTE.18.gene.,
         data = DT)

m1_robust_se <- sqrt(diag(vcovHC(m1, type="HC1")))
m2_robust_se <- sqrt(diag(vcovHC(m2, type="HC1")))
m3_robust_se <- sqrt(diag(vcovHC(m3, type="HC1")))
m4_robust_se <- sqrt(diag(vcovHC(m4, type="HC1")))
m5_robust_se <- sqrt(diag(vcovHC(m5, type="HC1")))
m6_robust_se <- sqrt(diag(vcovHC(m6, type="HC1")))
m7_robust_se <- sqrt(diag(vcovHC(m7, type="HC1")))


setwd("/Users/Epste/Dropbox/Gas Tax Repeal Draft/")

reg_no_gas_price <- stargazer(m1, m2, m3, m4, m5, m6, m7,
                              type = "latex", omit.stat = c("adj.rsq", "f", "ser"),
                              # type = "text",
                              omit = "county",
                              se = list(m1_robust_se, m2_robust_se,  m3_robust_se,  m4_robust_se,  m5_robust_se, m6_robust_se, m7_robust_se),
                              title = "Simple Regressions",
                              column.labels = c("Politics", "Incidence", "+ Politics", "+ Census", "+ County FEs", "+ \\% miles in Vehicle", "(5) + Gas Price"),
                              order = c("liberal.index.pct.18.gene.", "gas.tax.dollars", "pct.vmiles", "white", "male", "bachelor", "log.income", "log.density"),
                              covariate.labels  = c("Liberal Index", "Annual Gas Tax (weekdays)", "Mile share in vehicle",
                                                    "white", "Male", "4-yr degree", "Log Income", "Log Density"),
                              # out = "Results/Tables/simple_regression.tex",
                              column.sep.width = "1pt",
                              font.size = "normalsize",
                              dep.var.labels = "Percent 0:1 voting no on Prop 6 (Supportive of Gas Tax)"
)

reg_no_gas_price <- reg_no_gas_price[8:length(reg_no_gas_price)-1]

write(reg_no_gas_price, file = "Results/Tables/reg_no_gas_price_a.tex")

## Residual charting
#####
# This section must be rewritten if we want to use a different model
residual <- filter(DT, !is.na(liberal.index.pct.18.gene.), !is.na(p6n.pct.18.gene.), !is.na(gas.tax.dollars),
                   !is.na(white), !is.na(male), !is.na(bachelor), !is.na(log.income), !is.na(log.density), !is.na(county)) %>% 
  select(GEOID) %>% 
  mutate(residual = m4$residuals)


DT_residual <- inner_join(DT, residual, by="GEOID") %>% 
  mutate(gas_price_bin = as.factor(ntile(avg.gas.price, 10)))


ggplot(DT_residual, aes(x=gas_price_bin, y = residual)) + geom_boxplot() +
  labs(x = "Gas Price Bin", y = "Residual")
ggsave("/Users/Epste/Dropbox/Gas Tax Repeal Draft/Results/Figures/gas_price_residuals_a.png")
rm(m, m1, m2, m3, m4, m5, m6, m7, m1_robust_se, m2_robust_se, m3_robust_se, m4_robust_se, m5_robust_se, m6_robust_se, m7_robust_se, m_robust_se)

#######################





###########################
##OLS w/o Gas prices - B ##
###########################

m1 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene., 
         weights = TOTVOTE.18.gene.,
         data = DT)

m2 <- lm(p6n.pct.18.gene. ~ annual.incidence.dollars, 
         weights = TOTVOTE.18.gene.,
         data = DT)

m3 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + annual.incidence.dollars, 
         weights = TOTVOTE.18.gene.,
         data = DT)


m4 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + annual.incidence.dollars + 
           white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density,
         weights = TOTVOTE.18.gene.,
         data = DT)

m5 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + annual.incidence.dollars + 
           white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
         weights = TOTVOTE.18.gene.,
         data = DT)

m6 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + annual.incidence.dollars + pct.vmiles +
           white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
         weights = TOTVOTE.18.gene.,
         data = DT)

m7 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + annual.incidence.dollars +
           white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + avg.gas.price + as.factor(county),
         weights = TOTVOTE.18.gene.,
         data = DT)

m1_robust_se <- sqrt(diag(vcovHC(m1, type="HC1")))
m2_robust_se <- sqrt(diag(vcovHC(m2, type="HC1")))
m3_robust_se <- sqrt(diag(vcovHC(m3, type="HC1")))
m4_robust_se <- sqrt(diag(vcovHC(m4, type="HC1")))
m5_robust_se <- sqrt(diag(vcovHC(m5, type="HC1")))
m6_robust_se <- sqrt(diag(vcovHC(m6, type="HC1")))
m7_robust_se <- sqrt(diag(vcovHC(m7, type="HC1")))


setwd("/Users/Epste/Dropbox/Gas Tax Repeal Draft/")

reg_no_gas_price <- stargazer(m1, m2, m3, m4, m5, m6, m7,
                              type = "latex", omit.stat = c("adj.rsq", "f", "ser"),
                              # type = "text",
                              omit = "county",
                              se = list(m1_robust_se, m2_robust_se,  m3_robust_se,  m4_robust_se,  m5_robust_se, m6_robust_se, m7_robust_se),
                              title = "Simple Regressions",
                              column.labels = c("Politics", "Incidence", "+ Politics", "+ Census", "+ County FEs", "+ \\% miles in Vehicle", "(5) + Gas Price"),
                              order = c("liberal.index.pct.18.gene.", "annual.incidence.dollars", "pct.vmiles", "white", "male", "bachelor", "log.income", "log.density"),
                              covariate.labels  = c("Liberal Index", "RRAA Annual Incidence", "Mile share in vehicle",
                                                    "white", "Male", "4-yr degree", "Log Income", "Log Density"),
                              # out = "Results/Tables/simple_regression.tex",
                              column.sep.width = "1pt",
                              font.size = "normalsize",
                              dep.var.labels = "Percent 0:1 voting no on Prop 6 (Supportive of Gas Tax)"
)

reg_no_gas_price <- reg_no_gas_price[8:length(reg_no_gas_price)-1]

write(reg_no_gas_price, file = "Results/Tables/reg_no_gas_price_b.tex")

## Residual charting
#####
# This section must be rewritten if we want to use a different model
residual <- filter(DT, !is.na(liberal.index.pct.18.gene.), !is.na(p6n.pct.18.gene.), !is.na(annual.incidence.dollars),
                   !is.na(white), !is.na(male), !is.na(bachelor), !is.na(log.income), !is.na(log.density), !is.na(county)) %>% 
  select(GEOID) %>% 
  mutate(residual = m4$residuals)


DT_residual <- inner_join(DT, residual, by="GEOID") %>% 
  mutate(gas_price_bin = as.factor(ntile(avg.gas.price, 10)))


ggplot(DT_residual, aes(x=gas_price_bin, y = residual)) + geom_boxplot() +
  labs(x = "Gas Price Bin", y = "Residual")
ggsave("/Users/Epste/Dropbox/Gas Tax Repeal Draft/Results/Figures/gas_price_residuals_b.png")
rm(m, m1, m2, m3, m4, m5, m6, m7, m1_robust_se, m2_robust_se, m3_robust_se, m4_robust_se, m5_robust_se, m6_robust_se, m7_robust_se, m_robust_se)

#######################





###########################
##OLS w/o Gas prices - C ##
###########################

m1 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene., 
         weights = TOTVOTE.18.gene.,
         data = DT)

m2 <- lm(p6n.pct.18.gene. ~ gas.tax.dollars + annual.fee.dollars, 
         weights = TOTVOTE.18.gene.,
         data = DT)

m3 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + gas.tax.dollars + annual.fee.dollars, 
         weights = TOTVOTE.18.gene.,
         data = DT)


m4 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + gas.tax.dollars + annual.fee.dollars + 
           white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density,
         weights = TOTVOTE.18.gene.,
         data = DT)

m5 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + gas.tax.dollars + annual.fee.dollars + 
           white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
         weights = TOTVOTE.18.gene.,
         data = DT)

m6 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + gas.tax.dollars + annual.fee.dollars + pct.vmiles +
           white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
         weights = TOTVOTE.18.gene.,
         data = DT)

m7 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + gas.tax.dollars + annual.fee.dollars +
           white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + avg.gas.price + as.factor(county),
         weights = TOTVOTE.18.gene.,
         data = DT)

m1_robust_se <- sqrt(diag(vcovHC(m1, type="HC1")))
m2_robust_se <- sqrt(diag(vcovHC(m2, type="HC1")))
m3_robust_se <- sqrt(diag(vcovHC(m3, type="HC1")))
m4_robust_se <- sqrt(diag(vcovHC(m4, type="HC1")))
m5_robust_se <- sqrt(diag(vcovHC(m5, type="HC1")))
m6_robust_se <- sqrt(diag(vcovHC(m6, type="HC1")))
m7_robust_se <- sqrt(diag(vcovHC(m7, type="HC1")))


setwd("/Users/Epste/Dropbox/Gas Tax Repeal Draft/")

reg_no_gas_price <- stargazer(m1, m2, m3, m4, m5, m6, m7,
                              type = "latex", omit.stat = c("adj.rsq", "f", "ser"),
                              # type = "text",
                              omit = "county",
                              se = list(m1_robust_se, m2_robust_se,  m3_robust_se,  m4_robust_se,  m5_robust_se, m6_robust_se, m7_robust_se),
                              title = "Simple Regressions",
                              column.labels = c("Politics", "Incidence", "+ Politics", "+ Census", "+ County FEs", "+ \\% miles in Vehicle", "(5) + Gas Price"),
                              order = c("liberal.index.pct.18.gene.", "gas.tax.dollars", "annual.fee.dollars", "pct.vmiles", "white", "male", "bachelor", "log.income", "log.density"),
                              covariate.labels  = c("Liberal Index", "Annual Gas Tax (weekdays)", "RRAA Annual Fees", "Mile share in vehicle",
                                                    "white", "Male", "4-yr degree", "Log Income", "Log Density"),
                              # out = "Results/Tables/simple_regression.tex",
                              column.sep.width = "1pt",
                              font.size = "normalsize",
                              dep.var.labels = "Percent 0:1 voting no on Prop 6 (Supportive of Gas Tax)"
)

reg_no_gas_price <- reg_no_gas_price[8:length(reg_no_gas_price)-1]

write(reg_no_gas_price, file = "Results/Tables/reg_no_gas_price_c.tex")

## Residual charting
#####
# This section must be rewritten if we want to use a different model
residual <- filter(DT, !is.na(liberal.index.pct.18.gene.), !is.na(p6n.pct.18.gene.), !is.na(gas.tax.dollars), !is.na(annual.fee.dollars),
                   !is.na(white), !is.na(male), !is.na(bachelor), !is.na(log.income), !is.na(log.density), !is.na(county)) %>% 
  select(GEOID) %>% 
  mutate(residual = m4$residuals)


DT_residual <- inner_join(DT, residual, by="GEOID") %>% 
  mutate(gas_price_bin = as.factor(ntile(avg.gas.price, 10)))


ggplot(DT_residual, aes(x=gas_price_bin, y = residual)) + geom_boxplot() +
  labs(x = "Gas Price Bin", y = "Residual")
ggsave("/Users/Epste/Dropbox/Gas Tax Repeal Draft/Results/Figures/gas_price_residuals_c.png")
rm(m, m1, m2, m3, m4, m5, m6, m7, m1_robust_se, m2_robust_se, m3_robust_se, m4_robust_se, m5_robust_se, m6_robust_se, m7_robust_se, m_robust_se)

#######################





###############################################
## Regression split by quartile, liberal - A ##
###############################################

m <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + gas.tax.dollars + 
          avg.gas.price + white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
        weights = TOTVOTE.18.gene.,
        data = DT)

m1 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + gas.tax.dollars + 
           avg.gas.price +  white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
         weights = TOTVOTE.18.gene.,
         data = filter(DT, 
                       liberal.index.pct.18.gene. < quantile(DT$liberal.index.pct.18.gene., .25)
         )
)

m2 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + gas.tax.dollars + 
           avg.gas.price +  white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
         weights = TOTVOTE.18.gene.,
         data = filter(DT, 
                       liberal.index.pct.18.gene. > quantile(DT$liberal.index.pct.18.gene., .25),
                       liberal.index.pct.18.gene.< quantile(DT$liberal.index.pct.18.gene., .5)
         )
)




m3 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + gas.tax.dollars + 
           avg.gas.price + white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
         weights = TOTVOTE.18.gene.,
         data = filter(DT, 
                       liberal.index.pct.18.gene. > quantile(DT$liberal.index.pct.18.gene., .5),
                       liberal.index.pct.18.gene.< quantile(DT$liberal.index.pct.18.gene., .75)
         )
)




m4 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + gas.tax.dollars +
           avg.gas.price + white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
         weights = TOTVOTE.18.gene.,
         data = filter(DT, 
                       liberal.index.pct.18.gene. > quantile(DT$liberal.index.pct.18.gene., .75)
         )
)




m1_robust_se <- sqrt(diag(vcovHC(m1, type="HC1")))
m2_robust_se <- sqrt(diag(vcovHC(m2, type="HC1")))
m3_robust_se <- sqrt(diag(vcovHC(m3, type="HC1")))
m4_robust_se <- sqrt(diag(vcovHC(m4, type="HC1")))
m_robust_se <- sqrt(diag(vcovHC(m, type="HC1")))




setwd("/Users/Epste/Dropbox/Gas Tax Repeal Draft/")

reg_quartile <- stargazer(m, m1, m2, m3, m4,
                          type = "latex", omit.stat = c("adj.rsq", "f", "ser"), 
                          omit = "county",
                          se = list(m_robust_se, m1_robust_se, m2_robust_se,  m3_robust_se,  m4_robust_se),
                          title = "Quartile Regression",
                          column.labels = c("full", "q1", "q2", "q3", "q4"),
                          order = c("liberal.index.pct.18.gene.", "gas.tax.dollars", "avg.gas.price", "white", "male", "bachelor", "log.income", "log.density"),
                          covariate.labels  = c("Liberal Index", "Annual Gas Tax (weekdays)", "average gas price",
                                                "white", "Male", "4-yr degree", "Log Income", "Log Density"),
                          # out = "Results/Tables/simple_regression.tex",
                          column.sep.width = "1pt",
                          font.size = "normalsize",
                          dep.var.labels = "Percent 0:1 voting no on Prop 6 (Supportive of Gas Tax) - Quartiles by Liberal"
)

reg_quartile <- reg_quartile[8:length(reg_quartile)-1]

write(reg_quartile, file = "Results/Tables/reg_quartile_lib_a.tex")


rm(m, m1, m2, m3, m4, m5, m6, m1_robust_se, m2_robust_se, m3_robust_se, m4_robust_se, m5_robust_se, m6_robust_se, m_robust_se)
#############




###############################################
## Regression split by quartile, liberal - B ##
###############################################

m <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + annual.incidence.dollars + 
          avg.gas.price + white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
        weights = TOTVOTE.18.gene.,
        data = DT)

m1 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + annual.incidence.dollars + 
           avg.gas.price +  white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
         weights = TOTVOTE.18.gene.,
         data = filter(DT, 
                       liberal.index.pct.18.gene. < quantile(DT$liberal.index.pct.18.gene., .25)
         )
)

m2 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + annual.incidence.dollars + 
           avg.gas.price +  white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
         weights = TOTVOTE.18.gene.,
         data = filter(DT, 
                       liberal.index.pct.18.gene. > quantile(DT$liberal.index.pct.18.gene., .25),
                       liberal.index.pct.18.gene.< quantile(DT$liberal.index.pct.18.gene., .5)
         )
)




m3 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + annual.incidence.dollars + 
           avg.gas.price + white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
         weights = TOTVOTE.18.gene.,
         data = filter(DT, 
                       liberal.index.pct.18.gene. > quantile(DT$liberal.index.pct.18.gene., .5),
                       liberal.index.pct.18.gene.< quantile(DT$liberal.index.pct.18.gene., .75)
         )
)




m4 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + annual.incidence.dollars +
           avg.gas.price + white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
         weights = TOTVOTE.18.gene.,
         data = filter(DT, 
                       liberal.index.pct.18.gene. > quantile(DT$liberal.index.pct.18.gene., .75)
         )
)




m1_robust_se <- sqrt(diag(vcovHC(m1, type="HC1")))
m2_robust_se <- sqrt(diag(vcovHC(m2, type="HC1")))
m3_robust_se <- sqrt(diag(vcovHC(m3, type="HC1")))
m4_robust_se <- sqrt(diag(vcovHC(m4, type="HC1")))
m_robust_se <- sqrt(diag(vcovHC(m, type="HC1")))




setwd("/Users/Epste/Dropbox/Gas Tax Repeal Draft/")

reg_quartile <- stargazer(m, m1, m2, m3, m4,
                          # type = "text",
                          type = "latex",
                          omit.stat = c("adj.rsq", "f", "ser"), 
                          omit = "county",
                          se = list(m_robust_se, m1_robust_se, m2_robust_se,  m3_robust_se,  m4_robust_se),
                          title = "Quartile Regression",
                          column.labels = c("full", "q1", "q2", "q3", "q4"),
                          order = c("liberal.index.pct.18.gene.", "annual.incidence.dollars", "avg.gas.price", "white", "male", "bachelor", "log.income", "log.density"),
                          covariate.labels  = c("Liberal Index", "RRAA Annual Incidence", "average gas price",
                                                "white", "Male", "4-yr degree", "Log Income", "Log Density"),
                          # out = "Results/Tables/simple_regression.tex",
                          column.sep.width = "1pt",
                          font.size = "normalsize",
                          dep.var.labels = "Percent 0:1 voting no on Prop 6 (Supportive of Gas Tax) - Quartiles by Liberal"
)

reg_quartile <- reg_quartile[8:length(reg_quartile)-1]

write(reg_quartile, file = "Results/Tables/reg_quartile_lib_b.tex")


rm(m, m1, m2, m3, m4, m5, m6, m1_robust_se, m2_robust_se, m3_robust_se, m4_robust_se, m5_robust_se, m6_robust_se, m_robust_se)
#############




###############################################
## Regression split by quartile, liberal - C ##
###############################################

m <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + ad.per.household.TotalCost + gas.tax.dollars + annual.fee.dollars + 
          avg.gas.price + white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
        weights = TOTVOTE.18.gene.,
        data = DT)

m1 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + ad.per.household.TotalCost + gas.tax.dollars + annual.fee.dollars + 
           avg.gas.price +  white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
         weights = TOTVOTE.18.gene.,
         data = filter(DT, 
                       liberal.index.pct.18.gene. < quantile(DT$liberal.index.pct.18.gene., .25)
         )
)

m2 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + ad.per.household.TotalCost + gas.tax.dollars + annual.fee.dollars + 
           avg.gas.price +  white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
         weights = TOTVOTE.18.gene.,
         data = filter(DT, 
                       liberal.index.pct.18.gene. > quantile(DT$liberal.index.pct.18.gene., .25),
                       liberal.index.pct.18.gene.< quantile(DT$liberal.index.pct.18.gene., .5)
         )
)




m3 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + ad.per.household.TotalCost + gas.tax.dollars + annual.fee.dollars + 
           avg.gas.price + white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
         weights = TOTVOTE.18.gene.,
         data = filter(DT, 
                       liberal.index.pct.18.gene. > quantile(DT$liberal.index.pct.18.gene., .5),
                       liberal.index.pct.18.gene.< quantile(DT$liberal.index.pct.18.gene., .75)
         )
)




m4 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + ad.per.household.TotalCost + gas.tax.dollars + annual.fee.dollars +
           avg.gas.price + white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
         weights = TOTVOTE.18.gene.,
         data = filter(DT, 
                       liberal.index.pct.18.gene. > quantile(DT$liberal.index.pct.18.gene., .75)
         )
)




m1_robust_se <- sqrt(diag(vcovHC(m1, type="HC1")))
m2_robust_se <- sqrt(diag(vcovHC(m2, type="HC1")))
m3_robust_se <- sqrt(diag(vcovHC(m3, type="HC1")))
m4_robust_se <- sqrt(diag(vcovHC(m4, type="HC1")))
m_robust_se <- sqrt(diag(vcovHC(m, type="HC1")))




setwd("/Users/Epste/Dropbox/Gas Tax Repeal Draft/")

reg_quartile <- stargazer(m, m1, m2, m3, m4,
                          type = "latex",
                          # type = "text",
                          omit.stat = c("adj.rsq", "f", "ser"), 
                          omit = "county",
                          se = list(m_robust_se, m1_robust_se, m2_robust_se,  m3_robust_se,  m4_robust_se),
                          title = "Quartile Regression",
                          column.labels = c("full", "q1", "q2", "q3", "q4"),
                          order = c("liberal.index.pct.18.gene.", "ad.per.household.TotalCost", "gas.tax.dollars", "annual.fee.dollars", "avg.gas.price", "white", "male", "bachelor", "log.income", "log.density"),
                          covariate.labels  = c("Liberal Index", "Caltrans Funds (per household)", "Annual Gas Tax (weekdays)", "RRAA Annual Fee Increase", "average gas price",
                                                "white", "Male", "4-yr degree", "Log Income", "Log Density"),
                          # out = "Results/Tables/simple_regression.tex",
                          column.sep.width = "1pt",
                          font.size = "normalsize",
                          dep.var.labels = "Percent 0:1 voting no on Prop 6 (Supportive of Gas Tax) - Quartiles by Liberal"
)

reg_quartile <- reg_quartile[8:length(reg_quartile)-1]

write(reg_quartile, file = "Results/Tables/reg_quartile_lib_c.tex")


rm(m, m1, m2, m3, m4, m5, m6, m1_robust_se, m2_robust_se, m3_robust_se, m4_robust_se, m5_robust_se, m6_robust_se, m_robust_se)
#############



########################################################
## Regression split by quartile, gallons consumed - A ##
########################################################

m <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + gas.tax.dollars + 
          avg.gas.price + white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
        weights = TOTVOTE.18.gene.,
        data = DT)

m1 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + gas.tax.dollars + 
           avg.gas.price +  white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
         weights = TOTVOTE.18.gene.,
         data = filter(DT, 
                       weekday.gallons < quantile(DT$weekday.gallons, .25, na.rm = T)
         )
)

m2 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + gas.tax.dollars + 
           avg.gas.price +  white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
         weights = TOTVOTE.18.gene.,
         data = filter(DT, 
                       weekday.gallons > quantile(DT$weekday.gallons, .25, na.rm = T),
                       weekday.gallons< quantile(DT$weekday.gallons, .5, na.rm = T)
         )
)




m3 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + gas.tax.dollars + 
           avg.gas.price + white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
         weights = TOTVOTE.18.gene.,
         data = filter(DT, 
                       weekday.gallons > quantile(DT$weekday.gallons, .5, na.rm = T),
                       weekday.gallons < quantile(DT$weekday.gallons, .75, na.rm = T)
         )
)




m4 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + gas.tax.dollars +
           avg.gas.price + white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
         weights = TOTVOTE.18.gene.,
         data = filter(DT, 
                       weekday.gallons > quantile(DT$weekday.gallons, .75, na.rm = T)
         )
)





m1_robust_se <- sqrt(diag(vcovHC(m1, type="HC1")))
m2_robust_se <- sqrt(diag(vcovHC(m2, type="HC1")))
m3_robust_se <- sqrt(diag(vcovHC(m3, type="HC1")))
m4_robust_se <- sqrt(diag(vcovHC(m4, type="HC1")))
m_robust_se <- sqrt(diag(vcovHC(m, type="HC1")))



setwd("/Users/Epste/Dropbox/Gas Tax Repeal Draft/")

reg_quartile_gallons <- stargazer(m, m1, m2, m3, m4,
                                  type = "latex", omit.stat = c("adj.rsq", "f", "ser"), 
                                  omit = "county",
                                  se = list(m_robust_se, m1_robust_se, m2_robust_se,  m3_robust_se,  m4_robust_se),
                                  title = "Quartile Regression",
                                  column.labels = c("full", "q1", "q2", "q3", "q4"),
                                  order = c("liberal.index.pct.18.gene.", "gas.tax.dollars", "avg.gas.price", "white", "male", "bachelor", "log.income", "log.density"),
                                  covariate.labels  = c("Liberal Index", "Annual Gas Tax (weekdays)", "average gas price",
                                                        "white", "Male", "4-yr degree", "Log Income", "Log Density"),
                                  # out = "Results/Tables/simple_regression.tex"
                                  column.sep.width = "1pt",
                                  font.size = "normalsize",
                                  dep.var.labels = "Percent 0:1 voting no on Prop 6 (Supportive of Gas Tax) - Quartiles by gas consumed"
)

reg_quartile_gallons <- reg_quartile_gallons[8:length(reg_quartile_gallons)-1]

write(reg_quartile_gallons, file = "Results/Tables/reg_quartile_gallons_a.tex")


rm(m, m1, m2, m3, m4, m5, m6, m1_robust_se, m2_robust_se, m3_robust_se, m4_robust_se, m5_robust_se, m6_robust_se, m_robust_se)

##############





########################################################
## Regression split by quartile, gallons consumed - B ##
########################################################

m <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + annual.incidence.dollars + 
          avg.gas.price + white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
        weights = TOTVOTE.18.gene.,
        data = DT)

m1 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + annual.incidence.dollars + 
           avg.gas.price +  white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
         weights = TOTVOTE.18.gene.,
         data = filter(DT, 
                       weekday.gallons < quantile(DT$weekday.gallons, .25, na.rm = T)
         )
)

m2 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + annual.incidence.dollars + 
           avg.gas.price +  white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
         weights = TOTVOTE.18.gene.,
         data = filter(DT, 
                       weekday.gallons > quantile(DT$weekday.gallons, .25, na.rm = T),
                       weekday.gallons< quantile(DT$weekday.gallons, .5, na.rm = T)
         )
)


m3 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + annual.incidence.dollars + 
           avg.gas.price + white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
         weights = TOTVOTE.18.gene.,
         data = filter(DT, 
                       weekday.gallons > quantile(DT$weekday.gallons, .5, na.rm = T),
                       weekday.gallons < quantile(DT$weekday.gallons, .75, na.rm = T)
         )
)


m4 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + annual.incidence.dollars +
           avg.gas.price + white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
         weights = TOTVOTE.18.gene.,
         data = filter(DT, 
                       weekday.gallons > quantile(DT$weekday.gallons, .75, na.rm = T)
         )
)


m1_robust_se <- sqrt(diag(vcovHC(m1, type="HC1")))
m2_robust_se <- sqrt(diag(vcovHC(m2, type="HC1")))
m3_robust_se <- sqrt(diag(vcovHC(m3, type="HC1")))
m4_robust_se <- sqrt(diag(vcovHC(m4, type="HC1")))
m_robust_se <- sqrt(diag(vcovHC(m, type="HC1")))



setwd("/Users/Epste/Dropbox/Gas Tax Repeal Draft/")

reg_quartile_gallons <- stargazer(m, m1, m2, m3, m4,
                                  type = "latex", omit.stat = c("adj.rsq", "f", "ser"), 
                                  omit = "county",
                                  se = list(m_robust_se, m1_robust_se, m2_robust_se,  m3_robust_se,  m4_robust_se),
                                  title = "Quartile Regression",
                                  column.labels = c("full", "q1", "q2", "q3", "q4"),
                                  order = c("liberal.index.pct.18.gene.", "annual.incidence.dollars", "avg.gas.price", "white", "male", "bachelor", "log.income", "log.density"),
                                  covariate.labels  = c("Liberal Index", "RRAA Annual Incidence", "average gas price",
                                                        "white", "Male", "4-yr degree", "Log Income", "Log Density"),
                                  # out = "Results/Tables/simple_regression.tex"
                                  column.sep.width = "1pt",
                                  font.size = "normalsize",
                                  dep.var.labels = "Percent 0:1 voting no on Prop 6 (Supportive of Gas Tax) - Quartiles by gas consumed"
)

reg_quartile_gallons <- reg_quartile_gallons[8:length(reg_quartile_gallons)-1]

write(reg_quartile_gallons, file = "Results/Tables/reg_quartile_gallons_b.tex")


rm(m, m1, m2, m3, m4, m5, m6, m1_robust_se, m2_robust_se, m3_robust_se, m4_robust_se, m5_robust_se, m6_robust_se, m_robust_se)

##############





########################################################
## Regression split by quartile, gallons consumed - C ##
########################################################

m <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + gas.tax.dollars + annual.fee.dollars + 
          avg.gas.price + white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
        weights = TOTVOTE.18.gene.,
        data = DT)

m1 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + gas.tax.dollars + annual.fee.dollars + 
           avg.gas.price +  white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
         weights = TOTVOTE.18.gene.,
         data = filter(DT, 
                       weekday.gallons < quantile(DT$weekday.gallons, .25, na.rm = T)
         )
)

m2 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + gas.tax.dollars + annual.fee.dollars + 
           avg.gas.price +  white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
         weights = TOTVOTE.18.gene.,
         data = filter(DT, 
                       weekday.gallons > quantile(DT$weekday.gallons, .25, na.rm = T),
                       weekday.gallons< quantile(DT$weekday.gallons, .5, na.rm = T)
         )
)




m3 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + gas.tax.dollars + annual.fee.dollars + 
           avg.gas.price + white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
         weights = TOTVOTE.18.gene.,
         data = filter(DT, 
                       weekday.gallons > quantile(DT$weekday.gallons, .5, na.rm = T),
                       weekday.gallons < quantile(DT$weekday.gallons, .75, na.rm = T)
         )
)




m4 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + gas.tax.dollars + annual.fee.dollars +
           avg.gas.price + white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
         weights = TOTVOTE.18.gene.,
         data = filter(DT, 
                       weekday.gallons > quantile(DT$weekday.gallons, .75, na.rm = T)
         )
)





m1_robust_se <- sqrt(diag(vcovHC(m1, type="HC1")))
m2_robust_se <- sqrt(diag(vcovHC(m2, type="HC1")))
m3_robust_se <- sqrt(diag(vcovHC(m3, type="HC1")))
m4_robust_se <- sqrt(diag(vcovHC(m4, type="HC1")))
m_robust_se <- sqrt(diag(vcovHC(m, type="HC1")))



setwd("/Users/Epste/Dropbox/Gas Tax Repeal Draft/")

reg_quartile_gallons <- stargazer(m, m1, m2, m3, m4,
                                  type = "latex", omit.stat = c("adj.rsq", "f", "ser"), 
                                  omit = "county",
                                  se = list(m_robust_se, m1_robust_se, m2_robust_se,  m3_robust_se,  m4_robust_se),
                                  title = "Quartile Regression",
                                  column.labels = c("full", "q1", "q2", "q3", "q4"),
                                  order = c("liberal.index.pct.18.gene.", "gas.tax.dollars",  "annual.fee.dollars", "avg.gas.price", "white", "male", "bachelor", "log.income", "log.density"),
                                  covariate.labels  = c("Liberal Index", "Annual Gas Tax (weekdays)", "RRAA Annual Fee Increase", "average gas price",
                                                        "white", "Male", "4-yr degree", "Log Income", "Log Density"),
                                  # out = "Results/Tables/simple_regression.tex"
                                  column.sep.width = "1pt",
                                  font.size = "normalsize",
                                  dep.var.labels = "Percent 0:1 voting no on Prop 6 (Supportive of Gas Tax) - Quartiles by gas consumed"
)

reg_quartile_gallons <- reg_quartile_gallons[8:length(reg_quartile_gallons)-1]

write(reg_quartile_gallons, file = "Results/Tables/reg_quartile_gallons_c.tex")


rm(m, m1, m2, m3, m4, m5, m6, m1_robust_se, m2_robust_se, m3_robust_se, m4_robust_se, m5_robust_se, m6_robust_se, m_robust_se)

##############



###########################################################
## Regression w/o Gas prices (expanded Demographics) - A ##
###########################################################

vars <- c("race.1", "race.2", "race.3", "race.4", "race.5", "race.6", "race.7",
          "income.1", "income.2", "income.3", "income.4", "income.5", "income.6", "income.7", "income.8", "income.9", "income.10", "income.11", "income.12", "income.13", "income.14", "income.15", "income.16",
          "value.1", "value.2", "value.3", "value.4", "value.5", "value.6", "value.7", "value.8", "value.9", "value.10", "value.11", "value.12", "value.13", "value.14", "value.15", "value.16", "value.17", "value.18", "value.19", "value.20", "value.21", "value.22", "value.23", "value.24", "value.25", "value.26",
          "rooms.1", "rooms.2", "rooms.3", "rooms.4", "rooms.5", "rooms.6", "rooms.7", "rooms.8", "rooms.9",
          "edu.1", "edu.2", "edu.3", "edu.4", "edu.5", 
          "latino",
          "male",
          "owner.occupied",
          "household.size",
          "commute.1", "commute.2", "commute.3", "commute.4", "commute.5", "commute.6", "commute.7", "commute.8", "commute.9", "commute.10", "commute.11", "commute.12",
          "vehicle.hh.1", "vehicle.hh.2", "vehicle.hh.3", "vehicle.hh.4", "vehicle.hh.5")


m1 <- DT %>% recipe() %>% 
  update_role(p6n.pct.18.gene.,new_role = "outcome") %>%
  update_role(c(liberal.index.pct.18.gene.), new_role = "predictor") %>% 
  prep() %>% 
  formula() %>% 
  lm(DT,
     weights = TOTVOTE.18.gene.)



m2 <- DT %>% recipe() %>% 
  update_role(p6n.pct.18.gene. ,new_role = "outcome") %>%
  update_role(gas.tax.dollars, new_role = "predictor") %>% 
  prep() %>% 
  formula() %>% 
  lm(DT,
     weights = TOTVOTE.18.gene.)



m3 <- DT %>% recipe() %>% 
  update_role(p6n.pct.18.gene. ,new_role = "outcome") %>%
  update_role(c(gas.tax.dollars, liberal.index.pct.18.gene.), new_role = "predictor") %>% 
  prep() %>% 
  formula() %>% 
  lm(DT,
     weights = TOTVOTE.18.gene.)


m4 <- DT %>% recipe() %>% 
  update_role(p6n.pct.18.gene. ,new_role = "outcome") %>%
  update_role(c(gas.tax.dollars, liberal.index.pct.18.gene., vars, log.density), new_role = "predictor") %>% 
  prep() %>% 
  formula() %>% 
  lm(DT,
     weights = TOTVOTE.18.gene.)



m5 <- DT %>% recipe() %>% 
  update_role(p6n.pct.18.gene. ,new_role = "outcome") %>%
  update_role(c(gas.tax.dollars, liberal.index.pct.18.gene., vars, log.density, county), new_role = "predictor") %>% 
  prep() %>% 
  formula() %>% 
  lm(DT,
     weights = TOTVOTE.18.gene.)




m6 <- DT %>% recipe() %>% 
  update_role(p6n.pct.18.gene. ,new_role = "outcome") %>%
  update_role(c(gas.tax.dollars, liberal.index.pct.18.gene., vars, county, log.density, pct.vmiles), new_role = "predictor") %>% 
  prep() %>% 
  formula() %>% 
  lm(DT,
     weights = TOTVOTE.18.gene.)




m7 <- DT %>% recipe() %>% 
  update_role(p6n.pct.18.gene. ,new_role = "outcome") %>%
  update_role(c(gas.tax.dollars, liberal.index.pct.18.gene., vars, county, log.density, avg.gas.price), new_role = "predictor") %>% 
  prep() %>% 
  formula() %>% 
  lm(DT,
     weights = TOTVOTE.18.gene.)


m1_robust_se <- sqrt(diag(vcovHC(m1, type="HC1")))
m2_robust_se <- sqrt(diag(vcovHC(m2, type="HC1")))
m3_robust_se <- sqrt(diag(vcovHC(m3, type="HC1")))
m4_robust_se <- sqrt(diag(vcovHC(m4, type="HC1")))
m5_robust_se <- sqrt(diag(vcovHC(m5, type="HC1")))
m6_robust_se <- sqrt(diag(vcovHC(m6, type="HC1")))
m7_robust_se <- sqrt(diag(vcovHC(m7, type="HC1")))



reg_no_gas_price <- stargazer(m1, m2, m3, m4, m5, m6, m7,
                              type = "latex", omit.stat = c("adj.rsq", "f", "ser"),
                              # type = "text",
                              omit = c("county", vars),
                              se = list(m1_robust_se, m2_robust_se,  m3_robust_se,  m4_robust_se,  m5_robust_se, m6_robust_se, m7_robust_se),
                              # title = "Simple Regressions",
                              column.labels = c("Politics", "Incidence", "+ Politics", "+ Census", "+ County FEs", "+ \\% miles in Vehicle", "(5) + Gas Price"),
                              order = c("liberal.index.pct.18.gene.", "gas.tax.dollars", "pct.vmiles", "avg.gas.price", "log.density"),
                              covariate.labels  = c("Liberal index", "Annual Gas Tax (weekdays)", "Mile share in vehicle",
                                                    "Average Gas Price", "Log Density"),
                              # out = "Results/Tables/simple_regression.tex",
                              column.sep.width = "1pt",
                              font.size = "normalsize",
                              dep.var.labels = "Percent 0:1 voting no on Prop 6 (Supportive of Gas Tax)"
)

reg_no_gas_price <- reg_no_gas_price[8:length(reg_no_gas_price)-1]

write(reg_no_gas_price, file = "Results/Tables/reg_no_gas_price_ldemo_a.tex")


########################
## Residual Charting ##
#######################

# This section must be rewritten if we want to use a different model
residual <- filter(DT, !is.na(liberal.index.pct.18.gene.), !is.na(p6n.pct.18.gene.), !is.na(gas.tax.dollars), !is.na(county), 
                   !is.na(race.1), !is.na(household.size), !is.na(value.1), !is.na(latino), !is.na(rooms.1), !is.na(edu.1),
                   !is.na(owner.occupied), !is.na(male), !is.na(vehicle.hh.1), !is.na(commute.1), !is.na(county)) %>% 
  select(GEOID) %>% 
  mutate(residual = m5$residuals)

# quantiles <- quantile(DT$avg.gas.price, probs = c(.1, .2, .3, .4, .5, .6, .7), na.rm = T)

DT_residual <- inner_join(DT, residual, by="GEOID") %>% 
  mutate(gas_price_bin = as.factor(ntile(avg.gas.price, 10)))
# mutate(gas_price_bin = as.factor(case_when(
#   between(avg.gas)
# )))
# mutate(gas_price_bin = as.factor(cut(DT_residual$avg.gas.price, 10, labels = F)))

# view(select(DT_residual, gas_price_bin, avg.gas.price))

ggplot(DT_residual, aes(x=gas_price_bin, y = residual)) + geom_boxplot() +
  labs(x = "Gas Price Bin", y = "Residual")
ggsave("/Users/Epste/Dropbox/Gas Tax Repeal Draft/Results/Figures/gas_price_residuals_ldemo_a.png")

rm(m, m1, m2, m3, m4, m5, m6, m1_robust_se, m2_robust_se, m3_robust_se, m4_robust_se, m5_robust_se, m6_robust_se, m_robust_se)

#####





###########################################################
## Regression w/o Gas prices (expanded Demographics) - B ##
###########################################################

m1 <- DT %>% recipe() %>% 
  update_role(p6n.pct.18.gene.,new_role = "outcome") %>%
  update_role(c(liberal.index.pct.18.gene.), new_role = "predictor") %>% 
  prep() %>% 
  formula() %>% 
  lm(DT,
     weights = TOTVOTE.18.gene.)



m2 <- DT %>% recipe() %>% 
  update_role(p6n.pct.18.gene. ,new_role = "outcome") %>%
  update_role(annual.incidence.dollars, new_role = "predictor") %>% 
  prep() %>% 
  formula() %>% 
  lm(DT,
     weights = TOTVOTE.18.gene.)



m3 <- DT %>% recipe() %>% 
  update_role(p6n.pct.18.gene. ,new_role = "outcome") %>%
  update_role(c(annual.incidence.dollars, liberal.index.pct.18.gene.), new_role = "predictor") %>% 
  prep() %>% 
  formula() %>% 
  lm(DT,
     weights = TOTVOTE.18.gene.)


m4 <- DT %>% recipe() %>% 
  update_role(p6n.pct.18.gene. ,new_role = "outcome") %>%
  update_role(c(annual.incidence.dollars, liberal.index.pct.18.gene., vars, log.density), new_role = "predictor") %>% 
  prep() %>% 
  formula() %>% 
  lm(DT,
     weights = TOTVOTE.18.gene.)



m5 <- DT %>% recipe() %>% 
  update_role(p6n.pct.18.gene. ,new_role = "outcome") %>%
  update_role(c(annual.incidence.dollars, liberal.index.pct.18.gene., vars, log.density, county), new_role = "predictor") %>% 
  prep() %>% 
  formula() %>% 
  lm(DT,
     weights = TOTVOTE.18.gene.)




m6 <- DT %>% recipe() %>% 
  update_role(p6n.pct.18.gene. ,new_role = "outcome") %>%
  update_role(c(annual.incidence.dollars, liberal.index.pct.18.gene., vars, county, log.density, pct.vmiles), new_role = "predictor") %>% 
  prep() %>% 
  formula() %>% 
  lm(DT,
     weights = TOTVOTE.18.gene.)




m7 <- DT %>% recipe() %>% 
  update_role(p6n.pct.18.gene. ,new_role = "outcome") %>%
  update_role(c(annual.incidence.dollars, liberal.index.pct.18.gene., vars, county, log.density, avg.gas.price), new_role = "predictor") %>% 
  prep() %>% 
  formula() %>% 
  lm(DT,
     weights = TOTVOTE.18.gene.)


m1_robust_se <- sqrt(diag(vcovHC(m1, type="HC1")))
m2_robust_se <- sqrt(diag(vcovHC(m2, type="HC1")))
m3_robust_se <- sqrt(diag(vcovHC(m3, type="HC1")))
m4_robust_se <- sqrt(diag(vcovHC(m4, type="HC1")))
m5_robust_se <- sqrt(diag(vcovHC(m5, type="HC1")))
m6_robust_se <- sqrt(diag(vcovHC(m6, type="HC1")))
m7_robust_se <- sqrt(diag(vcovHC(m7, type="HC1")))



reg_no_gas_price <- stargazer(m1, m2, m3, m4, m5, m6, m7,
                              type = "latex", omit.stat = c("adj.rsq", "f", "ser"),
                              # type = "text",
                              omit = c("county", vars),
                              se = list(m1_robust_se, m2_robust_se,  m3_robust_se,  m4_robust_se,  m5_robust_se, m6_robust_se, m7_robust_se),
                              # title = "Simple Regressions",
                              column.labels = c("Politics", "Incidence", "+ Politics", "+ Census", "+ County FEs", "+ \\% miles in Vehicle", "(5) + Gas Price"),
                              order = c("liberal.index.pct.18.gene.", "annual.incidence.dollars", "pct.vmiles", "avg.gas.price", "log.density"),
                              covariate.labels  = c("Liberal index", "RRAA Annual Incidence", "Mile share in vehicle",
                                                    "Average Gas Price", "Log Density"),
                              # out = "Results/Tables/simple_regression.tex",
                              column.sep.width = "1pt",
                              font.size = "normalsize",
                              dep.var.labels = "Percent 0:1 voting no on Prop 6 (Supportive of Gas Tax)"
)

reg_no_gas_price <- reg_no_gas_price[8:length(reg_no_gas_price)-1]

write(reg_no_gas_price, file = "Results/Tables/reg_no_gas_price_ldemo_b.tex")


########################
## Residual Charting ##
#######################

# This section must be rewritten if we want to use a different model
residual <- filter(DT, !is.na(liberal.index.pct.18.gene.), !is.na(p6n.pct.18.gene.), !is.na(annual.incidence.dollars), !is.na(county), 
                   !is.na(race.1), !is.na(household.size), !is.na(value.1), !is.na(latino), !is.na(rooms.1), !is.na(edu.1),
                   !is.na(owner.occupied), !is.na(male), !is.na(vehicle.hh.1), !is.na(commute.1), !is.na(county)) %>% 
  select(GEOID) %>% 
  mutate(residual = m5$residuals)

# quantiles <- quantile(DT$avg.gas.price, probs = c(.1, .2, .3, .4, .5, .6, .7), na.rm = T)

DT_residual <- inner_join(DT, residual, by="GEOID") %>% 
  mutate(gas_price_bin = as.factor(ntile(avg.gas.price, 10)))
# mutate(gas_price_bin = as.factor(case_when(
#   between(avg.gas)
# )))
# mutate(gas_price_bin = as.factor(cut(DT_residual$avg.gas.price, 10, labels = F)))

# view(select(DT_residual, gas_price_bin, avg.gas.price))

ggplot(DT_residual, aes(x=gas_price_bin, y = residual)) + geom_boxplot() +
  labs(x = "Gas Price Bin", y = "Residual")
ggsave("/Users/Epste/Dropbox/Gas Tax Repeal Draft/Results/Figures/gas_price_residuals_ldemo_b.png")

rm(m, m1, m2, m3, m4, m5, m6, m1_robust_se, m2_robust_se, m3_robust_se, m4_robust_se, m5_robust_se, m6_robust_se, m_robust_se)

#####










###########################################################
## Regression w/o Gas prices (expanded Demographics) - C ##
###########################################################


m1 <- DT %>% recipe() %>% 
  update_role(p6n.pct.18.gene.,new_role = "outcome") %>%
  update_role(c(liberal.index.pct.18.gene.), new_role = "predictor") %>% 
  prep() %>% 
  formula() %>% 
  lm(DT,
     weights = TOTVOTE.18.gene.)



m2 <- DT %>% recipe() %>% 
  update_role(p6n.pct.18.gene. ,new_role = "outcome") %>%
  update_role(gas.tax.dollars, annual.fee.dollars, new_role = "predictor") %>% 
  prep() %>% 
  formula() %>% 
  lm(DT,
     weights = TOTVOTE.18.gene.)



m3 <- DT %>% recipe() %>% 
  update_role(p6n.pct.18.gene. ,new_role = "outcome") %>%
  update_role(c(gas.tax.dollars, annual.fee.dollars, liberal.index.pct.18.gene.), new_role = "predictor") %>% 
  prep() %>% 
  formula() %>% 
  lm(DT,
     weights = TOTVOTE.18.gene.)


m4 <- DT %>% recipe() %>% 
  update_role(p6n.pct.18.gene. ,new_role = "outcome") %>%
  update_role(c(gas.tax.dollars, annual.fee.dollars, liberal.index.pct.18.gene., vars, log.density), new_role = "predictor") %>% 
  prep() %>% 
  formula() %>% 
  lm(DT,
     weights = TOTVOTE.18.gene.)



m5 <- DT %>% recipe() %>% 
  update_role(p6n.pct.18.gene. ,new_role = "outcome") %>%
  update_role(c(gas.tax.dollars, annual.fee.dollars, liberal.index.pct.18.gene., vars, log.density, county), new_role = "predictor") %>% 
  prep() %>% 
  formula() %>% 
  lm(DT,
     weights = TOTVOTE.18.gene.)




m6 <- DT %>% recipe() %>% 
  update_role(p6n.pct.18.gene. ,new_role = "outcome") %>%
  update_role(c(gas.tax.dollars, annual.fee.dollars, liberal.index.pct.18.gene., vars, county, log.density, pct.vmiles), new_role = "predictor") %>% 
  prep() %>% 
  formula() %>% 
  lm(DT,
     weights = TOTVOTE.18.gene.)




m7 <- DT %>% recipe() %>% 
  update_role(p6n.pct.18.gene. ,new_role = "outcome") %>%
  update_role(c(gas.tax.dollars, annual.fee.dollars, liberal.index.pct.18.gene., vars, county, log.density, avg.gas.price), new_role = "predictor") %>% 
  prep() %>% 
  formula() %>% 
  lm(DT,
     weights = TOTVOTE.18.gene.)


m1_robust_se <- sqrt(diag(vcovHC(m1, type="HC1")))
m2_robust_se <- sqrt(diag(vcovHC(m2, type="HC1")))
m3_robust_se <- sqrt(diag(vcovHC(m3, type="HC1")))
m4_robust_se <- sqrt(diag(vcovHC(m4, type="HC1")))
m5_robust_se <- sqrt(diag(vcovHC(m5, type="HC1")))
m6_robust_se <- sqrt(diag(vcovHC(m6, type="HC1")))
m7_robust_se <- sqrt(diag(vcovHC(m7, type="HC1")))



reg_no_gas_price <- stargazer(m1, m2, m3, m4, m5, m6, m7,
                              type = "latex", omit.stat = c("adj.rsq", "f", "ser"),
                              # type = "text",
                              omit = c("county", vars),
                              se = list(m1_robust_se, m2_robust_se,  m3_robust_se,  m4_robust_se,  m5_robust_se, m6_robust_se, m7_robust_se),
                              # title = "Simple Regressions",
                              column.labels = c("Politics", "Incidence", "+ Politics", "+ Census", "+ County FEs", "+ \\% miles in Vehicle", "(5) + Gas Price"),
                              order = c("liberal.index.pct.18.gene.", "gas.tax.dollars", "annual.fee.dollars", "pct.vmiles", "avg.gas.price", "log.density"),
                              covariate.labels  = c("Liberal index", "Annual Gas Tax Increase (weekdays)", "RRAA Annual Fee Increase", "Mile share in vehicle",
                                                    "Average Gas Price", "Log Density"),
                              # out = "Results/Tables/simple_regression.tex",
                              column.sep.width = "1pt",
                              font.size = "normalsize",
                              dep.var.labels = "Percent 0:1 voting no on Prop 6 (Supportive of Gas Tax)"
)

reg_no_gas_price <- reg_no_gas_price[8:length(reg_no_gas_price)-1]

write(reg_no_gas_price, file = "Results/Tables/reg_no_gas_price_ldemo_c.tex")


########################
## Residual Charting ##
#######################

# This section must be rewritten if we want to use a different model
residual <- filter(DT, !is.na(liberal.index.pct.18.gene.), !is.na(p6n.pct.18.gene.), !is.na(gas.tax.dollars), !is.na(annual.fee.dollars), !is.na(county), 
                   !is.na(race.1), !is.na(household.size), !is.na(value.1), !is.na(latino), !is.na(rooms.1), !is.na(edu.1),
                   !is.na(owner.occupied), !is.na(male), !is.na(vehicle.hh.1), !is.na(commute.1), !is.na(county)) %>% 
  select(GEOID) %>% 
  mutate(residual = m5$residuals)

# quantiles <- quantile(DT$avg.gas.price, probs = c(.1, .2, .3, .4, .5, .6, .7), na.rm = T)

DT_residual <- inner_join(DT, residual, by="GEOID") %>% 
  mutate(gas_price_bin = as.factor(ntile(avg.gas.price, 10)))
# mutate(gas_price_bin = as.factor(case_when(
#   between(avg.gas)
# )))
# mutate(gas_price_bin = as.factor(cut(DT_residual$avg.gas.price, 10, labels = F)))

# view(select(DT_residual, gas_price_bin, avg.gas.price))

ggplot(DT_residual, aes(x=gas_price_bin, y = residual)) + geom_boxplot() +
  labs(x = "Gas Price Bin", y = "Residual")
ggsave("/Users/Epste/Dropbox/Gas Tax Repeal Draft/Results/Figures/gas_price_residuals_ldemo_c.png")

rm(m, m1, m2, m3, m4, m5, m6, m1_robust_se, m2_robust_se, m3_robust_se, m4_robust_se, m5_robust_se, m6_robust_se, m_robust_se)

#####


############################
## Liberal Quartile Dummy ##
############################
q1 <- quantile(DT$liberal.index.pct.18.gene., .25, na.rm=T)
q2 <- quantile(DT$liberal.index.pct.18.gene., .5, na.rm=T)
q3 <- quantile(DT$liberal.index.pct.18.gene., .75, na.rm=T)

DT <- mutate(DT,
             lib_q1 = if_else(DT$liberal.index.pct.18.gene. < q1, 1, 0),
             lib_q2 = if_else(DT$liberal.index.pct.18.gene. > q1 & DT$liberal.index.pct.18.gene. < q2, 1, 0),
             lib_q3 = if_else(DT$liberal.index.pct.18.gene. > q2 & DT$liberal.index.pct.18.gene. < q3, 1, 0),
             lib_q4 = if_else(DT$liberal.index.pct.18.gene. > q3, 1, 0)
            )




m1 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + ad.per.household.SB1Funds + ad.per.household.SB1Funds:lib_q1 + ad.per.household.SB1Funds:lib_q2 + ad.per.household.SB1Funds:lib_q3 + ad.per.household.SB1Funds:lib_q4 + gas.tax.dollars + annual.fee.dollars + 
           avg.gas.price +  white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
         data = filter(DT ),
         weights = TOTVOTE.18.gene.
)
m1_robust_se <- sqrt(diag(vcovHC(m1, type="HC1")))

# 
# m1 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + ad.per.household.SB1Funds + gas.tax.dollars + annual.fee.dollars + 
#            avg.gas.price +  white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
#          data = filter(DT
#          )
# )


reg_quartile <- stargazer(m1,
                          # type = "latex",
                          type = "text",
                          omit.stat = c("adj.rsq", "f", "ser"), 
                          omit = "county",
                          se = list(m1_robust_se),
                          title = "Quartile Regression",
                          column.labels = c("full", "q1", "q2", "q3", "q4"),
                          order = c("liberal.index.pct.18.gene.", "ad.per.household.SB1Funds", "ad.per.household.SB1Funds:lib_q1", "ad.per.household.SB1Funds:lib_q2", "ad.per.household.SB1Funds:lib_q3", "ad.per.household.SB1Funds:lib_q4",
                                    "gas.tax.dollars", "annual.fee.dollars", "avg.gas.price", "white", "male", "bachelor", "log.income", "log.density"),
                          covariate.labels  = c("Liberal Index", "SB1 Funds", "SB1 Funds X Lib Q1", "SB1 Funds X Lib Q2", "SB1 Funds X Lib Q3", "SB1 Funds X Lib Q4", "Annual Gas Tax (weekdays)", "RRAA Annual Fee Increase", "average gas price",
                                                "white", "Male", "4-yr degree", "Log Income", "Log Density"),
                          # out = "Results/Tables/simple_regression.tex",
                          column.sep.width = "1pt",
                          font.size = "normalsize",
                          dep.var.labels = "Percent 0:1 voting no on Prop 6 (Supportive of Gas Tax) - Quartiles by Liberal"
)










m1 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + ad.per.household.SB1Funds + 
           gas.tax.dollars:lib_q1 + gas.tax.dollars:lib_q2 + gas.tax.dollars:lib_q3 + gas.tax.dollars:lib_q4 + gas.tax.dollars +
           annual.fee.dollars:lib_q1 + annual.fee.dollars:lib_q2 + annual.fee.dollars:lib_q3 + annual.fee.dollars:lib_q4 + annual.fee.dollars +
           avg.gas.price +  white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
         data = filter(DT),
         weights = TOTVOTE.18.gene.
)
m1_robust_se <- sqrt(diag(vcovHC(m1, type="HC1")))

# 
# m1 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + ad.per.household.SB1Funds + gas.tax.dollars + annual.fee.dollars + 
#            avg.gas.price +  white + male + bachelor + log.income + home.value + commute.minutes + vehicle.hh.count + log.density + as.factor(county),
#          data = filter(DT
#          )
# )


reg_quartile <- stargazer(m1,
                          # type = "latex",
                          type = "text",
                          omit.stat = c("adj.rsq", "f", "ser"), 
                          omit = "county",
                          se = list(m1_robust_se),
                          title = "Quartile Regression",
                          column.labels = c("full", "q1", "q2", "q3", "q4"),
                          order = c("liberal.index.pct.18.gene.", "ad.per.household.SB1Funds", "gas.tax.dollars", "gas.tax.dollars:lib_q1", "gas.tax.dollars:lib_q2", "gas.tax.dollars:lib_q3", "gas.tax.dollars:lib_q4",
                                    "annual.fee.dollars","lib_q1:annual.fee.dollars", "lib_q2:annual.fee.dollars",  "lib_q3:annual.fee.dollars", "lib_q4:annual.fee.dollars",
                                    "avg.gas.price", "white", "male", "bachelor", "log.income", "log.density"),
                          # covariate.labels  = c("Liberal Index", "SB1 Funds X Lib Q1", "SB1 Funds X Lib Q2", "SB1 Funds X Lib Q3", "SB1 Funds X Lib Q4", "Annual Gas Tax (weekdays)", "RRAA Annual Fee Increase", "average gas price",
                          #                       "white", "Male", "4-yr degree", "Log Income", "Log Density"),
                          # out = "Results/Tables/simple_regression.tex",
                          column.sep.width = "1pt",
                          font.size = "normalsize",
                          dep.var.labels = "Percent 0:1 voting no on Prop 6 (Supportive of Gas Tax) - Quartiles by Liberal"
)









