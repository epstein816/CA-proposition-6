    
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
library(Hmisc)


dir <- "/Users/Epste/Dropbox/Gas Tax Repeal Draft/"
# dir <- "/stfm/dev2/m1lxe00/Projects/gas_tax/"
setwd(dir)

DT <- readRDS("Data/tracts/tract_clean.RDS")

#####

# DT$annual.incidence.dollars <- -DT$annual.incidence.dollars

DT$test <- log(DT$p6n.pct.18.gene./(1-DT$p6n.pct.18.gene.))

m4 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + ad.per.household.SB1Funds + annual.incidence.dollars, 
         data = DT,
         weights = TOTVOTE.18.gene.)
summary(m4)

m3 <- lm(test ~ liberal.index.pct.18.gene. + rraa.net.benefits, 
         data = DT,
         weights = TOTVOTE.18.gene.)
summary(m3)

m2 <- lm(p6n.pct.18.gene. ~ liberal.index.pct.18.gene. + rraa.net.benefits, 
         data = DT,
         weights = TOTVOTE.18.gene.)
summary(m2)

DT2 <- filter(DT, !is.na(liberal.index.pct.18.gene.), !is.na(rraa.net.benefits),  
       !is.na(white),  !is.na(bachelor),  !is.na(male),  !is.na(home.value),  !is.na(commute.minutes),
       !is.na(log.income), !is.na(vehicle.hh.count), !is.na(log.density), !is.na(county))

DT2$lib.index.0.center <- DT2$liberal.index.pct.18.gene. - mean(DT2$liberal.index.pct.18.gene.)

DT2<-mutate(DT2,
       gas.tax.dollars=-gas.tax.dollars,
       annual.incidence.dollars=-annual.incidence.dollars,
       annual.fee.dollars=-annual.fee.dollars)



# Perform analysis w/ net benefits

m5 <- lm(test ~ lib.index.0.center + rraa.net.benefits + 
           white + male + bachelor + log.income + home.value + commute.minutes + log.density ,
         data = DT2,
         weights = TOTVOTE.18.gene.)

DT2$wtp_tot <- DT2$test/m5$coefficients[3]
DT2$wtp_ideology <- DT2$lib.index.0.center* m5$coefficients[2]/m5$coefficients[3]
DT2$wtp_residual <- m5$residuals/m5$coefficients[3]
DT2$wtp_other <- DT2$wtp_tot - DT2$wtp_ideology - DT2$wtp_residual - DT2$rraa.net.benefits

DT2$p6.votes <- (1-DT2$p6.undervote.pct.18.gene.)*DT2$TOTVOTE.18.gene.



summary(select(DT2, contains("wtp"), rraa.net.benefits))

# DT3 <- transform(DT2, percentile=findInterval(wtp_tot, quantile(wtp_tot, seq(0,1, by=.01)))) %>%
DT3 <- transform(DT2, percentile=findInterval(wtp_tot, wtd.quantile(wtp_tot, seq(0,1, by=.01), weights = p6.votes))) %>%
  group_by(percentile) %>% 
  summarise(wtp_tot = wtd.mean(wtp_tot, weights = p6.votes),
            wtp_ideology = wtd.mean(wtp_ideology, weights = p6.votes),
            wtp_other = wtd.mean(wtp_other, weights = p6.votes),
            wtp_residual = wtd.mean(wtp_residual, weights = p6.votes),
            rraa.net.benefits = mean(rraa.net.benefits, weights = p6.votes))

net_benefits_wtp <- select(DT3, percentile, net_benefits_wtp = wtp_tot)

st(select(DT2,contains("wtp"), rraa.net.benefits))

DT4<-pivot_longer(DT3,c("wtp_tot", "wtp_ideology", "wtp_other","wtp_residual","rraa.net.benefits"),
                  names_to = "measure",
                  values_to = "value")

ggplot(DT4,aes(x=percentile,y=value,color=measure))+
  geom_line()+
  scale_color_manual(values=c("green","red","blue","gold","black"))+
  labs(title="Regression uses RRAA net benefits",
       x="Support for RRAA",
       y="Willingness to Pay")


ggsave("Results/Figures/wtp/net.benefits.png",
       height = 4,
       width = 9)




# Perform analysis w/ gas tax

m5 <- lm(test ~ lib.index.0.center + gas.tax.dollars + 
           white + male + bachelor + log.income + home.value + commute.minutes + log.density ,
         data = DT2,
         weights = TOTVOTE.18.gene.)

DT2$wtp_tot <- DT2$test/m5$coefficients[3]
DT2$wtp_ideology <- DT2$lib.index.0.center* m5$coefficients[2]/m5$coefficients[3]
DT2$wtp_residual <- m5$residuals/m5$coefficients[3]
DT2$wtp_other <- DT2$wtp_tot - DT2$wtp_ideology - DT2$wtp_residual - DT2$gas.tax.dollars

DT2$p6.votes <- (1-DT2$p6.undervote.pct.18.gene.)*DT2$TOTVOTE.18.gene.


DT3 <- transform(DT2, percentile=findInterval(wtp_tot, wtd.quantile(wtp_tot, seq(0,1, by=.01), weights = p6.votes))) %>% 
  group_by(percentile) %>% 
  summarise(wtp_tot = wtd.mean(wtp_tot, weights = p6.votes),
            wtp_ideology = wtd.mean(wtp_ideology, weights = p6.votes),
            wtp_other = wtd.mean(wtp_other, weights = p6.votes),
            wtp_residual = wtd.mean(wtp_residual, weights = p6.votes),
            gas.tax.dollars = mean(gas.tax.dollars, weights = p6.votes))



DT4<-pivot_longer(DT3,c("wtp_tot", "wtp_ideology", "wtp_other","wtp_residual","gas.tax.dollars"),
                  names_to = "measure",
                  values_to = "value")

ggplot(DT4,aes(x=percentile,y=value,color=measure))+
  geom_line()+
  scale_color_manual(values=c("green","red","blue","gold","black"))+
  labs(title="Regression uses Gas Tax Cost",
       x="Support for RRAA",
       y="Willingness to Pay")


ggsave("Results/Figures/wtp/gas.tax.png",
       height = 4,
       width = 9)







# Perform analysis w/ annual fee

m5 <- lm(test ~ lib.index.0.center + annual.fee.dollars + 
           white + male + bachelor + log.income + home.value + commute.minutes + log.density ,
         data = DT2,
         weights = TOTVOTE.18.gene.)

DT2$wtp_tot <- DT2$test/m5$coefficients[3]
DT2$wtp_ideology <- DT2$lib.index.0.center* m5$coefficients[2]/m5$coefficients[3]
DT2$wtp_residual <- m5$residuals/m5$coefficients[3]
DT2$wtp_other <- DT2$wtp_tot - DT2$wtp_ideology - DT2$wtp_residual - DT2$gas.tax.dollars

DT2$p6.votes <- (1-DT2$p6.undervote.pct.18.gene.)*DT2$TOTVOTE.18.gene.


DT3 <- transform(DT2, percentile=findInterval(wtp_tot, wtd.quantile(wtp_tot, seq(0,1, by=.01), weights = p6.votes))) %>% 
  group_by(percentile) %>% 
  summarise(wtp_tot = wtd.mean(wtp_tot, weights = p6.votes),
            wtp_ideology = wtd.mean(wtp_ideology, weights = p6.votes),
            wtp_other = wtd.mean(wtp_other, weights = p6.votes),
            wtp_residual = wtd.mean(wtp_residual, weights = p6.votes),
            annual.fee.dollars = mean(annual.fee.dollars, weights = p6.votes))



DT4<-pivot_longer(DT3,c("wtp_tot", "wtp_ideology", "wtp_other","wtp_residual","annual.fee.dollars"),
                  names_to = "measure",
                  values_to = "value")

ggplot(DT4,aes(x=percentile,y=value,color=measure))+
  geom_line()+
  scale_color_manual(values=c("green","red","blue","gold","black"),
                     labels = c("Net Incidence", "Ideology", "Other", "Residual", "Total"),
                     name = "WTP")+
  labs(title="Regression uses Annual Fee Cost",
       x="Support for RRAA",
       y="Willingness to Pay")


ggsave("Results/Figures/wtp/annual.fee.png",
       height = 4,
       width = 9)







# Perform analysis w/ gross costs

m5 <- lm(test ~ lib.index.0.center + annual.incidence.dollars + 
           white + male + bachelor + log.income + home.value + commute.minutes + log.density ,
         data = DT2,
         weights = TOTVOTE.18.gene.)


DT2$wtp_tot <- DT2$test/m5$coefficients[3]
DT2$wtp_ideology <- DT2$lib.index.0.center* m5$coefficients[2]/m5$coefficients[3]
DT2$wtp_residual <- m5$residuals/m5$coefficients[3]
DT2$wtp_other <- DT2$wtp_tot - DT2$wtp_ideology - DT2$wtp_residual - DT2$gas.tax.dollars

DT2$p6.votes <- (1-DT2$p6.undervote.pct.18.gene.)*DT2$TOTVOTE.18.gene.


DT3 <- transform(DT2, percentile=findInterval(wtp_tot, wtd.quantile(wtp_tot, seq(0,1, by=.01), weights = p6.votes))) %>% 
  group_by(percentile) %>% 
  summarise(wtp_tot = wtd.mean(wtp_tot, weights = p6.votes),
            wtp_ideology = wtd.mean(wtp_ideology, weights = p6.votes),
            wtp_other = wtd.mean(wtp_other, weights = p6.votes),
            wtp_residual = wtd.mean(wtp_residual, weights = p6.votes),
            annual.incidence.dollars = mean(annual.incidence.dollars, weights = p6.votes))

costs_wtp <- select(DT3, percentile, tot_costs = wtp_tot)

DT4<-pivot_longer(DT3,c("wtp_tot", "wtp_ideology", "wtp_other","wtp_residual","annual.incidence.dollars"),
                  names_to = "measure",
                  values_to = "value")

ggplot(DT4,aes(x=percentile,y=value,color=measure))+
  geom_line()+
  scale_color_manual(values=c("green","red","blue","gold","black"),
                     labels = c("Gross Costs", "Ideology", "Other", "Residual", "Total"),
                     name = "WTP")+
  labs(title="Regression uses Gross Costs",
       x="Support for RRAA",
       y="Willingness to Pay")


ggsave("Results/Figures/wtp/total.costs.png",
       height = 4,
       width = 9)







# Perform analysis w/ benefits

m5 <- lm(test ~ lib.index.0.center + ad.per.household.SB1Funds + 
           white + male + bachelor + log.income + home.value + commute.minutes + log.density ,
         data = DT2,
         weights = TOTVOTE.18.gene.)

DT2$wtp_tot <- DT2$test/m5$coefficients[3]
DT2$wtp_ideology <- DT2$lib.index.0.center* m5$coefficients[2]/m5$coefficients[3]
DT2$wtp_residual <- m5$residuals/m5$coefficients[3]
DT2$wtp_other <- DT2$wtp_tot - DT2$wtp_ideology - DT2$wtp_residual - DT2$gas.tax.dollars

DT2$p6.votes <- (1-DT2$p6.undervote.pct.18.gene.)*DT2$TOTVOTE.18.gene.


DT3 <- transform(DT2, percentile=findInterval(wtp_tot, wtd.quantile(wtp_tot, seq(0,1, by=.01), weights = p6.votes))) %>% 
  group_by(percentile) %>% 
  summarise(wtp_tot = wtd.mean(wtp_tot, weights = p6.votes),
            wtp_ideology = wtd.mean(wtp_ideology, weights = p6.votes),
            wtp_other = wtd.mean(wtp_other, weights = p6.votes),
            wtp_residual = wtd.mean(wtp_residual, weights = p6.votes),
            ad.per.household.SB1Funds = mean(ad.per.household.SB1Funds, weights = p6.votes))

gross_benefits_wtp <- select(DT3, percentile, tot_gross_benefits = wtp_tot)

DT4<-pivot_longer(DT3,c("wtp_tot", "wtp_ideology", "wtp_other","wtp_residual","ad.per.household.SB1Funds"),
                  names_to = "measure",
                  values_to = "value")

ggplot(DT4,aes(x=percentile,y=value,color=measure))+
  geom_line()+
  scale_color_manual(values=c("green","red","blue","gold","black"),
                     labels = c("Gross Benefits", "Ideology", "Other", "Residual", "Total"),
                     name = "WTP")+
  labs(title="Regression uses Gross Benefits",
       x="Support for RRAA",
       y="Willingness to Pay")


ggsave("Results/Figures/wtp/RRAA.benefits.png",
       height = 4,
       width = 9)




######################
combined <- inner_join(gross_benefits_wtp, costs_wtp) %>% 
  inner_join(net_benefits_wtp)%>% 
  rename(`Gross Benefits` = tot_gross_benefits, `Net Benefits` = net_benefits_wtp, `Gross Costs` = tot_costs)
  
DT4<-pivot_longer(combined,c(`Gross Benefits`, `Net Benefits`, `Gross Costs`),
                  names_to = "measure",
                  values_to = "value") 

ggplot(DT4,aes(x=percentile,y=value,color=measure)) +
  geom_line()+
  labs(title="Scaled WTP Estimates",
       x="Support for RRAA",
       y="Willingness to Pay") +
  scale_color_manual(name="WTP", values = c("green","red","blue","gold","black"))


ggsave("Results/Figures/wtp/combined.png",
       height = 4,
       width = 9)
 
#    
#    
#    
#    
# 
# m4$coefficients[1] + m4$coefficients[2] + m4$coefficients[3] + m4$coefficients[4]
# 
# m3$coefficients[1] + m3$coefficients[2] + m3$coefficients[3]
# 
# DT$wtp1 <- DT$test/m3$coefficients[2]
# summary(DT$wtp1)
# 
# DT$wtp2 <- (m3$coefficients[1] + DT$liberal.index.pct.18.gene.*m3$coefficients[2])/m3$coefficients[3]
# 
# tail(DT %>% select(contains("wtp")))
# 
# 
# p6n.pct.18.gene.
# 
# m5 <- lm(test ~ lib.index.0.center + rraa.net.benefits,
#            data = DT2)
# 
# DT2$wtp_tot <- DT2$p6n.pct.18.gene./m5$coefficients[3]
# DT2$wtp_ideology <- DT2$lib.index.0.center* m5$coefficients[2]/m5$coefficients[3]
# DT2$wtp_residual <- m5$residuals/m5$coefficients[3]
# DT2$wtp_other <- DT2$wtp_tot - DT2$wtp_ideology - DT2$wtp_residual - DT2$rraa.net.benefits
# 
# DT2$p6.votes <- (1-DT2$p6.undervote.pct.18.gene.)*DT2$TOTVOTE.18.gene.
# 
# summary(select(DT2, contains("wtp"), rraa.net.benefits))
# 
# DT3 <- transform(DT2, percentile=findInterval(wtp_tot, wtd.quantile(wtp_tot, seq(0,1, by=.01), weights = TOTVOTE.18.gene.))) %>% 
#   group_by(percentile) %>% 
#   summarise(wtp_tot = wtd.mean(wtp_tot, weights = p6.votes),
#             wtp_ideology = wtd.mean(wtp_ideology, weights = p6.votes),
#             wtp_other = wtd.mean(wtp_other, weights = p6.votes),
#             wtp_residual = wtd.mean(wtp_residual, weights = p6.votes),
#             rraa.net.benefits = mean(rraa.net.benefits, weights = p6.votes))
# 
# st(select(DT3,contains("wtp"), rraa.net.benefits),
# summ = c('notNA(x)','digits(mean(x))','digits(sd(x))','digits(min(x))','digits(max(x))'),
# summ.names = c('N', 'Mean', "Std. Dev.", 'Min', 'Max'))
# 
# ggplot(DT3, aes(x = percentile)) + 
#   geom_line(aes(y = wtp_tot)) + 
#   geom_line(aes(y = wtp_ideology), color = "red")  + 
#   geom_line(aes(y = rraa.net.benefits), color = "green")   + 
#   geom_line(aes(y = wtp_other), color = "blue")    + 
#   geom_line(aes(y = wtp_residual), color = "gold") 
# 
# 

ggplot(DT, aes(x = EV.old, y = EV))+geom_point(alpha = .2)
ggplot(DT, aes(x = old.annual.fee.dollars, y = annual.fee.dollars))+geom_point(alpha = .2)

