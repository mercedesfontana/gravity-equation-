Gravity <- readRDS("rawdata")
# SETUP -------------------------------------------------------------------

# Load packages

library(tidyverse)
library(foreign)
library(haven)
# Loading the data
Gravity_V202102<-readRDS("Downloads/Gravity_V202102.Rds")
x<-Gravity_V202102
#Selecting the necessary years

library(dplyr)
install.packages("dyplr",dependencies= TRUE)
x_app1 <- x %>% filter(year %in% seq(1960, 2020, 1))
#Construct symmetric pair ID-variables
names(x_app1)
attach(x_app1)
x_app1
library(base)
x_app1 = x_app1 %>% mutate(pair = paste(pmin(iso3_o,iso3_d),pmax(iso3_o,iso3_d),sep = "_")) %>% 
  group_by(pair) %>% 
  mutate(pair_id = cur_group_id())
#Calculating the necessary logs
x_app1 = x_app1 %>% mutate(across(c(tradeflow_comtrade_o,gdp_d,gdp_o,distw,pop_d, pop_o),~log(.x),.names="ln_{.col}"))
#Estimation
library(fixest)
attach(x_app1)
fit_ols=feols(ln_tradeflow_comtrade_o~ln_gdp_d+ln_gdp_o+ln_distw+ln_pop_d+ln_pop_o+contig+comlang_off+rta,
              data = x_app1 %>%
                filter(tradeflow_comtrade_d > 0 & iso3_d != iso3_o),vcov = cluster ~ pair_id)
summary(fit_ols)

#Preparing the data for fixed effects
x_app1 = x_app1 %>%
  unite("fe_exp_year",c(iso3_o,year),sep="_",remove=FALSE) %>%
  unite("fe_imp_year",c(iso3_d,year),sep="_",remove=FALSE)
# Running the regression with fixed effects
fit_fixedeffects = feols(ln_tradeflow_comtrade_o ~ ln_distw + contig + comlang_off + rta|
                           fe_exp_year + fe_imp_year,
                         data = x_app1 %>%
                           filter(tradeflow_comtrade_o > 0 & iso3_o != iso3_d),
                         vcov = cluster ~ pair_id)
summary(fit_fixedeffects)
#PPML with fixed effects

fit_poisson = fepois(tradeflow_comtrade_o ~ ln_distw + contig + comlang_off + rta|
                       fe_exp_year + fe_imp_year,
                     data = x_app1 %>% filter(iso3_o != iso3_d),
                     vcov = cluster ~ pair_id)
summary(fit_poisson)

library(fastDummies)

asean<-ifelse(pair==c("THA_IDN", "THA_VNM", "THA_MYS", "THA_SGP", "THA_PHL", "THA_BRN", "THA_LAO", 
                      "THA_KHM", "THA_MNR"),1,0)

asean2 <- ifelse(c(asean==1 | year >1967),1,0)
asean2

x_app2 <- x_app1 %>%
  add_column(Add_Column = "asean2")

#including trade agreement
fit_fixedeffects1 = feols(ln_tradeflow_comtrade_o ~ ln_distw + contig + comlang_off + rta+ asean2|
                           fe_exp_year + fe_imp_year,
                         data = x_app2 %>%
                           filter(tradeflow_comtrade_o > 0 & iso3_o != iso3_d),
                         vcov = cluster ~ pair_id)
summary(fit_fixedeffects1)
