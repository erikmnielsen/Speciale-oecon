#PRODUKTIVITET OG BESKÆFTIGELSE - EU KLEMS DATA

# Libraries and functions ---------------------------------------------------------------

library(readr)
library(readxl)
library(writexl)
library(reshape2)
library(fpp2)
library(tidyverse)
library(xts)
library(plm)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(gtools)
library(lmtest)
library(sandwich)

CGR = function(x){
  sapply(1:length(x), function(y){
    prod(1+x[1:y]) - 1
  })
}

func_coefs <- function(regression, name, type) {
  
  options(scipen=999, digits=4) 
  #options(scipen=0, digits=7) #default
  
  if (type=="Country"){
    
    coef = coeftest(regression, vcovPL(regression, cluster = ~ country, kernel = "Bartlett"))
    
    siglvl = stars.pval(coef[,4])
    reg_coef = cbind(coef[,c(1,4)], siglvl)
    colnames(reg_coef) <- paste(name, colnames(reg_coef), sep = "_")
    
    reg_coef
    
  } else if (type=="Year"){
    
    coef = coeftest(regression, vcovPL(regression, cluster = ~ year, kernel = "Bartlett"))
    
    siglvl = stars.pval(coef[,4])
    reg_coef = cbind(coef[,c(1,4)], siglvl)
    colnames(reg_coef) <- paste(name, colnames(reg_coef), sep = "_")
    
    reg_coef
    
    
  }else if (type=="None"){
    
    coef = coeftest(regression, vcovPL(regression, cluster = NULL, kernel = "Bartlett"))
    
    siglvl = stars.pval(coef[,4])
    reg_coef = cbind(coef[,c(1,4)], siglvl)
    colnames(reg_coef) <- paste(name, colnames(reg_coef), sep = "_")
    
    reg_coef
    
    
  } else {
    
    NA
  }
  
}
  
#Datasæt konstrueres
{
  ### Data for produktivitet og beskæftigelse
  EUK_nationalaccounts <- readRDS("Data/Statistical_National-Accounts.rds")
  data = EUK_nationalaccounts %>% filter(code %in% c("B","C","D","E","F","G","H","I","J","K","L","M_N"))
  
  data_emp = data %>% filter(var=="EMP") %>% mutate(EMP=value) %>% select(-var, -value)
  data_go = data %>% filter(var=="GO_Q") %>% mutate(GO=value) %>% select(country, code, year, GO)
  data = merge(data_emp, data_go, by=c("country","code", "year"), all.x = TRUE)
  data = na.omit(data) #sletter rækker hvor enten GO eller EMP tal mangler
  
  
  data$branche <- ifelse(data$code %in% c("B","D","E","F"), "b1", #B,D,E,F
                         ifelse(data$code %in% c("C"), "b2", #hele C
                                ifelse(data$code %in% c("J","K","M_N"), "b3", #("J58t60", "J61", "J62t63", "K", "MtN")
                                       ifelse(data$code %in% c("G","H","I","L"), "b4", #("G45", "G46", "G47", "H49t52","H53", "I", "L")
                                              "b0"))))
  
  #filtrering af lande/år
  test = data %>% group_by(country, year) %>% count() #492 obs
  data = merge(data, test, by=c("country", "year"), all.x = TRUE)
  data = data %>% filter(country %in% c("AT", "BE", "CZ", "DE", "DK", "EE","EL", "FI",
                                        "FR", "HR", "HU", "IT", "JP", "LV", "NL", "PL",
                                        "PT", "RO", "SE", "SI", "SK"), n>=12) # minus USA også
  
  
  ### Data for uddannelsesgrupper og andele af beskæftigelsen
  EUK_nationalaccountslabour <- read_csv("education_csv.csv") %>% select(-X1)
  data_educ = EUK_nationalaccountslabour
  data_educ$country = data_educ$Country
  data_educ$code = data_educ$Code
  data_educ$year = data_educ$variable
  data_educ = data_educ %>% select(country, code, year, Education, value)
  
  lowskill = data_educ %>% filter(Education=="3") %>% mutate(lowskill=value) %>% select(-Education, -value)
  midskill = data_educ %>% filter(Education=="2") %>% mutate(midskill=value) %>% select(-Education, -value)
  highskill = data_educ %>% filter(Education=="1") %>% mutate(highskill=value) %>% select(-Education, -value)
  
  data_educ = merge(lowskill, midskill, by=c("country","code", "year"), all.x = TRUE)
  data_educ = merge(data_educ, highskill, by=c("country","code", "year"), all.x = TRUE)
  
  ### Datasæt merges sammen 
  data = merge(data, data_educ, by=c("country","code", "year"), all.x = TRUE)
  data = na.omit(data) #sletter rækker hvor enten GO eller EMP tal mangler
  data = data %>% select(country, code, year, branche, EMP, GO, lowskill, midskill, highskill)
  
  # Vægte
  gns = data %>% group_by(country, code) %>% summarize(EMP_gns=sum(EMP))
  gns_2 = data %>% group_by(country) %>% summarize(EMP_gns_2=sum(EMP))
  gns = merge(gns, gns_2, by=c("country"), all.x = TRUE)
  gns$wgt_i_avg = gns$EMP_gns/gns$EMP_gns_2
  gns = gns %>% select(country, code, wgt_i_avg)
  data = merge(data, gns, by=c("country","code"), all.x = TRUE)
  
  #skill means eksporteres 
  skill_means = data %>% group_by(country, code) %>% summarize(lowskill_gns=mean(lowskill), midskill_gns=mean(midskill), highskill_gns=mean(highskill))
  skill_means2 = data %>% group_by(country) %>% summarize(lowskill_gns=mean(lowskill), midskill_gns=mean(midskill), highskill_gns=mean(highskill))
  skill_means3 = data %>% group_by(country, code, year) %>% summarize(lowskill_gns=mean(lowskill), midskill_gns=mean(midskill), highskill_gns=mean(highskill))
  skill_means4 = data %>% group_by(country, year) %>% summarize(lowskill_gns=mean(lowskill), midskill_gns=mean(midskill), highskill_gns=mean(highskill))
  
  
  #skill_means$tot = skill_means$lowskill_gns + skill_means$midskill_gns + skill_means$highskill_gns #tester om det summerer til 100
  skill_means_DK = skill_means %>% filter(country=="DK")
  skill_means_DK2 = skill_means2 %>% filter(country=="DK")
  skill_means_DK3 = skill_means3 %>% filter(country=="DK")
  skill_means_DK4 = skill_means4 %>% filter(country=="DK")
  saveRDS(skill_means_DK, file = "skill_means_DK_rds") # Savve an object to a file
  saveRDS(skill_means_DK2, file = "skill_means_DK2_rds") # Save an object to a file
  saveRDS(skill_means_DK3, file = "skill_means_DK3_rds") # Save an object to a file
  saveRDS(skill_means_DK4, file = "skill_means_DK4_rds") # Save an object to a file
  
  
  
  ### Paneldata laves 
  
  b_tot <- data %>% group_by(country, year, branche) %>% summarize(EMP_b=sum(EMP) , GO_b=sum(GO))
  #data = merge(data, b_tot, by=c("country", "year", "branche"), all.x = TRUE) 
  b_tot = b_tot %>% ungroup()
  b1 = b_tot %>% filter(branche=="b1") %>% mutate(EMP_b1=EMP_b) %>% mutate(GO_b1=GO_b) %>% select(country, year, EMP_b1,GO_b1)
  b2 = b_tot %>% filter(branche=="b2") %>% mutate(EMP_b2=EMP_b) %>% mutate(GO_b2=GO_b) %>% select(EMP_b2, GO_b2)
  b3 = b_tot %>% filter(branche=="b3") %>% mutate(EMP_b3=EMP_b) %>% mutate(GO_b3=GO_b) %>% select(EMP_b3, GO_b3)
  b4 = b_tot %>% filter(branche=="b4") %>% mutate(EMP_b4=EMP_b) %>% mutate(GO_b4=GO_b) %>% select(EMP_b4, GO_b4)
  b = cbind(b1,b2,b3,b4)
  data = merge(data, b, by=c("country","year"), all.x = TRUE)
  
  c_tot <- data %>% group_by(country, year) %>% summarize(EMP_c=sum(EMP) , GO_c=sum(GO))
  data = merge(data, c_tot, by=c("country","year"), all.x = TRUE)
 
  data$id_ci = data %>% group_indices(country, code)
  pdata = pdata.frame(data, index = c("id_ci", "year"))
  
  pdata$lowskill_changes = diff(pdata$lowskill, lag = 1, shift = "time")
  pdata$midskill_changes = diff(pdata$midskill, lag = 1, shift = "time")
  pdata$highskill_changes = diff(pdata$highskill, lag = 1, shift = "time")
  pdata$prod_logchanges = diff(log(pdata$GO/pdata$EMP), lag = 1, shift = "time")*100
  
  #Beta2 variable, mikro + makro
  pdata$dLP_CwoI = diff(log((pdata$GO_c-pdata$GO)/(pdata$EMP_c-pdata$EMP)), lag = 1, shift = "time")*100 
  
  #beta1 variable, sectoral spillover:
  pdata$dLP_I_b1 = ifelse(pdata$branche=="b1", pdata$prod_logchanges, 0)
  pdata$dLP_I_b2 = ifelse(pdata$branche=="b2", pdata$prod_logchanges, 0)
  pdata$dLP_I_b3 = ifelse(pdata$branche=="b3", pdata$prod_logchanges, 0)
  pdata$dLP_I_b4 = ifelse(pdata$branche=="b4", pdata$prod_logchanges, 0)
  
  #Beta2 variable, sektor spillover
  pdata$dLP_BwoI_b1 = ifelse(pdata$branche=="b1", diff(log((pdata$GO_b1-pdata$GO)/(pdata$EMP_b1-pdata$EMP)), lag = 1, shift = "time")*100, diff(log(pdata$GO_b1/pdata$EMP_b1), lag = 1, shift = "time")*100)
  pdata$dLP_BwoI_b2 = ifelse(pdata$branche=="b2", 0, diff(log(pdata$GO_b2/pdata$EMP_b2), lag = 1, shift = "time")*100)
  pdata$dLP_BwoI_b3 = ifelse(pdata$branche=="b3", diff(log((pdata$GO_b3-pdata$GO)/(pdata$EMP_b3-pdata$EMP)), lag = 1, shift = "time")*100, diff(log(pdata$GO_b3/pdata$EMP_b3), lag = 1, shift = "time")*100)
  pdata$dLP_BwoI_b4 = ifelse(pdata$branche=="b4", diff(log((pdata$GO_b4-pdata$GO)/(pdata$EMP_b4-pdata$EMP)), lag = 1, shift = "time")*100, diff(log(pdata$GO_b4/pdata$EMP_b4), lag = 1, shift = "time")*100)
  
  pdata = na.omit(pdata)
  
  #skal der bruges lags?
  pdata_lags = pdata
  
  #beta1
  pdata_lags$prod_logchanges_lag1 = lag(pdata_lags$prod_logchanges, k = 1, shift = "time")
  pdata_lags$prod_logchanges_lag2 = lag(pdata_lags$prod_logchanges, k = 2, shift = "time")
  pdata_lags$prod_logchanges_lag3 = lag(pdata_lags$prod_logchanges, k = 3, shift = "time")
  
  #beta1, sectoral spillover
  pdata_lags$dLP_I_b1_lag1 = lag(pdata_lags$dLP_I_b1, k = 1, shift = "time")
  pdata_lags$dLP_I_b1_lag2 = lag(pdata_lags$dLP_I_b1, k = 2, shift = "time")
  pdata_lags$dLP_I_b1_lag3 = lag(pdata_lags$dLP_I_b1, k = 3, shift = "time")
  
  pdata_lags$dLP_I_b2_lag1 = lag(pdata_lags$dLP_I_b2, k = 1, shift = "time")
  pdata_lags$dLP_I_b2_lag2 = lag(pdata_lags$dLP_I_b2, k = 2, shift = "time")
  pdata_lags$dLP_I_b2_lag3 = lag(pdata_lags$dLP_I_b2, k = 3, shift = "time")
  
  pdata_lags$dLP_I_b3_lag1 = lag(pdata_lags$dLP_I_b3, k = 1, shift = "time")
  pdata_lags$dLP_I_b3_lag2 = lag(pdata_lags$dLP_I_b3, k = 2, shift = "time")
  pdata_lags$dLP_I_b3_lag3 = lag(pdata_lags$dLP_I_b3, k = 3, shift = "time")
  
  pdata_lags$dLP_I_b4_lag1 = lag(pdata_lags$dLP_I_b4, k = 1, shift = "time")
  pdata_lags$dLP_I_b4_lag2 = lag(pdata_lags$dLP_I_b4, k = 2, shift = "time")
  pdata_lags$dLP_I_b4_lag3 = lag(pdata_lags$dLP_I_b4, k = 3, shift = "time")
  

  #beta2
  pdata_lags$dLP_CwoI_lag1 = lag(pdata_lags$dLP_CwoI, k = 1, shift = "time")
  pdata_lags$dLP_CwoI_lag2 = lag(pdata_lags$dLP_CwoI, k = 2, shift = "time")
  pdata_lags$dLP_CwoI_lag3 = lag(pdata_lags$dLP_CwoI, k = 3, shift = "time")
  
  #beta2, sectoral spillover
  pdata_lags$dLP_BwoI_b1_lag1 = lag(pdata_lags$dLP_BwoI_b1, k = 1, shift = "time")
  pdata_lags$dLP_BwoI_b1_lag2 = lag(pdata_lags$dLP_BwoI_b1, k = 2, shift = "time")
  pdata_lags$dLP_BwoI_b1_lag3 = lag(pdata_lags$dLP_BwoI_b1, k = 3, shift = "time")
  
  pdata_lags$dLP_BwoI_b2_lag1 = lag(pdata_lags$dLP_BwoI_b2, k = 1, shift = "time")
  pdata_lags$dLP_BwoI_b2_lag2 = lag(pdata_lags$dLP_BwoI_b2, k = 2, shift = "time")
  pdata_lags$dLP_BwoI_b2_lag3 = lag(pdata_lags$dLP_BwoI_b2, k = 3, shift = "time")
  
  pdata_lags$dLP_BwoI_b3_lag1 = lag(pdata_lags$dLP_BwoI_b3, k = 1, shift = "time")
  pdata_lags$dLP_BwoI_b3_lag2 = lag(pdata_lags$dLP_BwoI_b3, k = 2, shift = "time")
  pdata_lags$dLP_BwoI_b3_lag3 = lag(pdata_lags$dLP_BwoI_b3, k = 3, shift = "time")
  
  pdata_lags$dLP_BwoI_b4_lag1 = lag(pdata_lags$dLP_BwoI_b4, k = 1, shift = "time")
  pdata_lags$dLP_BwoI_b4_lag2 = lag(pdata_lags$dLP_BwoI_b4, k = 2, shift = "time")
  pdata_lags$dLP_BwoI_b4_lag3 = lag(pdata_lags$dLP_BwoI_b4, k = 3, shift = "time")
  
  pdata_lags = na.omit(pdata_lags)
  
  }

##### Regressioner -------------------------------

## Direkte effekter
{
 
#uden lags
lowskill = lm(lowskill_changes ~ prod_logchanges + factor(country) + factor(code) + factor(year), data=pdata, weights = wgt_i_avg)
lowskill_coef = func_coefs(lowskill, "low_skill", "Country")

midskill = lm(midskill_changes ~ prod_logchanges + factor(country) + factor(code) + factor(year), data=pdata, weights = wgt_i_avg)
midskill_coef = func_coefs(midskill, "mid_skill", "Country")

highskill = lm(highskill_changes ~ prod_logchanges + factor(country) + factor(code) + factor(year), data=pdata, weights = wgt_i_avg)
highskill_coef = func_coefs(highskill, "high_skill", "Country")

# med lags
lowskill_lags = lm(lowskill_changes ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + factor(country) + factor(code) + factor(year), data=pdata_lags, weights = wgt_i_avg)
lowskill_lags_coef = func_coefs(lowskill_lags, "low_skill_lags", "Country")

midskill_lags  = lm(midskill_changes ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + factor(country) + factor(code) + factor(year), data=pdata_lags, weights = wgt_i_avg)
midskill_lags_coef = func_coefs(midskill_lags , "mid_skill_lags ", "Country")

highskill_lags  = lm(highskill_changes ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + factor(country) + factor(code) + factor(year), data=pdata_lags, weights = wgt_i_avg)
highskill_lags_coef = func_coefs(highskill_lags , "high_skill_lags ", "Country")

}

## Direkte og indirekte effekter
{
#uden lags
lowskill = lm(lowskill_changes ~ prod_logchanges + dLP_CwoI + factor(country) + factor(code) + factor(year), data=pdata, weights = wgt_i_avg)
lowskill_coef = func_coefs(lowskill, "low_skill", "Country")

midskill = lm(midskill_changes ~ prod_logchanges + dLP_CwoI + factor(country) + factor(code) + factor(year), data=pdata, weights = wgt_i_avg)
midskill_coef = func_coefs(midskill, "mid_skill", "Country")

highskill = lm(highskill_changes ~ prod_logchanges + dLP_CwoI + factor(country) + factor(code) + factor(year), data=pdata, weights = wgt_i_avg)
highskill_coef = func_coefs(highskill, "high_skill", "Country")

# med lags
lowskill_lags = lm(lowskill_changes ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + dLP_CwoI + dLP_CwoI_lag1 + dLP_CwoI_lag2 + dLP_CwoI_lag3 + factor(country) + factor(code) + factor(year), data=pdata_lags, weights = wgt_i_avg)
lowskill_lags_coef = func_coefs(lowskill_lags, "low_skill_lags", "Country")

midskill_lags  = lm(midskill_changes ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + dLP_CwoI + dLP_CwoI_lag1 + dLP_CwoI_lag2 + dLP_CwoI_lag3 + factor(country) + factor(code) + factor(year), data=pdata_lags, weights = wgt_i_avg)
midskill_lags_coef = func_coefs(midskill_lags , "mid_skill_lags ", "Country")

highskill_lags  = lm(highskill_changes ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + dLP_CwoI + dLP_CwoI_lag1 + dLP_CwoI_lag2 + dLP_CwoI_lag3 + factor(country) + factor(code) + factor(year), data=pdata_lags, weights = wgt_i_avg)
highskill_lags_coef = func_coefs(highskill_lags , "high_skill_lags ", "Country")

}

## Direkte, sektor spillover
{
#uden lags
lowskill = lm(lowskill_changes ~ dLP_I_b1 + dLP_I_b2 + dLP_I_b3 + dLP_I_b4 + factor(country) + factor(code) + factor(year), data=pdata, weights = wgt_i_avg)
lowskill_coef = func_coefs(lowskill, "low_skill", "Country")

midskill = lm(midskill_changes ~ dLP_I_b1 + dLP_I_b2 + dLP_I_b3 + dLP_I_b4 + factor(country) + factor(code) + factor(year), data=pdata, weights = wgt_i_avg)
midskill_coef = func_coefs(midskill, "mid_skill", "Country")

highskill = lm(highskill_changes ~ dLP_I_b1 + dLP_I_b2 + dLP_I_b3 + dLP_I_b4 + factor(country) + factor(code) + factor(year), data=pdata, weights = wgt_i_avg)
highskill_coef = func_coefs(highskill, "high_skill", "Country")

# med lags
lowskill_lags = lm(lowskill_changes ~ dLP_I_b1 + dLP_I_b1_lag1 + dLP_I_b1_lag2 + dLP_I_b1_lag3 +
                     dLP_I_b2 + dLP_I_b2_lag1 + dLP_I_b2_lag2 + dLP_I_b2_lag3 +
                     dLP_I_b3 + dLP_I_b3_lag1 + dLP_I_b3_lag2 + dLP_I_b3_lag3 +
                     dLP_I_b4 + dLP_I_b4_lag1 + dLP_I_b4_lag2 + dLP_I_b4_lag3 + 
                     factor(country) + factor(code) + factor(year), data=pdata_lags, weights = wgt_i_avg)
lowskill_lags_coef = func_coefs(lowskill_lags, "low_skill_lags", "Country")

midskill_lags  = lm(midskill_changes ~ dLP_I_b1 + dLP_I_b1_lag1 + dLP_I_b1_lag2 + dLP_I_b1_lag3 +
                      dLP_I_b2 + dLP_I_b2_lag1 + dLP_I_b2_lag2 + dLP_I_b2_lag3 +
                      dLP_I_b3 + dLP_I_b3_lag1 + dLP_I_b3_lag2 + dLP_I_b3_lag3 +
                      dLP_I_b4 + dLP_I_b4_lag1 + dLP_I_b4_lag2 + dLP_I_b4_lag3 + 
                      factor(country) + factor(code) + factor(year), data=pdata_lags, weights = wgt_i_avg)
midskill_lags_coef = func_coefs(midskill_lags , "mid_skill_lags ", "Country")

highskill_lags  = lm(highskill_changes ~  dLP_I_b1 + dLP_I_b1_lag1 + dLP_I_b1_lag2 + dLP_I_b1_lag3 +
                       dLP_I_b2 + dLP_I_b2_lag1 + dLP_I_b2_lag2 + dLP_I_b2_lag3 +
                       dLP_I_b3 + dLP_I_b3_lag1 + dLP_I_b3_lag2 + dLP_I_b3_lag3 +
                       dLP_I_b4 + dLP_I_b4_lag1 + dLP_I_b4_lag2 + dLP_I_b4_lag3 + 
                       factor(country) + factor(code) + factor(year), data=pdata_lags, weights = wgt_i_avg)
highskill_lags_coef = func_coefs(highskill_lags , "high_skill_lags ", "Country")
}

## Direkte og indirekte effekter, sektor spillover
{
#uden lags
lowskill = lm(lowskill_changes ~ dLP_I_b1 + dLP_I_b2 + dLP_I_b3 + dLP_I_b4 + dLP_CwoI + factor(country) + factor(code) + factor(year), data=pdata, weights = wgt_i_avg)
lowskill_coef = func_coefs(lowskill, "low_skill", "Country")

midskill = lm(midskill_changes ~ dLP_I_b1 + dLP_I_b2 + dLP_I_b3 + dLP_I_b4 + dLP_CwoI + factor(country) + factor(code) + factor(year), data=pdata, weights = wgt_i_avg)
midskill_coef = func_coefs(midskill, "mid_skill", "Country")

highskill = lm(highskill_changes ~ dLP_I_b1 + dLP_I_b2 + dLP_I_b3 + dLP_I_b4 + dLP_CwoI + factor(country) + factor(code) + factor(year), data=pdata, weights = wgt_i_avg)
highskill_coef = func_coefs(highskill, "high_skill", "Country")

# med lags
lowskill_lags = lm(lowskill_changes ~ dLP_I_b1 + dLP_I_b1_lag1 + dLP_I_b1_lag2 + dLP_I_b1_lag3 +
                     dLP_I_b2 + dLP_I_b2_lag1 + dLP_I_b2_lag2 + dLP_I_b2_lag3 +
                     dLP_I_b3 + dLP_I_b3_lag1 + dLP_I_b3_lag2 + dLP_I_b3_lag3 +
                     dLP_I_b4 + dLP_I_b4_lag1 + dLP_I_b4_lag2 + dLP_I_b4_lag3 + 
                     dLP_CwoI + dLP_CwoI_lag1 + dLP_CwoI_lag2 + dLP_CwoI_lag3 + 
                     factor(country) + factor(code) + factor(year), data=pdata_lags, weights = wgt_i_avg)
lowskill_lags_coef = func_coefs(lowskill_lags, "low_skill_lags", "Country")

midskill_lags  = lm(midskill_changes ~ dLP_I_b1 + dLP_I_b1_lag1 + dLP_I_b1_lag2 + dLP_I_b1_lag3 +
                      dLP_I_b2 + dLP_I_b2_lag1 + dLP_I_b2_lag2 + dLP_I_b2_lag3 +
                      dLP_I_b3 + dLP_I_b3_lag1 + dLP_I_b3_lag2 + dLP_I_b3_lag3 +
                      dLP_I_b4 + dLP_I_b4_lag1 + dLP_I_b4_lag2 + dLP_I_b4_lag3 + 
                      dLP_CwoI + dLP_CwoI_lag1 + dLP_CwoI_lag2 + dLP_CwoI_lag3 + 
                      factor(country) + factor(code) + factor(year), data=pdata_lags, weights = wgt_i_avg)
midskill_lags_coef = func_coefs(midskill_lags , "mid_skill_lags ", "Country")

highskill_lags  = lm(highskill_changes ~  dLP_I_b1 + dLP_I_b1_lag1 + dLP_I_b1_lag2 + dLP_I_b1_lag3 +
                       dLP_I_b2 + dLP_I_b2_lag1 + dLP_I_b2_lag2 + dLP_I_b2_lag3 +
                       dLP_I_b3 + dLP_I_b3_lag1 + dLP_I_b3_lag2 + dLP_I_b3_lag3 +
                       dLP_I_b4 + dLP_I_b4_lag1 + dLP_I_b4_lag2 + dLP_I_b4_lag3 + 
                       dLP_CwoI + dLP_CwoI_lag1 + dLP_CwoI_lag2 + dLP_CwoI_lag3 + 
                       factor(country) + factor(code) + factor(year), data=pdata_lags, weights = wgt_i_avg)
highskill_lags_coef = func_coefs(highskill_lags , "high_skill_lags ", "Country")

}

## Direkte og indirekte effekter, sektor spillover - beta 2 udvidet
{

  #uden lags
  lowskill = lm(lowskill_changes ~ dLP_I_b1 + dLP_I_b2 + dLP_I_b3 + dLP_I_b4 + dLP_BwoI_b1 + dLP_BwoI_b2 + dLP_BwoI_b3 + dLP_BwoI_b4 + factor(country) + factor(code) + factor(year), data=pdata, weights = wgt_i_avg)
  lowskill_coef = func_coefs(lowskill, "low_skill", "Country")
  
  midskill = lm(midskill_changes ~ dLP_I_b1 + dLP_I_b2 + dLP_I_b3 + dLP_I_b4 + dLP_BwoI_b1 + dLP_BwoI_b2 + dLP_BwoI_b3 + dLP_BwoI_b4 + factor(country) + factor(code) + factor(year), data=pdata, weights = wgt_i_avg)
  midskill_coef = func_coefs(midskill, "mid_skill", "Country")
  
  highskill = lm(highskill_changes ~ dLP_I_b1 + dLP_I_b2 + dLP_I_b3 + dLP_I_b4 + dLP_BwoI_b1 + dLP_BwoI_b2 + dLP_BwoI_b3 + dLP_BwoI_b4 + factor(country) + factor(code) + factor(year), data=pdata, weights = wgt_i_avg)
  highskill_coef = func_coefs(highskill, "high_skill", "Country")
  
  # med lags
  lowskill_lags = lm(lowskill_changes ~ dLP_I_b1 + dLP_I_b1_lag1 + dLP_I_b1_lag2 + dLP_I_b1_lag3 +
                       dLP_I_b2 + dLP_I_b2_lag1 + dLP_I_b2_lag2 + dLP_I_b2_lag3 +
                       dLP_I_b3 + dLP_I_b3_lag1 + dLP_I_b3_lag2 + dLP_I_b3_lag3 +
                       dLP_I_b4 + dLP_I_b4_lag1 + dLP_I_b4_lag2 + dLP_I_b4_lag3 + 
                       dLP_BwoI_b1 + dLP_BwoI_b1_lag1 + dLP_BwoI_b1_lag2 + dLP_BwoI_b1_lag3 +
                       dLP_BwoI_b2 + dLP_BwoI_b2_lag1 + dLP_BwoI_b2_lag2 + dLP_BwoI_b2_lag3 +
                       dLP_BwoI_b3 + dLP_BwoI_b3_lag1 + dLP_BwoI_b3_lag2 + dLP_BwoI_b3_lag3 +
                       dLP_BwoI_b4 + dLP_BwoI_b4_lag1 + dLP_BwoI_b4_lag2 + dLP_BwoI_b4_lag3 +
                       factor(country) + factor(code) + factor(year), data=pdata_lags, weights = wgt_i_avg)
  lowskill_lags_coef = func_coefs(lowskill_lags, "low_skill_lags", "Country")
  
  midskill_lags  = lm(midskill_changes ~ dLP_I_b1 + dLP_I_b1_lag1 + dLP_I_b1_lag2 + dLP_I_b1_lag3 +
                        dLP_I_b2 + dLP_I_b2_lag1 + dLP_I_b2_lag2 + dLP_I_b2_lag3 +
                        dLP_I_b3 + dLP_I_b3_lag1 + dLP_I_b3_lag2 + dLP_I_b3_lag3 +
                        dLP_I_b4 + dLP_I_b4_lag1 + dLP_I_b4_lag2 + dLP_I_b4_lag3 + 
                        dLP_BwoI_b1 + dLP_BwoI_b1_lag1 + dLP_BwoI_b1_lag2 + dLP_BwoI_b1_lag3 +
                        dLP_BwoI_b2 + dLP_BwoI_b2_lag1 + dLP_BwoI_b2_lag2 + dLP_BwoI_b2_lag3 +
                        dLP_BwoI_b3 + dLP_BwoI_b3_lag1 + dLP_BwoI_b3_lag2 + dLP_BwoI_b3_lag3 +
                        dLP_BwoI_b4 + dLP_BwoI_b4_lag1 + dLP_BwoI_b4_lag2 + dLP_BwoI_b4_lag3 +
                        factor(country) + factor(code) + factor(year), data=pdata_lags, weights = wgt_i_avg)
  midskill_lags_coef = func_coefs(midskill_lags , "mid_skill_lags ", "Country")
  
  highskill_lags  = lm(highskill_changes ~  dLP_I_b1 + dLP_I_b1_lag1 + dLP_I_b1_lag2 + dLP_I_b1_lag3 +
                         dLP_I_b2 + dLP_I_b2_lag1 + dLP_I_b2_lag2 + dLP_I_b2_lag3 +
                         dLP_I_b3 + dLP_I_b3_lag1 + dLP_I_b3_lag2 + dLP_I_b3_lag3 +
                         dLP_I_b4 + dLP_I_b4_lag1 + dLP_I_b4_lag2 + dLP_I_b4_lag3 + 
                         dLP_BwoI_b1 + dLP_BwoI_b1_lag1 + dLP_BwoI_b1_lag2 + dLP_BwoI_b1_lag3 +
                         dLP_BwoI_b2 + dLP_BwoI_b2_lag1 + dLP_BwoI_b2_lag2 + dLP_BwoI_b2_lag3 +
                         dLP_BwoI_b3 + dLP_BwoI_b3_lag1 + dLP_BwoI_b3_lag2 + dLP_BwoI_b3_lag3 +
                         dLP_BwoI_b4 + dLP_BwoI_b4_lag1 + dLP_BwoI_b4_lag2 + dLP_BwoI_b4_lag3 +
                         factor(country) + factor(code) + factor(year), data=pdata_lags, weights = wgt_i_avg)
  highskill_lags_coef = func_coefs(highskill_lags , "high_skill_lags ", "Country")
  
}


# eksporter
regoutput_share_panel <- Reduce(function(a,b){
  ans <- merge(a,b,by="row.names", all=T)
  row.names(ans) <- ans[,"Row.names"]
  ans[,!names(ans) %in% "Row.names"]
}, list(lowskill_coef, midskill_coef, highskill_coef, lowskill_lags_coef, midskill_lags_coef, highskill_lags_coef))

regoutput_share_panel$vars = row.names(regoutput_share_panel)
write_xlsx(regoutput_share_panel, "regoutput_share_panel.xlsx", col_names = TRUE)



##### Deskroptiv -------------------------------

min = as.Date("2009-1-1")
max = NA

pdata$year <- as.Date(ISOdate(pdata$year, 1, 1))

pdata_dk = pdata %>% filter(country=="DK") %>% select(-branche)

pdata_dk_low = pdata_dk %>% group_by(year) %>% summarise(tot = sum(EMP*(lowskill/100))) %>% mutate(educ="low")
pdata_dk_mid = pdata_dk %>% group_by(year) %>% summarise(tot = sum(EMP*(midskill/100))) %>% mutate(educ="mid")
pdata_dk_high = pdata_dk %>% group_by(year) %>% summarise(tot = sum(EMP*(highskill/100))) %>% mutate(educ="high")
pdata_dk_all = pdata_dk %>% group_by(year) %>% summarise(tot = sum(EMP)) %>% mutate(educ="all")

pdata_dk_stat2 = cbind(pdata_dk_low, pdata_dk_mid, pdata_dk_high, pdata_dk_all)

pdata_dk_stat2$share1 = pdata_dk_stat2[,2]/pdata_dk_stat2[,11]
pdata_dk_stat2$share2 = pdata_dk_stat2[,5]/pdata_dk_stat2[,11]
pdata_dk_stat2$share3 = pdata_dk_stat2[,8]/pdata_dk_stat2[,11]

pdata_dk_stat = rbind(pdata_dk_low, pdata_dk_mid, pdata_dk_high, pdata_dk_all)

ggplot(data=pdata_dk_stat, aes(x=year, y=tot, group=educ, colour=educ)) + 
  geom_point() + 
  geom_line() +
  xlab("Tid") + ylab("Kumulativ prædikterede ændring i beskæftigelse, pct.") +
  ggtitle("Kumulativ ændring i beskæftigelse i Danmark") +
  guides(colour=guide_legend(title="Branche")) +
  theme_economist() +
  theme(legend.position="right") +
  scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
  scale_x_date(limits = c(min, max))

all = pdata_dk_stat %>% filter( educ != "all")
all = merge(all, pdata_dk_all, by=c("year"), all.x = TRUE)
all$share = all$tot.x/all$tot.y

ggplot(data=all, aes(x=year, y=share, group=educ.x, colour=educ.x)) + 
  geom_point() + 
  geom_line() +
  xlab("Tid") + ylab("Andele af beskæftigelsen") +
  ggtitle("Udvikling i kompetencegruppernes andele af beskæftigelsen i Danmark") +
  guides(colour=guide_legend(title="Kompetencegruppe")) +
  theme_economist() +
  theme(legend.position="right") +
  scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
  scale_x_date(limits = c(min, max))


###

pdata_dk = pdata %>% filter(country=="DK")

pdata_dk_low = pdata_dk %>% group_by(branche, year) %>% summarise(tot = sum(EMP*(lowskill/100))) %>% mutate(educ="low")
pdata_dk_mid = pdata_dk %>% group_by(branche, year) %>% summarise(tot = sum(EMP*(midskill/100))) %>% mutate(educ="mid")
pdata_dk_high = pdata_dk %>% group_by(branche, year) %>% summarise(tot = sum(EMP*(highskill/100))) %>% mutate(educ="high")
pdata_dk_all = pdata_dk %>% group_by(branche, year) %>% summarise(tot = sum(EMP)) %>% mutate(educ="all")

pdata_dk_stat2 = cbind(pdata_dk_low, pdata_dk_mid, pdata_dk_high, pdata_dk_all)

pdata_dk_stat2$share1 = pdata_dk_stat2[,2]/pdata_dk_stat2[,11]
pdata_dk_stat2$share2 = pdata_dk_stat2[,5]/pdata_dk_stat2[,11]
pdata_dk_stat2$share3 = pdata_dk_stat2[,8]/pdata_dk_stat2[,11]

pdata_dk_stat = rbind(pdata_dk_low, pdata_dk_mid, pdata_dk_high, pdata_dk_all)

ggplot(data=pdata_dk_low, aes(x=year, y=tot, group=branche, colour=branche)) + 
  geom_point() + 
  geom_line() +
  xlab("Tid") + ylab("Kumulativ prædikterede ændring i beskæftigelse, pct.") +
  ggtitle("Kumulativ ændring i beskæftigelse i Danmark") +
  guides(colour=guide_legend(title="Branche")) +
  theme_economist() +
  theme(legend.position="right") +
  scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
  scale_x_date(limits = c(min, max))


b1 = pdata_dk_stat %>% filter(branche=="b1", educ != "all")
b1 = merge(b1, pdata_dk_all, by=c("branche", "year"), all.x = TRUE)
b1$share = b1$tot.x/b1$tot.y

ggplot(data=b1, aes(x=year, y=share, group=educ.x, colour=educ.x)) + 
  geom_point() + 
  geom_line() +
  xlab("Tid") + ylab("Andele af beskæftigelsen") +
  ggtitle("Udvikling i kompetencegruppernes andele af beskæftigelsen i primære erhverv-branchen") +
  guides(colour=guide_legend(title="Kompetencegruppe")) +
  theme_economist() +
  theme(legend.position="right") +
  scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
  scale_x_date(limits = c(min, max))

b2 = pdata_dk_stat %>% filter(branche=="b2", educ != "all")
b2 = merge(b2, pdata_dk_all, by=c("branche", "year"), all.x = TRUE)
b2$share = b2$tot.x/b2$tot.y

ggplot(data=b2, aes(x=year, y=share, group=educ.x, colour=educ.x)) + 
  geom_point() + 
  geom_line() +
  xlab("Tid") + ylab("Andele af beskæftigelsen") +
  ggtitle("Udvikling i kompetencegruppernes andele af beskæftigelsen i fremstillingsbranchen") +
  guides(colour=guide_legend(title="Kompetencegruppe")) +
  theme_economist() +
  theme(legend.position="right") +
  scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
  scale_x_date(limits = c(min, max))

b3 = pdata_dk_stat %>% filter(branche=="b3", educ != "all")
b3 = merge(b3, pdata_dk_all, by=c("branche", "year"), all.x = TRUE)
b3$share = b3$tot.x/b3$tot.y

ggplot(data=b3, aes(x=year, y=share, group=educ.x, colour=educ.x)) + 
  geom_point() + 
  geom_line() +
  xlab("Tid") + ylab("Andele af beskæftigelsen") +
  ggtitle("Udvikling i kompetencegruppernes andele af beskæftigelsen i den højteknologiske service-branche") +
  guides(colour=guide_legend(title="Kompetencegruppe")) +
  theme_economist() +
  theme(legend.position="right") +
  scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
  scale_x_date(limits = c(min, max))

b4 = pdata_dk_stat %>% filter(branche=="b4", educ != "all")
b4 = merge(b4, pdata_dk_all, by=c("branche", "year"), all.x = TRUE)
b4$share = b4$tot.x/b4$tot.y

ggplot(data=b4, aes(x=year, y=share, group=educ.x, colour=educ.x)) + 
  geom_point() + 
  geom_line() +
  xlab("Tid") + ylab("Andele af beskæftigelsen") +
  ggtitle("Udvikling i kompetencegruppernes andele af beskæftigelsen i den lavteknologiske service-branche") +
  guides(colour=guide_legend(title="Kompetencegruppe")) +
  theme_economist() +
  theme(legend.position="right") +
  scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
  scale_x_date(limits = c(min, max))

#########################


pdata_all = pdata
pdata_all_low = pdata_all %>% group_by(year) %>% summarise(tot = sum(EMP*(lowskill/100))) %>% mutate(educ="low")
pdata_all_mid = pdata_all %>% group_by(year) %>% summarise(tot = sum(EMP*(midskill/100))) %>% mutate(educ="mid")
pdata_all_high = pdata_all %>% group_by(year) %>% summarise(tot = sum(EMP*(highskill/100))) %>% mutate(educ="high")
pdata_all_all = pdata_all %>% group_by(year) %>% summarise(tot = sum(EMP)) %>% mutate(educ="all")

pdata_all_stat2 = cbind(pdata_all_low, pdata_all_mid, pdata_all_high, pdata_all_all)

pdata_all_stat2$share1 = pdata_all_stat2[,2]/pdata_all_stat2[,11]
pdata_all_stat2$share2 = pdata_all_stat2[,5]/pdata_all_stat2[,11]
pdata_all_stat2$share3 = pdata_all_stat2[,8]/pdata_all_stat2[,11]

pdata_all_stat = rbind(pdata_all_low, pdata_all_mid, pdata_all_high, pdata_all_all)



ggplot(data=pdata_all_stat, aes(x=year, y=tot, group=educ, colour=educ)) + 
  geom_point() + 
  geom_line() +
  xlab("Tid") + ylab("Kumulativ prædikterede ændring i beskæftigelse, pct.") +
  ggtitle("Kumulativ ændring i beskæftigelse i Danmark") +
  guides(colour=guide_legend(title="Branche")) +
  theme_economist() +
  theme(legend.position="right") +
  scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
  scale_x_date(limits = c(min, max))








