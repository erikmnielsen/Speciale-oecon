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

CGR = function(x){
  sapply(1:length(x), function(y){
    prod(1+x[1:y]) - 1
  })
}

func_coefs <- function(regression, name, type, method) {
  
  options(scipen=999, digits=4) 
  #options(scipen=0, digits=7) #default
  
  if (type != "") {
    
    siglvl = stars.pval(coeftest(regression, vcov. = vcovHC, method=method, type=type)[,4])
    reg_coef = cbind(coeftest(regression, vcov. = vcovHC, type=type)[,c(1,4)], siglvl)
    colnames(reg_coef) <- paste(name, colnames(reg_coef), sep = "_")
    
  }  else if (method != "") {
    
    siglvl = stars.pval(coeftest(regression, vcov. = vcovHC, method=method)[,4])
    reg_coef = cbind(coeftest(regression, vcov. = vcovHC)[,c(1,4)], siglvl)
    colnames(reg_coef) <- paste(name, colnames(reg_coef), sep = "_")
    
  } else {
    
    siglvl = stars.pval(coeftest(regression, vcov. = vcovHC, method=method)[,4])
    reg_coef = cbind(coeftest(regression, vcov. = vcovHC)[,c(1,4)], siglvl)
    colnames(reg_coef) <- paste(name, colnames(reg_coef), sep = "_")
  }
  
  
  
  
  #coeftest(zz, vcov.=function(x) vcovHC(x, method="arellano", type="HC1", cluster="group"))
  
  reg_coef
  
}

func_empprod <- function(method, type) {
  
  #EUK_growthaccounts <- readRDS("~/OneDrive - Aalborg Universitet/10. SEMESTER (SPECIALE)/Speciale-oecon/Statistical_Growth-Accounts.rds")
  EUK_nationalaccounts <- readRDS("Data/Statistical_National-Accounts.rds")
  EUK_nationalaccountslabour <- read_csv("education_csv.csv") %>% select(-X1)

    
    #AutorSalomons Industrier minus branche 3 :
    
  data = EUK_nationalaccounts %>% filter(code %in% c("B","C","D","E","F","G","H","I","J","K","L","M_N"))
  data_educ = EUK_nationalaccountslabour

  data_emp = data %>% filter(var=="EMP") %>% mutate(EMP=value) %>% select(-var, -value)
  data_go = data %>% filter(var=="GO_Q") %>% mutate(GO=value) %>% select(country, code, year, GO)
  data = merge(data_emp, data_go, by=c("country","code", "year"), all.x = TRUE)
  data_educ$country = data_educ$Country
  data_educ$code = data_educ$Code
  data_educ$year = data_educ$variable
  data_educ = data_educ %>% select(country, code, year, Education, value)
  data = merge(data, data_educ, by=c("country","code", "year"), all.x = TRUE)
  data = na.omit(data) #sletter rækker hvor enten GO eller EMP tal mangler
  data = data %>% select(country, code, year, EMP, GO, Education, sum_value)
  
  gns = data %>% group_by(country, code) %>% summarize(EMP_gns=sum(EMP))
  gns_2 = data %>% group_by(country) %>% summarize(EMP_gns_2=sum(EMP))
  gns = merge(gns, gns_2, by=c("country"), all.x = TRUE)
  gns$wgt_i_avg = gns$EMP_gns/gns$EMP_gns_2
  gns = gns %>% select(country, code, wgt_i_avg)
  data = merge(data, gns, by=c("country","code"), all.x = TRUE)
  
  if (type==1) {
    
    data_type1 = data %>% filter(Education==1)
    data_type1$id_ci = data_type1 %>% group_indices(country, code)
    pdata = pdata.frame(data_type1, index = c("id_ci", "year")) #Hvis R melder duplikater, hvilket bare skyldes at der er to variable for hver
    
    pdata$educshare_changes = diff(pdata$value, lag = 1, shift = "time")
    pdata$prod_logchanges = diff(log(pdata$GO/pdata$EMP), lag = 1, shift = "time")*100
    pdata = na.omit(pdata)
    data = pdata
  
  } else if (type==2) {
    
    data_type2 = data %>% filter(Education==2)
    data_type2$id_ci = data_type2 %>% group_indices(country, code)
    pdata = pdata.frame(data_type2, index = c("id_ci", "year")) #Hvis R melder duplikater, hvilket bare skyldes at der er to variable for hver
    
    pdata$educshare_changes = diff(pdata$value, lag = 1, shift = "time")
    pdata$prod_logchanges = diff(log(pdata$GO/pdata$EMP), lag = 1, shift = "time")*100
    pdata = na.omit(pdata)
    
  } else if (type==3) {
      
    data_type3 = data %>% filter(Education==3)
    data_type3$id_ci = data_type3 %>% group_indices(country, code)
    pdata = pdata.frame(data_type3, index = c("id_ci", "year")) #Hvis R melder duplikater, hvilket bare skyldes at der er to variable for hver
    
    pdata$educshare_changes = diff(pdata$value, lag = 1, shift = "time")
    pdata$prod_logchanges = diff(log(pdata$GO/pdata$EMP), lag = 1, shift = "time")*100
    pdata = na.omit(pdata)
    
  } else {print("Error: forkert antal brancher")}
  
  #Kodning af variable:

  
}

c_panel_1 = func_empprod(,1)
na.omit(c_panel_1)
lsdv.c_pool1 = lm(educshare_changes ~ prod_logchanges, data=c_panel_1, weights = wgt_i_avg)
lsdv.1 = func_coefs(lsdv.c_pool1, "c_pool1", "HC3")

lsdv.c_pool1 = lm(educshare_changes ~ prod_logchanges + factor(country), data=c_panel_1, weights = wgt_i_avg)
lsdv.2 = func_coefs(lsdv.c_pool1, "c_fec1", "HC3")

lsdv.c_pool1 = lm(educshare_changes ~ prod_logchanges + factor(country) + factor(year), data=c_panel_1, weights = wgt_i_avg)
lsdv.3 = func_coefs(lsdv.c_pool1, "c_fecy1", "HC3")

lsdv.c_pool1 = lm(educshare_changes ~ prod_logchanges + factor(country) + factor(code) + factor(year), data=c_panel_1, weights = wgt_i_avg)
lsdv.4 = func_coefs(lsdv.c_pool1, "c_fecyi1", "HC3")

c_panel_2 = func_empprod(,2)
na.omit(c_panel_2)
lsdv.c_pool2 = lm(educshare_changes ~ prod_logchanges, data=c_panel_2, weights = wgt_i_avg)
lsdv.5 = func_coefs(lsdv.c_pool2, "c_pool2", "HC3")

lsdv.c_pool2 = lm(educshare_changes ~ prod_logchanges + factor(country), data=c_panel_2, weights = wgt_i_avg)
lsdv.6 = func_coefs(lsdv.c_pool2, "c_fec2", "HC3")

lsdv.c_pool2 = lm(educshare_changes ~ prod_logchanges + factor(country) + factor(year), data=c_panel_2, weights = wgt_i_avg)
lsdv.7 = func_coefs(lsdv.c_pool2, "c_fecy2", "HC3")

lsdv.c_pool2 = lm(educshare_changes ~ prod_logchanges + factor(country) + factor(code) + factor(year), data=c_panel_2, weights = wgt_i_avg)
lsdv.8 = func_coefs(lsdv.c_pool2, "c_fecyi2", "HC3")

c_panel_3 = func_empprod(,3)
na.omit(c_panel_3)
lsdv.c_pool3 = lm(educshare_changes ~ prod_logchanges, data=c_panel_3, weights = wgt_i_avg)
lsdv.9 = func_coefs(lsdv.c_pool3, "c_pool3", "HC3")

lsdv.c_pool3 = lm(educshare_changes ~ prod_logchanges + factor(country), data=c_panel_3, weights = wgt_i_avg)
lsdv.10 = func_coefs(lsdv.c_pool3, "c_fec3", "HC3")

lsdv.c_pool3 = lm(educshare_changes ~ prod_logchanges + factor(country) + factor(year), data=c_panel_3, weights = wgt_i_avg)
lsdv.11 = func_coefs(lsdv.c_pool3, "c_fecy3", "HC3")

lsdv.c_pool3 = lm(educshare_changes ~ prod_logchanges + factor(country) + factor(code) + factor(year), data=c_panel_3, weights = wgt_i_avg)
lsdv.12 = func_coefs(lsdv.c_pool3, "c_fecyi3", "HC3")


table(data$country, data$year)
