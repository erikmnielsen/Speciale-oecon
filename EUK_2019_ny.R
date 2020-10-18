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
library(dplyr)
library(ggthemes)
library(gtools)
library(lmtest)
library(olsrr)
library(sandwich)

CGR = function(x){
  sapply(1:length(x), function(y){
    prod(1+x[1:y]) - 1 # https://humbledollar.com/money-guide/annualized-vs-cumulative-returns/
  })
}

func_coefs <- function(regression, name, type) {
  
  options(scipen=999, digits=4) 
  #options(scipen=0, digits=7) #default
  
  if (type=="Country"){
  
  coef = coeftest(regression, vcovPL(regression, cluster = ~ country, kernel = "Bartlett"))
  
    siglvl = stars.pval(coef[,4])
    reg_coef = cbind(coef[,c(1,2)], siglvl)
    colnames(reg_coef) <- paste(name, colnames(reg_coef), sep = "_")

  reg_coef
  
  } else if (type=="Year"){
    
    coef = coeftest(regression, vcovPL(regression, cluster = ~ year, kernel = "Bartlett"))
    
    siglvl = stars.pval(coef[,4])
    reg_coef = cbind(coef[,c(1,2)], siglvl)
    colnames(reg_coef) <- paste(name, colnames(reg_coef), sep = "_")
    
    reg_coef
    
    
  }else if (type=="None"){
    
    coef = coeftest(regression, vcovPL(regression, cluster = NULL, kernel = "Bartlett"))
    
    siglvl = stars.pval(coef[,4])
    reg_coef = cbind(coef[,c(1,2)], siglvl)
    colnames(reg_coef) <- paste(name, colnames(reg_coef), sep = "_")
    
    reg_coef
    
    
  } else {
    
    NA
  }
  
}

#method = "MN_4" eller "AS_5"
#type = "industrier" eller "lande"
func_empprod <- function(method, type) {
  
  #EUK_growthaccounts <- readRDS("~/OneDrive - Aalborg Universitet/10. SEMESTER (SPECIALE)/Speciale-oecon/Statistical_Growth-Accounts.rds")
  EUK_nationalaccounts <- readRDS("Data/Statistical_National-Accounts.rds")
  
  if (method=="MN_4") {
    
    #AutorSalomons Industrier minus branche 3 :
    
    data = EUK_nationalaccounts %>% filter(var %in% c("EMP","GO_Q"), !indnr %in% c("Agg", "*Agg", "1", "34","35", "36","37","38","39","40")) #svarer til A, O, P, Q, R, S, T,U
    
    
    data$branche <- ifelse(data$indnr %in% c(2,16,17,18), "b1", #B,D,E,F
                           ifelse(data$indnr %in% c(3,4,5,6,7,8,9,10,11,12,13,14,15), "b2", #hele C
                                  ifelse(data$indnr %in% c(28,29,30,31, 33), "b3", #("J58t60", "J61", "J62t63", "K", "MtN")
                                         ifelse(data$indnr %in% c(19,20,21,22,23,24,25,26,27,32), "b4", #("G45", "G46", "G47", "H49t52","H53", "I", "L")
                                                "b0"))))
    
    data$branche_desc <- ifelse(data$branche=="b1","Mining, utilities, and construction", 
                                ifelse(data$branche=="b2","Manufacturing", 
                                       ifelse(data$branche=="b3","High-tech services",
                                              ifelse(data$branche=="b4","Low-tech services",
                                                     "Not relevant"
                                              ))))
  
  } else if (method=="AS_5") {
    #AutorSalomons Industrier:
    
    data = EUK_nationalaccounts %>% filter(var %in% c("EMP","GO_Q"), !indnr %in% c("Agg", "*Agg", "1", "34","39","40")) #svarer til A,O,T,U
  
    data$branche <- ifelse(data$indnr %in% c(2,16,17,18), "b1", #B,D,E,F
                           ifelse(data$indnr %in% c(3,4,5,6,7,8,9,10,11,12,13,14,15), "b2", #hele C
                                  ifelse(data$indnr %in% c(35,36,37,38), "b3", #("P","Q","R", "S")
                                  ifelse(data$indnr %in% c(28,29,30,31,33), "b4", #("58t60", "61", "62t63", "K", "MtN")
                                         ifelse(data$indnr %in% c(19,20,21,22,23,24,25,26,27,32), "b5", #("45", "46", "47", "49t52", "53",  "I", "L")
                                                "b0")))))
    
    data$branche_desc <- ifelse(data$branche=="b1","Mining, utilities, and construction", 
                                ifelse(data$branche=="b2","Manufacturing", 
                                       ifelse(data$branche=="b3","Education and health services", 
                                              ifelse(data$branche=="b4","High-tech services",
                                                     ifelse(data$branche=="b5","Low-tech services",
                                                            "Not relevant"
                                                     )))))
    
  } else {
    
    NA
    
  }
  
  data_emp = data %>% filter(var=="EMP") %>% mutate(EMP=value) %>% select(-var, -value)
  data_go = data %>% filter(var=="GO_Q") %>% mutate(GO=value) %>% select(country, code, year, GO)
  data = merge(data_emp, data_go, by=c("country","code", "year"), all.x = TRUE)
  data <- na.omit(data) #sletter rækker hvor enten GO eller EMP tal mangler
  
  #filtrering af lande/år hvor der mangler FOR mange industrier
  test = data %>% group_by(country, year) %>% count() #der skal være 32 industrier for hvert år i hvert land
  test2 = test %>% filter(n!=32)
  
  data = merge(data, test, by=c("country", "year"), all.x = TRUE)
  data = data %>% filter(n>=28)
  
  lande = data %>% select(country) %>% unique()

  
  #populationsvariabel
  {
  WKGPOP = read_csv("Data/DP_LIVE_17082020135004159.csv")
  POP = read_csv("Data/DP_LIVE_17082020142655432.csv")
  
  POP = data.frame(LOCATION = POP$LOCATION, TIME = POP$TIME, POP = POP$Value)
  POP = POP %>% select(LOCATION, TIME, POP)
  WKGPOP = merge(WKGPOP, POP, by=c("LOCATION", "TIME" ), all.x = TRUE)
  
  pop_var =  data.frame(country = ifelse(WKGPOP$LOCATION=="AUS", "AT",
                                        ifelse(WKGPOP$LOCATION=="BEL", "BE",
                                               ifelse(WKGPOP$LOCATION=="CZE", "CZ",
                                                      ifelse(WKGPOP$LOCATION=="DEU", "DE",
                                                             ifelse(WKGPOP$LOCATION=="DNK", "DK",
                                                                    ifelse(WKGPOP$LOCATION=="EST", "EE",
                                                                           ifelse(WKGPOP$LOCATION=="GRC", "EL",
                                                                                  ifelse(WKGPOP$LOCATION=="FIN", "FI",
                                                                                         ifelse(WKGPOP$LOCATION=="FRA", "FR",
                                        ifelse(WKGPOP$LOCATION=="HRV", "HR",
                                               ifelse(WKGPOP$LOCATION=="HUN", "HU",
                                                      ifelse(WKGPOP$LOCATION=="ITA", "IT",
                                                             ifelse(WKGPOP$LOCATION=="JPN", "JP",
                                                                    ifelse(WKGPOP$LOCATION=="LVA", "LV",
                                                                           ifelse(WKGPOP$LOCATION=="NLD", "NL",
                                                                                  ifelse(WKGPOP$LOCATION=="POL", "PL",
                                                                                         ifelse(WKGPOP$LOCATION=="PRT", "PT",
                                        ifelse(WKGPOP$LOCATION=="ROU", "RO",
                                               ifelse(WKGPOP$LOCATION=="SWE", "SE",
                                                      ifelse(WKGPOP$LOCATION=="SVN", "SI",
                                                             ifelse(WKGPOP$LOCATION=="SVK", "SK",
                                                                    ifelse(WKGPOP$LOCATION=="USA", "US",
                                                                           NA)))))))))))))))))))))),
                       year = WKGPOP$TIME,
                       wkgpop = (WKGPOP$Value/100) * WKGPOP$POP,
                       pop = WKGPOP$POP,
                       wkgpop_share = WKGPOP$Value
                       )
  
  }
  
  pop_var = na.omit(pop_var)

  # forberedelse af datasættet - land eller industriniveau
  if (type=="industrier") {
  
  data$id_ci = data %>% group_indices(country, code) 
  data = merge(data, pop_var, by=c("country", "year"), all.x = TRUE)
  pdata = pdata.frame(data, index = c("id_ci", "year")) #Hvis R melder duplikater, hvilket bare skyldes at der er to variable for hver
  
  pdata$emp_log = log(pdata$EMP)
  pdata$prod_log = log(pdata$GO/pdata$EMP)
  pdata$emp_logchanges = diff(log(pdata$EMP), lag = 1, shift = "time")*100
  pdata$go_logchanges = diff(log(pdata$GO), lag = 1, shift = "time")*100
  pdata$prod_logchanges = diff(log(pdata$GO/pdata$EMP), lag = 1, shift = "time")*100
  pdata$wkgpop_logchanges = diff(log(pdata$wkgpop), lag = 1, shift = "time")*100
  pdata$pop_logchanges = diff(log(pdata$pop), lag = 1, shift = "time")*100
  
  } else if (type=="lande"){
    
    data <- data %>% group_by(country, year) %>% summarise(EMP=sum(EMP) , GO=sum(GO), .groups = 'drop')
    data <- data %>% ungroup()
    data = merge(data, pop_var, by=c("country", "year"), all.x = TRUE)
    pdata = pdata.frame(data, index = c("country", "year")) 
    
    pdata$emp_log = log(pdata$EMP)
    pdata$prod_log = log(pdata$GO/pdata$EMP)
    pdata$emp_logchanges = diff(log(pdata$EMP), lag = 1, shift = "time")*100
    pdata$go_logchanges = diff(log(pdata$GO), lag = 1, shift = "time")*100
    pdata$prod_logchanges = diff(log(pdata$GO/pdata$EMP), lag = 1, shift = "time")*100
    pdata$wkgpop_logchanges = diff(log(pdata$wkgpop), lag = 1, shift = "time")*100
    pdata$pop_logchanges = diff(log(pdata$pop), lag = 1, shift = "time")*100

  } else {print("Error: forkert antal brancher")}

#Kodning af variable:

pdata
}

#method = "MN_4" eller "AS_5"
#type = "1" (uden lags) eller "2" (med lags)
func_regpanel <- function(dataset_1, method, type) {
  
    #angivelse af branche/industri totaler - der bør ikke være nogle branche = b0
    b_tot <- dataset_1 %>% group_by(country, year, branche) %>% summarize(EMP_b=sum(EMP) , GO_b=sum(GO))
    ind = merge(dataset_1, b_tot, by=c("country", "year", "branche"), all.x = TRUE) 
    
    if (method=="MN_4") {
      
      b_tot = b_tot %>% ungroup()
      
      #uden branche 3
      b1 = b_tot %>% filter(branche=="b1") %>% mutate(EMP_b1=EMP_b) %>% mutate(GO_b1=GO_b) %>% select(country, year, EMP_b1,GO_b1)
      b2 = b_tot %>% filter(branche=="b2") %>% mutate(EMP_b2=EMP_b) %>% mutate(GO_b2=GO_b) %>% select(EMP_b2, GO_b2)
      b3 = b_tot %>% filter(branche=="b3") %>% mutate(EMP_b3=EMP_b) %>% mutate(GO_b3=GO_b) %>% select(EMP_b3, GO_b3)
      b4 = b_tot %>% filter(branche=="b4") %>% mutate(EMP_b4=EMP_b) %>% mutate(GO_b4=GO_b) %>% select(EMP_b4, GO_b4)
      
      b = cbind(b1,b2,b3,b4)
      
    } else if (method=="AS_5") {
      
      b1 = b %>% filter(branche=="b1") %>% mutate(EMP_b1=EMP_b) %>% mutate(GO_b1=GO_b) %>% select(year, EMP_b1,GO_b1)
      b2 = b %>% filter(branche=="b2") %>% mutate(EMP_b2=EMP_b) %>% mutate(GO_b2=GO_b) %>% select(EMP_b2, GO_b2)
      b3 = b %>% filter(branche=="b3") %>% mutate(EMP_b3=EMP_b) %>% mutate(GO_b3=GO_b) %>% select(EMP_b3, GO_b3)
      b4 = b %>% filter(branche=="b4") %>% mutate(EMP_b4=EMP_b) %>% mutate(GO_b4=GO_b) %>% select(EMP_b4, GO_b4)
      b5 = b %>% filter(branche=="b5") %>% mutate(EMP_b5=EMP_b) %>% mutate(GO_b5=GO_b) %>% select(EMP_b5, GO_b5)
      
      b = cbind(b1,b2,b3,b4,b5)
      
    } else {print("Error: forkert antal brancher") }
    
    ind = merge(ind, b, by=c("country","year"), all.x = TRUE)

    #gennemsnit på tværs af år for den enkelte industri
    gns = ind %>% group_by(country, code) %>% summarize(EMP_gns=sum(EMP))
    gns_2 = ind %>% group_by(country) %>% summarize(EMP_gns_2=sum(EMP))
    gns = merge(gns, gns_2, by=c("country"), all.x = TRUE)
    gns$wgt_i_avg = gns$EMP_gns/gns$EMP_gns_2
    gns = gns %>% select(country, code, wgt_i_avg)
    
    ind = merge(ind, gns, by=c("country","code"), all.x = TRUE)
    
    if (type==1) {
      
    ind = pdata.frame(ind, index = c("id_ci", "year"))
    ind = na.omit(ind)
    
    
    } else if (type==2) {
    
    ind = pdata.frame(ind, index = c("id_ci", "year"))
      
    ind$prod_logchanges_lag1 = lag(ind$prod_logchanges, k = 1, shift = "time")
    ind$prod_logchanges_lag2 = lag(ind$prod_logchanges, k = 2, shift = "time")
    ind$prod_logchanges_lag3 = lag(ind$prod_logchanges, k = 3, shift = "time")
    
    #Beta2 variable og lags, mikro + makro
    c_tot <- dataset_1 %>% group_by(country, year) %>% summarize(EMP_c=sum(EMP) , GO_c=sum(GO))
    ind = merge(ind, c_tot, by=c("country","year"), all.x = TRUE)
    
    ind = pdata.frame(ind, index = c("id_ci", "year"))
    
    ind$dLP_CwoI = diff(log((ind$GO_c-ind$GO)/(ind$EMP_c-ind$EMP)), lag = 1, shift = "time")*100
    ind$dLP_CwoI_lag1 = lag(ind$dLP_CwoI, k = 1, shift = "time")
    ind$dLP_CwoI_lag2 = lag(ind$dLP_CwoI, k = 2, shift = "time")
    ind$dLP_CwoI_lag3 = lag(ind$dLP_CwoI, k = 3, shift = "time")
    
    
    #Beta2 og beta1 variable og lags, sektor spillover 
    
    if (method=="MN_4") {
      
      #Beta2 variable og lags, sektor spillover - obs hvis faste priser kan totaler fra euklems ikke bruges
      ind$dLP_BwoI_b1 = ifelse(ind$branche=="b1", diff(log((ind$GO_b1-ind$GO)/(ind$EMP_b1-ind$EMP)), lag = 1, shift = "time")*100, diff(log(ind$GO_b1/ind$EMP_b1), lag = 1, shift = "time")*100)
      ind$dLP_BwoI_b2 = ifelse(ind$branche=="b2", diff(log((ind$GO_b2-ind$GO)/(ind$EMP_b2-ind$EMP)), lag = 1, shift = "time")*100, diff(log(ind$GO_b2/ind$EMP_b2), lag = 1, shift = "time")*100)
      ind$dLP_BwoI_b3 = ifelse(ind$branche=="b3", diff(log((ind$GO_b3-ind$GO)/(ind$EMP_b3-ind$EMP)), lag = 1, shift = "time")*100, diff(log(ind$GO_b3/ind$EMP_b3), lag = 1, shift = "time")*100)
      ind$dLP_BwoI_b4 = ifelse(ind$branche=="b4", diff(log((ind$GO_b4-ind$GO)/(ind$EMP_b4-ind$EMP)), lag = 1, shift = "time")*100, diff(log(ind$GO_b4/ind$EMP_b4), lag = 1, shift = "time")*100)
      
      ind$dLP_BwoI_b1_lag1 = lag(ind$dLP_BwoI_b1, k = 1, shift = "time")
      ind$dLP_BwoI_b1_lag2 = lag(ind$dLP_BwoI_b1, k = 2, shift = "time")
      ind$dLP_BwoI_b1_lag3 = lag(ind$dLP_BwoI_b1, k = 3, shift = "time")
      
      ind$dLP_BwoI_b2_lag1 = lag(ind$dLP_BwoI_b2, k = 1, shift = "time")
      ind$dLP_BwoI_b2_lag2 = lag(ind$dLP_BwoI_b2, k = 2, shift = "time")
      ind$dLP_BwoI_b2_lag3 = lag(ind$dLP_BwoI_b2, k = 3, shift = "time")
      
      ind$dLP_BwoI_b3_lag1 = lag(ind$dLP_BwoI_b3, k = 1, shift = "time")
      ind$dLP_BwoI_b3_lag2 = lag(ind$dLP_BwoI_b3, k = 2, shift = "time")
      ind$dLP_BwoI_b3_lag3 = lag(ind$dLP_BwoI_b3, k = 3, shift = "time")
      
      ind$dLP_BwoI_b4_lag1 = lag(ind$dLP_BwoI_b4, k = 1, shift = "time")
      ind$dLP_BwoI_b4_lag2 = lag(ind$dLP_BwoI_b4, k = 2, shift = "time")
      ind$dLP_BwoI_b4_lag3 = lag(ind$dLP_BwoI_b4, k = 3, shift = "time")
      
      
      #beta1 variable, sectoral spillover:
      
      ind$dLP_I_b1 = ifelse(ind$branche=="b1", ind$prod_logchanges, 0)
      ind$dLP_I_b2 = ifelse(ind$branche=="b2", ind$prod_logchanges, 0)
      ind$dLP_I_b3 = ifelse(ind$branche=="b3", ind$prod_logchanges, 0)
      ind$dLP_I_b4 = ifelse(ind$branche=="b4", ind$prod_logchanges, 0)

      ind$dLP_I_b1_lag1 = lag(ind$dLP_I_b1, k = 1, shift = "time")
      ind$dLP_I_b1_lag2 = lag(ind$dLP_I_b1, k = 2, shift = "time")
      ind$dLP_I_b1_lag3 = lag(ind$dLP_I_b1, k = 3, shift = "time")
      
      ind$dLP_I_b2_lag1 = lag(ind$dLP_I_b2, k = 1, shift = "time")
      ind$dLP_I_b2_lag2 = lag(ind$dLP_I_b2, k = 2, shift = "time")
      ind$dLP_I_b2_lag3 = lag(ind$dLP_I_b2, k = 3, shift = "time")
      
      ind$dLP_I_b3_lag1 = lag(ind$dLP_I_b3, k = 1, shift = "time")
      ind$dLP_I_b3_lag2 = lag(ind$dLP_I_b3, k = 2, shift = "time")
      ind$dLP_I_b3_lag3 = lag(ind$dLP_I_b3, k = 3, shift = "time")
      
      ind$dLP_I_b4_lag1 = lag(ind$dLP_I_b4, k = 1, shift = "time")
      ind$dLP_I_b4_lag2 = lag(ind$dLP_I_b4, k = 2, shift = "time")
      ind$dLP_I_b4_lag3 = lag(ind$dLP_I_b4, k = 3, shift = "time")
      
      ind = na.omit(ind)
      
    } else if (method=="AS_5") {
      
      ind$dLP_BwoI_b1 = ifelse(ind$branche=="b1", diff(log((ind$GO_b1-ind$GO)/(ind$EMP_b1-ind$EMP)), lag = 1, shift = "time")*100, diff(log(ind$GO_b1/ind$EMP_b1), lag = 1, shift = "time")*100)
      ind$dLP_BwoI_b2 = ifelse(ind$branche=="b2", diff(log((ind$GO_b2-ind$GO)/(ind$EMP_b2-ind$EMP)), lag = 1, shift = "time")*100, diff(log(ind$GO_b2/ind$EMP_b2), lag = 1, shift = "time")*100)
      ind$dLP_BwoI_b3 = ifelse(ind$branche=="b3", diff(log((ind$GO_b3-ind$GO)/(ind$EMP_b3-ind$EMP)), lag = 1, shift = "time")*100, diff(log(ind$GO_b3/ind$EMP_b3), lag = 1, shift = "time")*100)
      ind$dLP_BwoI_b4 = ifelse(ind$branche=="b4", diff(log((ind$GO_b4-ind$GO)/(ind$EMP_b4-ind$EMP)), lag = 1, shift = "time")*100, diff(log(ind$GO_b4/ind$EMP_b4), lag = 1, shift = "time")*100)
      ind$dLP_BwoI_b5 = ifelse(ind$branche=="b5", diff(log((ind$GO_b5-ind$GO)/(ind$EMP_b5-ind$EMP)), lag = 1, shift = "time")*100, diff(log(ind$GO_b5/ind$EMP_b5), lag = 1, shift = "time")*100)
      
      ind$dLP_BwoI_b1_lag1 = lag(ind$dLP_BwoI_b1, k = 1, shift = "time")
      ind$dLP_BwoI_b1_lag2 = lag(ind$dLP_BwoI_b1, k = 2, shift = "time")
      ind$dLP_BwoI_b1_lag3 = lag(ind$dLP_BwoI_b1, k = 3, shift = "time")
      
      ind$dLP_BwoI_b2_lag1 = lag(ind$dLP_BwoI_b2, k = 1, shift = "time")
      ind$dLP_BwoI_b2_lag2 = lag(ind$dLP_BwoI_b2, k = 2, shift = "time")
      ind$dLP_BwoI_b2_lag3 = lag(ind$dLP_BwoI_b2, k = 3, shift = "time")
      
      ind$dLP_BwoI_b3_lag1 = lag(ind$dLP_BwoI_b3, k = 1, shift = "time")
      ind$dLP_BwoI_b3_lag2 = lag(ind$dLP_BwoI_b3, k = 2, shift = "time")
      ind$dLP_BwoI_b3_lag3 = lag(ind$dLP_BwoI_b3, k = 3, shift = "time")
      
      ind$dLP_BwoI_b4_lag1 = lag(ind$dLP_BwoI_b4, k = 1, shift = "time")
      ind$dLP_BwoI_b4_lag2 = lag(ind$dLP_BwoI_b4, k = 2, shift = "time")
      ind$dLP_BwoI_b4_lag3 = lag(ind$dLP_BwoI_b4, k = 3, shift = "time")
      
      ind$dLP_BwoI_b5_lag1 = lag(ind$dLP_BwoI_b5, k = 1, shift = "time")
      ind$dLP_BwoI_b5_lag2 = lag(ind$dLP_BwoI_b5, k = 2, shift = "time")
      ind$dLP_BwoI_b5_lag3 = lag(ind$dLP_BwoI_b5, k = 3, shift = "time")
      
      ind = na.omit(ind)
      
      ind$dLP_I_b1 = ifelse(ind$branche=="b1", ind$prod_logchanges, 0)
      ind$dLP_I_b1_dum = ifelse(ind$dLP_I_b1==0, 0, 1)
      ind$dLP_I_b2 = ifelse(ind$branche=="b2", ind$prod_logchanges, 0)
      ind$dLP_I_b2_dum = ifelse(ind$dLP_I_b2==0, 0, 1)
      ind$dLP_I_b3 = ifelse(ind$branche=="b3", ind$prod_logchanges, 0)
      ind$dLP_I_b3_dum = ifelse(ind$dLP_I_b3==0, 0, 1)
      ind$dLP_I_b4 = ifelse(ind$branche=="b4", ind$prod_logchanges, 0)
      ind$dLP_I_b4_dum = ifelse(ind$dLP_I_b4==0, 0, 1)
      ind$dLP_I_b5 = ifelse(ind$branche=="b5", ind$prod_logchanges, 0)
      ind$dLP_I_b5_dum = ifelse(ind$dLP_I_b5==0, 0, 1)
      
    } else {print("Error: forkert antal brancher") }
    
    } else {print("Error: 'type' ikke opgivet") }
    
    ind
}

c_panel = func_empprod("MN_4","lande")
ci_panel = func_empprod("MN_4","industrier")

# Country panel  -----------------------------------------------------

#c_panel = as.data.frame(c_panel)
#c_panel = pdata.frame(c_panel, index = c("country", "year"))

c_panel_lags = c_panel
  c_panel_lags$prod_log_lag1 = lag(c_panel_lags$prod_log, k = 1, shift = "time")
  c_panel_lags$prod_log_lag2 = lag(c_panel_lags$prod_log_lag1, k = 1, shift = "time")
  c_panel_lags$prod_log_lag3 = lag(c_panel_lags$prod_log_lag2, k = 1, shift = "time")
c_panel_lags$prod_logchanges_lag1 = lag(c_panel_lags$prod_logchanges, k = 1, shift = "time")
c_panel_lags$prod_logchanges_lag2 = lag(c_panel_lags$prod_logchanges_lag1, k = 1, shift = "time")
c_panel_lags$prod_logchanges_lag3 = lag(c_panel_lags$prod_logchanges_lag2, k = 1, shift = "time")

c_panel_lags = na.omit(c_panel_lags)
c_panel = na.omit(c_panel)

#Uden lags
lsdv.c_pool1 = lm(emp_logchanges ~ prod_logchanges, data=c_panel)
#lsdv.c_fey1 = lm(emp_logchanges ~ prod_logchanges + factor(year), data=c_panel)
lsdv.c_fec1 = lm(emp_logchanges ~ prod_logchanges + factor(country), data=c_panel) 
#lsdv.c_fecy1 = lm(emp_logchanges ~ prod_logchanges + factor(country) + factor(year) 

# når year tilføjes bliver estimaterne negative

#Med lags
lsdv.c_pool2 = lm(emp_logchanges ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3, data=c_panel_lags)
lsdv.c_fec2  = lm(emp_logchanges ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + factor(country), data=c_panel_lags)
#lsdv.c_fecy2  = lm(emp_logchanges ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + factor(country) + factor(year), data=c_panel_lags)
#lsdv.c_fec2_pop  = lm(emp_logchanges ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + factor(country) + pop_logchanges, data=c_panel_lags)

#Robuste standard fejl
lsdv.c_pool1_coef = func_coefs(lsdv.c_pool1, "c_pool1", "None")
lsdv.c_fec1_coef = func_coefs(lsdv.c_fec1, "c_fec1", "None")
lsdv.c_pool2_coef = func_coefs(lsdv.c_pool2, "c_pool2", "None")
lsdv.c_fec2_coef = func_coefs(lsdv.c_fec2, "c_fec2", "None")
#lsdv.c_fec2_pop_coef = func_coefs(lsdv.c_fec2_pop, "c_fec2_pop", "None")

regoutput_c_panel <- Reduce(function(a,b){
  ans <- merge(a,b,by="row.names", all=T)
  row.names(ans) <- ans[,"Row.names"]
  ans[,!names(ans) %in% "Row.names"]
}, list(lsdv.c_pool1_coef, lsdv.c_fec1_coef, lsdv.c_pool2_coef, lsdv.c_fec2_coef))

regoutput_c_panel$vars = row.names(regoutput_c_panel)
write_xlsx(regoutput_c_panel, "regoutput_c_panel.xlsx", col_names = TRUE)

# STATS
nrow(c_panel)
nrow(c_panel_lags)
summary(lsdv.c_pool1) 
summary(lsdv.c_fec1) 
summary(lsdv.c_pool2) 
summary(lsdv.c_fec2) 


#Tester resultater ved brug af plm istedet: 
{

  C0_fd = plm(emp_log ~ prod_log, data = c_panel, index = c("country", "year"), model = "fd")
  coeftest(C0_fd, vcov. = vcovHC, type = "HC3")
  coeftest(lsdv.c_pool1, vcov. = vcovHC, type = "HC3") #til sammenligning
  coeftest(lsdv.c_pool1, vcovHC) #til sammenligning
  
  coeftest(lsdv.c_pool1, vcov = vcovHC(lsdv.c_pool1, type="HC3"))
  coeftest(lsdv.c_pool1, vcov = vcovHAC(lsdv.c_pool1))
  coeftest(lsdv.c_pool1, vcovHAC)
  coeftest(lsdv.c_pool1, vcov. = vcovHC, method="arellano")
  
  
  C1_fd = plm(emp_log ~ prod_log + prod_log_lag1 + prod_log_lag2 + prod_log_lag3, data = c_panel_lags, index = c("country", "year"), model = "fd")
  coeftest(C1_fd, vcov. = vcovHC, type = "HC3")
  coeftest(lsdv.c_pool2, vcov. = vcovHC, type="HC3") #til sammenligning
  
}


######## Modeltests

{
  #### Poolability test
  interaktion_year = lm(emp_logchanges ~ prod_logchanges*factor(year), data=c_panel)
  interaktion_year = lm(emp_logchanges ~ prod_logchanges*year, data=c_panel)

  anova(lsdv.c_pool1, interaktion_year) # chow test, tester om der er signifikante forskelle - inspireret af woolridge
  anova(lsdv.c_fey1, interaktion_year) # chow test, tester om der er signifikante forskelle - inspireret af woolridge
  anova(lsdv.c_fec1, interaktion_country)

  
  pooltest(emp_log ~ prod_log, data =  c_panel, index = c("country", "year"), model = "within")
  
  plm_model <- plm(emp_log ~ prod_log, index = c("country", "year"), data= c_panel, model= "within") # 1. Run a normal OLS model with fixed effects (model="within")
  pvcm_model <- pvcm(emp_log ~ prod_log, index = c("country", "year"), data= c_panel, model= "within") # 2. Run a variable coefficients model with fixed effects (model="within")
  pooltest(plm_model, pvcm_model) # 3. Run the poolability test
  
  #The null hypothesis is that the dataset is poolable (i.e. individuals have the same slope coefficients), 
  #so if p<0.05 you reject the null and you need a variable coefficients model.
  
  plm_model <- plm(emp_log ~ prod_log, index = c("country", "year"), data=c_panel, model= "pooling") 
  pvcm_model <- pvcm(emp_log ~ prod_log, index = c("country", "year"), data= c_panel, model= "within")
  pooltest(plm_model, pvcm_model)
  
  pooltest(emp_log ~ prod_log, index = c("country", "year"), data=c_panel, model= "pooling")
  
  #### Test for uobserverede effekter
  
  pooled_c <- plm(emp_log ~ prod_log, data =  c_panel, index = c("country", "year"), model = "pooling")
  fd_c <- plm(emp_logchanges ~ prod_logchanges, data =  c_panel, index = c("country", "year"), model = "pooling")
  fd_c <- plm(emp_logchanges ~ prod_logchanges + factor(year), data =  c_panel, index = c("country", "year"), model = "pooling")
  fd_c <- plm(emp_logchanges ~ prod_logchanges + factor(country), data =  c_panel, index = c("country", "year"), model = "pooling")
  fd_c <- plm(emp_logchanges ~ prod_logchanges + factor(country) + factor(year), data =  c_panel, index = c("country", "year"), model = "pooling")
  plmtest(pooled_c, effect = "twoways", type = "ghm")
  plmtest(pooled_c, effect = "twoways")
  plmtest(pooled_c, effect = "individual")
  plmtest(pooled_c, effect = "time")
  plmtest(fd_c, effect = "twoways")
  plmtest(fd_c, effect = "individual")
  plmtest(fd_c, effect = "time")
  
  
  #### Test af heteroskedacity 
  
  plot(lsdv.c_pool1$fitted.values, lsdv.c_pool1$residuals)
  plot(lsdv.c_fecy1$fitted.values, lsdv.c_fecy1$residuals)
  plot(lsdv.c_pool2$fitted.values, lsdv.c_pool2$residuals) #ikke "markante" tegn på heteroskedacitet
  plot(lsdv.c_fecy2$fitted.values, lsdv.c_fecy2$residuals) #ikke "markante" tegn på heteroskedacitet
  
  #Breusch Pagan Test for Heteroskedasticity, H0: the variance is constant, Ha: the variance is not constant 
  ols_test_breusch_pagan(lsdv.c_pool1) #Ha
  ols_test_breusch_pagan(lsdv.c_fecy1) #Ha
  ols_test_breusch_pagan(lsdv.c_pool2) #Ha
  ols_test_breusch_pagan(lsdv.c_fecy2) #Ha
  
  #### Test for seriel korrelation
  bgtest(lsdv.c_pool1)
  bgtest(lsdv.c_pool2)
  pbgtest(fd_c, order=1)
  
  #As Woolridge observes, if the idiosyncratic errors in the model in levels are uncorrelated (which we label hypothesis "fe"), 
  #then the errors of the model in first differences (FD) must be serially correlated with -0.5. If on the contrary the levels 
  #model's errors are a random walk, then there must be no serial correlation in the FD errors (hypothesis "fd"). Both the fixed 
  #effects (FE) and the first--differenced (FD) estimators remain consistent under either assumption, but the relative efficiency 
  #changes: FE is more efficient under "fe", FD under "fd".
  
  pwfdtest(emp_log ~ prod_log, data =  c_panel, index = c("country", "year"), h0 = "fd") # h0 = c("fd", "fe")
  pwfdtest(emp_log ~ prod_log + prod_log_lag1 + prod_log_lag2 + prod_log_lag3, data=c_panel_lags, index = c("country", "year"), h0 = "fd") #giver meget ens resultat at tilføje lags
 
  pwfdtest(emp_log ~ prod_log, data =  c_panel, index = c("country", "year"), h0 = "fe") # det ser ud til at der både er seriel korrelation i den originale level-model samt FD model
  
  #### Test for cross sectional dependency
  
  pcdtest(emp_log ~ prod_log, data = c_panel, index = c("country", "year"), model="within")
  pcdtest(emp_log ~ prod_log, data = c_panel, index = c("country", "year"), model=NULL)
  pcdtest(emp_logchanges ~ prod_logchanges, data = c_panel, index = c("country", "year"), model=NULL)
  
  pcdtest(emp_log ~ prod_log, data = c_panel, index = c("country", "year"), model = "fd") #fd kan ikke lade sig gøre
  
  pcdtest(emp_log ~ prod_log, data = c_panel, index = c("country", "year"), model = "within") 
  pcdtest(emp_log ~ prod_log + prod_log_lag1 + prod_log_lag2 + prod_log_lag3, data=c_panel_lags, index = c("country", "year"), model = "within")
  
  ## Forskellige løsninger på XSD
  {
  #feasible GLS
  zz <- pggls(emp_log ~ prod_log, data = c_panel, index = c("country", "year"), model = "within")
  zz <- pggls(emp_log ~ prod_log, data = c_panel, index = c("country", "year"), model = "fd")
  zz <- pggls(emp_log ~ prod_log + factor(country), data = c_panel, index = c("country", "year"), model = "fd")
  zz <- pggls(emp_log ~ prod_log + prod_log_lag1 + prod_log_lag2 + prod_log_lag3, data=c_panel_lags, index = c("country", "year"), model = "fd")
  zz <- pggls(emp_log ~ prod_log + prod_log_lag1 + prod_log_lag2 + prod_log_lag3 , data=c_panel_lags, index = c("country", "year"), model = "fd")
  
  summary(zz)
  
  
  #std error robust of XSD
  zz <- plm(emp_log ~ prod_log, data = c_panel, index = c("country", "year"), model = "fd")
  zz <- lm(emp_logchanges ~ prod_logchanges, data = c_panel)
  zz <- lm(emp_logchanges ~ prod_logchanges + factor(country), data = c_panel)
  
  coeftest(zz, vcov. = vcovHC, type="HC3")
  coeftest(zz, vcov. = vcovHC, type="HC3", cluster=c("time"))
  
  coeftest(zz, vcov=vcovSCC)
  coeftest(zz, vcov=function(x) vcovHC(x, cluster="time"))
  coeftest(zz, vcov=function(x) vcovHC(x, cluster="group"))
  
  vcovSCC(zz, "HC3", inner = "cluster", cluster = "group")
  vcovSCC(zz, "HC3", inner = "cluster", cluster = "time") #giver meget anderledes signifikans for FD
  vcovSCC(zz, "HC3", inner = "white")
  vcovSCC(zz, "HC3", inner = "diagavg") 
  

  
  # PMG
  mgmod = pmg(emp_logchanges ~ prod_logchanges, data = c_panel)
  dmgmod = pmg(emp_logchanges ~ prod_logchanges, data = c_panel, index = c("country", "year"), model = "dmg")
  ccemgmod = pmg(emp_logchanges ~ prod_logchanges, data = c_panel, index = c("country", "year"), model = "cmg")
  mgmod = pmg(emp_log ~ prod_log, data = c_panel)
  dmgmod = pmg(emp_log ~ prod_log, data = c_panel, index = c("country", "year"), model = "dmg")
  ccemgmod = pmg(emp_log ~ prod_log, data = c_panel, index = c("country", "year"), model = "cmg")
  summary(mgmod)
  summary(dmgmod)
  summary(ccemgmod)
  

  #PCCE
  pcce_mg = pcce(emp_logchanges ~ prod_logchanges, data = c_panel, index = c("country", "year"), model = "mg")
  pcce_p = pcce(emp_logchanges ~ prod_logchanges, data = c_panel, index = c("country", "year"), model = "p")
  summary(pcce_mg)
  summary(pcce_p)
  
  #clustering til at fjerne XSD
  
    
    library(multiwayvcov) # Link: https://stats.stackexchange.com/questions/10017/standard-error-clustering-in-r-either-manually-or-in-plm
    library(lmtest)
    library(sandwich)
    
    coeftest(lsdv.c_pool1, vcov. = vcovHC, type="HC3")
    coeftest(lsdv.c_pool1, vcov = vcovHC(lsdv.c_pool1, type="HC3"))
    coeftest(lsdv.c_pool1, vcov = vcovHAC(lsdv.c_pool1))
    coeftest(lsdv.c_pool1, vcov. = vcovHC, method="arellano")
    
    vcov_country <- cluster.vcov(lsdv.c_pool1, c_panel$country) # Cluster by country
    coeftest(lsdv.c_pool1, vcov_country)
    
    vcov_industry <- cluster.vcov(lsdv.c_pool1, c_panel$code) # Cluster by industry
    coeftest(lsdv.c_pool1, vcov_industry)
    
    vcov_year <- cluster.vcov(lsdv.c_pool1, c_panel$year) # Cluster by year
    coeftest(lsdv.c_pool1, vcov_year)
    
    vcov_both <- cluster.vcov(lsdv.c_pool1, cbind(c_panel$country, c_panel$year)) # Double cluster by country and year
    coeftest(lsdv.c_pool1, vcov_both)
  }
  
  #### Stationaritetstest/Unit root test
  
  #test = c("levinlin", "ips", "madwu", "Pm", "invnormal", "logit", "hadri")
  #exo = c("none", "intercept", "trend") #lags = c("SIC", "AIC", "Hall")

  purtest(prod_log ~ 1, data = c_panel, index = c("country", "year"), exo = "none", test = "madwu", lags = 0) #stationær
  purtest(prod_log ~ 1, data = c_panel, index = c("country", "year"), exo = "intercept", test = "madwu", lags = 0) #stationær
  purtest(prod_log ~ 1, data = c_panel, index = c("country", "year"), exo = "trend", test = "madwu", lags = 0) #stationær
  
  purtest(prod_logchanges ~ 1, data = c_panel, index = c("country", "year"), exo = "none", test = "madwu", lags = 0) #stationær
  purtest(prod_logchanges ~ 1, data = c_panel, index = c("country", "year"), exo = "intercept", test = "madwu", lags = 0) #stationær
  purtest(prod_logchanges ~ 1, data = c_panel, index = c("country", "year"), exo = "trend", test = "madwu", lags = 0) #stationær
}


# Country industry panel --------------------------------------------------

#AS: Industry-by-country fixed effects are already implicitly taken out by first-differencing in the stacked firstdifference model.
#OBS AS bruger ikke lags i denne pga insignifikans
ci_panel_1 = func_regpanel(ci_panel, "MN_4", 1) #uden lags
#ci_panel_1 = na.omit(ci_panel_1)
ci_panel_1_lags = func_regpanel(ci_panel, "MN_4", 2) #med lags
#ci_panel_1_lags = na.omit(ci_panel_1_lags)

#ci_panel_1_lags = as.data.frame(ci_panel_1_lags)

#med vægte - rapoorteres ikke
#lsdv.ci_pool2 = lm(emp_logchanges ~ prod_logchanges, data=ci_panel_1, weights = wgt_i_avg)
#lsdv.ci_fec2 = lm(emp_logchanges ~ prod_logchanges + factor(country), data=ci_panel_1, weights = wgt_i_avg) 
#lsdv.ci_fecy2 = lm(emp_logchanges ~ prod_logchanges + factor(country) + factor(year), data=ci_panel_1, weights = wgt_i_avg)
#lsdv.ci_feci2 = lm(emp_logchanges ~ prod_logchanges + factor(country) + factor(code), data=ci_panel_1, weights = wgt_i_avg) #autor bruger ikke denne kombi
#lsdv.ci_feciy2 = lm(emp_logchanges ~ prod_logchanges + factor(country) + factor(code) + factor(year), data=ci_panel_1, weights = wgt_i_avg)
#lsdv.ci_feciy2_pop = lm(emp_logchanges ~ prod_logchanges + wkgpop_logchanges + factor(country) + factor(code) + factor(year), data=ci_panel_1, weights = wgt_i_avg)

#med vægte og lags

#kan bruges til at finde fejl i datasættet - fx finder den Inf observationer for Letland, LV
#target = c("DK", "US", "DE", "NL", "AT", "CZ", "FI", "FR","EL","LV")
#erik = ci_panel_1_lags %>% filter(country %in% target)

lsdv.ci_pool3 = lm(emp_logchanges ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3, data=ci_panel_1_lags, weights = wgt_i_avg)
lsdv.ci_fec3 = lm(emp_logchanges ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + factor(country), data=ci_panel_1_lags, weights = wgt_i_avg )
lsdv.ci_fecy3 = lm(emp_logchanges ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + factor(country) + factor(year), data=ci_panel_1_lags, weights = wgt_i_avg)
#lsdv.ci_feci3 = lm(emp_logchanges ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + factor(country) + factor(code), data=ci_panel_1_lags, weights = wgt_i_avg) #autor bruger ikke denne kombi
lsdv.ci_feciy3 = lm(emp_logchanges ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + factor(country) + factor(code) + factor(year), data=ci_panel_1_lags, weights = wgt_i_avg) #
#lsdv.ci_feciy3_pop = lm(emp_logchanges ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + wkgpop_logchanges + factor(country) + factor(code) + factor(year), data=ci_panel_1_lags, weights = wgt_i_avg)


#Robuste standard fejl

#lsdv.ci_pool2_coef = func_coefs(lsdv.ci_pool2, "ci_pool2", "Country") 
#lsdv.ci_fec2_coef = func_coefs(lsdv.ci_fec2, "ci_fec2", "Country")
#lsdv.ci_fecy2_coef = func_coefs(lsdv.ci_fecy2, "ci_fecy2", "Country")
#lsdv.ci_feciy2_coef = func_coefs(lsdv.ci_feciy2, "ci_feciy2", "Country")
#lsdv.ci_feciy2_pop_coef = func_coefs(lsdv.ci_feciy2_pop, "ci_feciy2_pop", "Country")
#list_ci2=list(lsdv.ci_pool2_coef, lsdv.ci_fec2_coef, lsdv.ci_fecy2_coef,  lsdv.ci_feciy2_coef, lsdv.ci_feciy2_pop_coef)

lsdv.ci_pool3_coef = func_coefs(lsdv.ci_pool3, "ci_pool3", "Country") 
lsdv.ci_fec3_coef = func_coefs(lsdv.ci_fec3, "ci_fec3", "Country")
lsdv.ci_fecy3_coef = func_coefs(lsdv.ci_fecy3, "ci_fecy3", "Country")
lsdv.ci_feciy3_coef = func_coefs(lsdv.ci_feciy3, "ci_feciy3", "Country")
#lsdv.ci_feciy3_pop_coef = func_coefs(lsdv.ci_feciy3_pop, "ci_feciy3_pop", "Country")

list_ci3=list(lsdv.ci_pool3_coef, lsdv.ci_fec3_coef, lsdv.ci_fecy3_coef,  lsdv.ci_feciy3_coef)

#export resultater til excel
regoutput_ci_panel <- Reduce(function(a,b){
  ans <- merge(a,b,by="row.names",all=T)
  row.names(ans) <- ans[,"Row.names"]
  ans[,!names(ans) %in% "Row.names"]
}, list_ci3)

regoutput_ci_panel$vars = row.names(regoutput_ci_panel)
write_xlsx(regoutput_ci_panel, "regoutput_ci_panel.xlsx", col_names = TRUE)

summary(lsdv.ci_pool3) 
summary(lsdv.ci_fec3) 
summary(lsdv.ci_fecy3) 
summary(lsdv.ci_feciy3) 

#split sample test - Danmark
{
  ci_panel_1_DK = ci_panel_1 %>% filter(country=="DK")
  ci_panel_1_lags_DK = ci_panel_1_lags %>% filter(country=="DK")  
  
  #med vægte
  lsdv.ci_pool_DK = lm(emp_logchanges ~ prod_logchanges, data=ci_panel_1_DK, weights = wgt_i_avg)
  lsdv.ci_fey_DK = lm(emp_logchanges ~ prod_logchanges + factor(year), data=ci_panel_1_DK, weights = wgt_i_avg)
  lsdv.ci_fei_DK = lm(emp_logchanges ~ prod_logchanges + factor(code), data=ci_panel_1_DK, weights = wgt_i_avg)
  lsdv.ci_feyi_DK = lm(emp_logchanges ~ prod_logchanges + factor(year) + factor(code), data=ci_panel_1_DK, weights = wgt_i_avg)
  
  #med vægte og lags 
  lsdv.ci_pool_DK = lm(emp_logchanges ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3, data=ci_panel_1_lags_DK, weights = wgt_i_avg)
  lsdv.ci_fey_DK = lm(emp_logchanges ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + factor(year), data=ci_panel_1_lags_DK, weights = wgt_i_avg)
  lsdv.ci_fei_DK = lm(emp_logchanges ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + factor(code), data=ci_panel_1_lags_DK, weights = wgt_i_avg)
  lsdv.ci_feyi_DK = lm(emp_logchanges ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + factor(year) + factor(code), data=ci_panel_1_lags_DK, weights = wgt_i_avg)
  
  #Robuste standard fejl
  lsdv.ci_pool_DK_coef = func_coefs(lsdv.ci_pool_DK, "ci_pool_DK", "HC3") 
  lsdv.ci_fey_DK_coef = func_coefs(lsdv.ci_fey_DK, "ci_fey_DK", "HC3")
  lsdv.ci_fei_DK_coef = func_coefs(lsdv.ci_fei_DK, "ci_fei_DK", "HC3")
  lsdv.ci_feyi_DK_coef = func_coefs(lsdv.ci_feyi_DK, "ci_feyi_DK", "HC3")

  list_ci_DK = list(lsdv.ci_pool_DK_coef, lsdv.ci_fey_DK_coef, lsdv.ci_fei_DK_coef,  lsdv.ci_feyi_DK_coef)
  
  #export resultater til excel
  regoutput_ci_DKpanel <- Reduce(function(a,b){
    ans <- merge(a,b,by="row.names",all=T)
    row.names(ans) <- ans[,"Row.names"]
    ans[,!names(ans) %in% "Row.names"]
  }, list_ci_DK)
  
  regoutput_ci_DKpanel$vars = row.names(regoutput_ci_DKpanel)
  write_xlsx(regoutput_ci_DKpanel, "regoutput_ci_DKpanel.xlsx", col_names = TRUE)
}

######## Modeltests
{
  #### Poolability test
  interaktion_year = lm(emp_logchanges ~ prod_logchanges*factor(year), data=ci_panel_1_lags)
  anova(lsdv.ci_pool3, interaktion_year) # chow test, tester om der er signifikante forskelle - inspireret af woolridge
  
  lsdv.ci_pool2 = lm(emp_logchanges ~ prod_logchanges + factor(year), data=ci_panel_1, weights = wgt_i_avg)
  interaktion_year = lm(emp_logchanges ~ prod_logchanges*factor(year), data=ci_panel_1, weights = wgt_i_avg)
  anova(lsdv.ci_pool2, interaktion_year) # chow test, tester om der er signifikante forskelle - inspireret af woolridge
  
  pooltest(emp_log ~ prod_log, data =  ci_panel_1_lags, index = c("country", "year"), model = "within")
  pooltest(emp_log ~ prod_log, index = c("country", "year"), data=ci_panel_1_lags, model= "pooling")
  
  plm_model <- plm(emp_log ~ prod_log, index = c("country", "year"), data= ci_panel_1_lags, model= "within") # 1. Run a normal OLS model with fixed effects (model="within")
  pvcm_model <- pvcm(emp_log ~ prod_log, index = c("country", "year"), data= ci_panel_1_lags, model= "within") # 2. Run a variable coefficients model with fixed effects (model="within")
  pooltest(plm_model, pvcm_model) # 3. Run the poolability test
  
  #The null hypothesis is that the dataset is poolable (i.e. individuals have the same slope coefficients), 
  #so if p<0.05 you reject the null and you need a variable coefficients model.
  

  plm_model <- plm(emp_log ~ prod_log, index = c("country", "year"), data=ci_panel_1_lags, model= "pooling") 
  pvcm_model <- pvcm(emp_log ~ prod_log, index = c("country", "year"), data= ci_panel_1_lags, model= "within")
  pooltest(plm_model, pvcm_model)
  
  
  #### Test for uobserverede effekter
  
  pooled_c <- plm(emp_log ~ prod_log, data =  c_panel, index = c("country", "year"), model = "pooling")
  fd_c <- plm(emp_logchanges ~ prod_logchanges, data =  c_panel, index = c("country", "year"), model = "pooling")
  fd_c <- plm(emp_logchanges ~ prod_logchanges + factor(year), data =  c_panel, index = c("country", "year"), model = "pooling")
  fd_c <- plm(emp_logchanges ~ prod_logchanges + factor(country), data =  c_panel, index = c("country", "year"), model = "pooling")
  fd_c <- plm(emp_logchanges ~ prod_logchanges + factor(country) + factor(year), data =  c_panel, index = c("country", "year"), model = "pooling")
  plmtest(pooled_c, effect = "twoways", type = "ghm")
  plmtest(pooled_c, effect = "twoways")
  plmtest(pooled_c, effect = "individual")
  plmtest(pooled_c, effect = "time")
  plmtest(fd_c, effect = "twoways")
  plmtest(fd_c, effect = "individual")
  plmtest(fd_c, effect = "time")
  
  
  #### Test af heteroskedacity 
  
  plot(lsdv.c_pool1$fitted.values, lsdv.c_pool1$residuals)
  plot(lsdv.c_fecy1$fitted.values, lsdv.c_fecy1$residuals)
  plot(lsdv.c_pool2$fitted.values, lsdv.c_pool2$residuals) #ikke "markante" tegn på heteroskedacitet
  plot(lsdv.c_fecy2$fitted.values, lsdv.c_fecy2$residuals) #ikke "markante" tegn på heteroskedacitet
  
  #Breusch Pagan Test for Heteroskedasticity, H0: the variance is constant, Ha: the variance is not constant 
  ols_test_breusch_pagan(lsdv.c_pool1) #Ha
  ols_test_breusch_pagan(lsdv.c_fecy1) #Ha
  ols_test_breusch_pagan(lsdv.c_pool2) #Ha
  ols_test_breusch_pagan(lsdv.c_fecy2) #Ha
  
  #### Test for seriel korrelation
  bgtest(lsdv.c_pool1)
  bgtest(lsdv.c_pool2)
  pbgtest(fd_c, order=1)
  
  pwfdtest(emp_log ~ prod_log, data =  c_panel, index = c("country", "year"), h0 = "fd") # h0 = c("fd", "fe")
  pwfdtest(emp_log ~ prod_log + prod_log_lag1 + prod_log_lag2 + prod_log_lag3, data=c_panel_lags, index = c("country", "year"), h0 = "fd") #giver meget ens resultat at tilføje lags
  
  pwfdtest(emp_log ~ prod_log, data =  c_panel, index = c("country", "year"), h0 = "fe") # det ser ud til at der både er seriel korrelation i den originale level-model samt FD model
  
  #### Test for cross sectional dependency
  
  pcdtest(emp_log ~ prod_log, data = c_panel, index = c("country", "year"), model="within")
  pcdtest(emp_log ~ prod_log, data = c_panel, index = c("country", "year"), model=NULL)
  pcdtest(emp_logchanges ~ prod_logchanges, data = c_panel, index = c("country", "year"), model=NULL)
  
  pcdtest(emp_log ~ prod_log, data = c_panel, index = c("country", "year"), model = "fd") #fd kan ikke lade sig gøre
  
  pcdtest(emp_log ~ prod_log, data = c_panel, index = c("country", "year"), model = "within") 
  pcdtest(emp_log ~ prod_log + prod_log_lag1 + prod_log_lag2 + prod_log_lag3, data=c_panel_lags, index = c("country", "year"), model = "within")

  
  #### Stationaritetstest/Unit root test
  
  #test = c("levinlin", "ips", "madwu", "Pm", "invnormal", "logit", "hadri")
  #exo = c("none", "intercept", "trend") #lags = c("SIC", "AIC", "Hall")
  
  purtest(prod_log ~ 1, data = c_panel, index = c("country", "year"), test = "madwu", lags = 0) #stationær
  purtest(prod_logchanges ~ 1, data = c_panel, index = c("country", "year"), test = "madwu", lags = 0) #stationær
  
  
}

# Sammensætning af mikro og makroelasticiteter --------------------------------------------------

ci_panel_2 = func_regpanel(ci_panel, "MN_4", 2)

#### uden vægte - rapporteres ikke

#lsdv.mm_pool1 = lm(emp_logchanges ~ prod_logchanges + dLP_CwoI + dLP_CwoI_lag1 + dLP_CwoI_lag2 + dLP_CwoI_lag3, data=ci_panel_2)
#lsdv.mm_fec1 = lm(emp_logchanges ~ prod_logchanges + dLP_CwoI + dLP_CwoI_lag1 + dLP_CwoI_lag2 + dLP_CwoI_lag3 + factor(country), data=ci_panel_2) 
#lsdv.mm_feci1 = lm(emp_logchanges ~ prod_logchanges + dLP_CwoI + dLP_CwoI_lag1 + dLP_CwoI_lag2 + dLP_CwoI_lag3 + factor(country) + factor(code), data=ci_panel_2)
#lsdv.mm_feciy1 = lm(emp_logchanges ~ prod_logchanges + dLP_CwoI + dLP_CwoI_lag1 + dLP_CwoI_lag2 + dLP_CwoI_lag3 + factor(country) + factor(code) + factor(year), data=ci_panel_2)

#### med vægte - rapporteres ikke

#lsdv.mm_pool2 = lm(emp_logchanges ~ prod_logchanges + dLP_CwoI + dLP_CwoI_lag1 + dLP_CwoI_lag2 + dLP_CwoI_lag3, data=ci_panel_2, weights = wgt_i_avg)
#lsdv.mm_fec2 = lm(emp_logchanges ~ prod_logchanges + dLP_CwoI + dLP_CwoI_lag1 + dLP_CwoI_lag2 + dLP_CwoI_lag3 + factor(country) , data=ci_panel_2, weights = wgt_i_avg) 
#lsdv.mm_fecy2 = lm(emp_logchanges ~ prod_logchanges + dLP_CwoI + dLP_CwoI_lag1 + dLP_CwoI_lag2 + dLP_CwoI_lag3 + factor(country) + factor(year), data=ci_panel_2, weights = wgt_i_avg)
#lsdv.mm_feciy2 = lm(emp_logchanges ~ prod_logchanges + dLP_CwoI + dLP_CwoI_lag1 + dLP_CwoI_lag2 + dLP_CwoI_lag3 + factor(country) + factor(code) + factor(year), data=ci_panel_2, weights = wgt_i_avg)
#lsdv.mm_feciy2_pop = lm(emp_logchanges ~ prod_logchanges + dLP_CwoI + dLP_CwoI_lag1 + dLP_CwoI_lag2 + dLP_CwoI_lag3 + wkgpop_logchanges + factor(country) + factor(code) + factor(year), data=ci_panel_2, weights = wgt_i_avg)

#### med vægte og beta1 lags

lsdv.mm_pool3 = lm(emp_logchanges ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + dLP_CwoI + dLP_CwoI_lag1 + dLP_CwoI_lag2 + dLP_CwoI_lag3, data=ci_panel_2, weights = wgt_i_avg)
lsdv.mm_fec3 = lm(emp_logchanges ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + dLP_CwoI + dLP_CwoI_lag1 + dLP_CwoI_lag2 + dLP_CwoI_lag3 + factor(country) , data=ci_panel_2, weights = wgt_i_avg) 
lsdv.mm_fecy3 = lm(emp_logchanges ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + dLP_CwoI + dLP_CwoI_lag1 + dLP_CwoI_lag2 + dLP_CwoI_lag3 + factor(country) + factor(year), data=ci_panel_2, weights = wgt_i_avg)
lsdv.mm_feciy3 = lm(emp_logchanges ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + dLP_CwoI + dLP_CwoI_lag1 + dLP_CwoI_lag2 + dLP_CwoI_lag3 + factor(country) + factor(code) + factor(year), data=ci_panel_2, weights = wgt_i_avg)
#lsdv.mm_feciy3_pop = lm(emp_logchanges ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + dLP_CwoI + dLP_CwoI_lag1 + dLP_CwoI_lag2 + dLP_CwoI_lag3 + wkgpop_logchanges + factor(country) + factor(code) + factor(year), data=ci_panel_2, weights = wgt_i_avg)


#Robuste standard fejl - uden beta1 lags
#lsdv.mm_pool2_coef = func_coefs(lsdv.mm_pool2, "mm_pool2", "HC3") 
#lsdv.mm_fec2_coef = func_coefs(lsdv.mm_fec2, "mm_fec2", "HC3")
#lsdv.mm_fecy2_coef = func_coefs(lsdv.mm_fecy2, "mm_fecy2", "HC3")
#lsdv.mm_feciy2_coef = func_coefs(lsdv.mm_feciy2, "mm_feciy2", "HC3")
#lsdv.mm_feciy2_pop_coef = func_coefs(lsdv.mm_feciy2_pop, "mm_feciy2_pop", "HC3")
#list_mm2=list(lsdv.mm_pool2_coef, lsdv.mm_fec2_coef, lsdv.mm_fecy2_coef, lsdv.mm_feciy2_coef, lsdv.mm_feciy2_pop_coef)

#Robuste standard fejl - med beta1 lags
lsdv.mm_pool3_coef = func_coefs(lsdv.mm_pool3, "mm_pool3", "Country") 
lsdv.mm_fec3_coef = func_coefs(lsdv.mm_fec3, "mm_fec3", "Country")
lsdv.mm_fecy3_coef = func_coefs(lsdv.mm_fecy3, "mm_fecy3", "Country")
lsdv.mm_feciy3_coef = func_coefs(lsdv.mm_feciy3, "mm_feciy3", "Country")
#lsdv.mm_feciy3_pop_coef = func_coefs(lsdv.mm_feciy3_pop, "mm_feciy3_pop", "Country")

list_mm3=list(lsdv.mm_pool3_coef, lsdv.mm_fec3_coef, lsdv.mm_fecy3_coef, lsdv.mm_feciy3_coef)

#export resultater til excel
regoutput_mm_panel <- Reduce(function(a,b){
  ans <- merge(a,b,by="row.names",all=T)
  row.names(ans) <- ans[,"Row.names"]
  ans[,!names(ans) %in% "Row.names"]
}, list_mm3)

regoutput_mm_panel$vars = row.names(regoutput_mm_panel)
write_xlsx(regoutput_mm_panel, "regoutput_mm_panel.xlsx", col_names = TRUE)

summary(lsdv.mm_pool3) 
summary(lsdv.mm_fec3) 
summary(lsdv.mm_fecy3) 
summary(lsdv.mm_feciy3) 

#split sample test - Danmark
{
  
  ci_panel_2_DK = ci_panel_2 %>% filter(country=="DK")
  
  #med vægte og lags
  lsdv.mm_pool_DK = lm(emp_logchanges ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + dLP_CwoI + dLP_CwoI_lag1 + dLP_CwoI_lag2 + dLP_CwoI_lag3, data=ci_panel_2_DK, weights = wgt_i_avg)
  lsdv.mm_fey_DK = lm(emp_logchanges ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + dLP_CwoI + dLP_CwoI_lag1 + dLP_CwoI_lag2 + dLP_CwoI_lag3 + factor(year) , data=ci_panel_2_DK, weights = wgt_i_avg) 
  lsdv.mm_fei_DK  = lm(emp_logchanges ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + dLP_CwoI + dLP_CwoI_lag1 + dLP_CwoI_lag2 + dLP_CwoI_lag3 + factor(code), data=ci_panel_2_DK, weights = wgt_i_avg)
  lsdv.mm_feyi_DK = lm(emp_logchanges ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + dLP_CwoI + dLP_CwoI_lag1 + dLP_CwoI_lag2 + dLP_CwoI_lag3 + factor(code) + factor(year), data=ci_panel_2_DK, weights = wgt_i_avg)
  lsdv.mm_feyi_DK_pop = lm(emp_logchanges ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + dLP_CwoI + dLP_CwoI_lag1 + dLP_CwoI_lag2 + dLP_CwoI_lag3 + wkgpop_logchanges + factor(code) + factor(year), data=ci_panel_2_DK, weights = wgt_i_avg)
  
  #Robuste standard fejl
  lsdv.mm_pool_DK_coef = func_coefs(lsdv.mm_pool_DK, "mm_pool_DK", "HC3") 
  lsdv.mm_fey_DK_coef = func_coefs(lsdv.mm_fey_DK, "mm_fey_DK", "HC3")
  lsdv.mm_fei_DK_coef = func_coefs(lsdv.mm_fei_DK, "mm_fei_DK", "HC3")
  lsdv.mm_feyi_DK_coef = func_coefs(lsdv.mm_feyi_DK, "mm_feyi_DK", "HC3")
  lsdv.mm_feyi_DK_pop_coef = func_coefs(lsdv.mm_feyi_DK_pop, "mm_feyi_DK_pop", "HC3")
  
  list_mm_DK = list(lsdv.mm_pool_DK_coef, lsdv.mm_fey_DK_coef, lsdv.mm_fei_DK_coef, lsdv.mm_feyi_DK_coef, lsdv.mm_feyi_DK_pop_coef)
  
  #export resultater til excel
  regoutput_mm_DKpanel <- Reduce(function(a,b){
    ans <- merge(a,b,by="row.names",all=T)
    row.names(ans) <- ans[,"Row.names"]
    ans[,!names(ans) %in% "Row.names"]
  }, list_mm_DK)
  
  regoutput_mm_DKpanel$vars = row.names(regoutput_mm_DKpanel)
  write_xlsx(regoutput_mm_DKpanel, "regoutput_mm_DKpanel.xlsx", col_names = TRUE)
}


# Sector spillover -------------------------------------------------

# How to deal with NA in a panel data regression? Link: https://stackoverflow.com/questions/14427781/how-to-deal-with-na-in-a-panel-data-regression
# Skal det vægtes? Og hvad skal vægtes?
# hvorfor bruger AS ikke year-industry fixed effects

ci_panel_ss = func_regpanel(ci_panel, "MN_4", 2)

#uden vægte
{
lsdv.ss_pool1 = {lm(emp_logchanges ~ dLP_I_b1 + dLP_I_b2 + dLP_I_b3 + dLP_I_b4 + dLP_I_b5 +
                      dLP_BwoI_b1 + dLP_BwoI_b1_lag1 + dLP_BwoI_b1_lag2 + dLP_BwoI_b1_lag3 +
                      dLP_BwoI_b2 + dLP_BwoI_b2_lag1 + dLP_BwoI_b2_lag2 + dLP_BwoI_b2_lag3 +
                      dLP_BwoI_b3 + dLP_BwoI_b3_lag1 + dLP_BwoI_b3_lag2 + dLP_BwoI_b3_lag3 +
                      dLP_BwoI_b4 + dLP_BwoI_b4_lag1 + dLP_BwoI_b4_lag2 + dLP_BwoI_b4_lag3, 
                    data=ci_panel_ss)} #dLP_BwoI_b5 + dLP_BwoI_b5_lag1 + dLP_BwoI_b5_lag2 + dLP_BwoI_b5_lag3

lsdv.ss_fec1 = {lm(emp_logchanges ~ dLP_I_b1 + dLP_I_b2 + dLP_I_b3 + dLP_I_b4 + dLP_I_b5 +
                     dLP_BwoI_b1 + dLP_BwoI_b1_lag1 + dLP_BwoI_b1_lag2 + dLP_BwoI_b1_lag3 +
                     dLP_BwoI_b2 + dLP_BwoI_b2_lag1 + dLP_BwoI_b2_lag2 + dLP_BwoI_b2_lag3 +
                     dLP_BwoI_b3 + dLP_BwoI_b3_lag1 + dLP_BwoI_b3_lag2 + dLP_BwoI_b3_lag3 +
                     dLP_BwoI_b4 + dLP_BwoI_b4_lag1 + dLP_BwoI_b4_lag2 + dLP_BwoI_b4_lag3 +
                     factor(country), data=ci_panel_ss)} #dLP_BwoI_b5 + dLP_BwoI_b5_lag1 + dLP_BwoI_b5_lag2 + dLP_BwoI_b5_lag3 + 

lsdv.ss_fecy1 = {lm(emp_logchanges ~ dLP_I_b1 + dLP_I_b2 + dLP_I_b3 + dLP_I_b4 + dLP_I_b5 +
                      dLP_BwoI_b1 + dLP_BwoI_b1_lag1 + dLP_BwoI_b1_lag2 + dLP_BwoI_b1_lag3 +
                      dLP_BwoI_b2 + dLP_BwoI_b2_lag1 + dLP_BwoI_b2_lag2 + dLP_BwoI_b2_lag3 +
                      dLP_BwoI_b3 + dLP_BwoI_b3_lag1 + dLP_BwoI_b3_lag2 + dLP_BwoI_b3_lag3 +
                      dLP_BwoI_b4 + dLP_BwoI_b4_lag1 + dLP_BwoI_b4_lag2 + dLP_BwoI_b4_lag3 +
                      factor(country) + factor(year), data=ci_panel_ss)} #dLP_BwoI_b5 + dLP_BwoI_b5_lag1 + dLP_BwoI_b5_lag2 + dLP_BwoI_b5_lag3 + 

lsdv.ss_feciy1 = {lm(emp_logchanges ~ dLP_I_b1 + dLP_I_b2 + dLP_I_b3 + dLP_I_b4 + dLP_I_b5 +
                       dLP_BwoI_b1 + dLP_BwoI_b1_lag1 + dLP_BwoI_b1_lag2 + dLP_BwoI_b1_lag3 +
                       dLP_BwoI_b2 + dLP_BwoI_b2_lag1 + dLP_BwoI_b2_lag2 + dLP_BwoI_b2_lag3 +
                       dLP_BwoI_b3 + dLP_BwoI_b3_lag1 + dLP_BwoI_b3_lag2 + dLP_BwoI_b3_lag3 +
                       dLP_BwoI_b4 + dLP_BwoI_b4_lag1 + dLP_BwoI_b4_lag2 + dLP_BwoI_b4_lag3 +
                       factor(country) + factor(year) + factor(code), data=ci_panel_ss)} # dLP_BwoI_b5 + dLP_BwoI_b5_lag1 + dLP_BwoI_b5_lag2 + dLP_BwoI_b5_lag3 +
}

#med vægte
{
lsdv.ss_pool2 = {lm(emp_logchanges ~ dLP_I_b1 + dLP_I_b2 + dLP_I_b3 + dLP_I_b4 +
                      dLP_BwoI_b1 + dLP_BwoI_b1_lag1 + dLP_BwoI_b1_lag2 + dLP_BwoI_b1_lag3 +
                      dLP_BwoI_b2 + dLP_BwoI_b2_lag1 + dLP_BwoI_b2_lag2 + dLP_BwoI_b2_lag3 +
                      dLP_BwoI_b3 + dLP_BwoI_b3_lag1 + dLP_BwoI_b3_lag2 + dLP_BwoI_b3_lag3 +
                      dLP_BwoI_b4 + dLP_BwoI_b4_lag1 + dLP_BwoI_b4_lag2 + dLP_BwoI_b4_lag3, 
                    data=ci_panel_ss, weights = wgt_i_avg)} #dLP_I_b5 + dLP_BwoI_b5 + dLP_BwoI_b5_lag1 + dLP_BwoI_b5_lag2 + dLP_BwoI_b5_lag3

lsdv.ss_fec2 = {lm(emp_logchanges ~ dLP_I_b1 + dLP_I_b2 + dLP_I_b3 + dLP_I_b4 + 
                     dLP_BwoI_b1 + dLP_BwoI_b1_lag1 + dLP_BwoI_b1_lag2 + dLP_BwoI_b1_lag3 +
                     dLP_BwoI_b2 + dLP_BwoI_b2_lag1 + dLP_BwoI_b2_lag2 + dLP_BwoI_b2_lag3 +
                     dLP_BwoI_b3 + dLP_BwoI_b3_lag1 + dLP_BwoI_b3_lag2 + dLP_BwoI_b3_lag3 +
                     dLP_BwoI_b4 + dLP_BwoI_b4_lag1 + dLP_BwoI_b4_lag2 + dLP_BwoI_b4_lag3 +
                     factor(country), data=ci_panel_ss, weights = wgt_i_avg)} #dLP_I_b5 + dLP_BwoI_b5 + dLP_BwoI_b5_lag1 + dLP_BwoI_b5_lag2 + dLP_BwoI_b5_lag3 + 

lsdv.ss_fecy2 = {lm(emp_logchanges ~ dLP_I_b1 + dLP_I_b2 + dLP_I_b3 + dLP_I_b4 + 
                      dLP_BwoI_b1 + dLP_BwoI_b1_lag1 + dLP_BwoI_b1_lag2 + dLP_BwoI_b1_lag3 +
                      dLP_BwoI_b2 + dLP_BwoI_b2_lag1 + dLP_BwoI_b2_lag2 + dLP_BwoI_b2_lag3 +
                      dLP_BwoI_b3 + dLP_BwoI_b3_lag1 + dLP_BwoI_b3_lag2 + dLP_BwoI_b3_lag3 +
                      dLP_BwoI_b4 + dLP_BwoI_b4_lag1 + dLP_BwoI_b4_lag2 + dLP_BwoI_b4_lag3 +
                      factor(country) + factor(year), data=ci_panel_ss, weights = wgt_i_avg)} #dLP_I_b5 + dLP_BwoI_b5 + dLP_BwoI_b5_lag1 + dLP_BwoI_b5_lag2 + dLP_BwoI_b5_lag3 + 

lsdv.ss_feciy2 = {lm(emp_logchanges ~ dLP_I_b1 + dLP_I_b2 + dLP_I_b3 + dLP_I_b4 + 
                       dLP_BwoI_b1 + dLP_BwoI_b1_lag1 + dLP_BwoI_b1_lag2 + dLP_BwoI_b1_lag3 +
                       dLP_BwoI_b2 + dLP_BwoI_b2_lag1 + dLP_BwoI_b2_lag2 + dLP_BwoI_b2_lag3 +
                       dLP_BwoI_b3 + dLP_BwoI_b3_lag1 + dLP_BwoI_b3_lag2 + dLP_BwoI_b3_lag3 +
                       dLP_BwoI_b4 + dLP_BwoI_b4_lag1 + dLP_BwoI_b4_lag2 + dLP_BwoI_b4_lag3 +
                       factor(country) + factor(year) + factor(code), data=ci_panel_ss, weights = wgt_i_avg)} #dLP_I_b5 + dLP_BwoI_b5 + dLP_BwoI_b5_lag1 + dLP_BwoI_b5_lag2 + dLP_BwoI_b5_lag3 +

lsdv.ss_feciy2_pop = {lm(emp_logchanges ~ dLP_I_b1 + dLP_I_b2 + dLP_I_b3 + dLP_I_b4 + 
                       dLP_BwoI_b1 + dLP_BwoI_b1_lag1 + dLP_BwoI_b1_lag2 + dLP_BwoI_b1_lag3 +
                       dLP_BwoI_b2 + dLP_BwoI_b2_lag1 + dLP_BwoI_b2_lag2 + dLP_BwoI_b2_lag3 +
                       dLP_BwoI_b3 + dLP_BwoI_b3_lag1 + dLP_BwoI_b3_lag2 + dLP_BwoI_b3_lag3 +
                       dLP_BwoI_b4 + dLP_BwoI_b4_lag1 + dLP_BwoI_b4_lag2 + dLP_BwoI_b4_lag3 + wkgpop_logchanges +
                       factor(country) + factor(year) + factor(code), data=ci_panel_ss, weights = wgt_i_avg)} #dLP_I_b5 + dLP_BwoI_b5 + dLP_BwoI_b5_lag1 + dLP_BwoI_b5_lag2 + dLP_BwoI_b5_lag3 +
}

#med vægte og beta1 lags
{
    lsdv.ss_pool3 = {lm(emp_logchanges ~ dLP_I_b1 + dLP_I_b1_lag1 + dLP_I_b1_lag2 + dLP_I_b1_lag3 +
                          dLP_I_b2 + dLP_I_b2_lag1 + dLP_I_b2_lag2 + dLP_I_b2_lag3 +
                          dLP_I_b3 + dLP_I_b3_lag1 + dLP_I_b3_lag2 + dLP_I_b3_lag3 +
                          dLP_I_b4 + dLP_I_b4_lag1 + dLP_I_b4_lag2 + dLP_I_b4_lag3 +
                          dLP_BwoI_b1 + dLP_BwoI_b1_lag1 + dLP_BwoI_b1_lag2 + dLP_BwoI_b1_lag3 +
                          dLP_BwoI_b2 + dLP_BwoI_b2_lag1 + dLP_BwoI_b2_lag2 + dLP_BwoI_b2_lag3 +
                          dLP_BwoI_b3 + dLP_BwoI_b3_lag1 + dLP_BwoI_b3_lag2 + dLP_BwoI_b3_lag3 +
                          dLP_BwoI_b4 + dLP_BwoI_b4_lag1 + dLP_BwoI_b4_lag2 + dLP_BwoI_b4_lag3, 
                        data=ci_panel_ss, weights = wgt_i_avg)} 
    
    lsdv.ss_fec3 = {lm(emp_logchanges ~ dLP_I_b1 + dLP_I_b1_lag1 + dLP_I_b1_lag2 + dLP_I_b1_lag3 +
                         dLP_I_b2 + dLP_I_b2_lag1 + dLP_I_b2_lag2 + dLP_I_b2_lag3 +
                         dLP_I_b3 + dLP_I_b3_lag1 + dLP_I_b3_lag2 + dLP_I_b3_lag3 +
                         dLP_I_b4 + dLP_I_b4_lag1 + dLP_I_b4_lag2 + dLP_I_b4_lag3 +
                         dLP_BwoI_b1 + dLP_BwoI_b1_lag1 + dLP_BwoI_b1_lag2 + dLP_BwoI_b1_lag3 +
                         dLP_BwoI_b2 + dLP_BwoI_b2_lag1 + dLP_BwoI_b2_lag2 + dLP_BwoI_b2_lag3 +
                         dLP_BwoI_b3 + dLP_BwoI_b3_lag1 + dLP_BwoI_b3_lag2 + dLP_BwoI_b3_lag3 +
                         dLP_BwoI_b4 + dLP_BwoI_b4_lag1 + dLP_BwoI_b4_lag2 + dLP_BwoI_b4_lag3 +
                         factor(country), data=ci_panel_ss, weights = wgt_i_avg)} 
    
    lsdv.ss_fecy3 = {lm(emp_logchanges ~ dLP_I_b1 + dLP_I_b1_lag1 + dLP_I_b1_lag2 + dLP_I_b1_lag3 +
                          dLP_I_b2 + dLP_I_b2_lag1 + dLP_I_b2_lag2 + dLP_I_b2_lag3 +
                          dLP_I_b3 + dLP_I_b3_lag1 + dLP_I_b3_lag2 + dLP_I_b3_lag3 +
                          dLP_I_b4 + dLP_I_b4_lag1 + dLP_I_b4_lag2 + dLP_I_b4_lag3 +
                          dLP_BwoI_b1 + dLP_BwoI_b1_lag1 + dLP_BwoI_b1_lag2 + dLP_BwoI_b1_lag3 +
                          dLP_BwoI_b2 + dLP_BwoI_b2_lag1 + dLP_BwoI_b2_lag2 + dLP_BwoI_b2_lag3 +
                          dLP_BwoI_b3 + dLP_BwoI_b3_lag1 + dLP_BwoI_b3_lag2 + dLP_BwoI_b3_lag3 +
                          dLP_BwoI_b4 + dLP_BwoI_b4_lag1 + dLP_BwoI_b4_lag2 + dLP_BwoI_b4_lag3 +
                          factor(country) + factor(year), data=ci_panel_ss, weights = wgt_i_avg)} 
    
    lsdv.ss_feciy3 = {lm(emp_logchanges ~ dLP_I_b1 + dLP_I_b1_lag1 + dLP_I_b1_lag2 + dLP_I_b1_lag3 +
                           dLP_I_b2 + dLP_I_b2_lag1 + dLP_I_b2_lag2 + dLP_I_b2_lag3 +
                           dLP_I_b3 + dLP_I_b3_lag1 + dLP_I_b3_lag2 + dLP_I_b3_lag3 +
                           dLP_I_b4 + dLP_I_b4_lag1 + dLP_I_b4_lag2 + dLP_I_b4_lag3 +
                           dLP_BwoI_b1 + dLP_BwoI_b1_lag1 + dLP_BwoI_b1_lag2 + dLP_BwoI_b1_lag3 +
                           dLP_BwoI_b2 + dLP_BwoI_b2_lag1 + dLP_BwoI_b2_lag2 + dLP_BwoI_b2_lag3 +
                           dLP_BwoI_b3 + dLP_BwoI_b3_lag1 + dLP_BwoI_b3_lag2 + dLP_BwoI_b3_lag3 +
                           dLP_BwoI_b4 + dLP_BwoI_b4_lag1 + dLP_BwoI_b4_lag2 + dLP_BwoI_b4_lag3 +
                           factor(country) + factor(year) + factor(code), data=ci_panel_ss, weights = wgt_i_avg)}
    
    lsdv.ss_feciy3_pop = {lm(emp_logchanges ~ dLP_I_b1 + dLP_I_b1_lag1 + dLP_I_b1_lag2 + dLP_I_b1_lag3 +
                               dLP_I_b2 + dLP_I_b2_lag1 + dLP_I_b2_lag2 + dLP_I_b2_lag3 +
                               dLP_I_b3 + dLP_I_b3_lag1 + dLP_I_b3_lag2 + dLP_I_b3_lag3 +
                               dLP_I_b4 + dLP_I_b4_lag1 + dLP_I_b4_lag2 + dLP_I_b4_lag3 +
                               dLP_BwoI_b1 + dLP_BwoI_b1_lag1 + dLP_BwoI_b1_lag2 + dLP_BwoI_b1_lag3 +
                               dLP_BwoI_b2 + dLP_BwoI_b2_lag1 + dLP_BwoI_b2_lag2 + dLP_BwoI_b2_lag3 +
                               dLP_BwoI_b3 + dLP_BwoI_b3_lag1 + dLP_BwoI_b3_lag2 + dLP_BwoI_b3_lag3 +
                               dLP_BwoI_b4 + dLP_BwoI_b4_lag1 + dLP_BwoI_b4_lag2 + dLP_BwoI_b4_lag3 + wkgpop_logchanges +
                               factor(country) + factor(year) + factor(code), data=ci_panel_ss, weights = wgt_i_avg)}
  }
  

#Robuste standard fejl

#lsdv.ss_pool1_coef = func_coefs(lsdv.ss_pool1, "ss_pool1", "HC3") 
#lsdv.ss_fec1_coef = func_coefs(lsdv.ss_fec1, "ss_fec1", "HC3")
#lsdv.ss_fecy1_coef = func_coefs(lsdv.ss_fecy1, "ss_fecy1", "HC3")
#lsdv.ss_feciy1_coef = func_coefs(lsdv.ss_feciy1, "ss_feciy1", "HC3")
#list_ss1=list(lsdv.ss_pool1_coef, lsdv.ss_fec1_coef, lsdv.ss_fecy1_coef, lsdv.ss_feciy1_coef)

#lsdv.ss_pool2_coef = func_coefs(lsdv.ss_pool2, "ss_pool2", "Country") 
#lsdv.ss_fec2_coef = func_coefs(lsdv.ss_fec2, "ss_fec2", "Country")
#lsdv.ss_fecy2_coef = func_coefs(lsdv.ss_fecy2, "ss_fecy2", "Country")
#lsdv.ss_feciy2_coef = func_coefs(lsdv.ss_feciy2, "ss_feciy2", "Country")
#lsdv.ss_feciy2_pop_coef = func_coefs(lsdv.ss_feciy2_pop, "ss_feciy2_pop", "Country")
#list_ss2=list(lsdv.ss_pool2_coef, lsdv.ss_fec2_coef, lsdv.ss_fecy2_coef, lsdv.ss_feciy2_coef, lsdv.ss_feciy2_pop_coef)

lsdv.ss_pool3_coef = func_coefs(lsdv.ss_pool3, "ss_pool3", "Country") 
lsdv.ss_fec3_coef = func_coefs(lsdv.ss_fec3, "ss_fec3", "Country")
lsdv.ss_fecy3_coef = func_coefs(lsdv.ss_fecy3, "ss_fecy3", "Country")
lsdv.ss_feciy3_coef = func_coefs(lsdv.ss_feciy3, "ss_feciy3", "Country")
#lsdv.ss_feciy3_pop_coef = func_coefs(lsdv.ss_feciy3_pop, "ss_feciy3_pop", "Country")

list_ss3=list(lsdv.ss_pool3_coef, lsdv.ss_fec3_coef, lsdv.ss_fecy3_coef, lsdv.ss_feciy3_coef)


#export resultater til excel
regoutput_ss_panel <- Reduce(function(a,b){
  ans <- merge(a,b,by="row.names",all=T)
  row.names(ans) <- ans[,"Row.names"]
  ans[,!names(ans) %in% "Row.names"]
}, list_ss3)


regoutput_ss_panel$vars = row.names(regoutput_ss_panel)
write_xlsx(regoutput_ss_panel, "regoutput_ss_panel.xlsx", col_names = TRUE)

summary(lsdv.ss_pool3) 
summary(lsdv.ss_fec3) 
summary(lsdv.ss_fecy3) 
summary(lsdv.ss_feciy3) 


# Deskriptiv --------------------------------------------------

#Forberedelse:

test_c = func_empprod("MN_4","lande")
test_i = func_empprod("MN_4","industrier")

#Gennemsnit/overblik over variable og enheder:
{
#lande gns på tværs af år
c_avg_niveau  = test_c %>% group_by(country) %>% summarise_at(vars(EMP, GO), list(mean = mean))
c_avg_ændringer  = na.omit(test_c) %>% group_by(country) %>% summarise_at(vars(EMP, GO, emp_logchanges, prod_logchanges), list(mean = mean))
tot_avg_ændringer  = na.omit(test_c) %>% group_by() %>% summarise_at(vars(EMP, GO, emp_logchanges, prod_logchanges), list(mean = mean))

#industri gns på tværs af år
i_avg_niveau = test_i %>% group_by(code) %>% summarise_at(vars(EMP, GO), list(mean = mean))
i_avg_ændringer = na.omit(test_i) %>% group_by(code) %>% summarise_at(vars(EMP, GO, emp_logchanges, prod_logchanges), list(mean = mean))

#industri gns på tværs af år - Danmark
DK_i_avg_niveau = test_i %>% filter(country=="DK") %>% group_by(code) %>% summarise_at(vars(EMP, GO), list(mean = mean))
DK_i_avg_ændringer = na.omit(test_i) %>% filter(country=="DK") %>% group_by(code) %>% summarise_at(vars(EMP, GO, emp_logchanges, prod_logchanges), list(mean = mean))

# på tværs af lande
c_panel_avg = na.omit(test_c) %>% group_by(year) %>% summarise_at(vars(emp_logchanges, prod_logchanges), list(mean = mean))

#brancher
{
b_tot <- test_i %>% group_by(country, year, branche) %>% summarise(EMP_b=sum(EMP) , GO_b=sum(GO)) #, branche_desc
b_tot = b_tot %>% ungroup()
b_tot$id_ci = b_tot %>% group_indices(country, branche) 

#cumulative emplyment share
sumEMP <- b_tot %>% group_by(country, year) %>% summarise(sum_EMP=sum(EMP_b))
b_tot = merge(b_tot, sumEMP, by=c("country","year"), all.x = TRUE)
b_tot$share_EMP = (b_tot$EMP_b/b_tot$sum_EMP)*100
b_tot = pdata.frame(b_tot, index = c("id_ci", "year"))
b_tot$share_EMP_ppchange = diff(b_tot$share_EMP, lag = 1, shift = "time")
b_tot$share_EMP_ppchange = ifelse(is.na(b_tot$share_EMP_ppchange)==T,0,b_tot$share_EMP_ppchange)
b_tot = b_tot %>% group_by(country, branche) %>% mutate(cumsum_EMP = cumsum(share_EMP_ppchange))

#cumulative productivity
b_tot = pdata.frame(b_tot, index = c("id_ci", "year")) 
b_tot$emp_logchanges = diff(log(b_tot$EMP_b), lag = 1, shift = "time")*100
b_tot$prod_logchanges = diff(log(b_tot$GO_b/b_tot$EMP_b), lag = 1, shift = "time")*100
b_tot$prod_logchanges2 = diff(log(b_tot$GO_b/b_tot$EMP_b), lag = 1, shift = "time")

b_tot$prod_logchanges2 <- ifelse(is.na(b_tot$prod_logchanges2)==T,0,b_tot$prod_logchanges2)

#b_tot = b_tot %>% group_by(country, branche) %>% mutate(prod_logCGR = order_by(year, CGR(prod_logchanges2[-1])*100)) %>% select(-prod_logchanges2) #metode 2
#erik = na.omit(b_tot)
#erik = erik %>% group_by(country, branche) %>% mutate(prod_logCGR =  cumprod(prod_logchanges2)*100) #metode 1  order_by(year,cumprod(1+prod_logdiff[-1])*100))

b_tot = b_tot %>% group_by(country, branche) %>% mutate(prod_logCGR =  (cumprod(1+prod_logchanges2)-1)*100) #metode 1  order_by(year,cumprod(1+prod_logdiff[-1])*100))

#b_tot = pdata.frame(b_tot, index = c("id_ci", "year")) 
#b_tot$prod_logCGR <- lag(b_tot$prod_logCGR, k= 1, shift="time")
#b_tot$prod_logCGR <- ifelse(is.na(b_tot$prod_logCGR)==T,0,b_tot$prod_logCGR)

b_tot_avg = b_tot %>% group_by(branche, year) %>% summarise_at(vars(prod_logCGR, cumsum_EMP), list(mean = mean))

#branche gns på tværs af år
b_avg_ændringer = na.omit(b_tot) %>% group_by(branche) %>% summarise_at(vars(EMP_b, GO_b, emp_logchanges, prod_logchanges), list(mean = mean))

#branche gns på tværs af år - Danmark
DK_b_avg_ændringer = na.omit(b_tot) %>% filter(country=="DK") %>% group_by(branche) %>% summarise_at(vars(EMP_b, GO_b, emp_logchanges, prod_logchanges), list(mean = mean))
}

}


#datoformat
min <- as.Date("1995-1-1")
max <- NA

library(devtools)
library(ggpubr) # ggpubr og dplyr overskriver hianden

test_c$year <- as.Date(ISOdate(test_c$year, 1, 1))
c_panel_avg$year <- as.Date(ISOdate(c_panel_avg$year, 1, 1))
b_tot$year = as.Date(ISOdate(b_tot$year, 1, 1))
b_tot_avg$year = as.Date(ISOdate(b_tot_avg$year, 1, 1))

#Undersøgelse af hvilke lande og industrier der indgår
u_industries = test_i %>% select(code) %>% unique()
n_industries = test_i %>% group_by(country, year) %>% count()
n_industries_miss = n_industries %>% filter(n!=32)
n_countries = test_c %>% group_by(country) %>% count()
test = data %>% group_by(country, year) %>% count() #der skal være 32 industrier for hvert år i hvert land

#### DESCRIPTIVE - Employment and productivty
{
DK_tot = test_c %>% filter(country=="DK")
DE_tot = test_c %>% filter(country=="DE")
US_tot = test_c %>% filter(country=="US")
NL_tot = test_c %>% filter(country=="NL")

#Produktivitet og beskæftigelse i absolutte tal DK  - Udviklingen i produktivitet og beskæftigelse i Danmark fra 1995-2017
{
  DK_tot$PROD = DK_tot$GO/DK_tot$EMP
  
  DK_tot_emp = ggplot(data = DK_tot) + 
    geom_line(aes(x = year, y = EMP, color = "Beskæftigelse", group=country),) +
    scale_color_manual(name = "Colors", values = c("Beskæftigelse" = "blue", "Produktivitet" = "red")) +
    xlab("") + ylab("Beskæftigede, i tusinde") +
    ggtitle("") +
    guides(colour=guide_legend(title="")) +
    theme_economist() +
    theme(legend.position="right") + 
    scale_x_date(date_breaks = "5 year", date_labels = "%Y")  + scale_x_date(limits = c(min, max))
  
  DK_tot_prod = ggplot(data = DK_tot) + 
    geom_line(aes(x = year, y = PROD, color = "Produktivitet", group=country),) +
    scale_color_manual(name = "Colors", values = c("Beskæftigelse" = "blue", "Produktivitet" = "red")) +
    xlab("") + ylab("Gross output pr. tusind beskæftigede, i millioner DKK") +
    ggtitle("") +
    guides(colour=guide_legend(title="")) +
    theme_economist() +
    theme(legend.position="right") + 
    scale_x_date(date_breaks = "5 year", date_labels = "%Y")  + scale_x_date(limits = c(min, max))
  
  ggarrange(DK_tot_emp, DK_tot_prod, #+ rremove("x.text")
            #labels = c("A", "B"),
            ncol = 1, nrow = 2)
  
  }


#Produktivitet- og beskæftigelsesvækst - LANDE
{
  { ggplot(data = DK_tot) + 
      geom_line(aes(x = year, y = emp_logchanges, color = "Beskæftigelsesvækst", group=country),) +
      geom_line(aes(x = year, y = prod_logchanges, color = "Produktivitetsvækst", group=country),) +
      scale_color_manual(name = "Colors", values = c("Beskæftigelsesvækst" = "blue", "Produktivitetsvækst" = "red")) +
      xlab("") + ylab("") +
      ggtitle("") +
      guides(colour=guide_legend(title="")) +
      theme_economist() +
      theme(legend.position="right") + 
      scale_x_date(date_breaks = "5 year", date_labels = "%Y")  + scale_x_date(limits = c(min, max))
    
    #In order to get a legend, you have to map something to color within aes. 
    #You can then use scale_color_manual to define the colors for the mapped character values. 
  } 
  
  {
    
    DE_tot_graf = ggplot(data = DE_tot) + 
      geom_line(aes(x = year, y = emp_logchanges, color = "Beskæftigelsesvækst", group=country),) +
      geom_line(aes(x = year, y = prod_logchanges, color = "Produktivitetsvækst", group=country),) +
      scale_color_manual(name = "Colors", values = c("Beskæftigelsesvækst" = "blue", "Produktivitetsvækst" = "red")) +
      xlab("") + ylab("") +
      ggtitle("") +
      guides(colour=guide_legend(title="")) +
      theme_economist() +
      theme(legend.position="right") + 
      scale_x_date(date_breaks = "5 year", date_labels = "%Y")  + scale_x_date(limits = c(min, max))

  
    US_tot_graf = ggplot(data = US_tot) + 
      geom_line(aes(x = year, y = emp_logchanges, color = "Beskæftigelsesvækst", group=country),) +
      geom_line(aes(x = year, y = prod_logchanges, color = "Produktivitetsvækst", group=country),) +
      scale_color_manual(name = "Colors", values = c("Beskæftigelsesvækst" = "blue", "Produktivitetsvækst" = "red")) +
      xlab("") + ylab("") +
      ggtitle("") +
      guides(colour=guide_legend(title="")) +
      theme_economist() +
      theme(legend.position="right") + 
      scale_x_date(date_breaks = "5 year", date_labels = "%Y")  + scale_x_date(limits = c(min, max))
  
  
    NL_tot_graf = ggplot(data = NL_tot) + 
      geom_line(aes(x = year, y = emp_logchanges, color = "Beskæftigelsesvækst", group=country),) +
      geom_line(aes(x = year, y = prod_logchanges, color = "Produktivitetsvækst", group=country),) +
      scale_color_manual(name = "Colors", values = c("Beskæftigelsesvækst" = "blue", "Produktivitetsvækst" = "red")) +
      xlab("") + ylab("") +
      ggtitle("") +
      guides(colour=guide_legend(title="")) +
      theme_economist() +
      theme(legend.position="right") + 
      scale_x_date(date_breaks = "5 year", date_labels = "%Y")  + scale_x_date(limits = c(min, max))
   
  
    ALL_tot_graf = ggplot(data = c_panel_avg) + 
      geom_line(aes(x = year, y = emp_logchanges_mean, color = "Beskæftigelsesvækst"),) +
      geom_line(aes(x = year, y = prod_logchanges_mean, color = "Produktivitetsvækst"),) +
      scale_color_manual(name = "Colors", values = c("Beskæftigelsesvækst" = "blue", "Produktivitetsvækst" = "red")) +
      xlab("") + ylab("") +
      ggtitle("") +
      guides(colour=guide_legend(title="")) +
      theme_economist() +
      theme(legend.position="right") + 
      scale_x_date(date_breaks = "5 year", date_labels = "%Y")  + scale_x_date(limits = c(min, max))
    
    
    ggarrange(DE_tot_graf, US_tot_graf, NL_tot_graf, ALL_tot_graf,#+ rremove("x.text")
              labels = c("Tyskland", "USA", "Holland", "Alle"),
              ncol = 2, nrow = 2)
    
  } 
  

}

#Produktivitet- og beskæftigelsesvækst - BRANCHER
{
  
  DK_b = b_tot %>% filter(country=="DK")
  
  DK_b$branche_desc <- ifelse(DK_b$branche=="b1","Primære erhverv", 
                              ifelse(DK_b$branche=="b2","Fremstilling", 
                                     ifelse(DK_b$branche=="b3","Højteknologiske services",
                                            ifelse(DK_b$branche=="b4","Lavteknologiske services",
                                                   "Not relevant"
                                            ))))
  
  
  
  #Kumulativ produktivitetsvækst fordelt på brancher
  
  {ggplot(data=DK_b, aes(x=year, y=prod_logCGR, group=branche_desc, colour=branche_desc)) + 
      geom_point() + 
      geom_line() +
      xlab("") + ylab("100 * kumulativ log ændring") +
      ggtitle("") +
      guides(colour=guide_legend(title="Branche")) +
      theme_economist() +
      theme(legend.position="right") +
      scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
      scale_x_date(limits = c(min, max))
   #DK
  
  b_tot_avg
  b_tot_avg$branche_desc <- ifelse(b_tot_avg$branche=="b1","Primære erhverv", 
                              ifelse(b_tot_avg$branche=="b2","Fremstilling", 
                                     ifelse(b_tot_avg$branche=="b3","Højteknologiske services",
                                            ifelse(b_tot_avg$branche=="b4","Lavteknologiske services",
                                                   "Not relevant"
                                            ))))
  
  ggplot(data=b_tot_avg, aes(x=year, y=prod_logCGR_mean, group=branche_desc, colour=branche_desc)) + 
      geom_point() + 
      geom_line() +
      xlab("") + ylab("100 * kumulativ log ændring") +
      ggtitle("") +
      guides(colour=guide_legend(title="Branche")) +
      theme_economist() +
      theme(legend.position="right") +
      scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
      scale_x_date(limits = c(min, max))
  
  
  #Kumulativ ændring i beskæftigelse fordelt på brancher
  ggplot(data=DK_b, aes(x=year, y=cumsum_EMP, group=branche_desc, colour=branche_desc)) + 
      geom_point() + 
      geom_line() +
      xlab("") + ylab("Ændring i andel (procent points)") +
      ggtitle("") +
      guides(colour=guide_legend(title="Branche")) +
      theme_economist() +
      theme(legend.position="right") +
      scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
      scale_x_date(limits = c(min, max))
   #DK
  
  ggplot(data=b_tot_avg, aes(x=year, y=cumsum_EMP_mean, group=branche_desc, colour=branche_desc)) + 
      geom_point() + 
      geom_line() +
      xlab("") + ylab("Ændring i andel (procent points)") +
      ggtitle("") +
      guides(colour=guide_legend(title="Branche")) +
      theme_economist() +
      theme(legend.position="right") +
      scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
      scale_x_date(limits = c(min, max))
  } 
  
}

}


#### DESCRIPTIVE - Predicted cumulative percentage employment change 
{
DK_effects = ci_panel_ss %>%  filter(country=="DK") %>% group_by(code) %>% 
    mutate(baseyearEMP = EMP[year == 2009]) %>% 
    select(country, year, code, indnr, branche, EMP, EMP_b, EMP_c, GO, baseyearEMP, 
    prod_logchanges, prod_logchanges_lag1, prod_logchanges_lag2, prod_logchanges_lag3,
    dLP_I_b1, dLP_I_b1_lag1, dLP_I_b1_lag2, dLP_I_b1_lag3,
    dLP_I_b2, dLP_I_b2_lag1, dLP_I_b2_lag2, dLP_I_b2_lag3,
    dLP_I_b3, dLP_I_b3_lag1, dLP_I_b3_lag2, dLP_I_b3_lag3,
    dLP_I_b4, dLP_I_b4_lag1, dLP_I_b4_lag2, dLP_I_b4_lag3,
    dLP_BwoI_b1 , dLP_BwoI_b1_lag1 , dLP_BwoI_b1_lag2 , dLP_BwoI_b1_lag3 ,
    dLP_BwoI_b2 , dLP_BwoI_b2_lag1 , dLP_BwoI_b2_lag2 , dLP_BwoI_b2_lag3 ,
    dLP_BwoI_b3 , dLP_BwoI_b3_lag1 , dLP_BwoI_b3_lag2 , dLP_BwoI_b3_lag3 ,
    dLP_BwoI_b4 , dLP_BwoI_b4_lag1 , dLP_BwoI_b4_lag2 , dLP_BwoI_b4_lag3)
  
  DK_effects$branche_desc <- ifelse(DK_effects$branche=="b1","Primære erhverv", 
                              ifelse(DK_effects$branche=="b2","Fremstilling", 
                                     ifelse(DK_effects$branche=="b3","Højteknologiske services",
                                            ifelse(DK_effects$branche=="b4","Lavteknologiske services",
                                                   "Not relevant"
                                            ))))
  
  
# INTERNAL EFFECT - Predicted cumulative percentage employment change from own-industry productivity growth originating in five sectors
{
#The percentage annual employment change from the internal effect is given by the annual productivity growth in each industry multiplied by its sector-specific coefficient 
#(denoted by the indicator function 1(i ∈ s) for the corresponding sector). This annual percentage change is applied to base-year employment levels, where 1992, close to the 
#midpoint of the sample period, serves as the base year.

# Country-Year-Industry FE:
DK_inteff = DK_effects %>% mutate(emp_change =ifelse(year==1999,0, 
                                                         ifelse(branche=="b1", prod_logchanges*(exp(-0.199)-1) + prod_logchanges_lag1*(exp(0.119)-1) + prod_logchanges_lag2*(exp(0.138)-1) + prod_logchanges_lag3*(exp(0.114)-1),
                                                                ifelse(branche=="b2", prod_logchanges*(exp(-0.262)-1) + prod_logchanges_lag1*(exp(-0.034)-1) + prod_logchanges_lag2*(exp(0.027)-1) + prod_logchanges_lag3*(exp(0.034)-1),
                                                                       ifelse(branche=="b3", prod_logchanges*(exp(-0.413)-1) + prod_logchanges_lag1*(exp(0.017)-1) + prod_logchanges_lag2*(exp(0.061)-1) + prod_logchanges_lag3*(exp(0.04)-1),
                                                                              ifelse(branche=="b4", prod_logchanges*(exp(-0.419)-1) + prod_logchanges_lag1*(exp(-0.007)-1) + prod_logchanges_lag2*(exp(-0.041)-1) + prod_logchanges_lag3*(exp(0.0)-1),
                                                                                     NA)))))) 
# Country FE: 
DK_inteff = DK_effects %>% mutate(emp_change =ifelse(year==1999,0, 
                                                     ifelse(branche=="b1", prod_logchanges*(exp(-0.225)-1) + prod_logchanges_lag1*(exp(0.082)-1) + prod_logchanges_lag2*(exp(0.104)-1) + prod_logchanges_lag3*(exp(0.073)-1),
                                                            ifelse(branche=="b2", prod_logchanges*(exp(-0.283)-1) + prod_logchanges_lag1*(exp(0.006)-1) + prod_logchanges_lag2*(exp(-0.011)-1) + prod_logchanges_lag3*(exp(-0.005)-1),
                                                                   ifelse(branche=="b3", prod_logchanges*(exp(-0.384)-1) + prod_logchanges_lag1*(exp(0.028)-1) + prod_logchanges_lag2*(exp(0.070)-1) + prod_logchanges_lag3*(exp(0.038)-1),
                                                                          ifelse(branche=="b4", prod_logchanges*(exp(-0.407)-1) + prod_logchanges_lag1*(exp(0.003)-1) + prod_logchanges_lag2*(exp(0.048)-1) + prod_logchanges_lag3*(exp(0.007)-1),
                                                                                 NA)))))) 

DK_inteff = DK_inteff %>% group_by(code) %>%  mutate(cumsum_EMP = cumsum(emp_change))
DK_inteff = DK_inteff %>% mutate(emp_basechange = baseyearEMP*(cumsum_EMP/100))

DK_inteff = DK_inteff %>% group_by(branche, year) %>% summarise(sumEMPchange=sum(emp_basechange), EMP_c_base = 1736) #1999=1674, 2009=1736
DK_inteff = as.data.frame(DK_inteff) %>% mutate(emp_basechange_pct = (sumEMPchange/EMP_c_base)*100)

DK_inteff_all = DK_inteff %>% group_by(year) %>% summarise(sumEMPchange=sum(sumEMPchange),  EMP_c_base = 1736)
DK_inteff_all = DK_inteff_all %>% mutate(emp_basechange_pct = (sumEMPchange/EMP_c_base)*100, branche="alle")
DK_inteff_all = DK_inteff_all %>% select(branche, year, sumEMPchange, EMP_c_base, emp_basechange_pct)
DK_inteff = rbind(DK_inteff,DK_inteff_all)



min <- as.Date("1999-1-1")
max <- NA

DK_inteff$year <- as.Date(ISOdate(DK_inteff$year, 1, 1))

DK_inteff$branche_desc <- ifelse(DK_inteff$branche=="b1","Primære erhverv", 
                                  ifelse(DK_inteff$branche=="b2","Fremstilling", 
                                         ifelse(DK_inteff$branche=="b3","Højteknologiske services",
                                                ifelse(DK_inteff$branche=="b4","Lavteknologiske services",
                                                       "Samlet"
                                                ))))

#Kumulativ ændring i beskæftigelse fra egen-industri produktivitetsvækst for de fire brancher
ggplot(data=DK_inteff, aes(x=year, y=emp_basechange_pct, group=branche_desc, colour=branche_desc)) + 
  geom_point() + 
  geom_line() +
  xlab("") + ylab("Kumulativ prædikteret ændring i beskæftigelse, pct.") +
  ggtitle("") +
  guides(colour=guide_legend(title="Branche")) +
  theme_economist() +
  theme(legend.position="right") +
  scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
  scale_x_date(limits = c(min, max))


}
  
# EXTERNAL EFFECT - Predicted cumulative percentage employment change from spillovers of productivity growth originating in five sectors
{
#Meanwhile, the percentage annual employment change resulting from the external productivity effect is given by the sum of productivity change in each sector s in the current 
#and past three years–leaving out the industry’s own productivity growth– multiplied by the respective sector-specific coefficients and their lags. This quantity is
#in turn multiplied by total country-level employment in the base year , since these external effects operate on the entire economy. 


#viser hvordan branchen bliver påvirket af sector spillovers
{
DK_exeff_1 = DK_effects %>% mutate(emp_change = ifelse(year==1999,0,  (dLP_BwoI_b1 * (exp(0.015)-1)) + (dLP_BwoI_b1_lag1 * (exp(0.012)-1)) + (dLP_BwoI_b1_lag2 * (exp(0.021)-1)) + (dLP_BwoI_b1_lag3 * (exp(0.004)-1)) + 
                                                         (dLP_BwoI_b2 * (exp(0.115)-1)) + (dLP_BwoI_b2_lag1 * (exp(0.054)-1)) + (dLP_BwoI_b2_lag2 * (exp(-0.059)-1)) + (dLP_BwoI_b2_lag3 * (exp(-0.003)-1)) + 
                                                         (dLP_BwoI_b3 * (exp(0.143)-1)) + (dLP_BwoI_b3_lag1 * (exp(0.093)-1)) + (dLP_BwoI_b3_lag2 * (exp(0.004)-1)) + (dLP_BwoI_b3_lag3 * (exp(-0.024)-1)) +
                                                         (dLP_BwoI_b4 * (exp(0.193)-1)) + (dLP_BwoI_b4_lag1 * (exp(0.176)-1)) + (dLP_BwoI_b4_lag2 * (exp(0.097)-1)) + (dLP_BwoI_b4_lag3 * (exp(-0.002)-1))))

DK_exeff_1 = DK_exeff_1 %>% group_by(code) %>%  mutate(cumsum_EMP = cumsum(emp_change))
DK_exeff_1 = DK_exeff_1 %>% mutate(emp_basechange = baseyearEMP *(cumsum_EMP/100))

DK_exeff_1 = DK_exeff_1 %>% group_by(branche, year) %>% summarise(sumEMPchange=sum(emp_basechange), EMP_c_base= 1736)
DK_exeff_1 = as.data.frame(DK_exeff_1) %>% mutate(emp_basechange_pct = (sumEMPchange/EMP_c_base)*100)

DK_exeff_1_all = DK_exeff_1 %>% group_by(year) %>% summarise(sumEMPchange=sum(sumEMPchange), EMP_c_base = 1736)
DK_exeff_1_all = DK_exeff_1_all %>% mutate(emp_basechange_pct = (sumEMPchange/EMP_c_base)*100, branche="alle")
DK_exeff_1_all = DK_exeff_1_all %>% select(branche, year, sumEMPchange, EMP_c_base, emp_basechange_pct)
DK_exeff_1 = rbind(DK_exeff_1,DK_exeff_1_all)

DK_inteff = DK_inteff %>% group_by(code) %>%  mutate(cumsum_EMP = cumsum(emp_change))
DK_inteff = DK_inteff %>% mutate(emp_basechange = baseyearEMP*(cumsum_EMP/100))

min = as.Date("1999-1-1")
max = NA

DK_exeff_1$year <- as.Date(ISOdate(DK_exeff_1$year, 1, 1))

ggplot(data=DK_exeff_1, aes(x=year, y=emp_basechange_pct, group=branche, colour=branche)) + 
  geom_point() + 
  geom_line() +
  xlab("Tid") + ylab("Kumulativ prædikterede ændring i beskæftigelse, pct.") +
  ggtitle("Kumulativ ændring i beskæftigelse i Danmark") +
  guides(colour=guide_legend(title="Branche")) +
  theme_economist() +
  theme(legend.position="right") +
  scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
  scale_x_date(limits = c(min, max))

DK_neteff1 = merge(DK_inteff, DK_exeff_1, by = c("branche","year"))
DK_neteff1$neteffect = DK_neteff1$emp_basechange_pct.x + DK_neteff1$emp_basechange_pct.y

min = as.Date("1999-1-1")
max = NA

DK_neteff1$year <- as.Date(ISOdate(DK_neteff1$year, 1, 1))

ggplot(data=DK_neteff1, aes(x=year, y=neteffect, group=branche, colour=branche)) + 
  geom_point() + 
  geom_line() +
  xlab("Tid") + ylab("Kumulativ prædikterede ændring i beskæftigelse, pct.") +
  ggtitle("Kumulativ ændring i beskæftigelse i Danmark") +
  guides(colour=guide_legend(title="Branche")) +
  theme_economist() +
  theme(legend.position="right") +
  scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
  scale_x_date(limits = c(min, max))

}
# Viser hvordan branchen påvirker økonomien - den vi bruger
{

# Country-Year-Industry FE:
DK_exeff_2 = DK_effects %>% mutate(emp_change_b1 = ifelse(year==1999,0, (dLP_BwoI_b1 * (exp(0.023)-1)) + (dLP_BwoI_b1_lag1 * (exp(0.053)-1)) + (dLP_BwoI_b1_lag2 * (exp(0.047)-1)) + (dLP_BwoI_b1_lag3 * (exp(0.040)-1))),
                                     emp_change_b2 = ifelse(year==1999,0, (dLP_BwoI_b2 * (exp(-0.052)-1)) + (dLP_BwoI_b2_lag1 * (exp(-0.048)-1)) + (dLP_BwoI_b2_lag2 * (exp(-0.074)-1)) + (dLP_BwoI_b2_lag3 * (exp(0.009)-1))),
                                     emp_change_b3 = ifelse(year==1999,0, (dLP_BwoI_b3 * (exp(0.064)-1)) + (dLP_BwoI_b3_lag1 * (exp(0.042)-1)) + (dLP_BwoI_b3_lag2 * (exp(-0.029)-1)) + (dLP_BwoI_b3_lag3 * (exp(-0.025)-1)) ),
                                     emp_change_b4 = ifelse(year==1999,0, (dLP_BwoI_b4 * (exp(0.132)-1)) + (dLP_BwoI_b4_lag1 * (exp(0.155)-1)) + (dLP_BwoI_b4_lag2 * (exp(0.075)-1)) + (dLP_BwoI_b4_lag3 * (exp(-0.038)-1))))
# Country FE:
DK_exeff_2 = DK_effects %>% mutate(emp_change_b1 = ifelse(year==1999,0, (dLP_BwoI_b1 * (exp(0.015)-1)) + (dLP_BwoI_b1_lag1 * (exp(0.012)-1)) + (dLP_BwoI_b1_lag2 * (exp(0.021)-1)) + (dLP_BwoI_b1_lag3 * (exp(0.004)-1))),
                                     emp_change_b2 = ifelse(year==1999,0, (dLP_BwoI_b2 * (exp(0.115)-1)) + (dLP_BwoI_b2_lag1 * (exp(0.054)-1)) + (dLP_BwoI_b2_lag2 * (exp(-0.059)-1)) + (dLP_BwoI_b2_lag3 * (exp(-0.003)-1))),
                                     emp_change_b3 = ifelse(year==1999,0, (dLP_BwoI_b3 * (exp(0.143)-1)) + (dLP_BwoI_b3_lag1 * (exp(0.093)-1)) + (dLP_BwoI_b3_lag2 * (exp(0.004)-1)) + (dLP_BwoI_b3_lag3 * (exp(-0.024)-1)) ),
                                     emp_change_b4 = ifelse(year==1999,0, (dLP_BwoI_b4 * (exp(0.193)-1)) + (dLP_BwoI_b4_lag1 * (exp(0.176)-1)) + (dLP_BwoI_b4_lag2 * (exp(0.097)-1)) + (dLP_BwoI_b4_lag3 * (exp(-0.002)-1))))

  
  
DK_exeff_2 = DK_exeff_2 %>% group_by(code) %>%  mutate(cumsum_EMP_b1 = cumsum(emp_change_b1), 
                                                       cumsum_EMP_b2 = cumsum(emp_change_b2), 
                                                       cumsum_EMP_b3 = cumsum(emp_change_b3), 
                                                       cumsum_EMP_b4 = cumsum(emp_change_b4), EMP_c_base = 1736)


DK_exeff_2 = DK_exeff_2 %>% mutate(emp_basechange_b1 = baseyearEMP * (cumsum_EMP_b1/100),
                                   emp_basechange_b2 = baseyearEMP * (cumsum_EMP_b2/100), 
                                   emp_basechange_b3 = baseyearEMP * (cumsum_EMP_b3/100), 
                                   emp_basechange_b4 = baseyearEMP * (cumsum_EMP_b4/100))

DK_exeff_2 = DK_exeff_2 %>% group_by(year) %>% summarise(sumEMPchange_b1=sum(emp_basechange_b1),
                                                         sumEMPchange_b2=sum(emp_basechange_b2),
                                                         sumEMPchange_b3=sum(emp_basechange_b3),
                                                         sumEMPchange_b4=sum(emp_basechange_b4),
                                                         sumEMPchange_all=sum(emp_basechange_b1 + emp_basechange_b2 + emp_basechange_b3 + emp_basechange_b4), EMP_c_base = 1736)

DK_exeff_2 = as.data.frame(DK_exeff_2) %>% mutate(b1 = (sumEMPchange_b1/EMP_c_base)*100,
                                                  b2 = (sumEMPchange_b2/EMP_c_base)*100,
                                                  b3 = (sumEMPchange_b3/EMP_c_base)*100,
                                                  b4 = (sumEMPchange_b4/EMP_c_base)*100,
                                                  alle = (sumEMPchange_all/EMP_c_base)*100)


DK_exeff_2_long = DK_exeff_2 %>% select(-EMP_c_base, -sumEMPchange_all, -sumEMPchange_b1, -sumEMPchange_b2, -sumEMPchange_b3, -sumEMPchange_b4)

DK_exeff_2_long <- melt(DK_exeff_2_long,
                        id.vars=c("year"),
                        variable.name="branche",
                        value.name= "emp_basechange_pct")



min <- as.Date("1999-1-1")
max <- NA

DK_exeff_2_long$year <- as.Date(ISOdate(DK_exeff_2_long$year, 1, 1))

DK_exeff_2_long$branche_desc <- ifelse(DK_exeff_2_long$branche=="b1","Primære erhverv", 
                                 ifelse(DK_exeff_2_long$branche=="b2","Fremstilling", 
                                        ifelse(DK_exeff_2_long$branche=="b3","Højteknologiske services",
                                               ifelse(DK_exeff_2_long$branche=="b4","Lavteknologiske services",
                                                      "Samlet"
                                               ))))

#Kumulativ ændring i beskæftigelse fra spillover produktivitetsvækst for de fire brancher
ggplot(data=DK_exeff_2_long, aes(x=year, y=emp_basechange_pct, group=branche_desc, colour=branche_desc)) + 
  geom_point() + 
  geom_line() +
  xlab("") + ylab("Kumulativ prædikterede ændring i beskæftigelse, pct.") +
  ggtitle("") +
  guides(colour=guide_legend(title="Branche")) +
  theme_economist() +
  theme(legend.position="right") +
  scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
  scale_x_date(limits = c(min, max))


DK_neteff2 = merge(DK_inteff, DK_exeff_2_long, by = c("branche","year"))
DK_neteff2$neteffect = DK_neteff2$emp_basechange_pct.x + DK_neteff2$emp_basechange_pct.y

min = as.Date("1999-1-1")
max = NA

DK_neteff2$year <- as.Date(ISOdate(DK_neteff2$year, 1, 1))

DK_neteff2$branche_desc <- ifelse(DK_neteff2$branche=="b1","Primære erhverv", 
                                       ifelse(DK_neteff2$branche=="b2","Fremstilling", 
                                              ifelse(DK_neteff2$branche=="b3","Højteknologiske services",
                                                     ifelse(DK_neteff2$branche=="b4","Lavteknologiske services",
                                                            "Samlet"
                                                     ))))


#Kumulativ ændring i beskæftigelse fra produktivitetsvækst for de fire brancher, egen-industri og spillover effekter summeret
ggplot(data=DK_neteff2, aes(x=year, y=neteffect, group=branche_desc, colour=branche_desc)) + 
  geom_point() + 
  geom_line() +
  xlab("") + ylab("Kumulativ prædikterede ændring i beskæftigelse, pct.") +
  ggtitle("") +
  guides(colour=guide_legend(title="Branche")) +
  theme_economist() +
  theme(legend.position="right") +
  scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
  scale_x_date(limits = c(min, max))



}
}
}


#### DESCRIPTIVE - Predicted cumulative percentage employment change for skill groups


DK_effects_skills = DK_effects %>% mutate(code_2 = ifelse(indnr == 2, "B",
                                                          ifelse(indnr %in% c(3,4,5,6,7,8,9,10,11,12,13,14,15), "C", #hele C))
                                                                 ifelse(indnr == 16, "D",
                                                                        ifelse(indnr == 17, "E",
                                                                               ifelse(indnr == 18, "F",
                                                                                      ifelse(indnr %in% c(19,20,21), "G",
                                                                                            ifelse(indnr %in% c(22,23,24,25,26), "H",
                                                                                                  ifelse(indnr == 27, "I",
                                                                                                         ifelse(indnr %in% c(28,29,30), "J",
                                                                                                                ifelse(indnr == 31, "K",
                                                                                                                       ifelse(indnr == 32, "L",
                                                                                                                              ifelse(indnr == 33, "M_N",
                                                                                                                                    NA)))))))))))))

#Metode 1: gennemsnitlig andel på tværs af tid
DK_effects_skills$year = as.numeric(levels(DK_effects_skills$year))[DK_effects_skills$year] 
DK_effects_skills = DK_effects_skills %>% filter(year > 2007) #Vi har har uddannelsesvariable fra 2008 og frem
skill_means_DK = readRDS(file = "skill_means_DK_rds") %>% rename(code_2 = code) #gennemsnitlig andel på tværs af tid
DK_effects_skills = merge(DK_effects_skills, skill_means_DK, by=c("country", "code_2"), all.x = TRUE)


#Metode 2: gennemsnitlig andel for hvert år
DK_effects_skills$year = as.numeric(levels(DK_effects_skills$year))[DK_effects_skills$year] 
DK_effects_skills = DK_effects_skills %>% filter(year > 2007) #Vi har har uddannelsesvariable fra 2008 og frem
skill_means_DK = readRDS(file = "skill_means_DK3_rds") %>% rename(code_2 = code) #gennemsnitlig andel for hvert år
DK_effects_skills = merge(DK_effects_skills, skill_means_DK, by=c("country", "code_2", "year"), all.x = TRUE)

#sortering
DK_effects_skills = DK_effects_skills[order(DK_effects_skills$code, DK_effects_skills$year),]



#### DESCRIPTIVE - Predicted cumulative percentage employment change by skill group from productivity growth originating in five sectors

#To quantify the non-neutrality of productivity growth for employment by skill, we calculate a variant of equation (5) above 
#where we scale predicted employment growth by industry as a function of both internal and external productivity growth by
#the average share of industry employment comprised by low-, middle-, and high education workers

#Paralleling our earlier calculations for aggregate employment, we normalize these predicted employment impacts by the base 
#employment level of each skill group in each country to obtain implied proportional impacts. This scaling also accounts for
#the fact that the three major skill groups are not typically equally large,

# Country FE - fra 1999 og frem
{
DK_effects_lowskill = DK_effects_skills %>% mutate(emp_change = ifelse(year==1999,0, 
                                                                       (lowskill_gns/100) *
                                                                       ((dLP_I_b1* (exp(-0.225)-1) + dLP_I_b1_lag1*(exp(0.082)-1) + dLP_I_b1_lag2*(exp(0.104)-1) + dLP_I_b1_lag3*(exp(0.073)-1)) + 
                                                                       (dLP_I_b2* (exp(-0.283)-1) + dLP_I_b2_lag1*(exp(0.006)-1) + dLP_I_b2_lag2*(exp(-0.011)-1) + dLP_I_b2_lag3*(exp(-0.005)-1)) +
                                                                       (dLP_I_b3*(exp(-0.407)-1) + dLP_I_b3_lag1*(exp(0.003)-1) + dLP_I_b3_lag2*(exp(0.048)-1) + dLP_I_b3_lag3*(exp(0.007)-1)) +
                                                                       (dLP_I_b4*(exp(-0.384)-1) + dLP_I_b4_lag1*(exp(0.028)-1) + dLP_I_b4_lag2*(exp(0.070)-1) + dLP_I_b4_lag3*(exp(0.038)-1)) +
                                                                       (dLP_BwoI_b1 * (exp(0.015)-1)) + (dLP_BwoI_b1_lag1 * (exp(0.012)-1)) + (dLP_BwoI_b1_lag2 * (exp(0.021)-1)) + (dLP_BwoI_b1_lag3 * (exp(0.004)-1)) + 
                                                                       (dLP_BwoI_b2 * (exp(0.115)-1)) + (dLP_BwoI_b2_lag1 * (exp(0.054)-1)) + (dLP_BwoI_b2_lag2 * (exp(-0.059)-1)) + (dLP_BwoI_b2_lag3 * (exp(-0.003)-1)) + 
                                                                       (dLP_BwoI_b3 * (exp(0.143)-1)) + (dLP_BwoI_b3_lag1 * (exp(0.093)-1)) + (dLP_BwoI_b3_lag2 * (exp(0.004)-1)) + (dLP_BwoI_b3_lag3 * (exp(-0.024)-1)) +
                                                                       (dLP_BwoI_b4 * (exp(0.193)-1)) + (dLP_BwoI_b4_lag1 * (exp(0.176)-1)) + (dLP_BwoI_b4_lag2 * (exp(0.097)-1)) + (dLP_BwoI_b4_lag3 * (exp(-0.002)-1)))))

DK_effects_midskill = DK_effects_skills %>% mutate(emp_change = ifelse(year==1999,0, 
                                                                       (midskill_gns/100) *
                                                                         ((dLP_I_b1* (exp(-0.225)-1) + dLP_I_b1_lag1*(exp(0.082)-1) + dLP_I_b1_lag2*(exp(0.104)-1) + dLP_I_b1_lag3*(exp(0.073)-1)) + 
                                                                            (dLP_I_b2* (exp(-0.283)-1) + dLP_I_b2_lag1*(exp(0.006)-1) + dLP_I_b2_lag2*(exp(-0.011)-1) + dLP_I_b2_lag3*(exp(-0.005)-1)) +
                                                                            (dLP_I_b3*(exp(-0.407)-1) + dLP_I_b3_lag1*(exp(0.003)-1) + dLP_I_b3_lag2*(exp(0.048)-1) + dLP_I_b3_lag3*(exp(0.007)-1)) +
                                                                            (dLP_I_b4*(exp(-0.384)-1) + dLP_I_b4_lag1*(exp(0.028)-1) + dLP_I_b4_lag2*(exp(0.070)-1) + dLP_I_b4_lag3*(exp(0.038)-1)) +
                                                                            (dLP_BwoI_b1 * (exp(0.015)-1)) + (dLP_BwoI_b1_lag1 * (exp(0.012)-1)) + (dLP_BwoI_b1_lag2 * (exp(0.021)-1)) + (dLP_BwoI_b1_lag3 * (exp(0.004)-1)) + 
                                                                            (dLP_BwoI_b2 * (exp(0.115)-1)) + (dLP_BwoI_b2_lag1 * (exp(0.054)-1)) + (dLP_BwoI_b2_lag2 * (exp(-0.059)-1)) + (dLP_BwoI_b2_lag3 * (exp(-0.003)-1)) + 
                                                                            (dLP_BwoI_b3 * (exp(0.143)-1)) + (dLP_BwoI_b3_lag1 * (exp(0.093)-1)) + (dLP_BwoI_b3_lag2 * (exp(0.004)-1)) + (dLP_BwoI_b3_lag3 * (exp(-0.024)-1)) +
                                                                            (dLP_BwoI_b4 * (exp(0.193)-1)) + (dLP_BwoI_b4_lag1 * (exp(0.176)-1)) + (dLP_BwoI_b4_lag2 * (exp(0.097)-1)) + (dLP_BwoI_b4_lag3 * (exp(-0.002)-1)))))                                                                    


DK_effects_highskill = DK_effects_skills %>% mutate(emp_change = ifelse(year==1999,0, 
                                                                       (highskill_gns/100) *
                                                                         ((dLP_I_b1* (exp(-0.225)-1) + dLP_I_b1_lag1*(exp(0.082)-1) + dLP_I_b1_lag2*(exp(0.104)-1) + dLP_I_b1_lag3*(exp(0.073)-1)) +
                                                                            (dLP_I_b2* (exp(-0.283)-1) + dLP_I_b2_lag1*(exp(0.006)-1) + dLP_I_b2_lag2*(exp(-0.011)-1) + dLP_I_b2_lag3*(exp(-0.005)-1)) +
                                                                            (dLP_I_b3*(exp(-0.407)-1) + dLP_I_b3_lag1*(exp(0.003)-1) + dLP_I_b3_lag2*(exp(0.048)-1) + dLP_I_b3_lag3*(exp(0.007)-1)) +
                                                                            (dLP_I_b4*(exp(-0.384)-1) + dLP_I_b4_lag1*(exp(0.028)-1) + dLP_I_b4_lag2*(exp(0.070)-1) + dLP_I_b4_lag3*(exp(0.038)-1)) +
                                                                            (dLP_BwoI_b1 * (exp(0.015)-1)) + (dLP_BwoI_b1_lag1 * (exp(0.012)-1)) + (dLP_BwoI_b1_lag2 * (exp(0.021)-1)) + (dLP_BwoI_b1_lag3 * (exp(0.004)-1)) + 
                                                                            (dLP_BwoI_b2 * (exp(0.115)-1)) + (dLP_BwoI_b2_lag1 * (exp(0.054)-1)) + (dLP_BwoI_b2_lag2 * (exp(-0.059)-1)) + (dLP_BwoI_b2_lag3 * (exp(-0.003)-1)) + 
                                                                            (dLP_BwoI_b3 * (exp(0.143)-1)) + (dLP_BwoI_b3_lag1 * (exp(0.093)-1)) + (dLP_BwoI_b3_lag2 * (exp(0.004)-1)) + (dLP_BwoI_b3_lag3 * (exp(-0.024)-1)) +
                                                                            (dLP_BwoI_b4 * (exp(0.193)-1)) + (dLP_BwoI_b4_lag1 * (exp(0.176)-1)) + (dLP_BwoI_b4_lag2 * (exp(0.097)-1)) + (dLP_BwoI_b4_lag3 * (exp(-0.002)-1)))))

DK_effects_all = DK_effects_skills %>% mutate(emp_change = ifelse(year==1999,0, 
                                                                  ((dLP_I_b1* (exp(-0.225)-1) + dLP_I_b1_lag1*(exp(0.082)-1) + dLP_I_b1_lag2*(exp(0.104)-1) + dLP_I_b1_lag3*(exp(0.073)-1)) +
                                                                     (dLP_I_b2* (exp(-0.283)-1) + dLP_I_b2_lag1*(exp(0.006)-1) + dLP_I_b2_lag2*(exp(-0.011)-1) + dLP_I_b2_lag3*(exp(-0.005)-1)) +
                                                                     (dLP_I_b3*(exp(-0.407)-1) + dLP_I_b3_lag1*(exp(0.003)-1) + dLP_I_b3_lag2*(exp(0.048)-1) + dLP_I_b3_lag3*(exp(0.007)-1)) +
                                                                     (dLP_I_b4*(exp(-0.384)-1) + dLP_I_b4_lag1*(exp(0.028)-1) + dLP_I_b4_lag2*(exp(0.070)-1) + dLP_I_b4_lag3*(exp(0.038)-1)) +
                                                                     (dLP_BwoI_b1 * (exp(0.015)-1)) + (dLP_BwoI_b1_lag1 * (exp(0.012)-1)) + (dLP_BwoI_b1_lag2 * (exp(0.021)-1)) + (dLP_BwoI_b1_lag3 * (exp(0.004)-1)) + 
                                                                     (dLP_BwoI_b2 * (exp(0.115)-1)) + (dLP_BwoI_b2_lag1 * (exp(0.054)-1)) + (dLP_BwoI_b2_lag2 * (exp(-0.059)-1)) + (dLP_BwoI_b2_lag3 * (exp(-0.003)-1)) + 
                                                                     (dLP_BwoI_b3 * (exp(0.143)-1)) + (dLP_BwoI_b3_lag1 * (exp(0.093)-1)) + (dLP_BwoI_b3_lag2 * (exp(0.004)-1)) + (dLP_BwoI_b3_lag3 * (exp(-0.024)-1)) +
                                                                     (dLP_BwoI_b4 * (exp(0.193)-1)) + (dLP_BwoI_b4_lag1 * (exp(0.176)-1)) + (dLP_BwoI_b4_lag2 * (exp(0.097)-1)) + (dLP_BwoI_b4_lag3 * (exp(-0.002)-1)))))

}

# Country FE - fra 2008 og frem
{
  DK_effects_lowskill = DK_effects_skills %>% mutate(emp_change = ifelse(year==2008,0, 
                                                                         (lowskill_gns/100) *
                                                                           ((dLP_I_b1* (exp(-0.225)-1) + dLP_I_b1_lag1*(exp(0.082)-1) + dLP_I_b1_lag2*(exp(0.104)-1) + dLP_I_b1_lag3*(exp(0.073)-1)) + 
                                                                              (dLP_I_b2* (exp(-0.283)-1) + dLP_I_b2_lag1*(exp(0.006)-1) + dLP_I_b2_lag2*(exp(-0.011)-1) + dLP_I_b2_lag3*(exp(-0.005)-1)) +
                                                                              (dLP_I_b3*(exp(-0.407)-1) + dLP_I_b3_lag1*(exp(0.003)-1) + dLP_I_b3_lag2*(exp(0.048)-1) + dLP_I_b3_lag3*(exp(0.007)-1)) +
                                                                              (dLP_I_b4*(exp(-0.384)-1) + dLP_I_b4_lag1*(exp(0.028)-1) + dLP_I_b4_lag2*(exp(0.070)-1) + dLP_I_b4_lag3*(exp(0.038)-1)) +
                                                                              (dLP_BwoI_b1 * (exp(0.015)-1)) + (dLP_BwoI_b1_lag1 * (exp(0.012)-1)) + (dLP_BwoI_b1_lag2 * (exp(0.021)-1)) + (dLP_BwoI_b1_lag3 * (exp(0.004)-1)) + 
                                                                              (dLP_BwoI_b2 * (exp(0.115)-1)) + (dLP_BwoI_b2_lag1 * (exp(0.054)-1)) + (dLP_BwoI_b2_lag2 * (exp(-0.059)-1)) + (dLP_BwoI_b2_lag3 * (exp(-0.003)-1)) + 
                                                                              (dLP_BwoI_b3 * (exp(0.143)-1)) + (dLP_BwoI_b3_lag1 * (exp(0.093)-1)) + (dLP_BwoI_b3_lag2 * (exp(0.004)-1)) + (dLP_BwoI_b3_lag3 * (exp(-0.024)-1)) +
                                                                              (dLP_BwoI_b4 * (exp(0.193)-1)) + (dLP_BwoI_b4_lag1 * (exp(0.176)-1)) + (dLP_BwoI_b4_lag2 * (exp(0.097)-1)) + (dLP_BwoI_b4_lag3 * (exp(-0.002)-1)))))
  
  DK_effects_midskill = DK_effects_skills %>% mutate(emp_change = ifelse(year==2008,0, 
                                                                         (midskill_gns/100) *
                                                                           ((dLP_I_b1* (exp(-0.225)-1) + dLP_I_b1_lag1*(exp(0.082)-1) + dLP_I_b1_lag2*(exp(0.104)-1) + dLP_I_b1_lag3*(exp(0.073)-1)) + 
                                                                              (dLP_I_b2* (exp(-0.283)-1) + dLP_I_b2_lag1*(exp(0.006)-1) + dLP_I_b2_lag2*(exp(-0.011)-1) + dLP_I_b2_lag3*(exp(-0.005)-1)) +
                                                                              (dLP_I_b3*(exp(-0.407)-1) + dLP_I_b3_lag1*(exp(0.003)-1) + dLP_I_b3_lag2*(exp(0.048)-1) + dLP_I_b3_lag3*(exp(0.007)-1)) +
                                                                              (dLP_I_b4*(exp(-0.384)-1) + dLP_I_b4_lag1*(exp(0.028)-1) + dLP_I_b4_lag2*(exp(0.070)-1) + dLP_I_b4_lag3*(exp(0.038)-1)) +
                                                                              (dLP_BwoI_b1 * (exp(0.015)-1)) + (dLP_BwoI_b1_lag1 * (exp(0.012)-1)) + (dLP_BwoI_b1_lag2 * (exp(0.021)-1)) + (dLP_BwoI_b1_lag3 * (exp(0.004)-1)) + 
                                                                              (dLP_BwoI_b2 * (exp(0.115)-1)) + (dLP_BwoI_b2_lag1 * (exp(0.054)-1)) + (dLP_BwoI_b2_lag2 * (exp(-0.059)-1)) + (dLP_BwoI_b2_lag3 * (exp(-0.003)-1)) + 
                                                                              (dLP_BwoI_b3 * (exp(0.143)-1)) + (dLP_BwoI_b3_lag1 * (exp(0.093)-1)) + (dLP_BwoI_b3_lag2 * (exp(0.004)-1)) + (dLP_BwoI_b3_lag3 * (exp(-0.024)-1)) +
                                                                              (dLP_BwoI_b4 * (exp(0.193)-1)) + (dLP_BwoI_b4_lag1 * (exp(0.176)-1)) + (dLP_BwoI_b4_lag2 * (exp(0.097)-1)) + (dLP_BwoI_b4_lag3 * (exp(-0.002)-1)))))                                                                    
  
  
  DK_effects_highskill = DK_effects_skills %>% mutate(emp_change = ifelse(year==2008,0, 
                                                                          (highskill_gns/100) *
                                                                            ((dLP_I_b1* (exp(-0.225)-1) + dLP_I_b1_lag1*(exp(0.082)-1) + dLP_I_b1_lag2*(exp(0.104)-1) + dLP_I_b1_lag3*(exp(0.073)-1)) +
                                                                               (dLP_I_b2* (exp(-0.283)-1) + dLP_I_b2_lag1*(exp(0.006)-1) + dLP_I_b2_lag2*(exp(-0.011)-1) + dLP_I_b2_lag3*(exp(-0.005)-1)) +
                                                                               (dLP_I_b3*(exp(-0.407)-1) + dLP_I_b3_lag1*(exp(0.003)-1) + dLP_I_b3_lag2*(exp(0.048)-1) + dLP_I_b3_lag3*(exp(0.007)-1)) +
                                                                               (dLP_I_b4*(exp(-0.384)-1) + dLP_I_b4_lag1*(exp(0.028)-1) + dLP_I_b4_lag2*(exp(0.070)-1) + dLP_I_b4_lag3*(exp(0.038)-1)) +
                                                                               (dLP_BwoI_b1 * (exp(0.015)-1)) + (dLP_BwoI_b1_lag1 * (exp(0.012)-1)) + (dLP_BwoI_b1_lag2 * (exp(0.021)-1)) + (dLP_BwoI_b1_lag3 * (exp(0.004)-1)) + 
                                                                               (dLP_BwoI_b2 * (exp(0.115)-1)) + (dLP_BwoI_b2_lag1 * (exp(0.054)-1)) + (dLP_BwoI_b2_lag2 * (exp(-0.059)-1)) + (dLP_BwoI_b2_lag3 * (exp(-0.003)-1)) + 
                                                                               (dLP_BwoI_b3 * (exp(0.143)-1)) + (dLP_BwoI_b3_lag1 * (exp(0.093)-1)) + (dLP_BwoI_b3_lag2 * (exp(0.004)-1)) + (dLP_BwoI_b3_lag3 * (exp(-0.024)-1)) +
                                                                               (dLP_BwoI_b4 * (exp(0.193)-1)) + (dLP_BwoI_b4_lag1 * (exp(0.176)-1)) + (dLP_BwoI_b4_lag2 * (exp(0.097)-1)) + (dLP_BwoI_b4_lag3 * (exp(-0.002)-1)))))
  
  DK_effects_all = DK_effects_skills %>% mutate(emp_change = ifelse(year==2008,0, 
                                                                    ((dLP_I_b1* (exp(-0.225)-1) + dLP_I_b1_lag1*(exp(0.082)-1) + dLP_I_b1_lag2*(exp(0.104)-1) + dLP_I_b1_lag3*(exp(0.073)-1)) +
                                                                       (dLP_I_b2* (exp(-0.283)-1) + dLP_I_b2_lag1*(exp(0.006)-1) + dLP_I_b2_lag2*(exp(-0.011)-1) + dLP_I_b2_lag3*(exp(-0.005)-1)) +
                                                                       (dLP_I_b3*(exp(-0.407)-1) + dLP_I_b3_lag1*(exp(0.003)-1) + dLP_I_b3_lag2*(exp(0.048)-1) + dLP_I_b3_lag3*(exp(0.007)-1)) +
                                                                       (dLP_I_b4*(exp(-0.384)-1) + dLP_I_b4_lag1*(exp(0.028)-1) + dLP_I_b4_lag2*(exp(0.070)-1) + dLP_I_b4_lag3*(exp(0.038)-1)) +
                                                                       (dLP_BwoI_b1 * (exp(0.015)-1)) + (dLP_BwoI_b1_lag1 * (exp(0.012)-1)) + (dLP_BwoI_b1_lag2 * (exp(0.021)-1)) + (dLP_BwoI_b1_lag3 * (exp(0.004)-1)) + 
                                                                       (dLP_BwoI_b2 * (exp(0.115)-1)) + (dLP_BwoI_b2_lag1 * (exp(0.054)-1)) + (dLP_BwoI_b2_lag2 * (exp(-0.059)-1)) + (dLP_BwoI_b2_lag3 * (exp(-0.003)-1)) + 
                                                                       (dLP_BwoI_b3 * (exp(0.143)-1)) + (dLP_BwoI_b3_lag1 * (exp(0.093)-1)) + (dLP_BwoI_b3_lag2 * (exp(0.004)-1)) + (dLP_BwoI_b3_lag3 * (exp(-0.024)-1)) +
                                                                       (dLP_BwoI_b4 * (exp(0.193)-1)) + (dLP_BwoI_b4_lag1 * (exp(0.176)-1)) + (dLP_BwoI_b4_lag2 * (exp(0.097)-1)) + (dLP_BwoI_b4_lag3 * (exp(-0.002)-1)))))
  
}


DK_lowskill = DK_effects_lowskill %>% group_by(code) %>% mutate(cumsum_EMP = cumsum(emp_change))
DK_lowskill = DK_lowskill %>% mutate(emp_basechange = (baseyearEMP*lowskill_gns/100) * (cumsum_EMP/100)) #base employment level of each skill group

DK_midskill = DK_effects_midskill %>% group_by(code) %>% mutate(cumsum_EMP = cumsum(emp_change))
DK_midskill = DK_midskill %>% mutate(emp_basechange = (baseyearEMP*(midskill_gns/100)) * (cumsum_EMP/100))  #base employment level of each skill group

DK_highskill = DK_effects_highskill %>% group_by(code) %>% mutate(cumsum_EMP = cumsum(emp_change))
DK_highskill = DK_highskill %>% mutate(emp_basechange = (baseyearEMP*(highskill_gns/100)) * (cumsum_EMP/100))  #base employment level of each skill group

#DK_all= DK_effects_all %>% group_by(code) %>% mutate(cumsum_EMP = cumsum(emp_change))
#DK_all = DK_all %>% mutate(emp_basechange = (baseyearEMP*(cumsum_EMP/100)))  #base employment level of each skill group


#vi kan nu vise to ting: 1, hvor meget uddannelsesgrupperne er vokset i absolutte termer eller 2, skalere den absolutte kumulative vækst ift gruppernes størrelse

#skill_means_DK2 = readRDS(file = "skill_means_DK2_rds") #bruges bare til at se skill gruppernes 

DK_lowskill = DK_lowskill %>% group_by(year) %>% summarise(sumEMPchange=sum(emp_basechange), EMP_c_base_share= 1736*0.2787) #skal akkumuleres for de tre skill grupper og ikke brancher
DK_lowskill = as.data.frame(DK_lowskill) %>% mutate(emp_basechange_pct = (sumEMPchange/EMP_c_base_share)*100)
DK_lowskill$education = "Ufaglærte"

DK_midskill = DK_midskill %>% group_by(year) %>% summarise(sumEMPchange=sum(emp_basechange), EMP_c_base_share = 1736*0.4842) #skal akkumuleres for de tre skill grupper og ikke brancher
DK_midskill = as.data.frame(DK_midskill) %>% mutate(emp_basechange_pct = (sumEMPchange/EMP_c_base_share)*100)
DK_midskill$education = "Mellemlang Uddannelse"

DK_highskill = DK_highskill %>% group_by(year) %>% summarise(sumEMPchange=sum(emp_basechange), EMP_c_base_share = 1736*0.2371) #skal akkumuleres for de tre skill grupper og ikke brancher
DK_highskill = as.data.frame(DK_highskill) %>% mutate(emp_basechange_pct = (sumEMPchange/EMP_c_base_share)*100)
DK_highskill$education = "Lang Uddannelse"

#DK_all = DK_all %>% group_by(year) %>% summarise(sumEMPchange=sum(emp_basechange), EMP_c_base_share= 1736) #skal akkumuleres for de tre skill grupper og ikke brancher
#DK_all = as.data.frame(DK_all) %>% mutate(emp_basechange_pct = (sumEMPchange/EMP_c_base_share)*100)
#DK_all$education = "All"

DK_skills = rbind(DK_lowskill, DK_midskill, DK_highskill)
#DK_skills = rbind(DK_lowskill, DK_midskill, DK_highskill, DK_all)

min = as.Date("2008-1-1")
max = NA

DK_skills$year <- as.Date(ISOdate(DK_skills$year, 1, 1))

#Kumulativ ændring i beskæftigelsen for de tre uddannelsesgrupper fra produktivitetsvækst med oprindelse i de fire brancher
ggplot(data=DK_skills, aes(x=year, y=emp_basechange_pct, group=education, colour=education)) + 
  geom_point() + 
  geom_line() +
  xlab("") + ylab("Kumulativ prædikterede beskæftigelseæmdringer (skaleret for gruppernes relative størrelse i basisåret)") +
  ggtitle("") +
  guides(colour=guide_legend(title="Uddannelsesgrupper:")) +
  theme_economist() +
  theme(legend.position="right") +
  scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
  scale_x_date(limits = c(min, max))


#viser hvordan kompetencegrupper bliver påvirket af interne effekter

DK_effects_lowskill = DK_effects_skills %>% mutate(emp_change = ifelse(year==2008,0, 
                                                                       (lowskill_gns/100) *
                                                                         (  (dLP_I_b1* (exp(-0.225)-1) + dLP_I_b1_lag1*(exp(0.082)-1) + dLP_I_b1_lag2*(exp(0.104)-1) + dLP_I_b1_lag3*(exp(0.073)-1)) + 
                                                                            (dLP_I_b2* (exp(-0.283)-1) + dLP_I_b2_lag1*(exp(0.006)-1) + dLP_I_b2_lag2*(exp(-0.011)-1) + dLP_I_b2_lag3*(exp(-0.005)-1)) +
                                                                            (dLP_I_b3*(exp(-0.407)-1) + dLP_I_b3_lag1*(exp(0.003)-1) + dLP_I_b3_lag2*(exp(0.048)-1) + dLP_I_b3_lag3*(exp(0.007)-1)) +
                                                                            (dLP_I_b4*(exp(-0.384)-1) + dLP_I_b4_lag1*(exp(0.028)-1) + dLP_I_b4_lag2*(exp(0.070)-1) + dLP_I_b4_lag3*(exp(0.038)-1)) )))
                                                                            
DK_effects_midskill = DK_effects_skills %>% mutate(emp_change = ifelse(year==2008,0, 
                                                                       (midskill_gns/100) *
                                                                         (  (dLP_I_b1* (exp(-0.225)-1) + dLP_I_b1_lag1*(exp(0.082)-1) + dLP_I_b1_lag2*(exp(0.104)-1) + dLP_I_b1_lag3*(exp(0.073)-1)) + 
                                                                            (dLP_I_b2* (exp(-0.283)-1) + dLP_I_b2_lag1*(exp(0.006)-1) + dLP_I_b2_lag2*(exp(-0.011)-1) + dLP_I_b2_lag3*(exp(-0.005)-1)) +
                                                                            (dLP_I_b3*(exp(-0.407)-1) + dLP_I_b3_lag1*(exp(0.003)-1) + dLP_I_b3_lag2*(exp(0.048)-1) + dLP_I_b3_lag3*(exp(0.007)-1)) +
                                                                            (dLP_I_b4*(exp(-0.384)-1) + dLP_I_b4_lag1*(exp(0.028)-1) + dLP_I_b4_lag2*(exp(0.070)-1) + dLP_I_b4_lag3*(exp(0.038)-1)) )))
                                                                          
DK_effects_highskill = DK_effects_skills %>% mutate(emp_change = ifelse(year==2008,0, 
                                                                        (highskill_gns/100) *
                                                                          (  (dLP_I_b1* (exp(-0.225)-1) + dLP_I_b1_lag1*(exp(0.082)-1) + dLP_I_b1_lag2*(exp(0.104)-1) + dLP_I_b1_lag3*(exp(0.073)-1)) +
                                                                             (dLP_I_b2* (exp(-0.283)-1) + dLP_I_b2_lag1*(exp(0.006)-1) + dLP_I_b2_lag2*(exp(-0.011)-1) + dLP_I_b2_lag3*(exp(-0.005)-1)) +
                                                                             (dLP_I_b3*(exp(-0.407)-1) + dLP_I_b3_lag1*(exp(0.003)-1) + dLP_I_b3_lag2*(exp(0.048)-1) + dLP_I_b3_lag3*(exp(0.007)-1)) +
                                                                             (dLP_I_b4*(exp(-0.384)-1) + dLP_I_b4_lag1*(exp(0.028)-1) + dLP_I_b4_lag2*(exp(0.070)-1) + dLP_I_b4_lag3*(exp(0.038)-1)) )))
                                                                             
#viser hvordan kompetencegrupper bliver påvirket af eksterne effekter

DK_effects_lowskill = DK_effects_skills %>% mutate(emp_change = ifelse(year==2008,0, 
                                                                       (lowskill_gns/100) *
                                                                         ((dLP_BwoI_b1 * (exp(0.015)-1)) + (dLP_BwoI_b1_lag1 * (exp(0.012)-1)) + (dLP_BwoI_b1_lag2 * (exp(0.021)-1)) + (dLP_BwoI_b1_lag3 * (exp(0.004)-1)) + 
                                                                            (dLP_BwoI_b2 * (exp(0.115)-1)) + (dLP_BwoI_b2_lag1 * (exp(0.054)-1)) + (dLP_BwoI_b2_lag2 * (exp(-0.059)-1)) + (dLP_BwoI_b2_lag3 * (exp(-0.003)-1)) + 
                                                                            (dLP_BwoI_b3 * (exp(0.143)-1)) + (dLP_BwoI_b3_lag1 * (exp(0.093)-1)) + (dLP_BwoI_b3_lag2 * (exp(0.004)-1)) + (dLP_BwoI_b3_lag3 * (exp(-0.024)-1)) +
                                                                            (dLP_BwoI_b4 * (exp(0.193)-1)) + (dLP_BwoI_b4_lag1 * (exp(0.176)-1)) + (dLP_BwoI_b4_lag2 * (exp(0.097)-1)) + (dLP_BwoI_b4_lag3 * (exp(-0.002)-1)))))

DK_effects_midskill = DK_effects_skills %>% mutate(emp_change = ifelse(year==2008,0, 
                                                                       (midskill_gns/100) *
                                                                         ((dLP_BwoI_b1 * (exp(0.015)-1)) + (dLP_BwoI_b1_lag1 * (exp(0.012)-1)) + (dLP_BwoI_b1_lag2 * (exp(0.021)-1)) + (dLP_BwoI_b1_lag3 * (exp(0.004)-1)) + 
                                                                            (dLP_BwoI_b2 * (exp(0.115)-1)) + (dLP_BwoI_b2_lag1 * (exp(0.054)-1)) + (dLP_BwoI_b2_lag2 * (exp(-0.059)-1)) + (dLP_BwoI_b2_lag3 * (exp(-0.003)-1)) + 
                                                                            (dLP_BwoI_b3 * (exp(0.143)-1)) + (dLP_BwoI_b3_lag1 * (exp(0.093)-1)) + (dLP_BwoI_b3_lag2 * (exp(0.004)-1)) + (dLP_BwoI_b3_lag3 * (exp(-0.024)-1)) +
                                                                            (dLP_BwoI_b4 * (exp(0.193)-1)) + (dLP_BwoI_b4_lag1 * (exp(0.176)-1)) + (dLP_BwoI_b4_lag2 * (exp(0.097)-1)) + (dLP_BwoI_b4_lag3 * (exp(-0.002)-1)))))                                                                    


DK_exeff_highskill = DK_effects_skills %>% mutate(emp_change = ifelse(year==2008,0, 
                                                                        (highskill_gns/100) *
                                                                          ((dLP_BwoI_b1 * (exp(0.015)-1)) + (dLP_BwoI_b1_lag1 * (exp(0.012)-1)) + (dLP_BwoI_b1_lag2 * (exp(0.021)-1)) + (dLP_BwoI_b1_lag3 * (exp(0.004)-1)) + 
                                                                             (dLP_BwoI_b2 * (exp(0.115)-1)) + (dLP_BwoI_b2_lag1 * (exp(0.054)-1)) + (dLP_BwoI_b2_lag2 * (exp(-0.059)-1)) + (dLP_BwoI_b2_lag3 * (exp(-0.003)-1)) + 
                                                                             (dLP_BwoI_b3 * (exp(0.143)-1)) + (dLP_BwoI_b3_lag1 * (exp(0.093)-1)) + (dLP_BwoI_b3_lag2 * (exp(0.004)-1)) + (dLP_BwoI_b3_lag3 * (exp(-0.024)-1)) +
                                                                             (dLP_BwoI_b4 * (exp(0.193)-1)) + (dLP_BwoI_b4_lag1 * (exp(0.176)-1)) + (dLP_BwoI_b4_lag2 * (exp(0.097)-1)) + (dLP_BwoI_b4_lag3 * (exp(-0.002)-1)))))


# De følgende koder kan køres for både interne og eksterne effekter

DK_lowskill = DK_effects_lowskill %>% group_by(code) %>% mutate(cumsum_EMP = cumsum(emp_change))
DK_lowskill = DK_lowskill %>% mutate(emp_basechange = (baseyearEMP*lowskill_gns/100) * (cumsum_EMP/100)) #base employment level of each skill group

DK_midskill = DK_effects_midskill %>% group_by(code) %>% mutate(cumsum_EMP = cumsum(emp_change))
DK_midskill = DK_midskill %>% mutate(emp_basechange = (baseyearEMP*(midskill_gns/100)) * (cumsum_EMP/100))  #base employment level of each skill group

DK_highskill = DK_effects_highskill %>% group_by(code) %>% mutate(cumsum_EMP = cumsum(emp_change))
DK_highskill = DK_highskill %>% mutate(emp_basechange = (baseyearEMP*(highskill_gns/100)) * (cumsum_EMP/100))  #base employment level of each skill group



DK_lowskill = DK_lowskill %>% group_by(year) %>% summarise(sumEMPchange=sum(emp_basechange), EMP_c_base_share= 1736*0.28) #skal akkumuleres for de tre skill grupper og ikke brancher - der ganges med andelen i 2009
DK_lowskill = as.data.frame(DK_lowskill) %>% mutate(emp_basechange_pct = (sumEMPchange/EMP_c_base_share)*100)
DK_lowskill$education = "lowskill"

DK_midskill = DK_midskill %>% group_by(year) %>% summarise(sumEMPchange=sum(emp_basechange), EMP_c_base_share = 1736*0.654) #skal akkumuleres for de tre skill grupper og ikke brancher - der ganges med andelen i 2009
DK_midskill = as.data.frame(DK_midskill) %>% mutate(emp_basechange_pct = (sumEMPchange/EMP_c_base_share)*100)
DK_midskill$education = "midskill"

DK_highskill = DK_highskill %>% group_by(year) %>% summarise(sumEMPchange=sum(emp_basechange), EMP_c_base_share = 1736*0.066) #skal akkumuleres for de tre skill grupper og ikke brancher - der ganges med andelen i 2009
DK_highskill = as.data.frame(DK_highskill) %>% mutate(emp_basechange_pct = (sumEMPchange/EMP_c_base_share)*100)
DK_highskill$education = "highskill"

DK_skills = rbind(DK_lowskill, DK_midskill, DK_highskill)

min = as.Date("2008-1-1")
max = NA

DK_skills$year <- as.Date(ISOdate(DK_skills$year, 1, 1))


ggplot(data=DK_skills, aes(x=year, y=emp_basechange_pct, group=education, colour=education)) + 
  geom_point() + 
  geom_line() +
  xlab("Tid") + ylab("Kumulativ prædikterede beskæftigelseæmdringer for kompetence grupperne") +
  ggtitle("Kumulativ ændring i beskæftigelsen for de tre kompetencegrupper fra produktivitetsvækst med oprindelse i de fire brancher") +
  guides(colour=guide_legend(title="Branche")) +
  theme_economist() +
  theme(legend.position="right") +
  scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
  scale_x_date(limits = c(min, max))


