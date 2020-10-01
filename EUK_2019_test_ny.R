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

#method = "MN_4" eller "AS_5"
#type = "industrier" eller "lande"
func_empprod <- function(method, type) {
  
  #EUK_growthaccounts <- readRDS("~/OneDrive - Aalborg Universitet/10. SEMESTER (SPECIALE)/Speciale-oecon/Statistical_Growth-Accounts.rds")
  EUK_nationalaccounts <- readRDS("Data/Statistical_National-Accounts.rds")
  
  if (method=="MN_4") {
    
    #AutorSalomons Industrier minus branche 3 :
    
    data = EUK_nationalaccounts %>% filter(var %in% c("EMP","GO_Q"), !indnr %in% c("Agg", "*Agg", "1", "34","35", "36","37","38","39","40")) #svarer til A, O, P, Q, R, S, T,U
    
    
    data$branche <- ifelse(data$indnr %in% c(2,16,17,18), "b1", #C,D,E,F
                           ifelse(data$indnr %in% c(3,4,5,6,7,8,9,10,11,12,13,14,15), "b2", #hele C
                                  ifelse(data$indnr %in% c(26,28,29,30,31, 33), "b3", #("53", "58t60", "61", "62t63", "K", "MtN")
                                         ifelse(data$indnr %in% c(19,20,21,22,23,24,25,27,32), "b4", #("45", "46", "47", "49t52", "I", "L")
                                                "b0"))))
    
    data$branche_desc <- ifelse(data$branche=="b1","Mining, utilities, and construction", 
                                ifelse(data$branche=="b2","Manufacturing", 
                                       ifelse(data$branche=="b3","High-tech services",
                                              ifelse(data$branche=="b4","Low-tech services",
                                                     "Not relevant"
                                              ))))
    
  } else if (method=="AS_5") {
    #AutorSalomons Industrier:
    
    data = EUK_nationalaccounts %>% filter(var %in% c("EMP","GO_Q"), !indnr %in% c("Agg", "*Agg", "1", "39","40")) #svarer til A,T,U
    
    data$branche <- ifelse(data$indnr %in% c(2,16,17,18), "b1", #C,D,E,F
                           ifelse(data$indnr %in% c(3,4,5,6,7,8,9,10,11,12,13,14,15), "b2", #hele C
                                  ifelse(data$indnr %in% c(35,36,37,38), "b3", #("P","Q","R", "S")
                                         ifelse(data$indnr %in% c(26,28,29,30,31, 33), "b4", #("53", "58t60", "61", "62t63", "K", "MtN")
                                                ifelse(data$indnr %in% c(19,20,21,22,23,24,25,27,32), "b5", #("45", "46", "47", "49t52", "I", "L")
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
  
  
  #populationsvariabel
  
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
  
  pop_var = na.omit(pop_var)
  
  
  if (type=="industrier") {
    
    #Kodning af variable:
    data$id_ci = data %>% group_indices(country, code) 
    data = merge(data, pop_var, by=c("country", "year"), all.x = TRUE)
    
    pdata = pdata.frame(data, index = c("id_ci", "year")) #Hvis R melder duplikater, hvilket bare skyldes at der er to variable for hver
    pdata$emp_logchanges = diff(log(pdata$EMP), lag = 1, shift = "time")*100
    pdata$prod_logchanges = diff(log(pdata$GO/pdata$EMP), lag = 1, shift = "time")*100
    pdata$wkgpop_logchanges = diff(log(pdata$wkgpop), lag = 1, shift = "time")*100
    pdata$pop_logchanges = diff(log(pdata$pop), lag = 1, shift = "time")*100
    
    
  } else if (type=="lande"){
    
    data <- data %>% group_by(country, year) %>% summarize(EMP=sum(EMP) , GO=sum(GO))
    data <- data %>% ungroup()
    
    data = merge(data, pop_var, by=c("country", "year"), all.x = TRUE)
    
    pdata = pdata.frame(data, index = c("country", "year")) 
    pdata$emp_logchanges = diff(log(pdata$EMP), lag = 1, shift = "time")*100
    pdata$prod_logchanges = diff(log(pdata$GO/pdata$EMP), lag = 1, shift = "time")*100
    pdata$wkgpop_logchanges = diff(log(pdata$wkgpop), lag = 1, shift = "time")*100
    pdata$pop_logchanges = diff(log(pdata$pop), lag = 1, shift = "time")*100
    
  } else {print("Error: forkert antal brancher")}
  
  pdata
}

#method = "MN_4" eller "AS_5"
#type = "1" (uden lags) eller "2" (med lags)
func_regpanel <- function(dataset_1, method, type) {
  
  #angivelse af branche/industri totaler - der bør ikke være nogle branche = b0
  b_tot <- dataset_1 %>% group_by(country, year, branche) %>% summarize(EMP_b=sum(EMP) , GO_b=sum(GO))
  ind = merge(dataset_1, b_tot, by=c("country", "year", "branche"), all.x = TRUE) 
  
  if (method=="MN_4") {
    
    #uden branche 3
    b1 = b_tot %>% filter(branche=="b1") %>% mutate(EMP_b1=EMP_b) %>% mutate(GO_b1=GO_b) %>% select(country, year, EMP_b1,GO_b1)
    b2 = b_tot %>% filter(branche=="b2") %>% mutate(EMP_b2=EMP_b) %>% mutate(GO_b2=GO_b) %>% select(EMP_b2, GO_b2)
    b3 = b_tot %>% filter(branche=="b3") %>% mutate(EMP_b3=EMP_b) %>% mutate(GO_b3=GO_b) %>% select(EMP_b3, GO_b3)
    b4 = b_tot %>% filter(branche=="b4") %>% mutate(EMP_b4=EMP_b) %>% mutate(GO_b4=GO_b) %>% select(EMP_b4, GO_b4)
    
    b = cbind(b1,b2,b3,b4)
    b = b %>% select(-country1,-country2, -country3,-year1,-year2,-year3)
    
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
      ind$dLP_BwoI_b1 = ifelse(ind$branche=="b1", 0, diff(log(ind$GO_b1/ind$EMP_b1), lag = 1, shift = "time")*100)
      ind$dLP_BwoI_b2 = ifelse(ind$branche=="b2", 0, diff(log(ind$GO_b2/ind$EMP_b2), lag = 1, shift = "time")*100)
      ind$dLP_BwoI_b3 = ifelse(ind$branche=="b3", 0, diff(log(ind$GO_b3/ind$EMP_b3), lag = 1, shift = "time")*100)
      ind$dLP_BwoI_b4 = ifelse(ind$branche=="b4", 0, diff(log(ind$GO_b4/ind$EMP_b4), lag = 1, shift = "time")*100)
      
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

# Deskriptiv --------------------------------------------------

#Forberedelse:
min <- as.Date("1996-1-1")
max <- NA

test_c = func_empprod("MN_4","lande")
test_i = func_empprod("MN_4","industrier")
test_c = na.omit(test_c)
c_panel_avg = test_c %>% group_by(year) %>% summarise_at(vars(emp_logchanges, prod_logchanges), list(mean = mean))

#sapply(test_c, class)
#test_c$year <- format(as.Date(test_c$year, "%Y"), format="%Y")
test_c$year <- as.Date(test_c$year, "%Y")
c_panel_avg$year <- as.Date(c_panel_avg$year, "%Y")


#Undersøgelse af hvilke lande og industrier der indgår
u_industries = test_i %>% select(code) %>% unique()
n_industries = test_i %>% group_by(country, year) %>% count()
n_industries_miss = n_industries %>% filter(n!=32)
n_countries = test_c %>% group_by(country) %>% count()
test = data %>% group_by(country, year) %>% count() #der skal være 32 industrier for hvert år i hvert land


# DESCRIPTIVE - Sectoral employment and productivty
{
  
  DK_tot = test_c %>% filter(country=="DK")
  DE_tot = test_c %>% filter(country=="DE")
  US_tot = test_c %>% filter(country=="US")
  NL_tot = test_c %>% filter(country=="NL")
  
  
  #Produktivitet- og beskæftigelsesvækst
  
  { ggplot(data = DK_tot) + 
      geom_line(aes(x = year, y = emp_logchanges, color = "emp_logchanges", group=country),) +
      geom_line(aes(x = year, y = prod_logchanges, color = "prod_logchanges", group=country),) +
      scale_color_manual(name = "Colors", values = c("emp_logchanges" = "blue", "prod_logchanges" = "red")) +
      xlab("Time") + ylab("") +
      ggtitle("Produktivitet- og beskæftigelsesvækst i DK") +
      guides(colour=guide_legend(title="")) +
      theme_economist() +
      theme(legend.position="right") + 
      scale_x_date(date_breaks = "5 year", date_labels = "%Y")  + scale_x_date(limits = c(min, max))
    
    #In order to get a legend, you have to map something to color within aes. 
    #You can then use scale_color_manual to define the colors for the mapped character values. 
  } 
  
  {ggplot(data = DE_tot) + 
      geom_line(aes(x = year, y = emp_logchanges, color = "emp_logchanges", group=country),) +
      geom_line(aes(x = year, y = prod_logchanges, color = "prod_logchanges", group=country),) +
      scale_color_manual(name = "Colors", values = c("emp_logchanges" = "blue", "prod_logchanges" = "red")) +
      xlab("Time") + ylab("") +
      ggtitle("Produktivitet- og beskæftigelsesvækst i DE") +
      guides(colour=guide_legend(title="")) +
      theme_economist() +
      theme(legend.position="right") + 
      scale_x_date(date_breaks = "5 year", date_labels = "%Y")  + scale_x_date(limits = c(min, max))
  } 
  
  {ggplot(data = US_tot) + 
      geom_line(aes(x = year, y = emp_logchanges, color = "emp_logchanges", group=country),) +
      geom_line(aes(x = year, y = prod_logchanges, color = "prod_logchanges", group=country),) +
      scale_color_manual(name = "Colors", values = c("emp_logchanges" = "blue", "prod_logchanges" = "red")) +
      xlab("Time") + ylab("") +
      ggtitle("Produktivitet- og beskæftigelsesvækst i US") +
      guides(colour=guide_legend(title="")) +
      theme_economist() +
      theme(legend.position="right") + 
      scale_x_date(date_breaks = "5 year", date_labels = "%Y")  + scale_x_date(limits = c(min, max))
  } 
  
  {ggplot(data = NL_tot) + 
      geom_line(aes(x = year, y = emp_logchanges, color = "emp_logchanges", group=country),) +
      geom_line(aes(x = year, y = prod_logchanges, color = "prod_logchanges", group=country),) +
      scale_color_manual(name = "Colors", values = c("emp_logchanges" = "blue", "prod_logchanges" = "red")) +
      xlab("Time") + ylab("") +
      ggtitle("Produktivitet- og beskæftigelsesvækst i NL") +
      guides(colour=guide_legend(title="")) +
      theme_economist() +
      theme(legend.position="right") + 
      scale_x_date(date_breaks = "5 year", date_labels = "%Y")  + scale_x_date(limits = c(min, max))
  } 
  
  {ggplot(data = c_panel_avg) + 
      geom_line(aes(x = year, y = emp_logchanges_mean, color = "emp_logchanges_mean"),) +
      geom_line(aes(x = year, y = prod_logchanges_mean, color = "prod_logchanges_mean"),) +
      scale_color_manual(name = "Colors", values = c("emp_logchanges_mean" = "blue", "prod_logchanges_mean" = "red")) +
      xlab("Time") + ylab("") +
      ggtitle("Produktivitet- og beskæftigelsesvækst, gns på tværs af lande") +
      guides(colour=guide_legend(title="")) +
      theme_economist() +
      theme(legend.position="right") + 
      scale_x_date(date_breaks = "5 year", date_labels = "%Y")  + scale_x_date(limits = c(min, max))
  } 
  
  
  
  #Kumulativ produktivitetsvækst fordelt på brancher
  {ggplot(data=DK_b, aes(x=year, y=prod_logCGR, group=desc, colour=desc)) + 
      geom_point() + 
      geom_line() +
      xlab("Time") + ylab("100 * kumulativ log ændring") +
      ggtitle("Kumulativ produktivitetsvækst") +
      guides(colour=guide_legend(title="Sector")) +
      theme_economist() +
      theme(legend.position="right") +
      scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
      scale_x_date(limits = c(min, max))
  } #DK
  {ggplot(data=UK_b, aes(x=year, y=prod_CGR, group=desc, colour=desc)) + 
      geom_point() + 
      geom_line() +
      xlab("Time") + ylab(")") +
      ggtitle("Kumulativ produktivitetsvækst UK") +
      guides(colour=guide_legend(title="Sector")) +
      theme_economist() +
      theme(legend.position="right")
    #scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
    #scale_x_date(limits = c(min, max))
  } #UK
  
  #Kumulativ ændring i beskæftigelse fordelt på brancher
  {ggplot(data=DK_b, aes(x=year, y=cumsum_EMP, group=desc, colour=desc)) + 
      geom_point() + 
      geom_line() +
      xlab("Time") + ylab("") +
      ggtitle("Kumulativ ændring i beskæftigelse") +
      guides(colour=guide_legend(title="Sector")) +
      theme_economist() +
      theme(legend.position="right") +
      scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
      scale_x_date(limits = c(min, max))
  } #DK
  
}







# Country panel  -----------------------------------------------------

#c_panel = as.data.frame(c_panel)
#c_panel = pdata.frame(c_panel, index = c("country", "year"))

c_panel_lags = c_panel
c_panel_lags$prod_logchanges_lag1 = lag(c_panel_lags$prod_logchanges, k = 1, shift = "time")
c_panel_lags$prod_logchanges_lag2 = lag(c_panel_lags$prod_logchanges_lag1, k = 1, shift = "time")
c_panel_lags$prod_logchanges_lag3 = lag(c_panel_lags$prod_logchanges_lag2, k = 1, shift = "time")

c_panel_lags = na.omit(c_panel_lags)
c_panel = na.omit(c_panel)

#Uden lags
lsdv.c_pool1 = lm(emp_logchanges ~ prod_logchanges, data=c_panel)
lsdv.c_fec1 = lm(emp_logchanges ~ prod_logchanges + factor(country), data=c_panel) 
#lsdv.c_fecy1 = lm(emp_logchanges ~ prod_logchanges + factor(country) + factor(year) -1, data=c_panel) #HC2 og 3 giver NA på std errors hvis regressionen har både land og år dummies

#Med lags
lsdv.c_pool2 = lm(emp_logchanges ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3, data=c_panel_lags)
lsdv.c_fec2  = lm(emp_logchanges ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + factor(country), data=c_panel_lags)
#lsdv.c_fecy2  = lm(emp_logchanges ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + factor(country) + factor(year) -1, data=c_panel)
lsdv.c_fec2_pop  = lm(emp_logchanges ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + wkgpop_logchanges + factor(country), data=c_panel_lags)


lsdv.c_pool1_coef = func_coefs(lsdv.c_pool1, "c_pool1", "HC3") # giver ikke nogen forskel at tilføje method=arrelano
lsdv.c_fec1_coef = func_coefs(lsdv.c_fec1, "c_fec1", "HC3")
#lsdv.c_fecy1_coef = func_coefs(lsdv.c_fecy1, "c_fecy1", "HC3")
lsdv.c_pool2_coef = func_coefs(lsdv.c_pool2, "c_pool2", "HC3")
lsdv.c_fec2_coef = func_coefs(lsdv.c_fec2, "c_fec2", "HC3")
#lsdv.c_fecy2_coef = func_coefs(lsdv.c_fecy2, "c_fecy2", "HC3")
lsdv.c_fec2_pop_coef = func_coefs(lsdv.c_fec2_pop, "c_fec2_pop", "HC3")

regoutput_c_panel <- Reduce(function(a,b){
  ans <- merge(a,b,by="row.names",all=T)
  row.names(ans) <- ans[,"Row.names"]
  ans[,!names(ans) %in% "Row.names"]
}, list(lsdv.c_pool1_coef, lsdv.c_fec1_coef, lsdv.c_pool2_coef, lsdv.c_fec2_coef, lsdv.c_fec2_pop_coef))

write.xlsx(regoutput_c_panel, "regoutput_c_panel.xlsx", col.names = TRUE, row.names = TRUE)


#Tester resultater ved brug af plm istedet: 
{
  model_linear1 = emp_logchanges ~ prod_logchanges 
  
  C0_pool = plm(model_linear1, data = c_panel, index = c("country", "year"), model = "pooling")
  C0_fd = plm(model_linear1, data = c_panel, index = c("country", "year"), model = "fd")
  C0_fe = plm(model_linear1, data = c_panel, index = c("country", "year"), model = "within")
  summary(C0_pool)
  summary(C0_fd)
  summary(C0_fe)
  
  C2_pool = plm(model_linear2, data = c_panel, index = c("country", "year"), model = "pooling")
  C2_fd = plm(model_linear2, data = c_panel, index = c("country", "year"), model = "fd")
  C2_fe = plm(model_linear2, data = c_panel, index = c("country", "year"), model = "within", effect = "individual")
  C2_fe_tw = plm(model_linear2, data = c_panel, index = c("country", "year"), model = "within", effect = "twoway")
  summary(C2_pool)
  summary(C2_fd)
  summary(C2_fe)
  summary(C2_fe_tw)
  
}


# Country industry panel --------------------------------------------------

#AS: Industry-by-country fixed effects are already implicitly taken out by first-differencing in the stacked firstdifference model.
#OBS AS bruger ikke lags i denne pga insignifikans

ci_panel = func_empprod("MN_4","industrier")
ci_panel_1 = func_regpanel(ci_panel, "MN_4", 1) #uden lags
ci_panel_1 = na.omit(ci_panel_1)
ci_panel_1_lags = func_regpanel(ci_panel, "MN_4", 2) #med lags
ci_panel_1_lags = na.omit(ci_panel_1_lags)


#med vægte
lsdv.ci_pool2 = lm(emp_logchanges ~ prod_logchanges, data=ci_panel_1, weights = wgt_i_avg)
lsdv.ci_fec2 = lm(emp_logchanges ~ prod_logchanges + factor(country), data=ci_panel_1, weights = wgt_i_avg) 
lsdv.ci_fecy2 = lm(emp_logchanges ~ prod_logchanges + factor(country) + factor(year), data=ci_panel_1, weights = wgt_i_avg)
#lsdv.ci_feci2 = lm(emp_logchanges ~ prod_logchanges + factor(country) + factor(code), data=ci_panel_1, weights = wgt_i_avg) #autor bruger ikke denne kombi
lsdv.ci_feciy2 = lm(emp_logchanges ~ prod_logchanges + factor(country) + factor(code) + factor(year), data=ci_panel_1, weights = wgt_i_avg)
lsdv.ci_feciy2_pop = lm(emp_logchanges ~ prod_logchanges + wkgpop_logchanges + factor(country) + factor(code) + factor(year), data=ci_panel_1, weights = wgt_i_avg)

#med vægte og lags

#kan bruges til at finde fejl i datasættet - fx finder den Inf observationer for Letland, LV
#target = c("DK", "US", "DE", "NL", "AT", "CZ", "FI", "FR","EL","LV")
#erik = ci_panel_1_lags %>% filter(country %in% target)

lsdv.ci_pool3 = lm(emp_logchanges ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3, data=ci_panel_1_lags, weights = wgt_i_avg)
lsdv.ci_fec3 = lm(emp_logchanges ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + factor(country), data=ci_panel_1_lags, weights = wgt_i_avg )
lsdv.ci_fecy3 = lm(emp_logchanges ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + factor(country) + factor(year), data=ci_panel_1_lags, weights = wgt_i_avg)
#lsdv.ci_feci3 = lm(emp_logchanges ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + factor(country) + factor(code), data=ci_panel_1_lags, weights = wgt_i_avg) #autor bruger ikke denne kombi
lsdv.ci_feciy3 = lm(emp_logchanges ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + factor(country) + factor(code) + factor(year), data=ci_panel_1_lags, weights = wgt_i_avg) #
lsdv.ci_feciy3_pop = lm(emp_logchanges ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + wkgpop_logchanges + factor(country) + factor(code) + factor(year), data=ci_panel_1_lags, weights = wgt_i_avg)


#Robuste standard fejl

lsdv.ci_pool2_coef = func_coefs(lsdv.ci_pool2, "ci_pool2", "HC3") 
lsdv.ci_fec2_coef = func_coefs(lsdv.ci_fec2, "ci_fec2", "HC3")
lsdv.ci_fecy2_coef = func_coefs(lsdv.ci_fecy2, "ci_fecy2", "HC3")
lsdv.ci_feciy2_coef = func_coefs(lsdv.ci_feciy2, "ci_feciy2", "HC3")
lsdv.ci_feciy2_pop_coef = func_coefs(lsdv.ci_feciy2_pop, "ci_feciy2_pop", "HC3")

list_ci2=list(lsdv.ci_pool2_coef, lsdv.ci_fec2_coef, lsdv.ci_fecy2_coef,  lsdv.ci_feciy2_coef, lsdv.ci_feciy2_pop_coef)

lsdv.ci_pool3_coef = func_coefs(lsdv.ci_pool3, "ci_pool3", "HC3") 
lsdv.ci_fec3_coef = func_coefs(lsdv.ci_fec3, "ci_fec3", "HC3")
lsdv.ci_fecy3_coef = func_coefs(lsdv.ci_fecy3, "ci_fecy3", "HC3")
lsdv.ci_feciy3_coef = func_coefs(lsdv.ci_feciy3, "ci_feciy3", "HC3")
lsdv.ci_feciy3_pop_coef = func_coefs(lsdv.ci_feciy3_pop, "ci_feciy3_pop", "HC3")

list_ci3=list(lsdv.ci_pool3_coef, lsdv.ci_fec3_coef, lsdv.ci_fecy3_coef,  lsdv.ci_feciy3_coef, lsdv.ci_feciy3_pop_coef)

#export resultater til excel
regoutput_ci_panel <- Reduce(function(a,b){
  ans <- merge(a,b,by="row.names",all=T)
  row.names(ans) <- ans[,"Row.names"]
  ans[,!names(ans) %in% "Row.names"]
}, list_ci3)

write.xlsx(regoutput_ci_panel, "regoutput_ci_panel.xlsx", col.names = TRUE, row.names = TRUE)


#test af plm
FixedEffects_indi <- plm(model_linear1, data = dk, index = c("code", "year"), weight=wgt, model = "within", effect = "individual")
FixedEffects_time <- plm(model_linear1, data = dk, index = c("code", "year"), weight=wgt, model = "within", effect = "time")
FixedEffects_twoway <- plm(model_linear1, data = dk, index = c("code", "year"), weight=wgt, model = "within", effect = "twoway")

summary(FixedEffects_indi)
summary(FixedEffects_time)
summary(FixedEffects_twoway)


# Sammensætning af mikro og makroelasticiteter --------------------------------------------------

ci_panel_2 = func_regpanel(ci_panel, "MN_4", 2)
ci_panel_2 = na.omit(ci_panel_2)

#regressioner
lsdv.mm_pool1 = lm(emp_logchanges ~ prod_logchanges + dLP_CwoI + dLP_CwoI_lag1 + dLP_CwoI_lag2 + dLP_CwoI_lag3, data=ci_panel_2)
lsdv.mm_fec1 = lm(emp_logchanges ~ prod_logchanges + dLP_CwoI + dLP_CwoI_lag1 + dLP_CwoI_lag2 + dLP_CwoI_lag3 + factor(country), data=ci_panel_2) 
lsdv.mm_feci1 = lm(emp_logchanges ~ prod_logchanges + dLP_CwoI + dLP_CwoI_lag1 + dLP_CwoI_lag2 + dLP_CwoI_lag3 + factor(country) + factor(code), data=ci_panel_2)
lsdv.mm_feciy1 = lm(emp_logchanges ~ prod_logchanges + dLP_CwoI + dLP_CwoI_lag1 + dLP_CwoI_lag2 + dLP_CwoI_lag3 + factor(country) + factor(code) + factor(year), data=ci_panel_2)

#med vægte
lsdv.mm_pool2 = lm(emp_logchanges ~ prod_logchanges + dLP_CwoI + dLP_CwoI_lag1 + dLP_CwoI_lag2 + dLP_CwoI_lag3, data=ci_panel_2, weights = wgt_i_avg)
lsdv.mm_fec2 = lm(emp_logchanges ~ prod_logchanges + dLP_CwoI + dLP_CwoI_lag1 + dLP_CwoI_lag2 + dLP_CwoI_lag3 + factor(country) , data=ci_panel_2, weights = wgt_i_avg) 
lsdv.mm_fecy2 = lm(emp_logchanges ~ prod_logchanges + dLP_CwoI + dLP_CwoI_lag1 + dLP_CwoI_lag2 + dLP_CwoI_lag3 + factor(country) + factor(year), data=ci_panel_2, weights = wgt_i_avg)
lsdv.mm_feciy2 = lm(emp_logchanges ~ prod_logchanges + dLP_CwoI + dLP_CwoI_lag1 + dLP_CwoI_lag2 + dLP_CwoI_lag3 + factor(country) + factor(code) + factor(year), data=ci_panel_2, weights = wgt_i_avg)
lsdv.mm_feciy2_pop = lm(emp_logchanges ~ prod_logchanges + dLP_CwoI + dLP_CwoI_lag1 + dLP_CwoI_lag2 + dLP_CwoI_lag3 + wkgpop_logchanges + factor(country) + factor(code) + factor(year), data=ci_panel_2, weights = wgt_i_avg)

#med vægte og lags
lsdv.mm_pool3 = lm(emp_logchanges ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + dLP_CwoI + dLP_CwoI_lag1 + dLP_CwoI_lag2 + dLP_CwoI_lag3, data=ci_panel_2, weights = wgt_i_avg)
lsdv.mm_fec3 = lm(emp_logchanges ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + dLP_CwoI + dLP_CwoI_lag1 + dLP_CwoI_lag2 + dLP_CwoI_lag3 + factor(country) , data=ci_panel_2, weights = wgt_i_avg) 
lsdv.mm_fecy3 = lm(emp_logchanges ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + dLP_CwoI + dLP_CwoI_lag1 + dLP_CwoI_lag2 + dLP_CwoI_lag3 + factor(country) + factor(year), data=ci_panel_2, weights = wgt_i_avg)
lsdv.mm_feciy3 = lm(emp_logchanges ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + dLP_CwoI + dLP_CwoI_lag1 + dLP_CwoI_lag2 + dLP_CwoI_lag3 + factor(country) + factor(code) + factor(year), data=ci_panel_2, weights = wgt_i_avg)
lsdv.mm_feciy3_pop = lm(emp_logchanges ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + dLP_CwoI + dLP_CwoI_lag1 + dLP_CwoI_lag2 + dLP_CwoI_lag3 + wkgpop_logchanges + factor(country) + factor(code) + factor(year), data=ci_panel_2, weights = wgt_i_avg)


#Robuste standard fejl
lsdv.mm_pool2_coef = func_coefs(lsdv.mm_pool2, "mm_pool2", "HC3") 
lsdv.mm_fec2_coef = func_coefs(lsdv.mm_fec2, "mm_fec2", "HC3")
lsdv.mm_fecy2_coef = func_coefs(lsdv.mm_fecy2, "mm_fecy2", "HC3")
lsdv.mm_feciy2_coef = func_coefs(lsdv.mm_feciy2, "mm_feciy2", "HC3")
lsdv.mm_feciy2_pop_coef = func_coefs(lsdv.mm_feciy2_pop, "mm_feciy2_pop", "HC3")

list_mm2=list(lsdv.mm_pool2_coef, lsdv.mm_fec2_coef, lsdv.mm_fecy2_coef, lsdv.mm_feciy2_coef, lsdv.mm_feciy2_pop_coef)

#Robuste standard fejl
lsdv.mm_pool3_coef = func_coefs(lsdv.mm_pool3, "mm_pool3", "HC3") 
lsdv.mm_fec3_coef = func_coefs(lsdv.mm_fec3, "mm_fec3", "HC3")
lsdv.mm_fecy3_coef = func_coefs(lsdv.mm_fecy3, "mm_fecy3", "HC3")
lsdv.mm_feciy3_coef = func_coefs(lsdv.mm_feciy3, "mm_feciy3", "HC3")
lsdv.mm_feciy3_pop_coef = func_coefs(lsdv.mm_feciy3_pop, "mm_feciy3_pop", "HC3")

list_mm3=list(lsdv.mm_pool3_coef, lsdv.mm_fec3_coef, lsdv.mm_fecy3_coef, lsdv.mm_feciy3_coef, lsdv.mm_feciy3_pop_coef)

#export resultater til excel
regoutput_mm_panel <- Reduce(function(a,b){
  ans <- merge(a,b,by="row.names",all=T)
  row.names(ans) <- ans[,"Row.names"]
  ans[,!names(ans) %in% "Row.names"]
}, list_mm3)

regoutput_mm_panel$vars = row.names(regoutput_mm_panel)
write_xlsx(regoutput_mm_panel, "regoutput_mm_panel.xlsx", col_names = TRUE)


# Sector spillover -------------------------------------------------

# How to deal with NA in a panel data regression? Link: https://stackoverflow.com/questions/14427781/how-to-deal-with-na-in-a-panel-data-regression------

#Skal det vægtes? Og hvad skal vægtes?

# hvorfor bruger AS ikke year-industry fixed effects

ci_panel_ss = func_regpanel(ci_panel, "MN_4", 2)
#ci_panel_ss = na.omit(ci_panel_ss)



DK_effects = ci_panel_ss %>%  filter(country=="DK") %>% group_by(code) %>% mutate(baseyearEMP = EMP[year == 1999]) %>% select(country, year, code, branche, EMP, EMP_b, EMP_c, GO, prod_logchanges, baseyearEMP, 
                                                                                                                              dLP_BwoI_b1 , dLP_BwoI_b1_lag1 , dLP_BwoI_b1_lag2 , dLP_BwoI_b1_lag3 ,
                                                                                                                              dLP_BwoI_b2 , dLP_BwoI_b2_lag1 , dLP_BwoI_b2_lag2 , dLP_BwoI_b2_lag3 ,
                                                                                                                              dLP_BwoI_b3 , dLP_BwoI_b3_lag1 , dLP_BwoI_b3_lag2 , dLP_BwoI_b3_lag3 ,
                                                                                                                              dLP_BwoI_b4 , dLP_BwoI_b4_lag1 , dLP_BwoI_b4_lag2 , dLP_BwoI_b4_lag3)

#Predicted cumulative percentage employment change from own-industry productivity growth} originating in five sectors

#The percentage annual employment change from the internal effect is given by the annual productivity growth in each industry multiplied by its sector-specific coefficient 
#(denoted by the indicator function 1(i ∈ s) for the corresponding sector). This annual percentage change is applied to base-year employment levels, where 1992, close to the 
#midpoint of the sample period, serves as the base year.

DK_inteff = DK_effects %>% mutate(emp_change =  ifelse(year==1999,0, 
                                                       ifelse(branche=="b1", prod_logchanges * (exp(-0.204)-1),
                                                              ifelse(branche=="b2", prod_logchanges * (exp(-0.264)-1),
                                                                     ifelse(branche=="b3", prod_logchanges * (exp(-0.416)-1),
                                                                            ifelse(branche=="b4", prod_logchanges * (exp(-0.422)-1), NA)))))) #skal lige modificeres en smule --> coefficienter og base year


DK_inteff = DK_inteff %>% group_by(code) %>%  mutate(cumsum_EMP = cumsum(emp_change))
DK_inteff = DK_inteff %>% mutate(emp_basechange = baseyearEMP*(cumsum_EMP/100))

DK_inteff = DK_inteff %>% group_by(branche, year) %>% summarise(sumEMPchange=sum(emp_basechange), EMP_c_base = 1674)
DK_inteff = as.data.frame(DK_inteff) %>% mutate(emp_basechange_pct = (sumEMPchange/EMP_c_base)*100)

DK_inteff_all = DK_inteff %>% group_by(year) %>% summarise(sumEMPchange=sum(sumEMPchange),  EMP_c_base = 1674)
DK_inteff_all = DK_inteff_all %>% mutate(emp_basechange_pct = (sumEMPchange/EMP_c_base)*100, branche="all")
DK_inteff_all = DK_inteff_all %>% select(branche, year, sumEMPchange, EMP_c_base, emp_basechange_pct)
DK_inteff = rbind(DK_inteff,DK_inteff_all)

#Predicted cumulative percentage employment change from spillovers of productivity growth originating in five sectors

#Meanwhile, the percentage annual employment change resulting from the external productivity effect is given by the sum of productivity change in each sector s in the current 
#and past three years–leaving out the industry’s own productivity growth– multiplied by the respective sector-specific coefficients and their lags. This quantity is
#in turn multiplied by total country-level employment in the base year , since these external effects operate on the entire economy. 


DK_exeff = DK_effects %>% mutate(emp_change = ifelse(year==1999,0, (dLP_BwoI_b1 * (exp(0.027)-1)) + (dLP_BwoI_b1_lag1 * (exp(0.054)-1)) + (dLP_BwoI_b1_lag2 * (exp(0.049)-1)) + (dLP_BwoI_b1_lag3 * (exp(0.040)-1)) + 
                                                       (dLP_BwoI_b2 * (exp(-0.056)-1)) + (dLP_BwoI_b2_lag1 * (exp(-0.032)-1)) + (dLP_BwoI_b2_lag2 * (exp(-0.063)-1)) + (dLP_BwoI_b2_lag3 * (exp(0.015)-1)) + 
                                                       (dLP_BwoI_b3 * (exp(0.079)-1))  + (dLP_BwoI_b3_lag1 * (exp(0.049)-1)) + (dLP_BwoI_b3_lag2 * (exp(-0.031)-1)) + (dLP_BwoI_b3_lag3 * (exp(-0.025)-1)) + 
                                                       (dLP_BwoI_b4 * (exp(0.130)-1)) + (dLP_BwoI_b4_lag1 * (exp(0.158)-1)) + (dLP_BwoI_b4_lag2 * (exp(0.097)-1)) + (dLP_BwoI_b4_lag3 * (exp(-0.027)-1))))

DK_exeff = DK_effects %>% mutate(emp_change_b1 = ifelse(year==1999,0, (dLP_BwoI_b1 * (exp(0.027)-1)) + (dLP_BwoI_b1_lag1 * (exp(0.054)-1)) + (dLP_BwoI_b1_lag2 * (exp(0.049)-1)) + (dLP_BwoI_b1_lag3 * (exp(0.040)-1))),
                                 emp_change_b2 = ifelse(year==1999,0, (dLP_BwoI_b2 * (exp(-0.056)-1)) + (dLP_BwoI_b2_lag1 * (exp(-0.032)-1)) + (dLP_BwoI_b2_lag2 * (exp(-0.063)-1)) + (dLP_BwoI_b2_lag3 * (exp(0.015)-1))),
                                 emp_change_b3 = ifelse(year==1999,0, (dLP_BwoI_b3 * (exp(0.079)-1)) + (dLP_BwoI_b3_lag1 * (exp(0.049)-1)) + (dLP_BwoI_b3_lag2 * (exp(-0.031)-1)) + (dLP_BwoI_b3_lag3 * (exp(-0.025)-1)) ),
                                 emp_change_b4 = ifelse(year==1999,0, (dLP_BwoI_b4 * (exp(0.130)-1)) + (dLP_BwoI_b4_lag1 * (exp(0.158)-1)) + (dLP_BwoI_b4_lag2 * (exp(0.097)-1)) + (dLP_BwoI_b4_lag3 * (exp(-0.027)-1))))


#test = DK_exeff %>% filter(year==2000)

DK_exeff = DK_exeff %>% group_by(code) %>%  mutate(cumsum_EMP_b1 = cumsum(emp_change_b1), 
                                                   cumsum_EMP_b2 = cumsum(emp_change_b2), 
                                                   cumsum_EMP_b3 = cumsum(emp_change_b3), 
                                                   cumsum_EMP_b4 = cumsum(emp_change_b4), EMP_c_base = 1674, )

DK_exeff = DK_exeff %>% mutate(emp_basechange_b1 = baseyearEMP * (cumsum_EMP_b1/100),
                               emp_basechange_b2 = baseyearEMP * (cumsum_EMP_b2/100), 
                               emp_basechange_b3 = baseyearEMP * (cumsum_EMP_b3/100), 
                               emp_basechange_b4 = baseyearEMP * (cumsum_EMP_b4/100))

DK_exeff = DK_exeff %>% group_by(year) %>% summarise(sumEMPchange_b1=sum(emp_basechange_b1),
                                                     sumEMPchange_b2=sum(emp_basechange_b2),
                                                     sumEMPchange_b3=sum(emp_basechange_b3),
                                                     sumEMPchange_b4=sum(emp_basechange_b4),
                                                     sumEMPchange_all=sum(emp_basechange_b1 + emp_basechange_b2 + emp_basechange_b3 + emp_basechange_b4), EMP_c_base = 1674)

test = as.data.frame(DK_exeff) %>% mutate(emp_basechange_pct_1 = (sumEMPchange_b1/EMP_c_base)*100,
                                          emp_basechange_pct_2 = (sumEMPchange_b2/EMP_c_base)*100,
                                          emp_basechange_pct_3 = (sumEMPchange_b3/EMP_c_base)*100,
                                          emp_basechange_pct_4 = (sumEMPchange_b4/EMP_c_base)*100,
                                          emp_basechange_pct_all = (sumEMPchange_all/EMP_c_base)*100)





DK_exeff = DK_exeff %>% group_by(code) %>%  mutate(cumsum_EMP = cumsum(emp_change))
DK_exeff = DK_exeff %>% mutate(emp_basechange = baseyearEMP *(cumsum_EMP/100))

DK_exeff = DK_exeff %>% group_by(branche, year) %>% summarise(sumEMPchange=sum(emp_basechange), EMP_c_base= 1674)
DK_exeff = as.data.frame(DK_exeff) %>% mutate(emp_basechange_pct = (sumEMPchange/EMP_c_base)*100)

DK_exeff_all = DK_exeff %>% group_by(year) %>% summarise(sumEMPchange=sum(sumEMPchange), EMP_c_base = 1674)
DK_exeff_all = DK_exeff_all %>% mutate(emp_basechange_pct = (sumEMPchange/EMP_c_base)*100, branche="all")
DK_exeff_all = DK_exeff_all %>% select(branche, year, sumEMPchange, EMP_c_base, emp_basechange_pct)
DK_exeff = rbind(DK_exeff,DK_exeff_all)




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

#med vægte og lags
{
  lsdv.ss_pool3 = {lm(emp_logchanges ~ dLP_I_b1 + dLP_I_b1_lag1 + dLP_I_b1_lag2 + dLP_I_b1_lag3 +
                        dLP_I_b2 + dLP_I_b2_lag1 + dLP_I_b2_lag2 + dLP_I_b2_lag3 +
                        dLP_I_b3 + dLP_I_b3_lag1 + dLP_I_b3_lag2 + dLP_I_b3_lag3 +
                        dLP_I_b4 + dLP_I_b4_lag1 + dLP_I_b4_lag2 + dLP_I_b4_lag3 +
                        dLP_BwoI_b1 + dLP_BwoI_b1_lag1 + dLP_BwoI_b1_lag2 + dLP_BwoI_b1_lag3 +
                        dLP_BwoI_b2 + dLP_BwoI_b2_lag1 + dLP_BwoI_b2_lag2 + dLP_BwoI_b2_lag3 +
                        dLP_BwoI_b3 + dLP_BwoI_b3_lag1 + dLP_BwoI_b3_lag2 + dLP_BwoI_b3_lag3 +
                        dLP_BwoI_b4 + dLP_BwoI_b4_lag1 + dLP_BwoI_b4_lag2 + dLP_BwoI_b4_lag3, 
                      data=ci_panel_ss, weights = wgt_i_avg)} #dLP_I_b5 + dLP_BwoI_b5 + dLP_BwoI_b5_lag1 + dLP_BwoI_b5_lag2 + dLP_BwoI_b5_lag3
  
  lsdv.ss_fec3 = {lm(emp_logchanges ~ dLP_I_b1 + dLP_I_b1_lag1 + dLP_I_b1_lag2 + dLP_I_b1_lag3 +
                       dLP_I_b2 + dLP_I_b2_lag1 + dLP_I_b2_lag2 + dLP_I_b2_lag3 +
                       dLP_I_b3 + dLP_I_b3_lag1 + dLP_I_b3_lag2 + dLP_I_b3_lag3 +
                       dLP_I_b4 + dLP_I_b4_lag1 + dLP_I_b4_lag2 + dLP_I_b4_lag3 +
                       dLP_BwoI_b1 + dLP_BwoI_b1_lag1 + dLP_BwoI_b1_lag2 + dLP_BwoI_b1_lag3 +
                       dLP_BwoI_b2 + dLP_BwoI_b2_lag1 + dLP_BwoI_b2_lag2 + dLP_BwoI_b2_lag3 +
                       dLP_BwoI_b3 + dLP_BwoI_b3_lag1 + dLP_BwoI_b3_lag2 + dLP_BwoI_b3_lag3 +
                       dLP_BwoI_b4 + dLP_BwoI_b4_lag1 + dLP_BwoI_b4_lag2 + dLP_BwoI_b4_lag3 +
                       factor(country), data=ci_panel_ss, weights = wgt_i_avg)} #dLP_I_b5 + dLP_BwoI_b5 + dLP_BwoI_b5_lag1 + dLP_BwoI_b5_lag2 + dLP_BwoI_b5_lag3 + 
  
  lsdv.ss_fecy3 = {lm(emp_logchanges ~ dLP_I_b1 + dLP_I_b1_lag1 + dLP_I_b1_lag2 + dLP_I_b1_lag3 +
                        dLP_I_b2 + dLP_I_b2_lag1 + dLP_I_b2_lag2 + dLP_I_b2_lag3 +
                        dLP_I_b3 + dLP_I_b3_lag1 + dLP_I_b3_lag2 + dLP_I_b3_lag3 +
                        dLP_I_b4 + dLP_I_b4_lag1 + dLP_I_b4_lag2 + dLP_I_b4_lag3 +
                        dLP_BwoI_b1 + dLP_BwoI_b1_lag1 + dLP_BwoI_b1_lag2 + dLP_BwoI_b1_lag3 +
                        dLP_BwoI_b2 + dLP_BwoI_b2_lag1 + dLP_BwoI_b2_lag2 + dLP_BwoI_b2_lag3 +
                        dLP_BwoI_b3 + dLP_BwoI_b3_lag1 + dLP_BwoI_b3_lag2 + dLP_BwoI_b3_lag3 +
                        dLP_BwoI_b4 + dLP_BwoI_b4_lag1 + dLP_BwoI_b4_lag2 + dLP_BwoI_b4_lag3 +
                        factor(country) + factor(year), data=ci_panel_ss, weights = wgt_i_avg)} #dLP_I_b5 + dLP_BwoI_b5 + dLP_BwoI_b5_lag1 + dLP_BwoI_b5_lag2 + dLP_BwoI_b5_lag3 + 
  
  lsdv.ss_feciy3 = {lm(emp_logchanges ~ dLP_I_b1 + dLP_I_b1_lag1 + dLP_I_b1_lag2 + dLP_I_b1_lag3 +
                         dLP_I_b2 + dLP_I_b2_lag1 + dLP_I_b2_lag2 + dLP_I_b2_lag3 +
                         dLP_I_b3 + dLP_I_b3_lag1 + dLP_I_b3_lag2 + dLP_I_b3_lag3 +
                         dLP_I_b4 + dLP_I_b4_lag1 + dLP_I_b4_lag2 + dLP_I_b4_lag3 +
                         dLP_BwoI_b1 + dLP_BwoI_b1_lag1 + dLP_BwoI_b1_lag2 + dLP_BwoI_b1_lag3 +
                         dLP_BwoI_b2 + dLP_BwoI_b2_lag1 + dLP_BwoI_b2_lag2 + dLP_BwoI_b2_lag3 +
                         dLP_BwoI_b3 + dLP_BwoI_b3_lag1 + dLP_BwoI_b3_lag2 + dLP_BwoI_b3_lag3 +
                         dLP_BwoI_b4 + dLP_BwoI_b4_lag1 + dLP_BwoI_b4_lag2 + dLP_BwoI_b4_lag3 +
                         factor(country) + factor(year) + factor(code), data=ci_panel_ss, weights = wgt_i_avg)} #dLP_I_b5 + dLP_BwoI_b5 + dLP_BwoI_b5_lag1 + dLP_BwoI_b5_lag2 + dLP_BwoI_b5_lag3 +
  
  lsdv.ss_feciy3_pop = {lm(emp_logchanges ~ dLP_I_b1 + dLP_I_b1_lag1 + dLP_I_b1_lag2 + dLP_I_b1_lag3 +
                             dLP_I_b2 + dLP_I_b2_lag1 + dLP_I_b2_lag2 + dLP_I_b2_lag3 +
                             dLP_I_b3 + dLP_I_b3_lag1 + dLP_I_b3_lag2 + dLP_I_b3_lag3 +
                             dLP_I_b4 + dLP_I_b4_lag1 + dLP_I_b4_lag2 + dLP_I_b4_lag3 +
                             dLP_BwoI_b1 + dLP_BwoI_b1_lag1 + dLP_BwoI_b1_lag2 + dLP_BwoI_b1_lag3 +
                             dLP_BwoI_b2 + dLP_BwoI_b2_lag1 + dLP_BwoI_b2_lag2 + dLP_BwoI_b2_lag3 +
                             dLP_BwoI_b3 + dLP_BwoI_b3_lag1 + dLP_BwoI_b3_lag2 + dLP_BwoI_b3_lag3 +
                             dLP_BwoI_b4 + dLP_BwoI_b4_lag1 + dLP_BwoI_b4_lag2 + dLP_BwoI_b4_lag3 + wkgpop_logchanges +
                             factor(country) + factor(year) + factor(code), data=ci_panel_ss, weights = wgt_i_avg)} #dLP_I_b5 + dLP_BwoI_b5 + dLP_BwoI_b5_lag1 + dLP_BwoI_b5_lag2 + dLP_BwoI_b5_lag3 +
}



#Robuste standard fejl
lsdv.ss_pool1_coef = func_coefs(lsdv.ss_pool1, "ss_pool1", "HC3") 
lsdv.ss_fec1_coef = func_coefs(lsdv.ss_fec1, "ss_fec1", "HC3")
lsdv.ss_fecy1_coef = func_coefs(lsdv.ss_fecy1, "ss_fecy1", "HC3")
lsdv.ss_feciy1_coef = func_coefs(lsdv.ss_feciy1, "ss_feciy1", "HC3")

list_ss1=list(lsdv.ss_pool1_coef, lsdv.ss_fec1_coef, lsdv.ss_fecy1_coef, lsdv.ss_feciy1_coef)

lsdv.ss_pool2_coef = func_coefs(lsdv.ss_pool2, "ss_pool2", "HC3") 
lsdv.ss_fec2_coef = func_coefs(lsdv.ss_fec2, "ss_fec2", "HC3")
lsdv.ss_fecy2_coef = func_coefs(lsdv.ss_fecy2, "ss_fecy2", "HC3")
lsdv.ss_feciy2_coef = func_coefs(lsdv.ss_feciy2, "ss_feciy2", "HC3")
lsdv.ss_feciy2_pop_coef = func_coefs(lsdv.ss_feciy2_pop, "ss_feciy2_pop", "HC3")

list_ss2=list(lsdv.ss_pool2_coef, lsdv.ss_fec2_coef, lsdv.ss_fecy2_coef, lsdv.ss_feciy2_coef, lsdv.ss_feciy2_pop_coef)

lsdv.ss_pool3_coef = func_coefs(lsdv.ss_pool3, "ss_pool3", "HC3") 
lsdv.ss_fec3_coef = func_coefs(lsdv.ss_fec3, "ss_fec3", "HC3")
lsdv.ss_fecy3_coef = func_coefs(lsdv.ss_fecy3, "ss_fecy3", "HC3")
lsdv.ss_feciy3_coef = func_coefs(lsdv.ss_feciy3, "ss_feciy3", "HC3")
lsdv.ss_feciy3_pop_coef = func_coefs(lsdv.ss_feciy3_pop, "ss_feciy3_pop", "HC3")

list_ss3=list(lsdv.ss_pool3_coef, lsdv.ss_fec3_coef, lsdv.ss_fecy3_coef, lsdv.ss_feciy3_coef, lsdv.ss_feciy3_pop_coef)



#export resultater til excel
regoutput_ss_panel <- Reduce(function(a,b){
  ans <- merge(a,b,by="row.names",all=T)
  row.names(ans) <- ans[,"Row.names"]
  ans[,!names(ans) %in% "Row.names"]
}, list_ss2)

regoutput_ss_panel$vars = row.names(regoutput_ss_panel)

write_xlsx(regoutput_ss_panel, "regoutput_ss_panel.xlsx", col_names = TRUE)


library(lmtest)
lsdv.ss_pool_coef = coeftest(lsdv.ss_pool, vcov. = vcovHC, type = "HC1")
lsdv.ss_feci_coef = coeftest(lsdv.ss_feci, vcov. = vcovHC, type = "HC1")
lsdv.ss_fecy_coef = coeftest(lsdv.ss_fecy, vcov. = vcovHC, type = "HC1")
lsdv.ss_feyi_coef = coeftest(lsdv.ss_feyi, vcov. = vcovHC, type = "HC1")
lsdv.ss_fecyi_coef = coeftest(lsdv.ss_feyi, vcov. = vcovHC, type = "HC1")
#coeftest(fixed.dum, vcov. = vcovHC, method = "arellano")

write.csv(cbind(lsdv.ss_pool_coef, lsdv.ss_feci_coef, lsdv.ss_fecy_coef, lsdv.ss_feyi_coef), "fixeddum_ci_panel.csv")

# Skills..... --------------------------------------------------


