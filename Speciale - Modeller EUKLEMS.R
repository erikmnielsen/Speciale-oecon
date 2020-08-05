#PRODUKTIVITET OG BESKÆFTIGELSE - EU KLEMS DATA

{
  #EU KLEMS is an industry level panel dataset covering OECD countries since 1970,
  #it contains detailed data for 32 industries in both the market and non-market economy
  
  #Methods used in the artcile "Robocalypse now?":
  
  #-They Focus on non-farm employment, and omit the poorly measured Private household sector, and Public administration, 
  # Defense and Extraterritorial organizations, which are almost entirely non-market sectors.
  
  #They operationalize the measurement of EMPLOYMENT and PRODUCTIVITY as follows. 
  #The primary EMPLOYMENT measure is the number of persons engaged in work, though we have also experimented with excluding the self-employed and obtain similar results.
  #The primary LABOR PRODUCTIVITY measure is real gross output per worker, because measurement of value-added outside of manufacturing is typically somewhat speculative 
  #- They also present a set of models using value-added per worker and value added based total factor productivity. 
  #- These alternative measures yield qualitatively similar findings, although total factor productivity growth seems to have the most strongly positive effect on employment. 
  
  
}
# Libraries and functions ---------------------------------------------------------------

library(readr)
library(readxl)
library(reshape2)
library(fpp2)
library(tidyverse)
library(xts)
library(plm)
library(ggplot2)
library(ggthemes)
library(dplyr)


func_coefs <- function(regression, name, method="") {
  
  options(scipen=999, digits=4) 
  #options(scipen=0, digits=7) #default
  
  if (method=="HC1") {
    
    siglvl = stars.pval(coeftest(regression, vcov. = vcovHC, type="HC1")[,4])
    reg_coef = cbind(coeftest(regression, vcov. = vcovHC, type="HC1")[,c(1,4)], siglvl)
    #regc5_coef = summary(regression)$coefficients[,c(1,4)]
    colnames(reg_coef) <- paste(name, colnames(reg_coef), sep = "_")
    
    
  } else if (method=="HC0") {
    
    siglvl = stars.pval(coeftest(regression, vcov. = vcovHC, type="HC0")[,4])
    reg_coef = cbind(coeftest(regression, vcov. = vcovHC)[,c(1,4)], siglvl)
    #reg_coef = summary(regression)$coefficients[,c(1,4)]
    colnames(reg_coef) <- paste(name, colnames(reg_coef), sep = "_")
    
    
  } else if (method=="HC2") {
    
    siglvl = stars.pval(coeftest(regression, vcov. = vcovHC, type="HC2")[,4])
    reg_coef = cbind(coeftest(regression, vcov. = vcovHC)[,c(1,4)], siglvl)
    #reg_coef = summary(regression)$coefficients[,c(1,4)]
    colnames(reg_coef) <- paste(name, colnames(reg_coef), sep = "_")
    
    
  } else if (method=="HC3") {
    
    siglvl = stars.pval(coeftest(regression, vcov. = vcovHC, type="HC3")[,4])
    reg_coef = cbind(coeftest(regression, vcov. = vcovHC)[,c(1,4)], siglvl)
    #reg_coef = summary(regression)$coefficients[,c(1,4)]
    colnames(reg_coef) <- paste(name, colnames(reg_coef), sep = "_")
    
    
  } else if (method=="HC4") {
    
    siglvl = stars.pval(coeftest(regression, vcov. = vcovHC, type="HC4")[,4])
    reg_coef = cbind(coeftest(regression, vcov. = vcovHC)[,c(1,4)], siglvl)
    #reg_coef = summary(regression)$coefficients[,c(1,4)]
    colnames(reg_coef) <- paste(name, colnames(reg_coef), sep = "_")
    
    
  } else {
    
    siglvl = stars.pval(coeftest(regression, vcov. = vcovHC)[,4])
    reg_coef = cbind(coeftest(regression, vcov. = vcovHC)[,c(1,4)], siglvl)
    #reg_coef = summary(regression)$coefficients[,c(1,4)]
    colnames(reg_coef) <- paste(name, colnames(reg_coef), sep = "_")
  }
  
  reg_coef
  
}

func_empprod <- function(dataset_1, dataset_2, country, measure_1="EMP", measure_2="GO", method) {
  
  colnames(dataset_1) <- gsub(measure_1, "", colnames(dataset_1))
  colnames(dataset_2) <- gsub(measure_2, "", colnames(dataset_2))
  
  dataset_1<- melt(dataset_1,
                   # ID variables - all the variables to keep but not split apart on
                   id.vars=c("desc", "code"),
                   # The source columns (not necessary here) # measure.vars=c("1970","1971",...),
                   # Name of the destination column that will identify the original column that the measurement came from
                   variable.name="year",
                   value.name= measure_1)
  
  dataset_2 <- melt(dataset_2,
                    id.vars=c("desc", "code"),
                    variable.name="year",
                    value.name= "GO")
  
  data = merge(dataset_1, dataset_2, by=c("desc","code", "year"), all.x = TRUE)
  data <- na.omit(data)
  
  #sapply(data, class)
  data$year <-  as.numeric(as.character(data[,"year"])) #ændres fordi "year" er en factor variabel
  data$GO <-  as.numeric(as.character(data[,"GO"]))
  data$code =gsub("-", "t", data[,2])
  data$country = country
  
  
  if (method=="AS") {
    #AutorSalomons Industrier:
    
    data$sel_industries <-factor(ifelse( data$code %in% c("TOT", "MARKT", "A","C","G","H","J","OtU","O","RtS","T","U"), 0,1))
    
    
    data$branche <- ifelse(data$code %in% c("B", "DtE", "F"), "b1",
                           ifelse(data$code %in% c("10t12", "13t15", "16t18", "19", "20t21", "22t23","24t25", "26t27", "28", "29t30","31t33"), "b2", #kan man ikke bare bruge C, Total Manufacturing?
                                  ifelse(data$code %in% c("P","Q","R", "S"), "b3",
                                         ifelse(data$code %in% c("53", "58t60", "61", "62t63", "K", "MtN"), "b4",
                                                ifelse(data$code %in% c("45", "46", "47", "49t52", "I", "L"), "b5", 
                                                       "b0")))))
    
    data$branche_desc <- ifelse(data$branche=="b1","Mining, utilities, and construction", 
                                ifelse(data$branche=="b2","Manufacturing", 
                                       ifelse(data$branche=="b3","Education and health services", 
                                              ifelse(data$branche=="b4","High-tech services",
                                                     ifelse(data$branche=="b5","Low-tech services",
                                                            "Not relevant"
                                                     )))))
    
  } else {
    
    #Brancher, 10:
    
    data$sel_industries <-factor(ifelse( data$code %in% c("A","B", "DtE", "F","10t12", "13t15", "16t18", "19", "20t21", "22t23","24t25", "26t27", "28", "29t30","31t33",
                                                          "53","58t60", "61", "62t63", "K", "MtN","45", "46", "47", "49t52", "I", "L"), 1,0)) #alt pånær O, P, Q (skal RtS, T og U også fjernes?) 
    
    
    data$branche <- ifelse(data$code=="A", "b1",
                           ifelse(data$code %in% c("B","10t12", "13t15", "16t18", "19", "20t21", "22t23","24t25", "26t27", "28", "29t30","31t33", "DtE"), "b2",
                                  ifelse(data$code=="F", "b3",
                                         ifelse(data$code %in% c("45", "46", "47","49t52","53", "I"), "b4",
                                                ifelse(data$code %in% c("58t60", "61", "62t63"), "b5",
                                                       ifelse(data$code=="K", "b6",
                                                              ifelse(data$code=="L", "b7",
                                                                     ifelse(data$code=="MtN", "b8",
                                                                            #  ifelse(data$code %in% c("O","P","Q"), "b9",
                                                                            #ifelse(data$code %in% c("R","S","T","U"), "b10",
                                                                            "b0"))))))))
    
    data$branche_desc <- ifelse(data$branche=="b1","Landbrug, skovbrug og fiskeri",
                                ifelse(data$branche=="b2","Industri, råstofindvinding og forsyningsvirksomhed",
                                       ifelse(data$branche=="b3","Bygge og anlæg", 
                                              ifelse(data$branche=="b4","Handel og transport mv.", 
                                                     ifelse(data$branche=="b5","Information og kommunikation",
                                                            ifelse(data$branche=="b6", "Finansiering og forsikring",
                                                                   ifelse(data$branche=="b7","Ejendomshandel og udlejning",
                                                                          ifelse(data$branche=="b8","Erhvervsservice",
                                                                                 #           ifelse(data$branche=="b9","Offentlig administration, undervisning og sundhed",
                                                                                 #     ifelse(data$branche=="b10","Kultur, fritid og anden service",
                                                                                 "Ikke relevant"))))))))
  }
  
  
  #angivelse af branche/industri totaler
  t4 <- data %>% filter(branche!="b0") %>% group_by(year, branche, branche_desc) %>% summarize(EMP=sum(EMP),GO=sum(GO))
  data2 <- data.frame(desc= t4$branche_desc,
                      code=t4$branche,
                      year=t4$year,
                      EMP=t4$EMP,
                      GO=t4$GO,
                      country=country,
                      sel_industries=0,
                      branche="b-tot",
                      branche_desc="Branche Total")
  
  
  #udregning af lande total, hvis visse brancher udelades (fx landbrug, offentlig sektor) 
  b <- data2 %>% filter(code=="b1")
  b_2 <- data2 %>% filter(code=="b2")
  b_3 <- data2 %>% filter(code=="b3")
  b_4 <- data2 %>% filter(code=="b4")
  b_5 <- data2 %>% filter(code=="b5")
  
  if (method!="AS") {
    b_6 <- data2 %>% filter(code=="b6")
    b_7 <- data2 %>% filter(code=="b7")
    b_8 <- data2 %>% filter(code=="b8")
    #b_9 <- data2 %>% filter(code=="b9")
    #b_10 <- data2 %>% filter(code=="b10")
    
    b$EMP = b$EMP + b_2$EMP + b_3$EMP + b_4$EMP + b_5$EMP + b_6$EMP + b_7$EMP + b_8$EMP #+ b_10$EMP + b_9$EMP 
    b$GO = b$GO + b_2$GO + b_3$GO + b_4$GO + b_5$GO + b_6$GO + b_7$GO + b_8$GO #+ b_10$GO + b_9$GO 
    b$desc = "TOTAL INDUSTRIES-MunkNielsen"
    b$code = "TOT_MN"
    b$branche = "TOT"
    
  } else {
    
    b$EMP = b$EMP + b_2$EMP + b_3$EMP + b_4$EMP + b_5$EMP
    b$GO = b$GO + b_2$GO + b_3$GO + b_4$GO + b_5$GO
    b$desc = "TOTAL INDUSTRIES-AutorSalomons"
    b$code = "TOT_AS"
    b$branche = "TOT" #lettere at de begge hedder "TOT" i brancher når der skal filtreres
  }
  
  b$branche_desc = "Lande Total"
  

  #Kodning af variable:
  
  data_fin <- rbind(data, b, data2)
  
  pdata = pdata.frame(data_fin, index = c("code", "year"))
  pdata$emp_logchanges = diff(log(pdata$EMP), lag = 1, shift = "time")*100
  pdata$prod_logchanges = diff(log(pdata$GO/pdata$EMP), lag = 1, shift = "time")*100
  
  pdata <- pdata %>% select(year, country, code, desc, sel_industries, branche, branche_desc, EMP, emp_logchanges, GO, prod_logchanges) %>% filter(code!="b0")
  #pdata = pdata.frame(pdata, index = c("code", "year"))
  
  pdata
  
  
}

#dataset_1 = DK_ep

func_regpanel <- function(dataset_1, type) {
  
  if (type==1) {
    
    tot = dataset_1 %>% filter(branche=="TOT")
    tot$EMP_tot = tot$EMP
    tot$GO_tot = tot$GO
    tot <- tot %>% select(year, EMP_tot, GO_tot)
    
    ind = dataset_1 %>% filter(sel_industries==1)
    
    b <- dataset_1 %>% filter(branche=="b-tot")
    b$branche = b$code
    #b$prod_logchanges_b = b$prod_logchanges
    b$EMP_b = b$EMP
    b$GO_b = b$GO
    
    b = b %>% select(year, branche, EMP_b, GO_b)
    ind = merge(ind, b, by=c("year", "branche"), all.x = TRUE) 
    
    b1 = b %>% filter(branche=="b1") %>% mutate(EMP_b1=EMP_b) %>% mutate(GO_b1=GO_b) %>% select(year, EMP_b1,GO_b1)
    b2 = b %>% filter(branche=="b2") %>% mutate(EMP_b2=EMP_b) %>% mutate(GO_b2=GO_b) %>% select(EMP_b2, GO_b2)
    b3 = b %>% filter(branche=="b3") %>% mutate(EMP_b3=EMP_b) %>% mutate(GO_b3=GO_b) %>% select(EMP_b3, GO_b3)
    b4 = b %>% filter(branche=="b4") %>% mutate(EMP_b4=EMP_b) %>% mutate(GO_b4=GO_b) %>% select(EMP_b4, GO_b4)
    b5 = b %>% filter(branche=="b5") %>% mutate(EMP_b5=EMP_b) %>% mutate(GO_b5=GO_b) %>% select(EMP_b5, GO_b5)
    
    test = b %>% count(branche) %>% nrow
    
    if (test==8) {
      
      #b6 = b %>% filter(branche=="b6") %>% mutate(prod_logchanges_b6=prod_logchanges_b) %>% select(prod_logchanges_b6)
      #b7 = b %>% filter(branche=="b7") %>% mutate(prod_logchanges_b7=prod_logchanges_b) %>% select(prod_logchanges_b7)
      #b8 = b %>% filter(branche=="b8") %>% mutate(prod_logchanges_b8=prod_logchanges_b) %>% select(prod_logchanges_b8)
      b = cbind(b1,b2,b3,b4,b5,b6,b7,b8)
      
    } else {
      
      b = cbind(b1,b2,b3,b4,b5)
    }
    
    ind = merge(ind, b, by=c("year"), all.x = TRUE)
    ind = merge(ind, tot, by=c("year"), all.x = TRUE)
    
    ind$wgt_i = ind$EMP/ind$EMP_tot
    ind$wgt_b = ind$EMP_b/ind$EMP_tot
    
    #gennemsnit på tværs af år for den enkelte industri
    test = ind %>% group_by(country, code) %>% summarize(EMP_test=sum(EMP))
    test_2 = ind %>% group_by(country) %>% summarize(EMP_test_2=sum(EMP))
    test = merge(test, test_2, by=c("country"), all.x = TRUE)
    test$wgt_i_avg = test$EMP_test/test$EMP_test_2
    test = test %>% select(code, wgt_i_avg)
    ind = merge(ind, test, by=c("code"), all.x = TRUE)
    

    #ind$prod_logchanges_wgt_lag1 = lag(ind$prod_logchanges_wgt, k = 1, shift = "time")
    #ind$prod_logchanges_wgt_lag2 = lag(ind$prod_logchanges_wgt_lag1, k = 1, shift = "time")
    #ind$prod_logchanges_wgt_lag3 = lag(ind$prod_logchanges_wgt_lag2, k = 1, shift = "time")
    
    ind = pdata.frame(ind, index = c("code", "year"))
    
    ind$prod_logchanges_lag1 = lag(ind$prod_logchanges, k = 1, shift = "time")
    ind$prod_logchanges_lag2 = lag(ind$prod_logchanges, k = 2, shift = "time")
    ind$prod_logchanges_lag3 = lag(ind$prod_logchanges, k = 3, shift = "time")
    
    #ind$prod_logchanges_lag1 = lag(ind$prod_logchanges, k = 1, shift = "time")
    #ind$prod_logchanges_lag2 = lag(ind$prod_logchanges_lag1, k = 1, shift = "time")
    #ind$prod_logchanges_lag3 = lag(ind$prod_logchanges_lag2, k = 1, shift = "time")
    
    #Beta2 variable og lags, mikro + makro
    ind$dLP_CwoI = diff(log((ind$GO_tot-ind$GO)/(ind$EMP_tot-ind$EMP)), lag = 1, shift = "time")*100
    ind$dLP_CwoI_lag1 = lag(ind$dLP_CwoI, k = 1, shift = "time")
    ind$dLP_CwoI_lag2 = lag(ind$dLP_CwoI, k = 2, shift = "time")
    ind$dLP_CwoI_lag3 = lag(ind$dLP_CwoI, k = 3, shift = "time")
    
    #Beta2 variable og lags, sektor spillover - obs hvis faste priser kan totaler fra euklems ikke bruges
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
    
    #beta1 variable, sectoral spillover:
    
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
    
    ind
    
  } else if (type==3) {
    
    #tot = dataset_1 %>% filter(code=="TOT")
    tot = dataset_1 %>% filter(branche=="TOT")
    #tot$year = lubridate::ymd(tot$year, truncated = 2L)
    tot
    
    
  } else {
    
    NA
  }
  
  
}


#Indlæs filer -----

#faste: DK, (US), DE, NL, (SE), AT, CZ, FI, FR, EL, (IT), LV, (SK), (SI)
#faste komplet: DK, DE, NL, AT, CZ, FI, FR, EL, LV

#løbende: DK, US, UK, DE, NL, (SE), AT, BE, CY, CZ, ES, FI, FR, (EE), EL, HU, IE, IT, LT, LV, PL, PT, (SK), (SI)
#løbende komplet: DK, UK, DE, NL, AT, BE, CY, CZ, ES, FI, FR, EL, HU, IE, IT, LT, LV, PL, PT, SI

DK_emp <- read_excel("Data/DK_output_17ii.xlsx", sheet = "EMP") #Number of persons engaged (thousands)
DK_go <- read_excel("Data/DK_output_17ii.xlsx", sheet = "GO") #Gross Output at current basic prices (in millions of national currency)
DK_gop <- read_excel("Data/DK_output_17ii.xlsx", sheet = "GO_QI")  #Gross output, price indices, 2010 = 100 #komplet 1975-2015

US_emp <- read_excel("Data/US_output_17ii.xlsx", sheet = "EMP") #komplet 2000-2015
US_gop <- read_excel("Data/US_output_17ii.xlsx", sheet = "GO_QI") #mangler 61 og T, ellers komplet fra 1970-2015
US_go <- read_excel("Data/US_output_17ii.xlsx", sheet = "GO") #mangler 45, 61 og T, ellers komplet fra 1970-2015

UK_emp <- read_excel("Data/UK_output_17ii.xlsx", sheet = "EMP") #komplet 1995-2014
#UK_gop <- read_excel("Data/UK_output_17ii.xlsx", sheet = "GO_QI") #findes ikke
UK_go <- read_excel("Data/UK_output_17ii.xlsx", sheet = "GO") #komplet 1995-2014

DE_emp <- read_excel("Data/DE_output_17ii.xlsx", sheet = "EMP") #komplet 1995-2015
DE_gop <- read_excel("Data/DE_output_17ii.xlsx", sheet = "GO_QI") #komplet 1995-2015
DE_go <- read_excel("Data/DE_output_17ii.xlsx", sheet = "GO") #komplet 1995-2015

NL_emp <- read_excel("Data/NL_output_17ii.xlsx", sheet = "EMP") #komplet 1995-2015
NL_gop <- read_excel("Data/NL_output_17ii.xlsx", sheet = "GO_QI") #komplet 1995-2015
NL_go <- read_excel("Data/NL_output_17ii.xlsx", sheet = "GO") #komplet 1995-2015

SE_emp <- read_excel("Data/SE_output_17ii.xlsx", sheet = "EMP") #næsten komplet 1993-2014
SE_gop <- read_excel("Data/SE_output_17ii.xlsx", sheet = "GO_QI") #mangler 20-21, 49-52 og 53, ellers komplet 1993-2014
SE_go <- read_excel("Data/SE_output_17ii.xlsx", sheet = "GO") #mangler 49-52 og 53, ellers komplet 1993-2014

AT_emp = read_excel("Data/AT_output_17ii.xlsx", sheet = "EMP") #komplet 1995-2015
AT_gop = read_excel("Data/AT_output_17ii.xlsx", sheet = "GO_QI") #komplet 1995-2015
AT_go= read_excel("Data/AT_output_17ii.xlsx", sheet = "GO") #komplet 1995-2015

BE_emp = read_excel("Data/BE_output_17ii.xlsx", sheet = "EMP") #komplet 1995-2015
#BE_gop = read_excel("Data/BE_output_17ii.xlsx", sheet = "GO_QI") #ikke angivet korrekt
BE_go = read_excel("Data/BE_output_17ii.xlsx", sheet = "GO") #komplet 1995-2015

CY_emp = read_excel("Data/CY_output_17ii.xlsx", sheet = "EMP") #komplet 1995-2015
#CY_gpo= read_excel("Data/CY_output_17ii.xlsx", sheet = "GO_QI") #GO_QI ikke mulig
CY_go= read_excel("Data/CY_output_17ii.xlsx", sheet = "GO") #komplet 1995-2015

CZ_emp = read_excel("Data/CZ_output_17ii.xlsx", sheet = "EMP") #komplet 1995-2015
CZ_gop = read_excel("Data/CZ_output_17ii.xlsx", sheet = "GO_QI") #komplet 1995-2014
CZ_go = read_excel("Data/CZ_output_17ii.xlsx", sheet = "GO") #komplet 1995-2015

ES_emp = read_excel("Data/ES_output_17ii.xlsx", sheet = "EMP") #komplet 1995-2015
#ES_gop= read_excel("Data/ES_output_17ii.xlsx", sheet = "GO_QI") #GO_QI ikke mulig
ES_go= read_excel("Data/ES_output_17ii.xlsx", sheet = "GO") #komplet 1995-2015

FI_emp = read_excel("Data/FI_output_17ii.xlsx", sheet = "EMP") #komplet 1980-2015
FI_gop = read_excel("Data/FI_output_17ii.xlsx", sheet = "GO_QI") #komplet 1980-2015
FI_go = read_excel("Data/FI_output_17ii.xlsx", sheet = "GO") #komplet 1980-2015

FR_emp = read_excel("Data/FR_output_17ii.xlsx", sheet = "EMP") #fra 1975-1989 mangler 45,46,47, 49-52,53  - komplet 1990-2015
FR_gop = read_excel("Data/FR_output_17ii.xlsx", sheet = "GO_QI") #fra 1975-1994 mangler 45,46,47, 49-52,53  - komplet 1995-2015
FR_go = read_excel("Data/FR_output_17ii.xlsx", sheet = "GO") #fra 1975-1994 mangler 45,46,47, 49-52,53  - komplet 1995-2015

EE_emp = read_excel("Data/EE_output_17ii.xlsx", sheet = "EMP") #T mangler, komplet 1995-2015
#EE_gop= read_excel("Data/EE_output_17ii.xlsx", sheet = "GO_QI") #GO_QI ikke mulig
EE_go= read_excel("Data/EE_output_17ii.xlsx", sheet = "GO") #T mangler, komplet 1995-2015

EL_emp = read_excel("Data/EL_output_17ii.xlsx", sheet = "EMP") #komplet 1995-2015
EL_gop = read_excel("Data/EL_output_17ii.xlsx", sheet = "GO_QI") #komplet 1995-2015
EL_go = read_excel("Data/EL_output_17ii.xlsx", sheet = "GO") #komplet 1995-2015

HU_emp = read_excel("Data/HU_output_17ii.xlsx", sheet = "EMP") #komplet 1995-2015
#HU_gop= read_excel("Data/HU_output_17ii.xlsx", sheet = "GO_QI") #GO_QI ikke mulig
HU_go= read_excel("Data/HU_output_17ii.xlsx", sheet = "GO") #komplet 1995-2015

IE_emp = read_excel("Data/IE_output_17ii.xlsx", sheet = "EMP") #komplet 1998-2015
#IE_gop= read_excel("Data/IE_output_17ii.xlsx", sheet = "GO_QI") #GO_QI ikke mulig
IE_go= read_excel("Data/IE_output_17ii.xlsx", sheet = "GO") #komplet 1998-2015

IT_emp = read_excel("Data/IT_output_17ii.xlsx", sheet = "EMP") #komplet 1995-2015
IT_gop = read_excel("Data/IT_output_17ii.xlsx", sheet = "GO_QI") #mangler R, S og T, ellers komplet 1995-2015
IT_go = read_excel("Data/IT_output_17ii.xlsx", sheet = "GO") #komplet 1995-2015

LT_emp = read_excel("Data/LT_output_17ii.xlsx", sheet = "EMP") #komplet 1995-2015
#LT_go= read_excel("Data/LT_output_17ii.xlsx", sheet = "GO") #GO_QI ikke mulig
LT_go= read_excel("Data/LT_output_17ii.xlsx", sheet = "GO") #komplet 1995-2015

#LU_emp = read_excel("Data/LU_output_17ii.xlsx", sheet = "EMP") 
#LU_gop = read_excel("Data/LU_output_17ii.xlsx", sheet = "GO_QI") #ikke komplet, mangler 19+20-21+26-27+28+49-52+53
#LU_go = read_excel("Data/LU_output_17ii.xlsx", sheet = "GO") #ikke komplet, mangler 19+20-21+26-27+28+49-52+53

LV_emp = read_excel("Data/LV_output_17ii.xlsx", sheet = "EMP") #komplet 2000-2015
LV_gop = read_excel("Data/LV_output_17ii.xlsx", sheet = "GO_QI") #mange huller fra 95-99, komplet 2000-2015
LV_go = read_excel("Data/LV_output_17ii.xlsx", sheet = "GO") #mange huller fra 95-99, komplet 2000-2015 

PL_emp = read_excel("Data/PL_output_17ii.xlsx", sheet = "EMP") #komplet 2000-2015
#PL_gop= read_excel("Data/PL_output_17ii.xlsx", sheet = "GO") #GO_QI ikke mulig
PL_go= read_excel("Data/PL_output_17ii.xlsx", sheet = "GO") #komplet 2000-2015

PT_emp = read_excel("Data/PT_output_17ii.xlsx", sheet = "EMP") #komplet 1995-2015
#PT_gop= read_excel("Data/PT_output_17ii.xlsx", sheet = "GO_QI")#GO_QI ikke mulig
PT_go= read_excel("Data/PT_output_17ii.xlsx", sheet = "GO") #komplet 2000-2015

SK_emp = read_excel("Data/SK_output_17ii.xlsx", sheet = "EMP") #komplet 1995-2015
SK_gop = read_excel("Data/SK_output_17ii.xlsx", sheet = "GO_QI") #mangler R, S, og T, ellers komplet 1995-2015
SK_go = read_excel("Data/SK_output_17ii.xlsx", sheet = "GO") #mangler T, ellers komplet 1995-2015

SI_emp = read_excel("Data/SI_output_17ii.xlsx", sheet = "EMP") #komplet 1995-2015
SI_gop = read_excel("Data/SI_output_17ii.xlsx", sheet = "GO_QI") #mangler R, S, og T, ellers komplet 2000-2015
SI_go = read_excel("Data/SI_output_17ii.xlsx", sheet = "GO") #komplet 1995-2015


# Country data  ----------------------------------------------------- 

#faste: DK, US, DE, NL, (SE), AT, CZ, FI, FR, EL, (IT), LV, (SK), (SI)
#faste komplet: DK, US, DE, NL, AT, CZ, FI, FR, EL, LV

#løbende: DK, US, UK, DE, NL, (SE), AT, BE, CY, CZ, ES, FI, FR, (EE), EL, HU, IE, IT, LT, LV, PL, PT, (SK), SI
#løbende komplet: DK, US, UK, DE, NL, AT, BE, CY, CZ, ES, FI, FR, EL, HU, IE, IT, LT, LV, PL, PT, SI

PRISER = "løbende"
PRISER = "faste" 
KOMPLET = FALSE
KOMPLET = TRUE

# nogle af industrierne (eller subkategorierne) findes ikke i alle lande, fx findes 45,46,47 ikke i Frankrig før 1995, selvom overkategorien G findes

#Employment and productivty

if (PRISER=="faste" & KOMPLET == TRUE) {
  DK_ep = func_empprod(DK_emp, DK_gop,"DK", "EMP", "GO_QI", "AS")
  US_ep = func_empprod(US_emp, US_gop,"US", "EMP", "GO_QI", "AS")
  DE_ep = func_empprod(DE_emp, DE_gop,"DE", "EMP", "GO_QI", "AS")
  NL_ep = func_empprod(NL_emp, NL_gop,"NL", "EMP", "GO_QI", "AS")
  AT_ep = func_empprod(AT_emp, AT_gop,"AT", "EMP", "GO_QI", "AS")
  CZ_ep = func_empprod(CZ_emp, CZ_gop,"CZ", "EMP", "GO_QI", "AS")
  FI_ep = func_empprod(FI_emp, FI_gop,"FI", "EMP", "GO_QI", "AS")
  FR_ep = func_empprod(FR_emp, FR_gop,"FR", "EMP", "GO_QI", "AS")
  EL_ep = func_empprod(EL_emp, EL_gop,"EL", "EMP", "GO_QI", "AS")
  LV_ep = func_empprod(LV_emp, LV_gop,"LV", "EMP", "GO_QI", "AS")
  
  #PLM
  
  DK_ind = func_regpanel(DK_ep, 1)
  DK_tot = func_regpanel(DK_ep, 3)
  US_ind = func_regpanel(US_ep, 1)
  US_tot = func_regpanel(US_ep, 3)
  DE_ind = func_regpanel(DE_ep, 1)
  DE_tot = func_regpanel(DE_ep, 3)
  NL_ind = func_regpanel(NL_ep, 1)
  NL_tot = func_regpanel(NL_ep, 3)
  AT_ind = func_regpanel(AT_ep, 1)
  AT_tot = func_regpanel(AT_ep, 3)
  CZ_ind = func_regpanel(CZ_ep, 1)
  CZ_tot = func_regpanel(CZ_ep, 3)
  EL_ind = func_regpanel(EL_ep, 1)
  EL_tot = func_regpanel(EL_ep, 3)
  FI_ind = func_regpanel(FI_ep, 1)
  FI_tot = func_regpanel(FI_ep, 3)
  FR_ind = func_regpanel(FR_ep, 1)
  FR_tot = func_regpanel(FR_ep, 3)
  LV_ind = func_regpanel(LV_ep, 1)
  LV_tot = func_regpanel(LV_ep, 3)

  
  #Final Panels for analysis:
  c_panel = rbind(DK_tot, US_tot, NL_tot, DE_tot, AT_tot, CZ_tot, EL_tot, FI_tot, FR_tot, LV_tot) 
  #table(index(c_panel), useNA = "ifany")
  ci_panel = rbind(DK_ind, US_ind, NL_ind, DE_ind, AT_ind, CZ_ind, EL_ind, FI_ind, FR_ind, LV_ind)
  ci_panel = ci_panel %>% filter(is.finite(emp_logchanges))
  
} else if (PRISER=="faste" & KOMPLET == FALSE) {
  DK_ep = func_empprod(DK_emp, DK_gop,"DK", "EMP", "GO_QI", "AS")
  US_ep = func_empprod(US_emp, US_gop,"US", "EMP", "GO_QI", "AS")
  DE_ep = func_empprod(DE_emp, DE_gop,"DE", "EMP", "GO_QI", "AS")
  NL_ep = func_empprod(NL_emp, NL_gop,"NL", "EMP", "GO_QI", "AS")
  SE_ep = func_empprod(SE_emp, SE_gop,"SE", "EMP", "GO_QI", "AS")
  AT_ep = func_empprod(AT_emp, AT_gop,"AT", "EMP", "GO_QI", "AS")
  CZ_ep = func_empprod(CZ_emp, CZ_gop,"CZ", "EMP", "GO_QI", "AS")
  FI_ep = func_empprod(FI_emp, FI_gop,"FI", "EMP", "GO_QI", "AS")
  FR_ep = func_empprod(FR_emp, FR_gop,"FR", "EMP", "GO_QI", "AS")
  EL_ep = func_empprod(EL_emp, EL_gop,"EL", "EMP", "GO_QI", "AS")
  IT_ep = func_empprod(IT_emp, IT_gop,"IT", "EMP", "GO_QI", "AS")
  LV_ep = func_empprod(LV_emp, LV_gop,"LV", "EMP", "GO_QI", "AS")
  SK_ep = func_empprod(SK_emp, SK_gop,"SK", "EMP", "GO_QI", "AS")
  SI_ep = func_empprod(SI_emp, SI_gop,"SI", "EMP", "GO_QI", "AS")

  #PLM
  
  DK_ind = func_regpanel(DK_ep, 1)
  DK_tot = func_regpanel(DK_ep, 3)
  US_ind = func_regpanel(US_ep, 1)
  US_tot = func_regpanel(US_ep, 3)
  DE_ind = func_regpanel(DE_ep, 1)
  DE_tot = func_regpanel(DE_ep, 3)
  NL_ind = func_regpanel(NL_ep, 1)
  NL_tot = func_regpanel(NL_ep, 3)
  SE_ind = func_regpanel(SE_ep, 1)
  SE_tot = func_regpanel(SE_ep, 3)
  AT_ind = func_regpanel(AT_ep, 1)
  AT_tot = func_regpanel(AT_ep, 3)
  CZ_ind = func_regpanel(CZ_ep, 1)
  CZ_tot = func_regpanel(CZ_ep, 3)
  EL_ind = func_regpanel(EL_ep, 1)
  EL_tot = func_regpanel(EL_ep, 3)
  FI_ind = func_regpanel(FI_ep, 1)
  FI_tot = func_regpanel(FI_ep, 3)
  FR_ind = func_regpanel(FR_ep, 1)
  FR_tot = func_regpanel(FR_ep, 3)
  IT_ind = func_regpanel(IT_ep, 1)
  IT_tot = func_regpanel(IT_ep, 3)
  LV_ind = func_regpanel(LV_ep, 1)
  LV_tot = func_regpanel(LV_ep, 3)
  SK_ind = func_regpanel(SK_ep, 1)
  SK_tot = func_regpanel(SK_ep, 3)
  SI_ind = func_regpanel(SI_ep, 1)
  SI_tot = func_regpanel(SI_ep, 3)
  
  #Final Panels for analysis:
  c_panel = rbind(DK_tot, US_tot, NL_tot, DE_tot, SE_tot, AT_tot, CZ_tot, EL_tot, FI_tot, FR_tot, IT_tot, LV_tot, SI_tot, SK_tot) 
  #table(index(c_panel), useNA = "ifany")
  ci_panel = rbind(DK_ind, US_ind, NL_ind, DE_ind, SE_ind, AT_ind, CZ_ind, EL_ind, FI_ind, FR_ind, IT_ind, LV_ind, SI_ind, SK_ind)
  ci_panel = ci_panel %>% filter(is.finite(emp_logchanges))
   
} else if (PRISER=="løbende" & KOMPLET == TRUE) {
  DK_ep = func_empprod(DK_emp, DK_go,"DK", "EMP", "GO", "AS")
  US_ep = func_empprod(US_emp, US_go,"US", "EMP", "GO", "AS")
  UK_ep = func_empprod(UK_emp, UK_go,"UK", "EMP", "GO", "AS")
  DE_ep = func_empprod(DE_emp, DE_go,"DE", "EMP", "GO", "AS")
  NL_ep = func_empprod(NL_emp, NL_go,"NL", "EMP", "GO", "AS")
  AT_ep = func_empprod(AT_emp, AT_go,"AT", "EMP", "GO", "AS")
  BE_ep = func_empprod(BE_emp, BE_go,"BE", "EMP", "GO", "AS")
  CZ_ep = func_empprod(CZ_emp, CZ_go,"CZ", "EMP", "GO", "AS")
  CY_ep = func_empprod(CY_emp, CY_go,"CY", "EMP", "GO", "AS")
  ES_ep = func_empprod(ES_emp, ES_go,"ES", "EMP", "GO", "AS")
  EL_ep = func_empprod(EL_emp, EL_go,"EL", "EMP", "GO", "AS")
  FI_ep = func_empprod(FI_emp, FI_go,"FI", "EMP", "GO", "AS")
  FR_ep = func_empprod(FR_emp, FR_go,"FR", "EMP", "GO", "AS")
  HU_ep = func_empprod(HU_emp, HU_go,"HU", "EMP", "GO", "AS")
  IT_ep = func_empprod(IT_emp, IT_go,"IT", "EMP", "GO", "AS")
  IE_ep = func_empprod(IE_emp, IE_go,"IE", "EMP", "GO", "AS")
  LV_ep = func_empprod(LV_emp, LV_go,"LV", "EMP", "GO", "AS")
  LT_ep = func_empprod(LT_emp, LT_go,"LT", "EMP", "GO", "AS")
  PL_ep = func_empprod(PL_emp, PL_go,"PL", "EMP", "GO", "AS")
  PT_ep = func_empprod(PT_emp, PT_go,"PT", "EMP", "GO", "AS")
  SI_ep = func_empprod(SI_emp, SI_go,"SI", "EMP", "GO", "AS")
  
  #PLM
  DK_ind = func_regpanel(DK_ep, 1)
  DK_tot = func_regpanel(DK_ep, 3)
  US_ind = func_regpanel(US_ep, 1)
  US_tot = func_regpanel(US_ep, 3)
  UK_ind = func_regpanel(UK_ep, 1)
  UK_tot = func_regpanel(UK_ep, 3)
  DE_ind = func_regpanel(DE_ep, 1)
  DE_tot = func_regpanel(DE_ep, 3)
  NL_ind = func_regpanel(NL_ep, 1)
  NL_tot = func_regpanel(NL_ep, 3)
  AT_ind = func_regpanel(AT_ep, 1)
  AT_tot = func_regpanel(AT_ep, 3)
  BE_ind = func_regpanel(BE_ep, 1)
  BE_tot = func_regpanel(BE_ep, 3)
  CZ_ind = func_regpanel(CZ_ep, 1)
  CZ_tot = func_regpanel(CZ_ep, 3)
  CY_ind = func_regpanel(CY_ep, 1)
  CY_tot = func_regpanel(CY_ep, 3)
  ES_ind = func_regpanel(ES_ep, 1)
  ES_tot = func_regpanel(ES_ep, 3)
  EL_ind = func_regpanel(EL_ep, 1)
  EL_tot = func_regpanel(EL_ep, 3)
  FI_ind = func_regpanel(FI_ep, 1)
  FI_tot = func_regpanel(FI_ep, 3)
  FR_ind = func_regpanel(FR_ep, 1)
  FR_tot = func_regpanel(FR_ep, 3)
  HU_ind = func_regpanel(HU_ep, 1)
  HU_tot = func_regpanel(HU_ep, 3)
  IE_ind = func_regpanel(IE_ep, 1)
  IE_tot = func_regpanel(IE_ep, 3)
  IT_ind = func_regpanel(IT_ep, 1)
  IT_tot = func_regpanel(IT_ep, 3)
  LV_ind = func_regpanel(LV_ep, 1)
  LV_tot = func_regpanel(LV_ep, 3)
  LT_ind = func_regpanel(LT_ep, 1)
  LT_tot = func_regpanel(LT_ep, 3)
  PL_ind = func_regpanel(PL_ep, 1)
  PL_tot = func_regpanel(PL_ep, 3)
  PT_ind = func_regpanel(PT_ep, 1)
  PT_tot = func_regpanel(PT_ep, 3)
  SI_ind = func_regpanel(SI_ep, 1)
  SI_tot = func_regpanel(SI_ep, 3)
  
  #Final Panels for analysis:
  c_panel = rbind(DK_tot, UK_tot, NL_tot, DE_tot, AT_tot, BE_tot, CY_tot, CZ_tot, ES_tot, EL_tot, FI_tot, FR_tot, HU_tot, IE_tot, IT_tot, LV_tot, LT_tot, PL_tot, PT_tot, SI_tot) 
  #table(index(c_panel), useNA = "ifany")
  ci_panel = rbind(DK_ind, UK_ind, NL_ind, DE_ind, AT_ind, BE_ind, CY_ind, CZ_ind, ES_ind, EL_ind, FI_ind, FR_ind, HU_ind, IE_ind, IT_ind, LV_ind, LT_ind, PL_ind, PT_ind, SI_ind)
  ci_panel = ci_panel %>% filter(is.finite(emp_logchanges))
  
} else if (PRISER=="løbende" & KOMPLET == FALSE) {
  DK_ep = func_empprod(DK_emp, DK_go,"DK", "EMP", "GO", "AS")
  US_ep = func_empprod(US_emp, US_go,"US", "EMP", "GO", "AS")
  UK_ep = func_empprod(UK_emp, UK_go,"UK", "EMP", "GO", "AS")
  DE_ep = func_empprod(DE_emp, DE_go,"DE", "EMP", "GO", "AS")
  NL_ep = func_empprod(NL_emp, NL_go,"NL", "EMP", "GO", "AS")
  SE_ep = func_empprod(SE_emp, SE_go,"SE", "EMP", "GO", "AS")
  AT_ep = func_empprod(AT_emp, AT_go,"AT", "EMP", "GO", "AS")
  BE_ep = func_empprod(BE_emp, BE_go,"BE", "EMP", "GO", "AS")
  CZ_ep = func_empprod(CZ_emp, CZ_go,"CZ", "EMP", "GO", "AS")
  CY_ep = func_empprod(CY_emp, CY_go,"CY", "EMP", "GO", "AS")
  EE_ep = func_empprod(EE_emp, EE_go,"EE", "EMP", "GO", "AS")
  ES_ep = func_empprod(ES_emp, ES_go,"ES", "EMP", "GO", "AS")
  EL_ep = func_empprod(EL_emp, EL_go,"EL", "EMP", "GO", "AS")
  FI_ep = func_empprod(FI_emp, FI_go,"FI", "EMP", "GO", "AS")
  FR_ep = func_empprod(FR_emp, FR_go,"FR", "EMP", "GO", "AS")
  HU_ep = func_empprod(HU_emp, HU_go,"HU", "EMP", "GO", "AS")
  IT_ep = func_empprod(IT_emp, IT_go,"IT", "EMP", "GO", "AS")
  IE_ep = func_empprod(IE_emp, IE_go,"IE", "EMP", "GO", "AS")
  LV_ep = func_empprod(LV_emp, LV_go,"LV", "EMP", "GO", "AS")
  LT_ep = func_empprod(LT_emp, LT_go,"LT", "EMP", "GO", "AS")
  PL_ep = func_empprod(PL_emp, PL_go,"PL", "EMP", "GO", "AS")
  PT_ep = func_empprod(PT_emp, PT_go,"PT", "EMP", "GO", "AS")
  SK_ep = func_empprod(SK_emp, SK_go,"SK", "EMP", "GO", "AS")
  SI_ep = func_empprod(SI_emp, SI_go,"SI", "EMP", "GO", "AS")
  
  #PLM
  
  DK_ind = func_regpanel(DK_ep, 1)
  DK_tot = func_regpanel(DK_ep, 3)
  US_ind = func_regpanel(US_ep, 1)
  US_tot = func_regpanel(US_ep, 3)
  UK_ind = func_regpanel(UK_ep, 1)
  UK_tot = func_regpanel(UK_ep, 3)
  DE_ind = func_regpanel(DE_ep, 1)
  DE_tot = func_regpanel(DE_ep, 3)
  NL_ind = func_regpanel(NL_ep, 1)
  NL_tot = func_regpanel(NL_ep, 3)
  SE_ind = func_regpanel(SE_ep, 1)
  SE_tot = func_regpanel(SE_ep, 3)
  AT_ind = func_regpanel(AT_ep, 1)
  AT_tot = func_regpanel(AT_ep, 3)
  BE_ind = func_regpanel(BE_ep, 1)
  BE_tot = func_regpanel(BE_ep, 3)
  CZ_ind = func_regpanel(CZ_ep, 1)
  CZ_tot = func_regpanel(CZ_ep, 3)
  CY_ind = func_regpanel(CY_ep, 1)
  CY_tot = func_regpanel(CY_ep, 3)
  EE_ind = func_regpanel(EE_ep, 1)
  EE_tot = func_regpanel(EE_ep, 3)
  ES_ind = func_regpanel(ES_ep, 1)
  ES_tot = func_regpanel(ES_ep, 3)
  EL_ind = func_regpanel(EL_ep, 1)
  EL_tot = func_regpanel(EL_ep, 3)
  FI_ind = func_regpanel(FI_ep, 1)
  FI_tot = func_regpanel(FI_ep, 3)
  FR_ind = func_regpanel(FR_ep, 1)
  FR_tot = func_regpanel(FR_ep, 3)
  HU_ind = func_regpanel(HU_ep, 1)
  HU_tot = func_regpanel(HU_ep, 3)
  IE_ind = func_regpanel(IE_ep, 1)
  IE_tot = func_regpanel(IE_ep, 3)
  IT_ind = func_regpanel(IT_ep, 1)
  IT_tot = func_regpanel(IT_ep, 3)
  LV_ind = func_regpanel(LV_ep, 1)
  LV_tot = func_regpanel(LV_ep, 3)
  LT_ind = func_regpanel(LT_ep, 1)
  LT_tot = func_regpanel(LT_ep, 3)
  PL_ind = func_regpanel(PL_ep, 1)
  PL_tot = func_regpanel(PL_ep, 3)
  PT_ind = func_regpanel(PT_ep, 1)
  PT_tot = func_regpanel(PT_ep, 3)
  SK_ind = func_regpanel(SK_ep, 1)
  SK_tot = func_regpanel(SK_ep, 3)
  SI_ind = func_regpanel(SI_ep, 1)
  SI_tot = func_regpanel(SI_ep, 3)
  
  #Final Panels for analysis:
  c_panel = rbind(DK_tot, US_tot, UK_tot, NL_tot, DE_tot, SE_tot, AT_tot, BE_tot, CY_tot, CZ_tot, EE_tot, ES_tot, EL_tot, FI_tot, FR_tot, HU_tot, IE_tot, IT_tot, LV_tot, LT_tot, PL_tot, PT_tot, SI_tot, SK_tot) 
  #table(index(c_panel), useNA = "ifany")
  ci_panel = rbind(DK_ind, US_ind, UK_ind, NL_ind, DE_ind, SE_ind, AT_ind, BE_ind, CY_ind, CZ_ind, EE_ind, ES_ind, EL_ind, FI_ind, FR_ind, HU_ind, IE_ind, IT_ind, LV_ind, LT_ind, PL_ind, PT_ind, SI_ind, SK_ind)
  ci_panel = ci_panel %>% filter(is.finite(emp_logchanges))

}

#test for NA og NaNs
#apply(DK_ind, 2, function(x) any(is.na(x) | is.infinite(x) ) ) # 2 means it look in the columns

#rm(list=setdiff(ls(), c("c_panel", "ci_panel")))

# Country panel  -----------------------------------------------------

c_panel = as.data.frame(c_panel)
c_panel = pdata.frame(c_panel, index = c("country", "year"))

c_panel$prod_logchanges_lag1 = lag(c_panel$prod_logchanges, k = 1, shift = "time")
c_panel$prod_logchanges_lag2 = lag(c_panel$prod_logchanges_lag1, k = 1, shift = "time")
c_panel$prod_logchanges_lag3 = lag(c_panel$prod_logchanges_lag2, k = 1, shift = "time")
c_panel = na.omit(c_panel)

lsdv.c_pool1 = lm(emp_logchanges ~ prod_logchanges, data=c_panel)
lsdv.c_fec1 = lm(emp_logchanges ~ prod_logchanges + factor(country) -1, data=c_panel) 
#lsdv.c_fecy1 = lm(emp_logchanges ~ prod_logchanges + factor(country) + factor(year) -1, data=c_panel) 
lsdv.c_pool2 = lm(emp_logchanges ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3, data=c_panel)
lsdv.c_fec2  = lm(emp_logchanges ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + factor(country) - 1, data=c_panel)
#lsdv.c_fecy2  = lm(emp_logchanges ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + factor(country) + factor(year) -1, data=c_panel)

#HC2 og 3 giver NA på std errors hvis regressionen har både land og år dummies
#summary(lsdv.c_fecy1)
#coeftest(lsdv.c_fecy1, vcov. = vcovHC, type="HC2") 

lsdv.c_pool1_coef = func_coefs(lsdv.c_pool1, "c_pool1", "HC3")
lsdv.c_fec1_coef = func_coefs(lsdv.c_fec1, "c_fec1", "HC3")
#lsdv.c_fecy1_coef = func_coefs(lsdv.c_fecy1, "c_fecy1", "HC3")
lsdv.c_pool2_coef = func_coefs(lsdv.c_pool2, "c_pool2", "HC3")
lsdv.c_fec2_coef = func_coefs(lsdv.c_fec2, "c_fec2", "HC3")
#lsdv.c_fecy2_coef = func_coefs(lsdv.c_fecy2, "c_fecy2", "HC3")


regoutput_c_panel <- Reduce(function(a,b){
  ans <- merge(a,b,by="row.names",all=T)
  row.names(ans) <- ans[,"Row.names"]
  ans[,!names(ans) %in% "Row.names"]
}, list(lsdv.c_pool1_coef, lsdv.c_fec1_coef, lsdv.c_pool2_coef, lsdv.c_fec2_coef))

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

ci_panel_1 = ci_panel %>% select(year, country, code, desc, emp_logchanges, prod_logchanges, wgt_i_avg)

#OBS AS bruger ikke lags i denne pga insignifikans

lsdv.ci_pool1 = lm(emp_logchanges ~ prod_logchanges, data=ci_panel_1)
lsdv.ci_fec1 = lm(emp_logchanges ~ prod_logchanges + factor(country) -1, data=ci_panel_1) 
lsdv.ci_feci1 = lm(emp_logchanges ~ prod_logchanges + factor(country) + factor(code) -1, data=ci_panel_1) #autor bruger ikke denne kombi
lsdv.ci_feciy1 = lm(emp_logchanges ~ prod_logchanges + factor(country) + factor(code) + factor(year) -1, data=ci_panel_1)
lsdv.ci_fecy1 = lm(emp_logchanges ~ prod_logchanges + factor(country) + factor(year) -1, data=ci_panel_1)

#med vægte
lsdv.ci_pool2 = lm(emp_logchanges ~ prod_logchanges, data=ci_panel_1, weights = wgt_i_avg)
lsdv.ci_fec2 = lm(emp_logchanges ~ prod_logchanges + factor(country) -1, data=ci_panel_1, weights = wgt_i_avg) 
lsdv.ci_feci2 = lm(emp_logchanges ~ prod_logchanges + factor(country) + factor(code) -1, data=ci_panel_1, weights = wgt_i_avg) #autor bruger ikke denne kombi
lsdv.ci_feciy2 = lm(emp_logchanges ~ prod_logchanges + factor(country) + factor(code) + factor(year) -1, data=ci_panel_1, weights = wgt_i_avg)
lsdv.ci_fecy2 = lm(emp_logchanges ~ prod_logchanges + factor(country) + factor(year) -1, data=ci_panel_1, weights = wgt_i_avg)

#med vægte og lags - kan ikke køre regressionerne, den siger der er NA/NaN i x variablerne

ci_panel_1_lags = ci_panel %>% select(year, country, code, desc, emp_logchanges, prod_logchanges, prod_logchanges_lag1, prod_logchanges_lag2, prod_logchanges_lag3, wgt_i_avg)

ci_panel_1_lags = na.omit(ci_panel_1_lags)

lsdv.ci_pool3 = lm(emp_logchanges ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3, data=ci_panel_1_lags, weights = wgt_i_avg)
lsdv.ci_fec3 = lm(emp_logchanges ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + factor(country) -1, data=ci_panel_1_lags, weights = wgt_i_avg )
lsdv.ci_feci3 = lm(emp_logchanges ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + factor(country) + factor(code) -1, data=ci_panel_1_lags, weights = wgt_i_avg) #autor bruger ikke denne kombi
lsdv.ci_feciy3 = lm(emp_logchanges ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + factor(country) + factor(code) + factor(year) -1, data=ci_panel_1_lags, weights = wgt_i_avg)
lsdv.ci_fecy3 = lm(emp_logchanges ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + factor(country) + factor(year) -1, data=ci_panel_1_lags, weights = wgt_i_avg)


#Robuste standard fejl

lsdv.c_pool1_coef = func_coefs(lsdv.ci_pool1, "c_pool1", "HC3") 
lsdv.c_fec1_coef = func_coefs(lsdv.ci_fec1, "c_fec1", "HC3")
lsdv.c_feci1_coef = func_coefs(lsdv.ci_feci1, "c_feci1", "HC3")
lsdv.c_feciy1_coef = func_coefs(lsdv.ci_feciy1, "c_feciy1", "HC3")
lsdv.c_fecy1_coef = func_coefs(lsdv.ci_fecy1, "c_fecy1", "HC3")

lsdv.c_pool2_coef = func_coefs(lsdv.ci_pool2, "c_pool2", "HC3") 
lsdv.c_fec2_coef = func_coefs(lsdv.ci_fec2, "c_fec2", "HC3")
lsdv.c_feci2_coef = func_coefs(lsdv.ci_feci2, "c_feci2", "HC3")
lsdv.c_feciy2_coef = func_coefs(lsdv.ci_feciy1, "c_feciy2", "HC3")
lsdv.c_fecy2_coef = func_coefs(lsdv.ci_fecy1, "c_fecy2", "HC3")

lsdv.c_pool3_coef = func_coefs(lsdv.ci_pool3, "c_pool3", "HC3") 
lsdv.c_fec3_coef = func_coefs(lsdv.ci_fec3, "c_fec3", "HC3")
lsdv.c_feci3_coef = func_coefs(lsdv.ci_feci3, "c_feci3", "HC3")
lsdv.c_feciy3_coef = func_coefs(lsdv.ci_feciy1, "c_feciy3", "HC3")
lsdv.c_fecy3_coef = func_coefs(lsdv.ci_fecy1, "c_fecy3", "HC3")

#export resultater til excel
#options(digits = 3)

regoutput_ci_panel <- Reduce(function(a,b){
  ans <- merge(a,b,by="row.names",all=T)
  row.names(ans) <- ans[,"Row.names"]
  ans[,!names(ans) %in% "Row.names"]
}, list(lsdv.c_pool2_coef, lsdv.c_fec2_coef, lsdv.c_feci2_coef, lsdv.c_feciy2_coef, lsdv.c_fecy2_coef))

write.xlsx(regoutput_ci_panel, "regoutput_ci_panel.xlsx", col.names = TRUE, row.names = TRUE)


#test af plm
FixedEffects_indi <- plm(model_linear1, data = dk, index = c("code", "year"), weight=wgt, model = "within", effect = "individual")
FixedEffects_time <- plm(model_linear1, data = dk, index = c("code", "year"), weight=wgt, model = "within", effect = "time")
FixedEffects_twoway <- plm(model_linear1, data = dk, index = c("code", "year"), weight=wgt, model = "within", effect = "twoway")

summary(FixedEffects_indi)
summary(FixedEffects_time)
summary(FixedEffects_twoway)





# Sammensætning af mikro og makroelasticiteter --------------------------------------------------

ci_panel_2 = ci_panel %>% select(year, country, code, desc, emp_logchanges, prod_logchanges, dLP_CwoI, dLP_CwoI_lag1, dLP_CwoI_lag2, dLP_CwoI_lag3 ,wgt_i_avg)

lsdv.mm_pool1 = lm(emp_logchanges ~ prod_logchanges + dLP_CwoI + dLP_CwoI_lag1 + dLP_CwoI_lag2 + dLP_CwoI_lag3, data=ci_panel_2)
lsdv.mm_fec1 = lm(emp_logchanges ~ prod_logchanges + dLP_CwoI + dLP_CwoI_lag1 + dLP_CwoI_lag2 + dLP_CwoI_lag3 + factor(country) -1, data=ci_panel_2) 
lsdv.mm_feci1 = lm(emp_logchanges ~ prod_logchanges + dLP_CwoI + dLP_CwoI_lag1 + dLP_CwoI_lag2 + dLP_CwoI_lag3 + factor(country) + factor(code) -1, data=ci_panel_2)
lsdv.mm_feciy1 = lm(emp_logchanges ~ prod_logchanges + dLP_CwoI + dLP_CwoI_lag1 + dLP_CwoI_lag2 + dLP_CwoI_lag3 + factor(country) + factor(code) + factor(year) -1, data=ci_panel_2)

summary(lsdv.mm_pool1)
summary(lsdv.mm_fec1)
summary(lsdv.mm_feci1)
summary(lsdv.mm_feciy1)

#med vægte
lsdv.mm_pool2 = lm(emp_logchanges ~ prod_logchanges + dLP_CwoI + dLP_CwoI_lag1 + dLP_CwoI_lag2 + dLP_CwoI_lag3, data=ci_panel_2, weights = wgt_i_avg)
lsdv.mm_fec2 = lm(emp_logchanges ~ prod_logchanges + dLP_CwoI + dLP_CwoI_lag1 + dLP_CwoI_lag2 + dLP_CwoI_lag3 + factor(country) -1, data=ci_panel_2, weights = wgt_i_avg) 
lsdv.mm_feci2 = lm(emp_logchanges ~ prod_logchanges + dLP_CwoI + dLP_CwoI_lag1 + dLP_CwoI_lag2 + dLP_CwoI_lag3 + factor(country) + factor(code) -1, data=ci_panel_2, weights = wgt_i_avg)
lsdv.mm_feciy2 = lm(emp_logchanges ~ prod_logchanges + dLP_CwoI + dLP_CwoI_lag1 + dLP_CwoI_lag2 + dLP_CwoI_lag3 + factor(country) + factor(code) + factor(year) -1, data=ci_panel_2, weights = wgt_i_avg)

summary(lsdv.mm_pool2)
summary(lsdv.mm_fec2)
summary(lsdv.mm_feci2)
summary(lsdv.mm_feciy2)

# Sector spillover -------------------------------------------------

# How to deal with NA in a panel data regression? Link: https://stackoverflow.com/questions/14427781/how-to-deal-with-na-in-a-panel-data-regression------

#Skal det vægtes? Og hvad skal vægtes?


ci_panel_ss = ci_panel %>% select(year, country, code, desc, branche, branche_desc, wgt_i, wgt_i_avg, emp_logchanges, 
                                    dLP_I_b1, dLP_I_b2, dLP_I_b3, dLP_I_b4, dLP_I_b5,
                                    #dLP_I_b1_dum, dLP_I_b2_dum, dLP_I_b3_dum, dLP_I_b4_dum, dLP_I_b5_dum,
                                    dLP_BwoI_b1, dLP_BwoI_b1_lag1, dLP_BwoI_b1_lag2, dLP_BwoI_b1_lag3,
                                    dLP_BwoI_b2, dLP_BwoI_b2_lag1, dLP_BwoI_b2_lag2, dLP_BwoI_b2_lag3,
                                    dLP_BwoI_b3, dLP_BwoI_b3_lag1, dLP_BwoI_b3_lag2, dLP_BwoI_b3_lag3,
                                    dLP_BwoI_b4, dLP_BwoI_b4_lag1, dLP_BwoI_b4_lag2, dLP_BwoI_b4_lag3,
                                    dLP_BwoI_b5, dLP_BwoI_b5_lag1, dLP_BwoI_b5_lag2, dLP_BwoI_b5_lag3)

ci_panel_ss = as.data.frame(ci_panel_ss)

#is.pconsecutive(ci_panel_ss)

ci_panel_ss$id = ci_panel_ss %>% group_indices(code, country)

#ci_panel_ss$id = ci_panel_ss %>% group_indices(code, country)
#ci_panel_ss$prod_logchanges = ci_panel_ss$prod_logchanges*ci_panel$wgt
#ci_panel_ss$emp_logchanges = ci_panel_ss$emp_logchanges*ci_panel$wgt

lsdv.ss_pool = {lm(emp_logchanges ~ dLP_I_b1 + dLP_I_b2 + dLP_I_b3 + dLP_I_b4 + dLP_I_b5 +
                     dLP_BwoI_b1 + dLP_BwoI_b1_lag1 + dLP_BwoI_b1_lag2 + dLP_BwoI_b1_lag3 +
                     dLP_BwoI_b2 + dLP_BwoI_b2_lag1 + dLP_BwoI_b2_lag2 + dLP_BwoI_b2_lag3 +
                     dLP_BwoI_b3 + dLP_BwoI_b3_lag1 + dLP_BwoI_b3_lag2 + dLP_BwoI_b3_lag3 +
                     dLP_BwoI_b4 + dLP_BwoI_b4_lag1 + dLP_BwoI_b4_lag2 + dLP_BwoI_b4_lag3 +
                     dLP_BwoI_b5 + dLP_BwoI_b5_lag1 + dLP_BwoI_b5_lag2 + 
                     dLP_BwoI_b5_lag3, data=ci_panel_ss)}

lsdv.ss_fecy = {lm(emp_logchanges ~ dLP_I_b1 + dLP_I_b2 + dLP_I_b3 + dLP_I_b4 + dLP_I_b5 +
                     dLP_BwoI_b1 + dLP_BwoI_b1_lag1 + dLP_BwoI_b1_lag2 + dLP_BwoI_b1_lag3 +
                     dLP_BwoI_b2 + dLP_BwoI_b2_lag1 + dLP_BwoI_b2_lag2 + dLP_BwoI_b2_lag3 +
                     dLP_BwoI_b3 + dLP_BwoI_b3_lag1 + dLP_BwoI_b3_lag2 + dLP_BwoI_b3_lag3 +
                     dLP_BwoI_b4 + dLP_BwoI_b4_lag1 + dLP_BwoI_b4_lag2 + dLP_BwoI_b4_lag3 +
                     dLP_BwoI_b5 + dLP_BwoI_b5_lag1 + dLP_BwoI_b5_lag2 + dLP_BwoI_b5_lag3 + 
                     factor(country) + factor(year) -1, data=ci_panel_ss)}

summary(lsdv.ss_pool)

lsdv.ss_feci = {lm(emp_logchanges ~ dLP_I_b1 + dLP_I_b2 + dLP_I_b3 + dLP_I_b4 + dLP_I_b5 +
                     dLP_BwoI_b1 + dLP_BwoI_b1_lag1 + dLP_BwoI_b1_lag2 + dLP_BwoI_b1_lag3 +
                     dLP_BwoI_b2 + dLP_BwoI_b2_lag1 + dLP_BwoI_b2_lag2 + dLP_BwoI_b2_lag3 +
                     dLP_BwoI_b3 + dLP_BwoI_b3_lag1 + dLP_BwoI_b3_lag2 + dLP_BwoI_b3_lag3 +
                     dLP_BwoI_b4 + dLP_BwoI_b4_lag1 + dLP_BwoI_b4_lag2 + dLP_BwoI_b4_lag3 +
                     dLP_BwoI_b5 + dLP_BwoI_b5_lag1 + dLP_BwoI_b5_lag2 + dLP_BwoI_b5_lag3 +
                     factor(country) + factor(code) -1, data=ci_panel_ss)}

lsdv.ss_feyi = {lm(emp_logchanges ~ dLP_I_b1 + dLP_I_b2 + dLP_I_b3 + dLP_I_b4 + dLP_I_b5 +
                     dLP_BwoI_b1 + dLP_BwoI_b1_lag1 + dLP_BwoI_b1_lag2 + dLP_BwoI_b1_lag3 +
                     dLP_BwoI_b2 + dLP_BwoI_b2_lag1 + dLP_BwoI_b2_lag2 + dLP_BwoI_b2_lag3 +
                     dLP_BwoI_b3 + dLP_BwoI_b3_lag1 + dLP_BwoI_b3_lag2 + dLP_BwoI_b3_lag3 +
                     dLP_BwoI_b4 + dLP_BwoI_b4_lag1 + dLP_BwoI_b4_lag2 + dLP_BwoI_b4_lag3 +
                     dLP_BwoI_b5 + dLP_BwoI_b5_lag1 + dLP_BwoI_b5_lag2 + dLP_BwoI_b5_lag3 +
                     factor(year) + factor(code) -1, data=ci_panel_ss)}

lsdv.ss_fecyi = {lm(emp_logchanges ~ dLP_I_b1 + dLP_I_b2 + dLP_I_b3 + dLP_I_b4 + dLP_I_b5 +
                      dLP_BwoI_b1 + dLP_BwoI_b1_lag1 + dLP_BwoI_b1_lag2 + dLP_BwoI_b1_lag3 +
                      dLP_BwoI_b2 + dLP_BwoI_b2_lag1 + dLP_BwoI_b2_lag2 + dLP_BwoI_b2_lag3 +
                      dLP_BwoI_b3 + dLP_BwoI_b3_lag1 + dLP_BwoI_b3_lag2 + dLP_BwoI_b3_lag3 +
                      dLP_BwoI_b4 + dLP_BwoI_b4_lag1 + dLP_BwoI_b4_lag2 + dLP_BwoI_b4_lag3 +
                      dLP_BwoI_b5 + dLP_BwoI_b5_lag1 + dLP_BwoI_b5_lag2 + dLP_BwoI_b5_lag3 +
                      factor(country) + factor(year) + factor(code) -1, data=ci_panel_ss)}


#med vægte

lsdv.ss_pool = {lm(emp_logchanges ~ dLP_I_b1 + dLP_I_b2 + dLP_I_b3 + dLP_I_b4 + dLP_I_b5 +
                     dLP_BwoI_b1 + dLP_BwoI_b1_lag1 + dLP_BwoI_b1_lag2 + dLP_BwoI_b1_lag3 +
                     dLP_BwoI_b2 + dLP_BwoI_b2_lag1 + dLP_BwoI_b2_lag2 + dLP_BwoI_b2_lag3 +
                     dLP_BwoI_b3 + dLP_BwoI_b3_lag1 + dLP_BwoI_b3_lag2 + dLP_BwoI_b3_lag3 +
                     dLP_BwoI_b4 + dLP_BwoI_b4_lag1 + dLP_BwoI_b4_lag2 + dLP_BwoI_b4_lag3 +
                     dLP_BwoI_b5 + dLP_BwoI_b5_lag1 + dLP_BwoI_b5_lag2 + dLP_BwoI_b5_lag3, data=ci_panel_ss, weights = wgt_i_avg)}

lsdv.ss_fecy = {lm(emp_logchanges ~ dLP_I_b1 + dLP_I_b2 + dLP_I_b3 + dLP_I_b4 + dLP_I_b5 +
                     dLP_BwoI_b1 + dLP_BwoI_b1_lag1 + dLP_BwoI_b1_lag2 + dLP_BwoI_b1_lag3 +
                     dLP_BwoI_b2 + dLP_BwoI_b2_lag1 + dLP_BwoI_b2_lag2 + dLP_BwoI_b2_lag3 +
                     dLP_BwoI_b3 + dLP_BwoI_b3_lag1 + dLP_BwoI_b3_lag2 + dLP_BwoI_b3_lag3 +
                     dLP_BwoI_b4 + dLP_BwoI_b4_lag1 + dLP_BwoI_b4_lag2 + dLP_BwoI_b4_lag3 +
                     dLP_BwoI_b5 + dLP_BwoI_b5_lag1 + dLP_BwoI_b5_lag2 + dLP_BwoI_b5_lag3 + 
                     factor(country) + factor(year) -1, data=ci_panel_ss, weights = wgt_i_avg)}

summary(lsdv.ss_pool)

lsdv.ss_feci = {lm(emp_logchanges ~ dLP_I_b1 + dLP_I_b2 + dLP_I_b3 + dLP_I_b4 + dLP_I_b5 +
                     dLP_BwoI_b1 + dLP_BwoI_b1_lag1 + dLP_BwoI_b1_lag2 + dLP_BwoI_b1_lag3 +
                     dLP_BwoI_b2 + dLP_BwoI_b2_lag1 + dLP_BwoI_b2_lag2 + dLP_BwoI_b2_lag3 +
                     dLP_BwoI_b3 + dLP_BwoI_b3_lag1 + dLP_BwoI_b3_lag2 + dLP_BwoI_b3_lag3 +
                     dLP_BwoI_b4 + dLP_BwoI_b4_lag1 + dLP_BwoI_b4_lag2 + dLP_BwoI_b4_lag3 +
                     dLP_BwoI_b5 + dLP_BwoI_b5_lag1 + dLP_BwoI_b5_lag2 + dLP_BwoI_b5_lag3 +
                     factor(country) + factor(code) -1, data=ci_panel_ss, weights = wgt_i_avg)}

lsdv.ss_feyi = {lm(emp_logchanges ~ dLP_I_b1 + dLP_I_b2 + dLP_I_b3 + dLP_I_b4 + dLP_I_b5 +
                     dLP_BwoI_b1 + dLP_BwoI_b1_lag1 + dLP_BwoI_b1_lag2 + dLP_BwoI_b1_lag3 +
                     dLP_BwoI_b2 + dLP_BwoI_b2_lag1 + dLP_BwoI_b2_lag2 + dLP_BwoI_b2_lag3 +
                     dLP_BwoI_b3 + dLP_BwoI_b3_lag1 + dLP_BwoI_b3_lag2 + dLP_BwoI_b3_lag3 +
                     dLP_BwoI_b4 + dLP_BwoI_b4_lag1 + dLP_BwoI_b4_lag2 + dLP_BwoI_b4_lag3 +
                     dLP_BwoI_b5 + dLP_BwoI_b5_lag1 + dLP_BwoI_b5_lag2 + dLP_BwoI_b5_lag3 +
                     factor(year) + factor(code) -1, data=ci_panel_ss, weights = wgt_i_avg)}

lsdv.ss_fecyi = {lm(emp_logchanges ~ dLP_I_b1 + dLP_I_b2 + dLP_I_b3 + dLP_I_b4 + dLP_I_b5 +
                      dLP_BwoI_b1 + dLP_BwoI_b1_lag1 + dLP_BwoI_b1_lag2 + dLP_BwoI_b1_lag3 +
                      dLP_BwoI_b2 + dLP_BwoI_b2_lag1 + dLP_BwoI_b2_lag2 + dLP_BwoI_b2_lag3 +
                      dLP_BwoI_b3 + dLP_BwoI_b3_lag1 + dLP_BwoI_b3_lag2 + dLP_BwoI_b3_lag3 +
                      dLP_BwoI_b4 + dLP_BwoI_b4_lag1 + dLP_BwoI_b4_lag2 + dLP_BwoI_b4_lag3 +
                      dLP_BwoI_b5 + dLP_BwoI_b5_lag1 + dLP_BwoI_b5_lag2 + dLP_BwoI_b5_lag3 +
                      factor(country) + factor(year) + factor(code) -1, data=ci_panel_ss, weights = wgt_i_avg)}


options(digits = 3)
options("scipen"=100, "digits"=4)

library(lmtest)
lsdv.ss_pool_coef = coeftest(lsdv.ss_pool, vcov. = vcovHC, type = "HC1")
lsdv.ss_feci_coef = coeftest(lsdv.ss_feci, vcov. = vcovHC, type = "HC1")
lsdv.ss_fecy_coef = coeftest(lsdv.ss_fecy, vcov. = vcovHC, type = "HC1")
lsdv.ss_feyi_coef = coeftest(lsdv.ss_feyi, vcov. = vcovHC, type = "HC1")
lsdv.ss_fecyi_coef = coeftest(lsdv.ss_feyi, vcov. = vcovHC, type = "HC1")
#coeftest(fixed.dum, vcov. = vcovHC, method = "arellano")

write.csv(cbind(lsdv.ss_pool_coef, lsdv.ss_feci_coef, lsdv.ss_fecy_coef, lsdv.ss_feyi_coef), "fixeddum_ci_panel.csv")

# Skills..... --------------------------------------------------


