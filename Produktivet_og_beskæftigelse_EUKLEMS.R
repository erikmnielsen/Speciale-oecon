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

# Libraries ---------------------------------------------------------------

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

{
country="DK"
country="SI"

dataset_1 <- read_excel("Data/SI_output_17ii.xlsx", sheet = "EMP") #Number of persons engaged (thousands)
dataset_2 <- read_excel("Data/SI_output_17ii.xlsx", sheet = "GO_P")
 
dataset_1 <- read_excel("Data/DK_output_17ii.xlsx", sheet = "EMP")
dataset_2 <- read_excel("Data/DK_output_17ii.xlsx", sheet = "GO")
dataset_2 <- read_excel("Data/DK_output_17ii.xlsx", sheet = "GO_P") #Gross output, price indices, 2010 = 100

measure_1="EMP"
measure_2="GO_P"

dataset_1 <- read_excel("Data/DK_output_17ii.xlsx", sheet = "COMP")
dataset_2 <- read_excel("Data/DK_output_17ii.xlsx", sheet = "VA")
dataset_3 <- read_excel("Data/DK_output_17ii.xlsx", sheet = "LAB")
measure_1="COMP"
measure_2="VA"
measure_3="LAB"

}

CGR = function(x){
  sapply(1:length(x), function(y){
    prod(1+x[1:y]) - 1
  })
}

func_labshare <- function(dataset_1, dataset_2, dataset_3, country, measure_1="COMP", measure_2="VA", measure_3="LAB") {
  
  colnames(dataset_1) <- gsub(measure_1, "", colnames(dataset_1))
  colnames(dataset_2) <- gsub(measure_2, "", colnames(dataset_2))
  colnames(dataset_3) <- gsub(measure_3, "", colnames(dataset_3))
  
  dataset_1<- melt(dataset_1,
                   id.vars=c("desc", "code"),
                   variable.name="year",
                   value.name= measure_1)
  
  dataset_2 <- melt(dataset_2,
                    id.vars=c("desc", "code"),
                    variable.name="year",
                    value.name= measure_2)
  
  dataset_3 <- melt(dataset_3,
                    id.vars=c("desc", "code"),
                    variable.name="year",
                    value.name= measure_3)
  
  data = merge(dataset_1, dataset_2, by=c("desc","code", "year"), all.x = TRUE)
  data = merge(data, dataset_3, by=c("desc","code", "year"), all.x = TRUE)
  
  data <- na.omit(data)
  
  sapply(data, class)
  data$year <-  as.numeric(as.character(data[,"year"])) #ændres fordi "year" er en factor variabel
  data$code =gsub("-", "t", data[,2])
  data$country = country
  data$LS = data$LAB/data$VA
  data$LSe = data$COMP/data$VA
  
  data = data %>% filter(code=="TOT")
  
  #data$indeksLS = (data$LS/0.6880021)*100
  #data$indeksLSe = (data$LSe/0.5954859)*100
  
  pdata = pdata.frame(data, index = c("code", "year"))
  
  #Kumulativ vækst (viser det samme som indeks....)
  {
  pdata$LS_diff = diff(pdata$LS, lag = 1, shift = "time")
  pdata$LSe_diff = diff(pdata$LSe, lag = 1, shift = "time")
  pdata$LS_changes <- pdata$LS_diff/lag(pdata$LS, k = 1, shift = "time")
  pdata$LSe_changes <- pdata$LSe_diff/lag(pdata$LSe, k = 1, shift = "time")
  
  pdata$LS_CGR = order_by(pdata$year, CGR(pdata$LS_changes[-1])*100)
  pdata$LSe_CGR = order_by(pdata$year, CGR(pdata$LSe_changes[-1])*100)
  
  #pdata = pdata.frame(pdata, index = c("code", "year"))
  
  pdata$LS_CGR <- lag(pdata$LS_CGR, k=1, shift="time")
  pdata$LS_CGR = ifelse(is.na(pdata$LS_CGR)==T,0,pdata$LS_CGR)
 
  pdata$LSe_CGR <- lag(pdata$LSe_CGR, k=1, shift="time")
  pdata$LSe_CGR = ifelse(is.na(pdata$LSe_CGR)==T,0,pdata$LSe_CGR)
  }
  
  pdata
  
}

func_empprod <- function(dataset_1, dataset_2, country, measure_1="EMP", measure_2="GO", Emma) {
  
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
  
  data$sel_industries <-factor(ifelse( data$code %in% c("A","B", "DtE", "F","10t12", "13t15", "16t18", "19", "20t21", "22t23","24t25", "26t27", "28", "29t30","31t33",
                                                        "53","58t60", "61", "62t63", "K", "MtN","45", "46", "47", "49t52", "I", "L"), 1,0)) #alt pånær O, P, Q (skal RtS, T og U også fjernes?) 
                                                          
  #Brancher, 10:
  {data$branche <- ifelse(data$code=="A", "b1",
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
  #Nedenstående skal bruges til hvis vores udvalgte brancher adskiller sig fra "TOTAL INDUSTRIES"
  
  b <- data2 %>% filter(code=="b1")
  b_2 <- data2 %>% filter(code=="b2")
  b_3 <- data2 %>% filter(code=="b3")
  b_4 <- data2 %>% filter(code=="b4")
  b_5 <- data2 %>% filter(code=="b5")
  b_6 <- data2 %>% filter(code=="b6")
  b_7 <- data2 %>% filter(code=="b7")
  b_8 <- data2 %>% filter(code=="b8")
  #b_9 <- data2 %>% filter(code=="b9")
  #b_10 <- data2 %>% filter(code=="b10")
  
  b$EMP = b$EMP + b_2$EMP + b_3$EMP + b_4$EMP + b_5$EMP + b_6$EMP + b_7$EMP + b_8$EMP #+ b_10$EMP + b_9$EMP 
  b$desc = "TOTAL INDUSTRIES-MunkNielsen"
  b$code = "TOT_MN"
  b$branche = "TOT_MN"
  b$branche_desc = "Lande Total"
  
  data_fin <- rbind(data, data2, b)
  pdata = pdata.frame(data_fin, index = c("code", "year"))
  
  pdata$emp_log <- log(pdata$EMP)
  pdata$emp_diff = diff(pdata$EMP, lag = 1, shift = "time")
  pdata$emp_logdiff = diff(pdata$emp_log, lag = 1, shift = "time")
  pdata$emp_changes <- pdata$emp_diff/lag(pdata$EMP, k = 1, shift = "time")*100
  pdata$emp_logchanges = diff(pdata$emp_log, lag = 1, shift = "time")*100
    
  pdata$prod <- pdata$GO/pdata$EMP
  
  pdata$prod_diff <- diff(pdata$prod, lag = 1, shift = "time")
  pdata$prod_changes <- pdata$prod_diff/lag(pdata$prod, k = 1, shift = "time")*100
  pdata$prod_changes2 <- pdata$prod_diff/lag(pdata$prod, k = 1, shift = "time")

  pdata$prod_log <- log(pdata$prod)
  pdata$prod_logchanges<- diff(pdata$prod_log, lag = 1, shift = "time")*100
  pdata$prod_logdiff<- diff(pdata$prod_log, lag = 1, shift = "time")
  
if (Emma==F) {
  pdata = pdata %>% group_by(code) %>% mutate(prod_CGR_logchanges =  order_by(year,cumprod(1+prod_logdiff[-1])*100)) #metode 1
  pdata = pdata %>% group_by(code) %>% mutate(prod_logCGR = order_by(year, CGR(prod_logdiff[-1])*100)) #metode 2
  
  pdata = pdata %>% group_by(code) %>% mutate(prod_CGR= order_by(year, CGR(prod_changes2[-1])*100))
  #df = pdata %>% group_by(code) %>% mutate(cumsum = cumsum())
  
  pdata <- pdata %>% select(year, country, code, desc, sel_industries, branche, branche_desc, EMP, emp_logchanges, GO, prod, prod_logchanges,prod_changes, prod_CGR, prod_logCGR, prod_CGR_logchanges) %>% 
    filter(code!="b0",code!="s0")
  
  pdata = pdata.frame(pdata, index = c("code", "year"))
  
  pdata$prod_logCGR <- lag(pdata$prod_logCGR, k=1, shift="time")
  pdata$prod_CGR <- lag(pdata$prod_CGR, k=1, shift="time")
}
  
if (Emma==T) {
  
  pdata <- pdata %>% select(year, country, code, desc, sel_industries, branche, branche_desc, EMP, emp_logchanges, GO, prod, prod_logchanges,prod_changes) %>% 
  filter(code!="b0")
  #pdata = pdata.frame(pdata, index = c("code", "year"))
                      
      }

  pdata
  

}

func_regpanel <- function(dataset_1, type) {

  dataset1 = DK_ep
    
if (type==1) {

  tot = dataset_1 %>% filter(code=="TOT_MN")
  tot$TOTmn = tot$EMP
  tot <- tot %>% select(year, country, TOTmn)
  ind = dataset_1 %>% filter(sel_industries==1)
  b <- dataset_1 %>% filter(branche=="b-tot")
  b$branche = b$code
  #dk$emp_logchanges_b = dk$emp_logchanges
  b$prod_logchanges_b = b$prod_logchanges
  b = b %>% select(year, branche, prod_logchanges_b)
  b1 = b %>% filter(branche=="b1") %>% mutate(prod_logchanges_b1=prod_logchanges_b) %>% select(year, prod_logchanges_b1 )
  b2 = b %>% filter(branche=="b2") %>% mutate(prod_logchanges_b2=prod_logchanges_b) %>% select(prod_logchanges_b2)
  b3 = b %>% filter(branche=="b3") %>% mutate(prod_logchanges_b3=prod_logchanges_b) %>% select(prod_logchanges_b3)
  b4 = b %>% filter(branche=="b4") %>% mutate(prod_logchanges_b4=prod_logchanges_b) %>% select(prod_logchanges_b4)
  b5 = b %>% filter(branche=="b5") %>% mutate(prod_logchanges_b5=prod_logchanges_b) %>% select(prod_logchanges_b5)
  b6 = b %>% filter(branche=="b6") %>% mutate(prod_logchanges_b6=prod_logchanges_b) %>% select(prod_logchanges_b6)
  b7 = b %>% filter(branche=="b7") %>% mutate(prod_logchanges_b7=prod_logchanges_b) %>% select(prod_logchanges_b7)
  b8 = b %>% filter(branche=="b8") %>% mutate(prod_logchanges_b8=prod_logchanges_b) %>% select(prod_logchanges_b8)
  
  b = cbind(b1,b2,b3,b4,b5,b6,b7,b8)
  ind = merge(ind, b, by=c("year"), all.x = TRUE)
  ind = merge(ind,tot, by=c("year", "country"), all.x = TRUE)
  ind$wgt = ind$EMP/ind$TOTmn
  
  
  
  
  
  ind
} else if (type==2) {
    
    b = dataset_1 %>% filter(branche=="b-tot")
    sumEMP <- b %>% group_by(year) %>% summarize(sum_EMP=sum(EMP))
    b = merge(b, sumEMP, by=c("year"), all.x = TRUE)
    b$share_EMP = (b$EMP/b$sum_EMP)*100
    b = pdata.frame(b, index = c("code", "year"))
    b$share_EMP_ppchange = diff(b$share_EMP, lag = 1, shift = "time")
    b$share_EMP_ppchange = ifelse(is.na(b$share_EMP_ppchange)==T,0,b$share_EMP_ppchange)
    b = b %>% group_by(code) %>% mutate(cumsum_EMP = cumsum(share_EMP_ppchange))
    b$year = lubridate::ymd(b$year, truncated = 2L)
    
    b
  } else if (type==3) {
    
    tot = dataset_1 %>% filter(code=="TOT")
    tot$year = lubridate::ymd(tot$year, truncated = 2L)
    
    tot
    
  } else {
    
    NA
  }
  

}


#key_table <- read_csv("EUklems-data-master/key_table.csv")

# Country data  ----------------------------------------------------- 

# Danmark
    DK_emp <- read_excel("Data/DK_output_17ii.xlsx", sheet = "EMP") #Number of persons engaged (thousands)
    #DK_go <- read_excel("Data/DK_output_17ii.xlsx", sheet = "GO") #Gross Output at current basic prices (in millions of national currency)
    DK_gop <- read_excel("Data/DK_output_17ii.xlsx", sheet = "GO_P") #Gross output, price indices, 2010 = 100
    DK_comp <- read_excel("Data/DK_output_17ii.xlsx", sheet = "COMP") #Compensation of employees  (in millions of national currency)
    DK_lab <- read_excel("Data/DK_output_17ii.xlsx", sheet = "LAB") #Labour compensation (in millions of national currency)
    DK_va <- read_excel("Data/DK_output_17ii.xlsx", sheet = "VA") #Gross value added at current basic prices (in millions of national currency), svarer labour + capital compensation
    
    
    
    #Employment and productivty
    DK_ep = func_empprod(DK_emp, DK_gop,"DK", "EMP", "GO_P", F)
    DK_ep = func_empprod(DK_emp, DK_gop,"DK", "EMP", "GO_P", T)
    
    #PLM analyse
    DK_ind = func_regpanel(DK_ep, 1)
    DK_tot = func_regpanel(DK_ep, 3)
    
    #deskriptiv
    DK_b = func_regpanel(DK_ep, 2)
    
    #Labour share
    DK_ls = func_labshare(DK_comp, DK_va, DK_lab, "DK", "COMP", "VA", "LAB")
    DK_ls$year = lubridate::ymd(DK_ls$year, truncated = 2L)
    

# USA

  US_emp <- read_excel("Data/US_output_17ii.xlsx", sheet = "EMP") #Number of persons engaged (thousands)
  #US_go <- read_excel("Data/US_output_17ii.xlsx", sheet = "GO") #Gross Output at current basic prices (in millions of national currency)
  US_gop <- read_excel("Data/US_output_17ii.xlsx", sheet = "GO_P") #Gross Output at current basic prices (in millions of national currency)
  US_comp <- read_excel("Data/US_output_17ii.xlsx", sheet = "COMP") #Compensation of employees  (in millions of national currency)
  US_lab <- read_excel("Data/US_output_17ii.xlsx", sheet = "LAB") #Labour compensation (in millions of national currency)
  US_va <- read_excel("Data/US_output_17ii.xlsx", sheet = "VA") #Gross value added at current basic prices (in millions of national currency), svarer labour + capital compensation
  
  #Labour share
  US_ls = func_labshare(US_comp, US_va, US_lab, "US", "COMP", "VA", "LAB")
  US_ls$year = lubridate::ymd(US_ls$year, truncated = 2L)
  
  #Employment and productivty
  US_ep = func_empprod(US_emp, US_gop,"US", "EMP", "GO_P", F)
  US_ep = func_empprod(US_emp, US_gop,"US", "EMP", "GO_P", T)
  
  #PLM analyse
  US_ind = func_regpanel(US_ep, 1)
  US_tot = func_regpanel(US_ep, 3)
  
  #deskriptiv
  US_b = func_regpanel(US_ep, 2)


# UK . faste priser findes ikke

  UK_emp <- read_excel("Data/UK_output_17ii.xlsx", sheet = "EMP") #Number of persons engaged (thousands)
  #UK_go <- read_excel("Data/UK_output_17ii.xlsx", sheet = "GO") #Gross Output at current basic prices (in millions of national currency)
  UK_gop <- read_excel("Data/UK_output_17ii.xlsx", sheet = "GO_P") #Gross Output at current basic prices (in millions of national currency)
  UK_comp <- read_excel("Data/UK_output_17ii.xlsx", sheet = "COMP") #Compensation of employees  (in millions of national currency)
  UK_lab <- read_excel("Data/UK_output_17ii.xlsx", sheet = "LAB") #Labour compensation (in millions of national currency)
  UK_va <- read_excel("Data/UK_output_17ii.xlsx", sheet = "VA") #Gross value added at current basic prices (in millions of national currency), svarer labour + capital compensation
  
  #Labour share
  UK_ls = func_labshare(UK_comp, UK_va, UK_lab, "UK", "COMP", "VA", "LAB")
  UK_ls$year = lubridate::ymd(UK_ls$year, truncated = 2L)
  
  #Employment and productivty
  #Employment and productivty
  UK_ep = func_empprod(UK_emp, UK_gop,"UK", "EMP", "GO_P", F)
  UK_ep = func_empprod(UK_emp, UK_gop,"UK", "EMP", "GO_P", T)
  
  #PLM analyse
  UK_ind = func_regpanel(UK_ep, 1)
  UK_tot = func_regpanel(UK_ep, 3)
  
  #deskriptiv
  UK_b = func_regpanel(UK_ep, 2)


# Tyskland

  DE_emp <- read_excel("Data/DE_output_17ii.xlsx", sheet = "EMP") #Number of persons engaged (thousands)
  #DE_go <- read_excel("Data/DE_output_17ii.xlsx", sheet = "GO") #Gross Output at current basic prices (in millions of national currency)
  DE_gop <- read_excel("Data/DE_output_17ii.xlsx", sheet = "GO_P") #Gross Output at current basic prices (in millions of national currency)
  DE_comp <- read_excel("Data/DE_output_17ii.xlsx", sheet = "COMP") #Compensation of employees  (in millions of national currency)
  DE_lab <- read_excel("Data/DE_output_17ii.xlsx", sheet = "LAB") #Labour compensation (in millions of national currency)
  DE_va <- read_excel("Data/DE_output_17ii.xlsx", sheet = "VA") #Gross value added at current basic prices (in millions of national currency), svarer labour + capital compensation
  
  #Labour share
  DE_ls = func_labshare(DE_comp, DE_va, DE_lab, "DE", "COMP", "VA", "LAB")
  DE_ls$year = lubridate::ymd(DE_ls$year, truncated = 2L)
  
  #Employment and productivty
  DE_ep = func_empprod(DE_emp, DE_gop,"DE", "EMP", "GO_P", F)
  DE_ep = func_empprod(DE_emp, DE_gop,"DE", "EMP", "GO_P", T)
  
  #PLM analyse
  DE_ind = func_regpanel(DE_ep, 1)
  DE_tot = func_regpanel(DE_ep, 3)
  
  #deskriptiv
  DE_b = func_regpanel(DE_ep, 2)


# Holland

  NL_emp <- read_excel("Data/NL_output_17ii.xlsx", sheet = "EMP") #Number of persons engaged (thoNLands)
  #NL_go <- read_excel("Data/NL_output_17ii.xlsx", sheet = "GO") #Gross Output at current basic prices (in millions of national currency)
  NL_gop <- read_excel("Data/NL_output_17ii.xlsx", sheet = "GO_P") #Gross Output at current basic prices (in millions of national currency)
  NL_comp <- read_excel("Data/NL_output_17ii.xlsx", sheet = "COMP") #Compensation of employees  (in millions of national currency)
  NL_lab <- read_excel("Data/NL_output_17ii.xlsx", sheet = "LAB") #Labour compensation (in millions of national currency)
  NL_va <- read_excel("Data/NL_output_17ii.xlsx", sheet = "VA") #Gross value added at current basic prices (in millions of national currency), svarer labour + capital compensation
  
  #Labour share
  NL_ls = func_labshare(NL_comp, NL_va, NL_lab, "NL", "COMP", "VA", "LAB")
  NL_ls$year = lubridate::ymd(NL_ls$year, truncated = 2L)
  
  #Employment and productivty
  NL_ep = func_empprod(NL_emp, NL_gop,"NL", "EMP", "GO_P", F)
  NL_ep = func_empprod(NL_emp, NL_gop,"NL", "EMP", "GO_P", T)
  
  #PLM analyse
  NL_ind = func_regpanel(NL_ep, 1)
  NL_tot = func_regpanel(NL_ep, 3)
  
  #deskriptiv
  NL_b = func_regpanel(NL_ep, 2)


# Sverige

  SE_emp <- read_excel("Data/SE_output_17ii.xlsx", sheet = "EMP") #Number of persons engaged (thousands)
  #SE_go <- read_excel("Data/SE_output_17ii.xlsx", sheet = "GO") #Gross Output at current basic prices (in millions of national currency)
  SE_gop <- read_excel("Data/SE_output_17ii.xlsx", sheet = "GO_P") #Gross Output at current basic prices (in millions of national currency)
  SE_comp <- read_excel("Data/SE_output_17ii.xlsx", sheet = "COMP") #Compensation of employees  (in millions of national currency)
  SE_lab <- read_excel("Data/SE_output_17ii.xlsx", sheet = "LAB") #Labour compensation (in millions of national currency)
  SE_va <- read_excel("Data/SE_output_17ii.xlsx", sheet = "VA") #Gross value added at current basic prices (in millions of national currency), svarer labour + capital compensation
  
  
  #Labour share
  SE_ls = func_labshare(SE_comp, SE_va, SE_lab, "SE", "COMP", "VA", "LAB")
  SE_ls$year = lubridate::ymd(SE_ls$year, truncated = 2L)
  
  #Employment and productivty
  SE_ep = func_empprod(SE_emp, SE_gop,"SE", "EMP", "GO_P", F)
  SE_ep = func_empprod(SE_emp, SE_gop,"SE", "EMP", "GO_P", T)
  
  #PLM analyse
  SE_ind = func_regpanel(SE_ep, 1)
  SE_tot = func_regpanel(SE_ep, 3)
  
  #deskriptiv
  SE_b = func_regpanel(SE_ep, 2)


# Østrig

  AT_emp = read_excel("Data/AT_output_17ii.xlsx", sheet = "EMP") #Number of persons engaged (thousands)
  #AT_go = read_excel("Data/AT_output_17ii.xlsx", sheet = "GO") #Gross Output at current basic prices (in millions of national currency)
  AT_gop = read_excel("Data/AT_output_17ii.xlsx", sheet = "GO_P") #Gross output, price indices, 2010 = 100
  AT_comp = read_excel("Data/AT_output_17ii.xlsx", sheet = "COMP") #Compensation of employees  (in millions of national currency)
  AT_lab = read_excel("Data/AT_output_17ii.xlsx", sheet = "LAB") #Labour compensation (in millions of national currency)
  AT_va = read_excel("Data/AT_output_17ii.xlsx", sheet = "VA") #Gross value added at current basic prices (in millions of national currency), svarer labour + capital compensation
  
  #Labour share
  AT_ls = func_labshare(AT_comp, AT_va, AT_lab, "AT", "COMP", "VA", "LAB")
  AT_ls$year = lubridate::ymd(AT_ls$year, truncated = 2L)
  
  #Employment and productivty
  AT_ep = func_empprod(AT_emp, AT_gop,"AT", "EMP", "GO_P", F)
  AT_ep = func_empprod(AT_emp, AT_gop,"AT", "EMP", "GO_P", T)
  
  #PLM analyse
  AT_ind = func_regpanel(AT_ep, 1)
  AT_tot = func_regpanel(AT_ep, 3)
  
  #deskriptiv
  AT_b = func_regpanel(AT_ep, 2)
  


# Belgium

  BE_emp = read_excel("Data/BE_output_17ii.xlsx", sheet = "EMP") #Number of persons engaged (thousands)
  #BE_go = read_excel("Data/BE_output_17ii.xlsx", sheet = "GO") #Gross Output at current basic prices (in millions of national currency)
  BE_gop = read_excel("Data/BE_output_17ii.xlsx", sheet = "GO_P") #Gross output, price indices, 2010 = 100
  BE_comp = read_excel("Data/BE_output_17ii.xlsx", sheet = "COMP") #Compensation of employees  (in millions of national currency)
  BE_lab = read_excel("Data/BE_output_17ii.xlsx", sheet = "LAB") #Labour compensation (in millions of national currency)
  BE_va = read_excel("Data/BE_output_17ii.xlsx", sheet = "VA") #Gross value added at current basic prices (in millions of national currency), svarer labour + capital compensation
  
  #Labour share
  BE_ls = func_labshare(BE_comp, BE_va, BE_lab, "BE", "COMP", "VA", "LAB")
  BE_ls$year = lubridate::ymd(BE_ls$year, truncated = 2L)
  
  #Employment and productivty
  BE_ep = func_empprod(BE_emp, BE_gop,"BE", "EMP", "GO_P", F)
  BE_ep = func_empprod(BE_emp, BE_gop,"BE", "EMP", "GO_P", T)
  
  #PLM analyse
  BE_ind = func_regpanel(BE_ep, 1)
  BE_tot = func_regpanel(BE_ep, 3)
  
  #deskriptiv
  BE_b = func_regpanel(BE_ep, 2)
  


# Tjekkiet

  CZ_emp = read_excel("Data/CZ_output_17ii.xlsx", sheet = "EMP") #Number of persons engaged (thousands)
  #CZ_go = read_excel("Data/CZ_output_17ii.xlsx", sheet = "GO") #Gross Output at current basic prices (in millions of national currency)
  CZ_gop = read_excel("Data/CZ_output_17ii.xlsx", sheet = "GO_P") #Gross output, price indices, 2010 = 100
  CZ_comp = read_excel("Data/CZ_output_17ii.xlsx", sheet = "COMP") #Compensation of employees  (in millions of national currency)
  CZ_lab = read_excel("Data/CZ_output_17ii.xlsx", sheet = "LAB") #Labour compensation (in millions of national currency)
  CZ_va = read_excel("Data/CZ_output_17ii.xlsx", sheet = "VA") #Gross value added at current basic prices (in millions of national currency), svarer labour + capital compensation
  
  #Labour share
  CZ_ls = func_labshare(CZ_comp, CZ_va, CZ_lab, "CZ", "COMP", "VA", "LAB")
  CZ_ls$year = lubridate::ymd(CZ_ls$year, truncated = 2L)
  
  #Employment and productivty
  CZ_ep = func_empprod(CZ_emp, CZ_gop,"CZ", "EMP", "GO_P", F)
  CZ_ep = func_empprod(CZ_emp, CZ_gop,"CZ", "EMP", "GO_P", T)
  
  #PLM analyse
  CZ_ind = func_regpanel(CZ_ep, 1)
  CZ_tot = func_regpanel(CZ_ep, 3)
  
  #deskriptiv
  CZ_b = func_regpanel(CZ_ep, 2)
  


# Finland

  FI_emp = read_excel("Data/FI_output_17ii.xlsx", sheet = "EMP") #Number of persons engaged (thousands)
  #FI_go = read_excel("Data/FI_output_17ii.xlsx", sheet = "GO") #Gross Output at current basic prices (in millions of national currency)
  FI_gop = read_excel("Data/FI_output_17ii.xlsx", sheet = "GO_P") #Gross output, price indices, 2010 = 100
  FI_comp = read_excel("Data/FI_output_17ii.xlsx", sheet = "COMP") #Compensation of employees  (in millions of national currency)
  FI_lab = read_excel("Data/FI_output_17ii.xlsx", sheet = "LAB") #Labour compensation (in millions of national currency)
  FI_va = read_excel("Data/FI_output_17ii.xlsx", sheet = "VA") #Gross value added at current basic prices (in millions of national currency), svarer labour + capital compensation
  
  #Labour share
  FI_ls = func_labshare(FI_comp, FI_va, FI_lab, "FI", "COMP", "VA", "LAB")
  FI_ls$year = lubridate::ymd(FI_ls$year, truncated = 2L)
  
  #Employment and productivty
  FI_ep = func_empprod(FI_emp, FI_gop,"FI", "EMP", "GO_P", F)
  FI_ep = func_empprod(FI_emp, FI_gop,"FI", "EMP", "GO_P", T)
  
  #PLM analyse
  FI_ind = func_regpanel(FI_ep, 1)
  FI_tot = func_regpanel(FI_ep, 3)
  
  #deskriptiv
  FI_b = func_regpanel(FI_ep, 2)


# Frankrig

  FR_emp = read_excel("Data/FR_output_17ii.xlsx", sheet = "EMP") #Number of persons engaged (thousands)
  #FR_go = read_excel("Data/FR_output_17ii.xlsx", sheet = "GO") #Gross Output at current basic prices (in millions of national currency)
  FR_gop = read_excel("Data/FR_output_17ii.xlsx", sheet = "GO_P") #Gross output, price indices, 2010 = 100
  FR_comp = read_excel("Data/FR_output_17ii.xlsx", sheet = "COMP") #Compensation of employees  (in millions of national currency)
  FR_lab = read_excel("Data/FR_output_17ii.xlsx", sheet = "LAB") #Labour compensation (in millions of national currency)
  FR_va = read_excel("Data/FR_output_17ii.xlsx", sheet = "VA") #Gross value added at current basic prices (in millions of national currency), svarer labour + capital compensation
  
  #Labour share
  FR_ls = func_labshare(FR_comp, FR_va, FR_lab, "FR", "COMP", "VA", "LAB")
  FR_ls$year = lubridate::ymd(FR_ls$year, truncated = 2L)
  
  #Employment and productivty
  FR_ep = func_empprod(FR_emp, FR_gop,"FR", "EMP", "GO_P", F)
  FR_ep = func_empprod(FR_emp, FR_gop,"FR", "EMP", "GO_P", T)
  
  #PLM analyse
  FR_ind = func_regpanel(FR_ep, 1)
  FR_tot = func_regpanel(FR_ep, 3)
  
  #deskriptiv
  FR_b = func_regpanel(FR_ep, 2)
  


# Grækenland

  EL_emp = read_excel("Data/EL_output_17ii.xlsx", sheet = "EMP") #Number of persons engaged (thousands)
  #EL_go = read_excel("Data/EL_output_17ii.xlsx", sheet = "GO") #Gross Output at current basic prices (in millions of national currency)
  EL_gop = read_excel("Data/EL_output_17ii.xlsx", sheet = "GO_P") #Gross output, price indices, 2010 = 100
  EL_comp = read_excel("Data/EL_output_17ii.xlsx", sheet = "COMP") #Compensation of employees  (in millions of national currency)
  EL_lab = read_excel("Data/EL_output_17ii.xlsx", sheet = "LAB") #Labour compensation (in millions of national currency)
  EL_va = read_excel("Data/EL_output_17ii.xlsx", sheet = "VA") #Gross value added at current basic prices (in millions of national currency), svarer labour + capital compensation
  
  #Labour share
  EL_ls = func_labshare(EL_comp, EL_va, EL_lab, "EL", "COMP", "VA", "LAB")
  EL_ls$year = lubridate::ymd(EL_ls$year, truncated = 2L)
  
  #Employment and productivty
  EL_ep = func_empprod(EL_emp, EL_gop,"EL", "EMP", "GO_P", F)
  EL_ep = func_empprod(EL_emp, EL_gop,"EL", "EMP", "GO_P", T)
  
  #PLM analyse
  EL_ind = func_regpanel(EL_ep, 1)
  EL_tot = func_regpanel(EL_ep, 3)
  
  #deskriptiv
  EL_b = func_regpanel(EL_ep, 2)


# Italien

  IT_emp = read_excel("Data/IT_output_17ii.xlsx", sheet = "EMP") #Number of persons engaged (thousands)
  #IT_go = read_excel("Data/IT_output_17ii.xlsx", sheet = "GO") #Gross Output at current basic prices (in millions of national currency)
  IT_gop = read_excel("Data/IT_output_17ii.xlsx", sheet = "GO_P") #Gross output, price indices, 2010 = 100
  IT_comp = read_excel("Data/IT_output_17ii.xlsx", sheet = "COMP") #Compensation of employees  (in millions of national currency)
  IT_lab = read_excel("Data/IT_output_17ii.xlsx", sheet = "LAB") #Labour compensation (in millions of national currency)
  IT_va = read_excel("Data/IT_output_17ii.xlsx", sheet = "VA") #Gross value added at current basic prices (in millions of national currency), svarer labour + capital compensation
  
  #Labour share
  IT_ls = func_labshare(IT_comp, IT_va, IT_lab, "IT", "COMP", "VA", "LAB")
  IT_ls$year = lubridate::ymd(IT_ls$year, truncated = 2L)
  
  #Employment and productivty
  IT_ep = func_empprod(IT_emp, IT_gop,"IT", "EMP", "GO_P", F)
  IT_ep = func_empprod(IT_emp, IT_gop,"IT", "EMP", "GO_P", T)
  
  #PLM analyse
  IT_ind = func_regpanel(IT_ep, 1)
  IT_tot = func_regpanel(IT_ep, 3)
  
  #deskriptiv
  IT_b = func_regpanel(IT_ep, 2)
  


# Letland

  LV_emp = read_excel("Data/LV_output_17ii.xlsx", sheet = "EMP") #Number of persons engaged (thousands)
  #LV_go = read_excel("Data/LV_output_17ii.xlsx", sheet = "GO") #Gross Output at current basic prices (in millions of national currency)
  LV_gop = read_excel("Data/LV_output_17ii.xlsx", sheet = "GO_P") #Gross output, price indices, 2010 = 100
  LV_comp = read_excel("Data/LV_output_17ii.xlsx", sheet = "COMP") #Compensation of employees  (in millions of national currency)
  LV_lab = read_excel("Data/LV_output_17ii.xlsx", sheet = "LAB") #Labour compensation (in millions of national currency)
  LV_va = read_excel("Data/LV_output_17ii.xlsx", sheet = "VA") #Gross value added at current basic prices (in millions of national currency), svarer labour + capital compensation
  
  #Labour share
  LV_ls = func_labshare(LV_comp, LV_va, LV_lab, "LV", "COMP", "VA", "LAB")
  LV_ls$year = lubridate::ymd(LV_ls$year, truncated = 2L)
  
  #Employment and productivty
  LV_ep = func_empprod(LV_emp, LV_gop,"LV", "EMP", "GO_P", F)
  LV_ep = func_empprod(LV_emp, LV_gop,"LV", "EMP", "GO_P", T)
  
  #PLM analyse
  LV_ind = func_regpanel(LV_ep, 1)
  LV_tot = func_regpanel(LV_ep, 3)
  
  #deskriptiv
  LV_b = func_regpanel(LV_ep, 2) 


# Luxenborg

  LU_emp = read_excel("Data/LU_output_17ii.xlsx", sheet = "EMP") #Number of persons engaged (thousands)
  #LU_go = read_excel("Data/LU_output_17ii.xlsx", sheet = "GO") #Gross Output at current basic prices (in millions of national currency)
  LU_gop = read_excel("Data/LU_output_17ii.xlsx", sheet = "GO_P") #Gross output, price indices, 2010 = 100
  LU_comp = read_excel("Data/LU_output_17ii.xlsx", sheet = "COMP") #Compensation of employees  (in millions of national currency)
  LU_lab = read_excel("Data/LU_output_17ii.xlsx", sheet = "LAB") #Labour compensation (in millions of national currency)
  LU_va = read_excel("Data/LU_output_17ii.xlsx", sheet = "VA") #Gross value added at current basic prices (in millions of national currency), svarer labour + capital compensation
  
  #Labour share
  LU_ls = func_labshare(LU_comp, LU_va, LU_lab, "LU", "COMP", "VA", "LAB")
  LU_ls$year = lubridate::ymd(LU_ls$year, truncated = 2L)
  
  #Employment and productivty
  LU_ep = func_empprod(LU_emp, LU_gop,"LU", "EMP", "GO_P", F)
  LU_ep = func_empprod(LU_emp, LU_gop,"LU", "EMP", "GO_P", T)
  
  #PLM analyse
  LU_ind = func_regpanel(LU_ep, 1)
  LU_tot = func_regpanel(LU_ep, 3)
  
  #deskriptiv
  LU_b = func_regpanel(LU_ep, 2)



# Slovakiet

  SK_emp = read_excel("Data/SK_output_17ii.xlsx", sheet = "EMP") #Number of persons engaged (thousands)
  #SK_go = read_excel("Data/SK_output_17ii.xlsx", sheet = "GO") #Gross Output at current basic prices (in millions of national currency)
  SK_gop = read_excel("Data/SK_output_17ii.xlsx", sheet = "GO_P") #Gross output, price indices, 2010 = 100
  SK_comp = read_excel("Data/SK_output_17ii.xlsx", sheet = "COMP") #Compensation of employees  (in millions of national currency)
  SK_lab = read_excel("Data/SK_output_17ii.xlsx", sheet = "LAB") #Labour compensation (in millions of national currency)
  SK_va = read_excel("Data/SK_output_17ii.xlsx", sheet = "VA") #Gross value added at current basic prices (in millions of national currency), svarer labour + capital compensation
  
  #Labour share
  SK_ls = func_labshare(SK_comp, SK_va, SK_lab, "SK", "COMP", "VA", "LAB")
  SK_ls$year = lubridate::ymd(SK_ls$year, truncated = 2L)
  
  #Employment and productivty
  SK_ep = func_empprod(SK_emp, SK_gop,"SK", "EMP", "GO_P", F)
  SK_ep = func_empprod(SK_emp, SK_gop,"SK", "EMP", "GO_P", T)
  
  #PLM analyse
  SK_ind = func_regpanel(SK_ep, 1)
  SK_tot = func_regpanel(SK_ep, 3)
  
  #deskriptiv
  SK_b = func_regpanel(SK_ep, 2)



# Slovenien

  SI_emp = read_excel("Data/SI_output_17ii.xlsx", sheet = "EMP") #Number of persons engaged (thousands)
  #SI_go = read_excel("Data/SI_output_17ii.xlsx", sheet = "GO") #Gross Output at current basic prices (in millions of national currency)
  SI_gop = read_excel("Data/SI_output_17ii.xlsx", sheet = "GO_P") #Gross output, price indices, 2010 = 100
  SI_comp = read_excel("Data/SI_output_17ii.xlsx", sheet = "COMP") #Compensation of employees  (in millions of national currency)
  SI_lab = read_excel("Data/SI_output_17ii.xlsx", sheet = "LAB") #Labour compensation (in millions of national currency)
  SI_va = read_excel("Data/SI_output_17ii.xlsx", sheet = "VA") #Gross value added at current basic prices (in millions of national currency), svarer labour + capital compensation
  
  #Labour share
  SI_ls = func_labshare(SI_comp, SI_va, SI_lab, "SI", "COMP", "VA", "LAB")
  SI_ls$year = lubridate::ymd(SI_ls$year, truncated = 2L)
  
  #Employment and productivty
  SI_ep = func_empprod(SI_emp, SI_gop,"SI", "EMP", "GO_P", F)
  SI_ep = func_empprod(SI_emp, SI_gop,"SI", "EMP", "GO_P", T)
  
  #PLM analyse
  SI_ind = func_regpanel(SI_ep, 1)
  SI_tot = func_regpanel(SI_ep, 3)
  
  #deskriptiv
  SI_b = func_regpanel(SI_ep, 2)


# Descriptive -------------------------------------------------------------

min <- as.Date("1975-1-1")
max <- NA

# DESCRIPTIVE - Labour share of national income
{
  
#Aggregate Labour Share of National Invome, Total Labour Force
  
{ggplot(DK_ls, aes(year, LS)) + 
  geom_point() + 
  geom_line() +
  geom_smooth(method = "lm") +
  xlab("Time") + ylab("") +
  ggtitle("Aggregate Labour Share of National Income, Total Labour Force") +
  guides(colour=guide_legend(title="Branche")) +
  theme_economist() +
  theme(legend.position="right") +
  scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
  scale_x_date(limits = c(min, max))
  } #DK

#Aggregate Labour Share of National Income, Employees

  {ggplot(DK_ls, aes(year, LSe)) + 
    geom_point() + 
    geom_line() +
    geom_smooth(method = "lm") +
    xlab("Time") + ylab("") +
    ggtitle("Aggregate Labour Share of National Income, Employees") +
    guides(colour=guide_legend(title="Branche")) +
    theme_economist() +
    theme(legend.position="right") +
    scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
    scale_x_date(limits = c(min, max))
} #DK
  {ggplot(US_ls, aes(year, LSe)) + 
      geom_point() + 
      geom_line() +
      geom_smooth(method = "lm") +
      xlab("Time") + ylab("") +
      ggtitle("USA - Aggregate Labour Share of National Income, Employees") +
      guides(colour=guide_legend(title="Branche")) +
      theme_economist() +
      theme(legend.position="right") +
      scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
      scale_x_date(limits = c(min, max))
  } #US
  {ggplot(UK_ls, aes(year, LSe)) + 
      geom_point() + 
      geom_line() +
      geom_smooth(method = "lm") +
      xlab("Time") + ylab("") +
      ggtitle("Storbritannien - Aggregate Labour Share of National Income, Employees") +
      guides(colour=guide_legend(title="Branche")) +
      theme_economist() +
      theme(legend.position="right") +
      scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
      scale_x_date(limits = c(min, max))
  } #UK
  {ggplot(GE_ls, aes(year, LSe)) + 
      geom_point() + 
      geom_line() +
      geom_smooth(method = "lm") +
      xlab("Time") + ylab("") +
      ggtitle("Tyskland - Aggregate Labour Share of National Income, Employees") +
      guides(colour=guide_legend(title="Branche")) +
      theme_economist() +
      theme(legend.position="right") +
      scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
      scale_x_date(limits = c(min, max))
  } #GE
  {ggplot(NL_ls, aes(year, LSe)) + 
      geom_point() + 
      geom_line() +
      geom_smooth(method = "lm") +
      xlab("Time") + ylab("") +
      ggtitle("Holland - Aggregate Labour Share of National Income, Employees") +
      guides(colour=guide_legend(title="Branche")) +
      theme_economist() +
      theme(legend.position="right") +
      scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
      scale_x_date(limits = c(min, max))
  } #NL
  {ggplot(SE_ls, aes(year, LSe)) + 
      geom_point() + 
      geom_line() +
      geom_smooth(method = "lm") +
      xlab("Time") + ylab("") +
      ggtitle("Sverige - Aggregate Labour Share of National Income, Employees") +
      guides(colour=guide_legend(title="Branche")) +
      theme_economist() +
      theme(legend.position="right") +
      scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
      scale_x_date(limits = c(min, max))
  } #SE

  
#Aggregate Labour Share of National Income, comparison
{ggplot(data = DK_ls) + 
    geom_line(aes(x = year, y = LS, color = "LS"),) +
    geom_line(aes(x = year, y = LSe, color = "LSe"),) +
    scale_color_manual(name = "Colors", values = c("LS" = "blue", "LSe" = "red")) +
    xlab("Time") + ylab("") +
    ggtitle("Aggregate Labour Share of National Income, comparison") +
    guides(colour=guide_legend(title="")) +
    theme_economist() +
    theme(legend.position="right") +
    scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
    scale_x_date(limits = c(min, max))
  } #DK

#Indekstal
{ggplot(data = DK_ls) + 
      geom_line(aes(x = year, y = indeksLS, color = "LS"),) +
      geom_line(aes(x = year, y = indeksLSe, color = "LSe"),) +
      scale_color_manual(name = "Colors", values = c("LS" = "blue", "LSe" = "red")) +
      xlab("Time") + ylab("") +
      ggtitle("Indekstal, 1975=100") +
      guides(colour=guide_legend(title="")) +
      theme_economist() +
      theme(legend.position="right") +
      scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
      scale_x_date(limits = c(min, max))
  } #DK
  
#Cumulative growth of Labour Share of National Income, comparison
{ggplot(data = DK_ls) + 
  geom_line(aes(x = year, y = LS_CGR, color = "LS"),) +
  geom_line(aes(x = year, y = LSe_CGR, color = "LSe"),) +
  scale_color_manual(name = "Colors", values = c("LS" = "blue", "LSe" = "red")) +
  xlab("Time") + ylab("") +
  ggtitle("") +
  guides(colour=guide_legend(title="")) +
  theme_economist() +
  theme(legend.position="right") +
  scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
  scale_x_date(limits = c(min, max))
} #DK

  }

# DESCRIPTIVE - Sectoral employment and productivty
{

#Branchebeskæftigelse
{ggplot(data=DK_b, aes(x=year, y=EMP, group=desc, colour=desc)) + 
  geom_point() + 
  geom_line() +
  xlab("Time") + ylab("Number of persons engaged in work (thousands)") +
  ggtitle("Employment by Sector") +
  guides(colour=guide_legend(title="Branche")) +
  theme_economist() +
  theme(legend.position="right") +
  #scale_x_date(date_labels = "%Y") +
  scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
  scale_x_date(limits = c(min, max))
  #theme(axis.text.x = element_text(angle = 90, hjust = 1))
  #scale_color_economist()
} #DK
  
#Produktivitet- og beskæftigelsesvækst
  
{ggplot(data = DK_tot) + 
  geom_line(aes(x = year, y = emp_logchanges, color = "emp_logchanges"),) +
  geom_line(aes(x = year, y = prod_logchanges, color = "prod_logchanges"),) +
  scale_color_manual(name = "Colors", values = c("emp_logchanges" = "blue", "prod_logchanges" = "red")) +
  xlab("Time") + ylab("") +
  ggtitle("Produktivitet- og beskæftigelsesvækst i DK") +
  guides(colour=guide_legend(title="")) +
  theme_economist() +
  theme(legend.position="right") +
  scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
  scale_x_date(limits = c(min, max))
  
  #In order to get a legend, you have to map something to color within aes. 
  #You can then use scale_color_manual to define the colors for the mapped character values. 
  
} #DK
{ggplot(data = UK_tot) + 
      geom_line(aes(x = year, y = emp_logchanges, color = "emp_logchanges"),) +
      geom_line(aes(x = year, y = prod_logchanges, color = "prod_logchanges"),) +
      scale_color_manual(name = "Colors", values = c("emp_logchanges" = "blue", "prod_logchanges" = "red")) +
      xlab("Time") + ylab("") +
      ggtitle("Produktivitet- og beskæftigelsesvækst i UK") +
      guides(colour=guide_legend(title="")) +
      theme_economist() +
      theme(legend.position="right")
      #scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
      #scale_x_date(limits = c(min, max))
  } #UK
  

#Beskæftigelsesvækst fordelt på brancher
{ggplot(data=DK_b, aes(x=year, y=emp_logchanges, group=desc, colour=desc)) +
  geom_point() + 
    geom_line() +
    xlab("Time") + ylab("") +
    ggtitle("Beskæftigelsesvækst fordelt på brancher") +
    guides(colour=guide_legend(title="Sector")) +
    theme_economist() +
    theme(legend.position="right") +
    scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
    scale_x_date(limits = c(min, max))
} #DK
  
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

c_panel = rbind(DK_tot, SE_tot, US_tot, NL_tot, DE_tot, AT_tot, BE_tot, CZ_tot, EL_tot, FI_tot, FR_tot, 
                IT_tot, LU_tot, LV_tot, SI_tot, SK_tot)
c_panel = na.omit(c_panel)

model_linear1 = emp_logchanges ~ prod_logchanges 

C0_pool = plm(model_linear1, data = c_panel, index = c("country", "year"), model = "pooling")
C0_fd = plm(model_linear1, data = c_panel, index = c("country", "year"), model = "fd")
C0_fe = plm(model_linear1, data = c_panel, index = c("country", "year"), model = "within")
summary(C0_pool)
summary(C0_fd)
summary(C0_fe)

c_panel = rbind(DK_tot, SE_tot, US_tot, NL_tot, DE_tot, AT_tot, BE_tot, CZ_tot, EL_tot, FI_tot, FR_tot, 
                IT_tot, LU_tot, LV_tot, SI_tot, SK_tot)

c_panel = pdata.frame(c_panel, index = c("country", "year"))
c_panel$prod_logchanges_lag1 = lag(c_panel$prod_logchanges, k = 1, shift = "time")
c_panel$prod_logchanges_lag2 = lag(c_panel$prod_logchanges_lag1, k = 1, shift = "time")
c_panel$prod_logchanges_lag3 = lag(c_panel$prod_logchanges_lag2, k = 1, shift = "time")
c_panel = na.omit(c_panel)

model_linear2 = emp_logchanges ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3
fixed.dum = lm(emp_logchanges ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + factor(country) + factor(year), data=c_panel)
summary(fixed.dum)

C2_pool = plm(model_linear2, data = c_panel, index = c("country", "year"), model = "pooling")
C2_fd = plm(model_linear2, data = c_panel, index = c("country", "year"), model = "fd")
C2_fe = plm(model_linear2, data = c_panel, index = c("country", "year"), model = "within", effect = "individual")
C2_fe_tw = plm(model_linear2, data = c_panel, index = c("country", "year"), model = "within", effect = "twoway")
summary(C2_pool)
summary(C2_fd)
summary(C2_fe)
summary(C2_fe_tw)



# Country industry panel --------------------------------------------------

ci_panel = rbind(DK_ind, SE_ind, US_ind, NL_ind, DE_ind, AT_ind, BE_ind, CZ_ind, EL_ind, FI_ind, FR_ind, IT_ind , LU_ind, SI_ind, SK_ind) #, LV_ind)
ci_panel = ci_panel %>% select(year, country, code, desc, emp_logchanges, prod_logchanges, wgt)
ci_panel$id = ci_panel %>% group_indices(code, country)
ci_panel$prod_logchanges_wgt = ci_panel$prod_logchanges*ci_panel$wgt
ci_panel$emp_logchanges_wgt = ci_panel$emp_logchanges*ci_panel$wgt
ci_panel = na.omit(ci_panel) #obs vigtigt at køre efter unødvendige variable er fjernet

model_linear1 = emp_logchanges_wgt ~ prod_logchanges_wgt
model_linear1 = emp_logchanges ~ prod_logchanges

ci.reg <- plm(model_linear1, data = ci_panel, index = c("id", "year"), model = "within")

summary(ci.reg)

fixed.dum = lm(emp_logchanges ~ prod_logchanges, data=ci_panel)

fixed.dum = lm(emp_logchanges_wgt ~ prod_logchanges_wgt, data=ci_panel)
fixed.dum = lm(emp_logchanges_wgt ~ prod_logchanges_wgt + factor(country), data=ci_panel)
fixed.dum = lm(emp_logchanges_wgt ~ prod_logchanges_wgt + factor(country) + factor(code), data=ci_panel)
fixed.dum = lm(emp_logchanges_wgt ~ prod_logchanges_wgt + factor(country) + factor(code) + factor(year), data=ci_panel)
fixed.dum = lm(emp_logchanges_wgt ~ prod_logchanges_wgt + factor(code) + factor(year), data=ci_panel)
fixed.dum = lm(emp_logchanges_wgt ~ prod_logchanges_wgt + factor(country) + factor(year), data=ci_panel)

summary(fixed.dum)

#options(digits = 3)
#pols = coeftest(poolOLS, vcov. = vcovHC, method = "arellano")

#FE-modeller
FixedEffects_indi <- plm(model_linear1, data = dk, index = c("code", "year"), weight=wgt, model = "within", effect = "individual")
FixedEffects_time <- plm(model_linear1, data = dk, index = c("code", "year"), weight=wgt, model = "within", effect = "time")
FixedEffects_twoway <- plm(model_linear1, data = dk, index = c("code", "year"), weight=wgt, model = "within", effect = "twoway")

summary(FixedEffects_indi)
summary(FixedEffects_time)
summary(FixedEffects_twoway)


options(digits = 3)
options("scipen"=100, "digits"=4)
coeftest(FixedEffects_indi, vcov. = vcovHC, type = "HC1")
fe = coeftest(FixedEffects_indi, vcov. = vcovHC, method = "arellano")
attributes(Arellano)

Arellano

# Sammensætning af mikro og makroelasticiteter --------------------------------------------------

#hvad gør vi med lande hvor nogle industrier mangler?

sum_prod_yc <- ci_panel %>% group_by(year, country) %>% count(sum(prod_logchanges_wgt))
ci_panel = merge(ci_panel, sum_prod_yc, by=c( "year", "country"), all.x = TRUE)
ci_panel$avgLP_oi = (ci_panel$`sum(prod_logchanges_wgt)` - ci_panel$prod_logchanges_wgt)/(ci_panel$n - 1) #bør det vægtes?



ci_panel = pdata.frame(ci_panel, index = c("id", "year"))
ci_panel$avgLP_oi_lag1 = lag(ci_panel$avgLP_oi, k = 1, shift = "time")
ci_panel$avgLP_oi_lag2 = lag(ci_panel$avgLP_oi, k = 2, shift = "time")
ci_panel$avgLP_oi_lag3 = lag(ci_panel$avgLP_oi, k = 3, shift = "time")
ci_panel = na.omit(ci_panel)


#model_linear2 = emp_logchanges ~ prod_logchanges + avgLP_oi + avgLP_oi_lag1 + avgLP_oi_lag2 + avgLP_oi_lag3

fixed.dum = lm(emp_logchanges_wgt ~ prod_logchanges_wgt + avgLP_oi + avgLP_oi_lag1 + avgLP_oi_lag2 + avgLP_oi_lag3  + factor(country) + factor(code) + factor(year), data=ci_panel)
fixed.dum = lm(emp_logchanges_wgt ~ prod_logchanges_wgt + avgLP_oi + avgLP_oi_lag1 + avgLP_oi_lag2 + avgLP_oi_lag3  + factor(country) + factor(year), data=ci_panel)
fixed.dum = lm(emp_logchanges_wgt ~ prod_logchanges_wgt + avgLP_oi + avgLP_oi_lag1 + avgLP_oi_lag2 + avgLP_oi_lag3  + factor(country) , data=ci_panel)
fixed.dum = lm(emp_logchanges_wgt ~ prod_logchanges_wgt + avgLP_oi + avgLP_oi_lag1 + avgLP_oi_lag2 + avgLP_oi_lag3 , data=ci_panel)

summary(fixed.dum)


# Sector spillover --------------------------------------------------

ci_panel_ss = rbind(DK_ind, SE_ind, US_ind, NL_ind, DE_ind, AT_ind, BE_ind, CZ_ind, EL_ind, FI_ind, FR_ind, IT_ind , LU_ind, SI_ind, SK_ind) #, LV_ind)
ci_panel_ss = ci_panel %>% select(year, country, code, branche, desc, emp_logchanges, prod_logchanges, wgt, prod_logchanges_b1,
                                  prod_logchanges_b2, prod_logchanges_b3, prod_logchanges_b4, prod_logchanges_b5, prod_logchanges_b6, 
                                  prod_logchanges_b7, prod_logchanges_b8)
ci_panel_ss$id = ci_panel_ss %>% group_indices(code, country)
ci_panel_ss$prod_logchanges_wgt = ci_panel_ss$prod_logchanges*ci_panel$wgt
ci_panel_ss$emp_logchanges_wgt = ci_panel_ss$emp_logchanges*ci_panel$wgt
ci_panel_ss = na.omit(ci_panel_ss) #obs vigtigt at køre efter unødvendige variable er fjernet

sum_prod_yc_1 <- ci_panel_ss %>% group_by(year, country, branche) %>% count(sum(prod_logchanges))


ci_panel_ny = merge(ci_panel_ss, sum_prod_yc_1, by=c( "year", "country", "branche"), all.x = TRUE)

b1 = ci_panel_ny %>% filter(branche=="b1")



ci_panel_ny$avgLP_c1 = (b1$`sum(prod_logchanges)` - b1$prod_logchanges)/(b1$n - 1)


# Skills..... --------------------------------------------------





# TIME SERIES - Import and preparation of data --------------------------------------------------------------------

EMP <- read_excel("DK_output_17ii.xlsx", sheet = "EMP_2")
GO <- read_excel("DK_output_17ii.xlsx", sheet = "GO_2")
GO_QI <- read_excel("DK_output_17ii.xlsx", sheet = "GO_QI_2")

data <- data.frame(emp_tot = EMP$TOT,
                 emp_markt = EMP$MARKT,
                 emp = (EMP$TOT-EMP$A-EMP$O-EMP$T),
                 go_tot = GO$TOT,
                 go_markt = GO$MARKT,
                 go = (GO$TOT-GO$A-GO$O-GO$T),
                 goqi_tot=GO_QI$TOT,
                 goqi_markt=GO_QI$MARKT,
                 go_real = (GO_QI$TOT-GO_QI$A-GO_QI$O-GO_QI$T))

data$emp_log <- log(data$emp)
data$emp_diff <- diff.xts(data$emp)
data$emp_ldiff <- diff.xts(data$emp_log)
data$emp_changes <- data$emp_diff/lag.xts(data$emp,1)*100
data$emp_lchanges <- (data$emp_ldiff/lag.xts(data$emp_log,1))*100

data <- data %>% mutate(prod=go/emp)
data$prod_log <- log(data$prod)
data$prod_diff <- diff.xts(data$prod)
data$prod_ldiff <- diff.xts(data$prod_log)
data$prod_changes <- data$prod_diff/lag.xts(data$prod,1)*100
data$prod_lchanges <- data$prod_ldiff/lag.xts(data$prod_log,1)*100

data.ts <- ts(data, start=1975)
data.ts <- na.omit(data.ts)

autoplot(data.ts[,c("prod_lchanges","prod_changes")])
autoplot(data.ts[,c("emp_lchanges","emp_changes")])

autoplot(data.ts[,c("prod_lchanges","emp_lchanges")])
autoplot(data.ts[,c("prod_changes","emp_changes")])

reg_dk <- lm(emp_lchanges~ prod_lchanges+lag(prod_lchanges,1)+lag(prod_lchanges,2)+lag(prod_lchanges,3), data = data.ts)
reg_dk2 <- lm(emp_lchanges~ prod_lchanges, data = data.ts)

summary(reg_dk)
summary(reg_dk2)







