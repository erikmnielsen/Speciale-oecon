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
country="US"

dataset_1 <- read_excel("Data/US_output_17ii.xlsx", sheet = "EMP") #Number of persons engaged (thousands)
dataset_2 <- read_excel("Data/US_output_17ii.xlsx", sheet = "GO_P")
  
dataset_1 <- read_excel("Data/DK_output_17ii.xlsx", sheet = "EMP")
dataset_2 <- read_excel("Data/DK_output_17ii.xlsx", sheet = "GO")
dataset_2 <- read_excel("Data/DK_output_17ii.xlsx", sheet = "GO_P") #Gross output, price indices, 2010 = 100

measure_1="EMP"
measure_2="GO_P"

dataset_1 <- read_excel("DK_output_17ii.xlsx", sheet = "COMP")
dataset_2 <- read_excel("DK_output_17ii.xlsx", sheet = "VA")
dataset_3 <- read_excel("DK_output_17ii.xlsx", sheet = "LAB")
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

func_empprod <- function(dataset_1, dataset_2, country, measure_1="EMP", measure_2="GO") {
  
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
  
  
  #AutorSalomons Industrier:
  
  {
  data$sel_industries <-factor(ifelse( data$code %in% c("TOT", "MARKT", "A","C","G","H","J","OtU","O","R","S","T","U"), 0,1))
  
  
  data$sector <- ifelse(data$code %in% c("B", "DtE", "F"), "s1",
                         ifelse(data$code %in% c("10t12", "13t15", "16t18", "19", "20t21", "22t23","24t25", "26t27", "28", "29t30","31t33"), "s2", #kan man ikke bare bruge C, Total Manufacturing?
                                ifelse(data$code %in% c("P","Q","RtS"), "s3",
                                       ifelse(data$code %in% c("53", "58t60", "61", "62t63", "K", "MtN"), "s4",
                                              ifelse(data$code %in% c("45", "46", "47", "49t52", "I", "L"), "s5",
                                                     "s0")))))
  
  data$sector_desc <- ifelse(data$sector=="s1","Mining, utilities, and construction", 
                             ifelse(data$sector=="s2","Manufacturing", 
                                    ifelse(data$sector=="s3","Education and health services", 
                                           ifelse(data$sector=="s4","High-tech services",
                                                  ifelse(data$sector=="s5","Low-tech services",
                                                         "Not relevant"
                                                  )))))
  
  }
  
  #Brancher, 10:
  {data$branche <- ifelse(data$code=="A", "b1",
                                ifelse(data$code %in% c("B","C", "DtE"), "b2",
                                       ifelse(data$code=="F", "b3",
                                              ifelse(data$code %in% c("G","H", "I"), "b4",
                                                     ifelse(data$code=="J", "b5",
                                                            ifelse(data$code=="K", "b6",
                                                                   ifelse(data$code=="L", "b7",
                                                                          ifelse(data$code=="MtN", "b8",
                                                                                 ifelse(data$code %in% c("O","P","Q"), "b9",
                                                                                        ifelse(data$code %in% c("RtS","T","U"), "b10",
                                                                                               "b0"))))))))))
  
  data$branche_desc <- ifelse(data$branche=="b1","Landbrug, skovbrug og fiskeri",
                              ifelse(data$branche=="b2","Industri, råstofindvinding og forsyningsvirksomhed",
                                     ifelse(data$branche=="b3","Bygge og anlæg", 
                                            ifelse(data$branche=="b4","Handel og transport mv.", 
                                                   ifelse(data$branche=="b5","Information og kommunikation",
                                                          ifelse(data$branche=="b6", "Finansiering og forsikring",
                                                                 ifelse(data$branche=="b7","Ejendomshandel og udlejning",
                                                                        ifelse(data$branche=="b8","Erhvervsservice",
                                                                               ifelse(data$branche=="b9","Offentlig administration, undervisning og sundhed",
                                                                                      ifelse(data$branche=="b10","Kultur, fritid og anden service",
                                                                                             "Ikke relevant"))))))))))
  }
  

  #angivelse af branche/industri totaler
   t2 <- data %>% filter(sector!=0) %>% group_by(year,sector, sector_desc) %>% summarize(EMP=sum(EMP),GO=sum(GO))
   
   
   t3 <- data %>% filter(branche!=0) %>% group_by(year, branche, branche_desc) %>% summarize(EMP=sum(EMP),GO=sum(GO))

   t4=rbind(t2,t3)
   
   
   
  data2 <- data.frame(desc=ifelse(is.na(t4$sector_desc)==T, t4$branche_desc, t4$sector_desc),
                      code=ifelse(is.na(t4$sector)==T, t4$branche, t4$sector),
                      year=t4$year,
                      EMP=t4$EMP,
                      GO=t4$GO,
                      country=country,
                      sel_industries=0,
                      sector=ifelse(is.na(t4$sector)==T, "s0", "s-tot"),
                      sector_desc=ifelse(is.na(t4$sector)==T, "Not relevant", "Sector Total"),
                      branche=ifelse(is.na(t4$sector)==T, "b-tot", "b0"),
                      branche_desc=ifelse(is.na(t4$sector)==T, "Branche Total", "Ikke Relevant"))
  
  
  #udregning af lande total, hvis visse brancher udelades (fx landbrug, offentlig sektor)
  {
  # Totaler ala Autor Salomons
  
  #t <- data %>% filter(code=="TOT")
  #t_A <- data %>% filter(code=="A")
  #t_O <- data %>% filter(code=="O")
  #t_T <- data %>% filter(code=="T")
  
  #t$EMP = t$EMP -t_A$EMP - t_O$EMP - t_T$EMP
  #t$desc = "TOTAL INDUSTRIES-AutorSalomons"
  #t$code = "TOT_AS"
  
  #Nedenstående skal bruges til hvis vores udvalgte brancher adskiller sig fra "TOTAL INDUSTRIES"
  
  #b <- data2 %>% filter(code=="b1")
  #b_2 <- data2 %>% filter(code=="b2")
  #b_3 <- data2 %>% filter(code=="b3")
  #b_4 <- data2 %>% filter(code=="b4")
  #b_5 <- data2 %>% filter(code=="b5")
 # b_6 <- data2 %>% filter(code=="b6")
  #b_7 <- data2 %>% filter(code=="b7")
  #b_8 <- data2 %>% filter(code=="b8")
  #b_9 <- data2 %>% filter(code=="b9")
  #b_10 <- data2 %>% filter(code=="b10")
  
  #b$EMP = b$EMP + b_2$EMP + b_3$EMP + b_4$EMP + b_5$EMP + b_6$EMP + b_7$EMP + b_8$EMP + b_9$EMP + b_10$EMP
  #b$desc = "TOTAL INDUSTRIES-MunkNielsen"
  #b$code = "TOT_MN"
  }
  
  #data_fin <- rbind(data, data2, t)
  data_fin <- rbind(data, data2)
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
  
  pdata = pdata %>% group_by(code) %>% mutate(prod_CGR_logchanges =  order_by(year,cumprod(1+prod_logdiff[-1])*100)) #metode 1
  pdata = pdata %>% group_by(code) %>% mutate(prod_logCGR = order_by(year, CGR(prod_logdiff[-1])*100)) #metode 2
  
  pdata = pdata %>% group_by(code) %>% mutate(prod_CGR= order_by(year, CGR(prod_changes2[-1])*100))
  #df = pdata %>% group_by(code) %>% mutate(cumsum = cumsum())
  
  pdata <- pdata %>% select(year, country, code, desc, sel_industries, sector, sector_desc, branche, branche_desc, EMP, emp_logchanges, GO, prod, prod_logchanges,prod_changes, prod_CGR, prod_logCGR, prod_CGR_logchanges) %>% 
    filter(code!="b0",code!="s0")
  
  pdata = pdata.frame(pdata, index = c("code", "year"))
  
  pdata$prod_logCGR <- lag(pdata$prod_logCGR, k=1, shift="time")
  pdata$prod_CGR <- lag(pdata$prod_CGR, k=1, shift="time")

  pdata
}

#key_table <- read_csv("EUklems-data-master/key_table.csv")

# Country data -----------------------------------------------------------------------

# Danmark
{
  DK_emp <- read_excel("Data/DK_output_17ii.xlsx", sheet = "EMP") #Number of persons engaged (thousands)
  #DK_go <- read_excel("Data/DK_output_17ii.xlsx", sheet = "GO") #Gross Output at current basic prices (in millions of national currency)
  DK_gop <- read_excel("Data/DK_output_17ii.xlsx", sheet = "GO_P") #Gross output, price indices, 2010 = 100
  DK_comp <- read_excel("Data/DK_output_17ii.xlsx", sheet = "COMP") #Compensation of employees  (in millions of national currency)
  DK_lab <- read_excel("Data/DK_output_17ii.xlsx", sheet = "LAB") #Labour compensation (in millions of national currency)
  DK_va <- read_excel("Data/DK_output_17ii.xlsx", sheet = "VA") #Gross value added at current basic prices (in millions of national currency), svarer labour + capital compensation
  
  #Labour share
  DK_ls = func_labshare(DK_comp, DK_va, DK_lab, "DK", "COMP", "VA", "LAB")
  DK_ls$year = lubridate::ymd(DK_ls$year, truncated = 2L)
  
  #Employment and productivty
  DK_ep = func_empprod(DK_emp, DK_gop,"DK", "EMP", "GO_P")
  
  #PLM analyse
  DK_tot <- DK_ep %>% filter(code=="TOT")
  DK_tot$TOT = DK_tot$EMP
  DK_tot <- DK_tot %>% select(year, country, TOT)
  
  #dk <- DK_ep %>% filter(branche=="b-tot", year!="1975")
  dk <- DK_ep %>% filter(branche=="b-tot")
  dk = merge(dk,DK_tot, by=c("year", "country"), all.x = TRUE)
  dk <- na.omit(dk)
  
  dk$wgt = dk$EMP/dk$TOT
  
  #deskriptiv
  DK_b = DK_ep %>% filter(branche=="b-tot")
  {
    sumEMP <- DK_b %>% group_by(year) %>% summarize(sum_EMP=sum(EMP))
    DK_b = merge(DK_b, sumEMP, by=c("year"), all.x = TRUE)
    DK_b$share_EMP = (DK_b$EMP/DK_b$sum_EMP)*100
    DK_b = pdata.frame(DK_b, index = c("code", "year"))
    DK_b$share_EMP_ppchange = diff(DK_b$share_EMP, lag = 1, shift = "time")
    DK_b$share_EMP_ppchange = ifelse(is.na(DK_b$share_EMP_ppchange)==T,0,DK_b$share_EMP_ppchange)
    DK_b = DK_b %>% group_by(code) %>% mutate(cumsum_EMP = cumsum(share_EMP_ppchange))
  }
  DK_b$year = lubridate::ymd(DK_b$year, truncated = 2L)
  
  DK_tot = DK_ep %>% filter(code=="TOT")
  DK_tot$year = lubridate::ymd(DK_tot$year, truncated = 2L)
  
  
}

  # USA
  {
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
    US_ep = func_empprod(US_emp, US_gop,"US", "EMP", "GO_P")
    
    #PLM analyse
    US_tot <- US_ep %>% filter(code=="TOT")
    US_tot$TOT = US_tot$EMP
    US_tot <- US_tot %>% select(year, country, TOT)
    
    #US <- US_ep %>% filter(branche=="b-tot", year!="1975")
    US <- US_ep %>% filter(branche=="b-tot")
    US = merge(US,US_tot, by=c("year", "country"), all.x = TRUE)
    US <- na.omit(US)
    
    US$wgt = US$EMP/US$TOT
    
    #deskriptiv
    US_b = US_ep %>% filter(branche=="b-tot")
    {
      sumEMP <- US_b %>% group_by(year) %>% summarize(sum_EMP=sum(EMP))
      US_b = merge(US_b, sumEMP, by=c("year"), all.x = TRUE)
      US_b$share_EMP = (US_b$EMP/US_b$sum_EMP)*100
      US_b = pdata.frame(US_b, index = c("code", "year"))
      US_b$share_EMP_ppchange = diff(US_b$share_EMP, lag = 1, shift = "time")
      US_b$share_EMP_ppchange = ifelse(is.na(US_b$share_EMP_ppchange)==T,0,US_b$share_EMP_ppchange)
      US_b = US_b %>% group_by(code) %>% mutate(cumsum_EMP = cumsum(share_EMP_ppchange))
    }
    US_b$year = lubridate::ymd(US_b$year, truncated = 2L)
    
    US_tot = US_ep %>% filter(code=="TOT")
    US_tot$year = lubridate::ymd(US_tot$year, truncated = 2L)
    
  }
  
  # UK . faste priser findes ikke
  {
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
    UK_ep = func_empprod(UK_emp, UK_gop,"UK", "EMP", "GO_P")
    
    #PLM analyse
    UK_tot <- UK_ep %>% filter(code=="TOT")
    UK_tot$TOT = UK_tot$EMP
    UK_tot <- UK_tot %>% select(year, country, TOT)
    
    UK <- UK_ep %>% filter(branche=="b-tot", year!="1975")
    UK = merge(UK,UK_tot, by=c("year", "country"), all.x = TRUE)
    
    UK$wgt = UK$EMP/UK$TOT
    
    #deskriptiv
    UK_b = UK_ep %>% filter(branche=="b-tot")
    {
      sumEMP <- UK_b %>% group_by(year) %>% summarize(sum_EMP=sum(EMP))
      UK_b = merge(UK_b, sumEMP, by=c("year"), all.x = TRUE)
      UK_b$share_EMP = (UK_b$EMP/UK_b$sum_EMP)*100
      UK_b = pdata.frame(UK_b, index = c("code", "year"))
      UK_b$share_EMP_ppchange = diff(UK_b$share_EMP, lag = 1, shift = "time")
      UK_b$share_EMP_ppchange = ifelse(is.na(UK_b$share_EMP_ppchange)==T,0,UK_b$share_EMP_ppchange)
      UK_b = UK_b %>% group_by(code) %>% mutate(cumsum_EMP = cumsum(share_EMP_ppchange))
    }
    UK_b$year = lubridate::ymd(UK_b$year, truncated = 2L)
    
    UK_tot = UK_ep %>% filter(code=="TOT")
    UK_tot$year = lubridate::ymd(UK_tot$year, truncated = 2L)
  }
  
  # Tyskland
  {
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
    DE_ep = func_empprod(DE_emp, DE_gop,"DE", "EMP", "GO_P")
    
    #PLM analyse
    DE_tot <- DE_ep %>% filter(code=="TOT")
    DE_tot$TOT = DE_tot$EMP
    DE_tot <- DE_tot %>% select(year, country, TOT)
    
    #DE <- DE_ep %>% filter(branche=="b-tot", year!="1975")
    DE <- DE_ep %>% filter(branche=="b-tot")
    DE = merge(DE,DE_tot, by=c("year", "country"), all.x = TRUE)
    DE <- na.omit(DE)
    
    DE$wgt = DE$EMP/DE$TOT
    
    #deskriptiv
    DE_b = DE_ep %>% filter(branche=="b-tot")
    {
      sumEMP <- DE_b %>% group_by(year) %>% summarize(sum_EMP=sum(EMP))
      DE_b = merge(DE_b, sumEMP, by=c("year"), all.x = TRUE)
      DE_b$share_EMP = (DE_b$EMP/DE_b$sum_EMP)*100
      DE_b = pdata.frame(DE_b, index = c("code", "year"))
      DE_b$share_EMP_ppchange = diff(DE_b$share_EMP, lag = 1, shift = "time")
      DE_b$share_EMP_ppchange = ifelse(is.na(DE_b$share_EMP_ppchange)==T,0,DE_b$share_EMP_ppchange)
      DE_b = DE_b %>% group_by(code) %>% mutate(cumsum_EMP = cumsum(share_EMP_ppchange))
    }
    DE_b$year = lubridate::ymd(DE_b$year, truncated = 2L)
    
    DE_tot = DE_ep %>% filter(code=="TOT")
    DE_tot$year = lubridate::ymd(DE_tot$year, truncated = 2L)
  }
  
  # Holland
  {
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
    NL_ep = func_empprod(NL_emp, NL_gop,"NL", "EMP", "GO_P")
    
    #PLM analyse
    NL_tot <- NL_ep %>% filter(code=="TOT")
    NL_tot$TOT = NL_tot$EMP
    NL_tot <- NL_tot %>% select(year, country, TOT)
    
    NL <- NL_ep %>% filter(branche=="b-tot", year!="1975")
    NL <- NL_ep %>% filter(branche=="b-tot")
    NL = merge(NL,NL_tot, by=c("year", "country"), all.x = TRUE)
    NL = na.omit(NL)
    
    NL$wgt = NL$EMP/NL$TOT
    
    #deskriptiv
    NL_b = NL_ep %>% filter(branche=="b-tot")
    {
      sumEMP <- NL_b %>% group_by(year) %>% summarize(sum_EMP=sum(EMP))
      NL_b = merge(NL_b, sumEMP, by=c("year"), all.x = TRUE)
      NL_b$share_EMP = (NL_b$EMP/NL_b$sum_EMP)*100
      NL_b = pdata.frame(NL_b, index = c("code", "year"))
      NL_b$share_EMP_ppchange = diff(NL_b$share_EMP, lag = 1, shift = "time")
      NL_b$share_EMP_ppchange = ifelse(is.na(NL_b$share_EMP_ppchange)==T,0,NL_b$share_EMP_ppchange)
      NL_b = NL_b %>% group_by(code) %>% mutate(cumsum_EMP = cumsum(share_EMP_ppchange))
    }
    NL_b$year = lubridate::ymd(NL_b$year, truncated = 2L)
    
    NL_tot = NL_ep %>% filter(code=="TOT")
    NL_tot$year = lubridate::ymd(NL_tot$year, truncated = 2L)
  }
  
  # Sverige
  {
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
    SE_ep = func_empprod(SE_emp, SE_gop,"SE", "EMP", "GO_P")
    
    #PLM analyse
    SE_tot <- SE_ep %>% filter(code=="TOT")
    SE_tot$TOT = SE_tot$EMP
    SE_tot <- SE_tot %>% select(year, country, TOT)
    
    #SE <- SE_ep %>% filter(branche=="b-tot", year!="1975")
    SE <- SE_ep %>% filter(branche=="b-tot")
    SE = merge(SE,SE_tot, by=c("year", "country"), all.x = TRUE)
    SE = na.omit(SE)
    
    SE$wgt = SE$EMP/SE$TOT
    
    #deskriptiv
    SE_b = SE_ep %>% filter(branche=="b-tot")
    {
      sumEMP <- SE_b %>% group_by(year) %>% summarize(sum_EMP=sum(EMP))
      SE_b = merge(SE_b, sumEMP, by=c("year"), all.x = TRUE)
      SE_b$share_EMP = (SE_b$EMP/SE_b$sum_EMP)*100
      SE_b = pdata.frame(SE_b, index = c("code", "year"))
      SE_b$share_EMP_ppchange = diff(SE_b$share_EMP, lag = 1, shift = "time")
      SE_b$share_EMP_ppchange = ifelse(is.na(SE_b$share_EMP_ppchange)==T,0,SE_b$share_EMP_ppchange)
      SE_b = SE_b %>% group_by(code) %>% mutate(cumsum_EMP = cumsum(share_EMP_ppchange))
    }
    SE_b$year = lubridate::ymd(SE_b$year, truncated = 2L)
    
    SE_tot = SE_ep %>% filter(code=="TOT")
    SE_tot$year = lubridate::ymd(SE_tot$year, truncated = 2L)
    
  }
 
# Østrig
{
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
  AT_ep = func_empprod(AT_emp, AT_gop,"AT", "EMP", "GO_P")
  
  #PLM analyse
  AT_tot = AT_ep %>% filter(code=="TOT")
  AT_tot$TOT = AT_tot$EMP
  AT_tot = AT_tot %>% select(year, country, TOT)
  
  #AT = AT_ep %>% filter(branche=="b-tot", year!="1975")
  AT = AT_ep %>% filter(branche=="b-tot")
  AT = merge(AT,AT_tot, by=c("year", "country"), all.x = TRUE)
  AT = na.omit(AT)
  
  AT$wgt = AT$EMP/AT$TOT
  
  #deskriptiv
  AT_b = AT_ep %>% filter(branche=="b-tot")
  {
    sumEMP = AT_b %>% group_by(year) %>% summarize(sum_EMP=sum(EMP))
    AT_b = merge(AT_b, sumEMP, by=c("year"), all.x = TRUE)
    AT_b$share_EMP = (AT_b$EMP/AT_b$sum_EMP)*100
    AT_b = pdata.frame(AT_b, index = c("code", "year"))
    AT_b$share_EMP_ppchange = diff(AT_b$share_EMP, lag = 1, shift = "time")
    AT_b$share_EMP_ppchange = ifelse(is.na(AT_b$share_EMP_ppchange)==T,0,AT_b$share_EMP_ppchange)
    AT_b = AT_b %>% group_by(code) %>% mutate(cumsum_EMP = cumsum(share_EMP_ppchange))
  }
  AT_b$year = lubridate::ymd(AT_b$year, truncated = 2L)
  
  AT_tot = AT_ep %>% filter(code=="TOT")
  AT_tot$year = lubridate::ymd(AT_tot$year, truncated = 2L)
  
  
} 

# Belgium
{
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
  BE_ep = func_empprod(BE_emp, BE_gop,"BE", "EMP", "GO_P")
  
  #PLM analyse
  BE_tot = BE_ep %>% filter(code=="TOT")
  BE_tot$TOT = BE_tot$EMP
  BE_tot = BE_tot %>% select(year, country, TOT)
  
  #BE = BE_ep %>% filter(branche=="b-tot", year!="1975")
  BE = BE_ep %>% filter(branche=="b-tot")
  BE = merge(BE,BE_tot, by=c("year", "country"), all.x = TRUE)
  BE = na.omit(BE)
  
  BE$wgt = BE$EMP/BE$TOT
  
  #deskriptiv
  BE_b = BE_ep %>% filter(branche=="b-tot")
  {
    sumEMP = BE_b %>% group_by(year) %>% summarize(sum_EMP=sum(EMP))
    BE_b = merge(BE_b, sumEMP, by=c("year"), all.x = TRUE)
    BE_b$share_EMP = (BE_b$EMP/BE_b$sum_EMP)*100
    BE_b = pdata.frame(BE_b, index = c("code", "year"))
    BE_b$share_EMP_ppchange = diff(BE_b$share_EMP, lag = 1, shift = "time")
    BE_b$share_EMP_ppchange = ifelse(is.na(BE_b$share_EMP_ppchange)==T,0,BE_b$share_EMP_ppchange)
    BE_b = BE_b %>% group_by(code) %>% mutate(cumsum_EMP = cumsum(share_EMP_ppchange))
  }
  BE_b$year = lubridate::ymd(BE_b$year, truncated = 2L)
  
  BE_tot = BE_ep %>% filter(code=="TOT")
  BE_tot$year = lubridate::ymd(BE_tot$year, truncated = 2L)
  
  
}

# Tjekkiet
{
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
  CZ_ep = func_empprod(CZ_emp, CZ_gop,"CZ", "EMP", "GO_P")
  
  #PLM analyse
  CZ_tot = CZ_ep %>% filter(code=="TOT")
  CZ_tot$TOT = CZ_tot$EMP
  CZ_tot = CZ_tot %>% select(year, country, TOT)
  
  #CZ = CZ_ep %>% filter(branche=="b-tot", year!="1975")
  CZ = CZ_ep %>% filter(branche=="b-tot")
  CZ = merge(CZ,CZ_tot, by=c("year", "country"), all.x = TRUE)
  CZ = na.omit(CZ)
  
  CZ$wgt = CZ$EMP/CZ$TOT
  
  #deskriptiv
  CZ_b = CZ_ep %>% filter(branche=="b-tot")
  {
    sumEMP = CZ_b %>% group_by(year) %>% summarize(sum_EMP=sum(EMP))
    CZ_b = merge(CZ_b, sumEMP, by=c("year"), all.x = TRUE)
    CZ_b$share_EMP = (CZ_b$EMP/CZ_b$sum_EMP)*100
    CZ_b = pdata.frame(CZ_b, index = c("code", "year"))
    CZ_b$share_EMP_ppchange = diff(CZ_b$share_EMP, lag = 1, shift = "time")
    CZ_b$share_EMP_ppchange = ifelse(is.na(CZ_b$share_EMP_ppchange)==T,0,CZ_b$share_EMP_ppchange)
    CZ_b = CZ_b %>% group_by(code) %>% mutate(cumsum_EMP = cumsum(share_EMP_ppchange))
  }
  CZ_b$year = lubridate::ymd(CZ_b$year, truncated = 2L)
  
  CZ_tot = CZ_ep %>% filter(code=="TOT")
  CZ_tot$year = lubridate::ymd(CZ_tot$year, truncated = 2L)
  
  
}

# Finland
{
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
  FI_ep = func_empprod(FI_emp, FI_gop,"FI", "EMP", "GO_P")
  
  #PLM analyse
  FI_tot = FI_ep %>% filter(code=="TOT")
  FI_tot$TOT = FI_tot$EMP
  FI_tot = FI_tot %>% select(year, country, TOT)
  
  #FI = FI_ep %>% filter(branche=="b-tot", year!="1975")
  FI = FI_ep %>% filter(branche=="b-tot")
  FI = merge(FI,FI_tot, by=c("year", "country"), all.x = TRUE)
  FI = na.omit(FI)
  
  FI$wgt = FI$EMP/FI$TOT
  
  #deskriptiv
  FI_b = FI_ep %>% filter(branche=="b-tot")
  {
    sumEMP = FI_b %>% group_by(year) %>% summarize(sum_EMP=sum(EMP))
    FI_b = merge(FI_b, sumEMP, by=c("year"), all.x = TRUE)
    FI_b$share_EMP = (FI_b$EMP/FI_b$sum_EMP)*100
    FI_b = pdata.frame(FI_b, index = c("code", "year"))
    FI_b$share_EMP_ppchange = diff(FI_b$share_EMP, lag = 1, shift = "time")
    FI_b$share_EMP_ppchange = ifelse(is.na(FI_b$share_EMP_ppchange)==T,0,FI_b$share_EMP_ppchange)
    FI_b = FI_b %>% group_by(code) %>% mutate(cumsum_EMP = cumsum(share_EMP_ppchange))
  }
  FI_b$year = lubridate::ymd(FI_b$year, truncated = 2L)
  
  FI_tot = FI_ep %>% filter(code=="TOT")
  FI_tot$year = lubridate::ymd(FI_tot$year, truncated = 2L)
  
  
}

# Frankrig
{
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
  FR_ep = func_empprod(FR_emp, FR_gop,"FR", "EMP", "GO_P")
  
  #PLM analyse
  FR_tot = FR_ep %>% filter(code=="TOT")
  FR_tot$TOT = FR_tot$EMP
  FR_tot = FR_tot %>% select(year, country, TOT)
  
  #FR = FR_ep %>% filter(branche=="b-tot", year!="1975")
  FR = FR_ep %>% filter(branche=="b-tot")
  FR = merge(FR,FR_tot, by=c("year", "country"), all.x = TRUE)
  FR = na.omit(FR)
  
  FR$wgt = FR$EMP/FR$TOT
  
  #deskriptiv
  FR_b = FR_ep %>% filter(branche=="b-tot")
  {
    sumEMP = FR_b %>% group_by(year) %>% summarize(sum_EMP=sum(EMP))
    FR_b = merge(FR_b, sumEMP, by=c("year"), all.x = TRUE)
    FR_b$share_EMP = (FR_b$EMP/FR_b$sum_EMP)*100
    FR_b = pdata.frame(FR_b, index = c("code", "year"))
    FR_b$share_EMP_ppchange = diff(FR_b$share_EMP, lag = 1, shift = "time")
    FR_b$share_EMP_ppchange = ifelse(is.na(FR_b$share_EMP_ppchange)==T,0,FI_b$share_EMP_ppchange)
    FR_b = FR_b %>% group_by(code) %>% mutate(cumsum_EMP = cumsum(share_EMP_ppchange))
  }
  FR_b$year = lubridate::ymd(FR_b$year, truncated = 2L)
  
  FR_tot = FR_ep %>% filter(code=="TOT")
  FR_tot$year = lubridate::ymd(FR_tot$year, truncated = 2L)
  
  
}

# Grækenland
{
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
  EL_ep = func_empprod(EL_emp, EL_gop,"EL", "EMP", "GO_P")
  
  #PLM analyse
  EL_tot = EL_ep %>% filter(code=="TOT")
  EL_tot$TOT = EL_tot$EMP
  EL_tot = EL_tot %>% select(year, country, TOT)
  
  #EL = EL_ep %>% filter(branche=="b-tot", year!="1975")
  EL = EL_ep %>% filter(branche=="b-tot")
  EL = merge(EL,EL_tot, by=c("year", "country"), all.x = TRUE)
  EL = na.omit(EL)
  
  EL$wgt = EL$EMP/EL$TOT
  
  #deskriptiv
  EL_b = EL_ep %>% filter(branche=="b-tot")
  {
    sumEMP = EL_b %>% group_by(year) %>% summarize(sum_EMP=sum(EMP))
    EL_b = merge(EL_b, sumEMP, by=c("year"), all.x = TRUE)
    EL_b$share_EMP = (EL_b$EMP/EL_b$sum_EMP)*100
    EL_b = pdata.frame(EL_b, index = c("code", "year"))
    EL_b$share_EMP_ppchange = diff(EL_b$share_EMP, lag = 1, shift = "time")
    EL_b$share_EMP_ppchange = ifelse(is.na(EL_b$share_EMP_ppchange)==T,0,EL_b$share_EMP_ppchange)
    EL_b = EL_b %>% group_by(code) %>% mutate(cumsum_EMP = cumsum(share_EMP_ppchange))
  }
  EL_b$year = lubridate::ymd(EL_b$year, truncated = 2L)
  
  EL_tot = EL_ep %>% filter(code=="TOT")
  EL_tot$year = lubridate::ymd(EL_tot$year, truncated = 2L)
  
  
}

# Italien
{
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
  IT_ep = func_empprod(IT_emp, IT_gop,"IT", "EMP", "GO_P")
  
  #PLM analyse
  IT_tot = IT_ep %>% filter(code=="TOT")
  IT_tot$TOT = IT_tot$EMP
  IT_tot = IT_tot %>% select(year, country, TOT)
  
  #IT = IT_ep %>% filter(branche=="b-tot", year!="1975")
  IT = IT_ep %>% filter(branche=="b-tot")
  IT = merge(IT,IT_tot, by=c("year", "country"), all.x = TRUE)
  IT = na.omit(IT)
  
  IT$wgt = IT$EMP/IT$TOT
  
  #deskriptiv
  IT_b = IT_ep %>% filter(branche=="b-tot")
  {
    sumEMP = IT_b %>% group_by(year) %>% summarize(sum_EMP=sum(EMP))
    IT_b = merge(IT_b, sumEMP, by=c("year"), all.x = TRUE)
    IT_b$share_EMP = (IT_b$EMP/IT_b$sum_EMP)*100
    IT_b = pdata.frame(IT_b, index = c("code", "year"))
    IT_b$share_EMP_ppchange = diff(IT_b$share_EMP, lag = 1, shift = "time")
    IT_b$share_EMP_ppchange = ifelse(is.na(IT_b$share_EMP_ppchange)==T,0,IT_b$share_EMP_ppchange)
    IT_b = IT_b %>% group_by(code) %>% mutate(cumsum_EMP = cumsum(share_EMP_ppchange))
  }
  IT_b$year = lubridate::ymd(IT_b$year, truncated = 2L)
  
  IT_tot = IT_ep %>% filter(code=="TOT")
  IT_tot$year = lubridate::ymd(IT_tot$year, truncated = 2L)
  
  
}

# Letland
{
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
  LV_ep = func_empprod(LV_emp, LV_gop,"LV", "EMP", "GO_P")
  
  #PLM analyse
  LV_tot = LV_ep %>% filter(code=="TOT")
  LV_tot$TOT = LV_tot$EMP
  LV_tot = LV_tot %>% select(year, country, TOT)
  
  #LV = LV_ep %>% filter(branche=="b-tot", year!="1975")
  LV = LV_ep %>% filter(branche=="b-tot")
  LV = merge(LV,LV_tot, by=c("year", "country"), all.x = TRUE)
  LV = na.omit(LV)
  
  LV$wgt = LV$EMP/LV$TOT
  
  #deskriptiv
  LV_b = LV_ep %>% filter(branche=="b-tot")
  {
    sumEMP = LV_b %>% group_by(year) %>% summarize(sum_EMP=sum(EMP))
    LV_b = merge(LV_b, sumEMP, by=c("year"), all.x = TRUE)
    LV_b$share_EMP = (LV_b$EMP/LV_b$sum_EMP)*100
    LV_b = pdata.frame(LV_b, index = c("code", "year"))
    LV_b$share_EMP_ppchange = diff(LV_b$share_EMP, lag = 1, shift = "time")
    LV_b$share_EMP_ppchange = ifelse(is.na(LV_b$share_EMP_ppchange)==T,0,LV_b$share_EMP_ppchange)
    LV_b = LV_b %>% group_by(code) %>% mutate(cumsum_EMP = cumsum(share_EMP_ppchange))
  }
  LV_b$year = lubridate::ymd(LV_b$year, truncated = 2L)
  
  LV_tot = LV_ep %>% filter(code=="TOT")
  LV_tot$year = lubridate::ymd(LV_tot$year, truncated = 2L)
  
  
}

# Luxenborg
{
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
  LU_ep = func_empprod(LU_emp, LU_gop,"LU", "EMP", "GO_P")
  
  #PLM analyse
  LU_tot = LU_ep %>% filter(code=="TOT")
  LU_tot$TOT = LU_tot$EMP
  LU_tot = LU_tot %>% select(year, country, TOT)
  
  #LU = LU_ep %>% filter(branche=="b-tot", year!="1975")
  LU = LU_ep %>% filter(branche=="b-tot")
  LU = merge(LU,LU_tot, by=c("year", "country"), all.x = TRUE)
  LU = na.omit(LU)
  
  LU$wgt = LU$EMP/LU$TOT
  
  #deskriptiv
  LU_b = LU_ep %>% filter(branche=="b-tot")
  {
    sumEMP = LU_b %>% group_by(year) %>% summarize(sum_EMP=sum(EMP))
    LU_b = merge(LU_b, sumEMP, by=c("year"), all.x = TRUE)
    LU_b$share_EMP = (LU_b$EMP/LU_b$sum_EMP)*100
    LU_b = pdata.frame(LU_b, index = c("code", "year"))
    LU_b$share_EMP_ppchange = diff(LU_b$share_EMP, lag = 1, shift = "time")
    LU_b$share_EMP_ppchange = ifelse(is.na(LU_b$share_EMP_ppchange)==T,0,LU_b$share_EMP_ppchange)
    LU_b = LU_b %>% group_by(code) %>% mutate(cumsum_EMP = cumsum(share_EMP_ppchange))
  }
  LU_b$year = lubridate::ymd(LU_b$year, truncated = 2L)
  
  LU_tot = LU_ep %>% filter(code=="TOT")
  LU_tot$year = lubridate::ymd(LU_tot$year, truncated = 2L)
  
  
}

# Slovakiet
{
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
  SK_ep = func_empprod(SK_emp, SK_gop,"SK", "EMP", "GO_P")
  
  #PLM analyse
  SK_tot = SK_ep %>% filter(code=="TOT")
  SK_tot$TOT = SK_tot$EMP
  SK_tot = SK_tot %>% select(year, country, TOT)
  
  #SK = SK_ep %>% filter(branche=="b-tot", year!="1975")
  SK = SK_ep %>% filter(branche=="b-tot")
  SK = merge(SK,SK_tot, by=c("year", "country"), all.x = TRUE)
  SK = na.omit(SK)
  
  SK$wgt = SK$EMP/SK$TOT
  
  #deskriptiv
  SK_b = SK_ep %>% filter(branche=="b-tot")
  {
    sumEMP = SK_b %>% group_by(year) %>% summarize(sum_EMP=sum(EMP))
    SK_b = merge(SK_b, sumEMP, by=c("year"), all.x = TRUE)
    SK_b$share_EMP = (SK_b$EMP/SK_b$sum_EMP)*100
    SK_b = pdata.frame(SK_b, index = c("code", "year"))
    SK_b$share_EMP_ppchange = diff(SK_b$share_EMP, lag = 1, shift = "time")
    SK_b$share_EMP_ppchange = ifelse(is.na(SK_b$share_EMP_ppchange)==T,0,SK_b$share_EMP_ppchange)
    SK_b = SK_b %>% group_by(code) %>% mutate(cumsum_EMP = cumsum(share_EMP_ppchange))
  }
  SK_b$year = lubridate::ymd(SK_b$year, truncated = 2L)
  
  SK_tot = SK_ep %>% filter(code=="TOT")
  SK_tot$year = lubridate::ymd(SK_tot$year, truncated = 2L)
  
  
}


# Slovenien
{
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
  SI_ep = func_empprod(SI_emp, SI_gop,"SI", "EMP", "GO_P")
  
  #PLM analyse
  SI_tot = SI_ep %>% filter(code=="TOT")
  SI_tot$TOT = SI_tot$EMP
  SI_tot = SI_tot %>% select(year, country, TOT)
  
  #SI = SI_ep %>% filter(branche=="b-tot", year!="1975")
  SI = SI_ep %>% filter(branche=="b-tot")
  SI = merge(SI,SI_tot, by=c("year", "country"), all.x = TRUE)
  SI = na.omit(SI)
  
  SI$wgt = SI$EMP/SI$TOT
  
  #deskriptiv
  SI_b = SI_ep %>% filter(branche=="b-tot")
  {
    sumEMP = SI_b %>% group_by(year) %>% summarize(sum_EMP=sum(EMP))
    SI_b = merge(SI_b, sumEMP, by=c("year"), all.x = TRUE)
    SI_b$share_EMP = (SI_b$EMP/SI_b$sum_EMP)*100
    SI_b = pdata.frame(SI_b, index = c("code", "year"))
    SI_b$share_EMP_ppchange = diff(SI_b$share_EMP, lag = 1, shift = "time")
    SI_b$share_EMP_ppchange = ifelse(is.na(SI_b$share_EMP_ppchange)==T,0,SI_b$share_EMP_ppchange)
    SI_b = SI_b %>% group_by(code) %>% mutate(cumsum_EMP = cumsum(share_EMP_ppchange))
  }
  SI_b$year = lubridate::ymd(SI_b$year, truncated = 2L)
  
  SI_tot = SI_ep %>% filter(code=="TOT")
  SI_tot$year = lubridate::ymd(SI_tot$year, truncated = 2L)
  
  
}


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
ci_panel = rbind(dk, SE, US, NL, DE, AT, BE, CZ, EL, FI, FR,  IT, LU, LV, SI, SK)
ci_panel = ci_panel %>% select(c(year, country, code, desc, emp_logchanges, prod_logchanges, wgt))
ci_panel$id = ci_panel %>% group_indices(code, country)

ci_panel$prod_logchanges_wgt = ci_panel$prod_logchanges*ci_panel$wgt
ci_panel$emp_logchanges_wgt = ci_panel$emp_logchanges*ci_panel$wgt

model_linear1 = emp_logchanges_wgt ~ prod_logchanges_wgt

ci.reg <- plm(model_linear1, data = ci_panel, index = c("id", "year"), model = "within")

summary(ci.reg)

fixed.dum = lm(emp_logchanges_wgt ~ prod_logchanges_wgt, data=ci_panel)
fixed.dum = lm(emp_logchanges_wgt ~ prod_logchanges_wgt + factor(code) + factor(country), data=ci_panel)
fixed.dum = lm(emp_logchanges_wgt ~ prod_logchanges_wgt  + factor(code) + factor(country) + factor(year), data=ci_panel)
fixed.dum = lm(emp_logchanges_wgt ~ prod_logchanges_wgt  + factor(code) + factor(year), data=ci_panel)
fixed.dum = lm(emp_logchanges_wgt ~ prod_logchanges_wgt + factor(country) + factor(year), data=ci_panel)
fixed.dum = lm(emp_logchanges_wgt ~ prod_logchanges_wgt + factor(country), data=ci_panel)
summary(fixed.dum)

a = table(index(ci_panel), useNA = "ifany")

table(a)
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

ci_panel = rbind(dk, SE, US, NL, DE, AT, BE, CZ, EL, FI, FR, IT, LU, LV, SI, SK)
ci_panel = ci_panel %>% select(c(year, country, code, desc, emp_logchanges, prod_logchanges, wgt))
ci_panel$id = ci_panel %>% group_indices(code, country)

#hvad gør vi med lande hvor nogle industrier mangler?

sum_prod_yc <- ci_panel %>% group_by(year, country) %>% count(sum(prod_logchanges))
ci_panel = merge(ci_panel, sum_prod_yc, by=c( "year", "country"), all.x = TRUE)
ci_panel$avgLP_oi = (ci_panel$`sum(prod_logchanges)` - ci_panel$prod_logchanges)/(ci_panel$n - 1) #bør det vægtes

ci_panel = pdata.frame(ci_panel, index = c("id", "year"))
ci_panel$avgLP_oi_lag1 = lag(ci_panel$avgLP_oi, k = 1, shift = "time")
ci_panel$avgLP_oi_lag2 = lag(ci_panel$avgLP_oi, k = 2, shift = "time")
ci_panel$avgLP_oi_lag3 = lag(ci_panel$avgLP_oi, k = 3, shift = "time")
ci_panel = na.omit(ci_panel)


model_linear2 = emp_logchanges ~ prod_logchanges + avgLP_oi + avgLP_oi_lag1 + avgLP_oi_lag2 + avgLP_oi_lag3

fixed.dum = lm(emp_logchanges ~ prod_logchanges + avgLP_oi + avgLP_oi_lag1 + avgLP_oi_lag2 + avgLP_oi_lag3  + factor(country) + factor(code) + factor(year), data=ci_panel)
fixed.dum = lm(emp_logchanges ~ prod_logchanges + avgLP_oi + avgLP_oi_lag1 + avgLP_oi_lag2 + avgLP_oi_lag3  + factor(country) + factor(year), data=ci_panel)
fixed.dum = lm(emp_logchanges ~ prod_logchanges + avgLP_oi + avgLP_oi_lag1 + avgLP_oi_lag2 + avgLP_oi_lag3  + factor(country) , data=ci_panel)
fixed.dum = lm(emp_logchanges ~ prod_logchanges + avgLP_oi + avgLP_oi_lag1 + avgLP_oi_lag2 + avgLP_oi_lag3 , data=ci_panel)

summary(fixed.dum)


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







