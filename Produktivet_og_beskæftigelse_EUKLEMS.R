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
  
  dk <- DK_ep %>% filter(branche=="b-tot", year!="1975")
  dk = merge(dk,tot, by=c("year", "country"), all.x = TRUE)
  
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
    
    US <- US_ep %>% filter(branche=="b-tot", year!="1975")
    US = merge(US,tot, by=c("year", "country"), all.x = TRUE)
    
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
  
  # UK
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
    UK = merge(UK,tot, by=c("year", "country"), all.x = TRUE)
    
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
    
    DE <- DE_ep %>% filter(branche=="b-tot", year!="1975")
    DE = merge(DE,tot, by=c("year", "country"), all.x = TRUE)
    
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
    NL = merge(NL,tot, by=c("year", "country"), all.x = TRUE)
    
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
    
    SE <- SE_ep %>% filter(branche=="b-tot", year!="1975")
    SE = merge(SE,tot, by=c("year", "country"), all.x = TRUE)
    
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
  
  
#US .....
  
  min <- as.Date("1975-1-1")
  max <- NA
  

# Descriptive -------------------------------------------------------------

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


# Panel data analysis -----------------------------------------------------


{
DK_tot <- DK_ep %>% filter(code=="TOT")
DK_tot$TOT = DK_tot$EMP
DK_tot <- DK_tot %>% select(year, country, TOT)


dk <- DK_ep %>% filter(branche=="b-tot", year!="1975")
dk = merge(dk,tot, by=c("year", "country"), all.x = TRUE)

dk$wgt = dk$EMP/dk$TOT


model_linear1 = emp_logchanges ~ prod_logchanges 

poolOLS <- plm(model_linear1, data = dk, index = c("code", "year"), model = "pooling", weight=wgt)
summary(poolOLS)

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
}

#Sammensætning af mikro og makroelasticiteter
{
GE_tot <- GE_ep %>% filter(code=="TOT")
GE_tot$TOT = GE_tot$EMP
GE_tot <- GE_tot %>% select(year, country, TOT)

}

#sapply(DK_ep, class)
#DK_ep$year <-  as.numeric(as.character(DK_ep[,"year"])) #ændres fordi "year" er en factor variabel

#dk <- DK_ep %>% filter(branche=="b-tot", year>1980, year<1990)

dk = merge(dk,tot, by=c("year", "country"), all.x = TRUE)





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







