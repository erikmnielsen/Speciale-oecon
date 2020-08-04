#PRODUKTIVITET OG BESKÆFTIGELSE - EU KLEMS DATA

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

CGR = function(x){
  sapply(1:length(x), function(y){
    prod(1+x[1:y]) - 1
  })
}

func_empprod <- function(dataset_1, dataset_2, country, measure_1="EMP", measure_2="GO_P") {
  
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
  
  
  #data$sel_industries <-factor(ifelse( data$code %in% c("TOT", "MARKT", "A","C","G","H","J","OtU","O","R","S","T","U"), 0,1))
  
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
  
  #pdata = pdata %>% group_by(code) %>% dplyr::mutate(prod_CGR_logchanges =  order_by(year,cumprod(1+prod_logdiff[-1])*100)) #metode 1
  #pdata = pdata %>% group_by(code) %>% dplyr::mutate(prod_logCGR = order_by(year, CGR(prod_logdiff[-1])*100)) #metode 2
  
  #pdata = pdata %>% group_by(code) %>% dplyr::mutate(prod_CGR= order_by(year, CGR(prod_changes2[-1])*100))
  #df = pdata %>% group_by(code) %>% mutate(cumsum = cumsum())
  
  pdata <- pdata %>% select(year, country, code, desc, sel_industries, branche, branche_desc, EMP, emp_logchanges, GO, prod, prod_logchanges,prod_changes) %>% 
    filter(code!="b0",code!="s0")
  
  #pdata = pdata.frame(pdata, index = c("code", "year"))
  
  #pdata$prod_logCGR <- lag(pdata$prod_logCGR, k=1, shift="time")
  #pdata$prod_CGR <- lag(pdata$prod_CGR, k=1, shift="time")
  
  #pdata
}

#key_table <- read_csv("EUklems-data-master/key_table.csv")

# Country data  ----------------------------------------------------- 

# Danmark
{
  DK_emp <- read_excel("Data/DK_output_17ii.xlsx", sheet = "EMP") #Number of persons engaged (thousands)
  #DK_go <- read_excel("Data/DK_output_17ii.xlsx", sheet = "GO") #Gross Output at current basic prices (in millions of national currency)
  DK_gop <- read_excel("Data/DK_output_17ii.xlsx", sheet = "GO_P") #Gross output, price indices, 2010 = 100

  #Employment and productivty
  DK_ep = func_empprod(DK_emp, DK_gop,"DK", "EMP", "GO_P")
  
  #PLM analyse
  DK_tot <- DK_ep %>% filter(code=="TOT_MN")
  DK_tot$TOTmn = DK_tot$EMP
  DK_tot <- DK_tot %>% select(year, country, TOTmn)
  
  DK_ind = DK_ep %>% filter(sel_industries==1)
  
  dk <- DK_ep %>% filter(branche=="b-tot")
  dk = merge(dk,DK_tot, by=c("year", "country"), all.x = TRUE)
  dk$wgt = dk$EMP/dk$TOTmn
  dk$branche = dk$code
  dk$emp_logchanges_b = dk$emp_logchanges
  dk$prod_logchanges_b = dk$prod_logchanges
  dk = dk %>% select(year, branche,emp_logchanges_b,prod_logchanges_b,wgt)
  
  DK_ind = merge(DK_ind, dk, by=c("year", "branche"), all.x = TRUE)
  DK_ind = na.omit(DK_ind)
}

# USA
{
  US_emp <- read_excel("Data/US_output_17ii.xlsx", sheet = "EMP") #Number of persons engaged (thousands)
  #US_go <- read_excel("Data/US_output_17ii.xlsx", sheet = "GO") #Gross Output at current basic prices (in millions of national currency)
  US_gop <- read_excel("Data/US_output_17ii.xlsx", sheet = "GO_P") #Gross Output at current basic prices (in millions of national currency)
  US_comp <- read_excel("Data/US_output_17ii.xlsx", sheet = "COMP") #Compensation of employees  (in millions of national currency)
  US_lab <- read_excel("Data/US_output_17ii.xlsx", sheet = "LAB") #Labour compensation (in millions of national currency)
  US_va <- read_excel("Data/US_output_17ii.xlsx", sheet = "VA") #Gross value added at current basic prices (in millions of national currency), svarer labour + capital compensation
  
  #Employment and productivty
  US_ep = func_empprod(US_emp, US_gop,"US", "EMP", "GO_P")
  
  #PLM analyse
  US_tot <- US_ep %>% filter(code=="TOT_MN")
  US_tot$TOTmn = US_tot$EMP
  US_tot <- US_tot %>% select(year, country, TOTmn)
  
  US_ind = US_ep %>% filter(sel_industries==1)
  
  US <- US_ep %>% filter(branche=="b-tot")
  US = merge(US,US_tot, by=c("year", "country"), all.x = TRUE)
  US$wgt = US$EMP/US$TOTmn
  US$branche = US$code
  US$emp_logchanges_b = US$emp_logchanges
  US$prod_logchanges_b = US$prod_logchanges
  US = US %>% select(year, branche,emp_logchanges_b,prod_logchanges_b,wgt)
  
  US_ind = merge(US_ind, US, by=c("year", "branche"), all.x = TRUE)
  US_ind = na.omit(US_ind)
  
  
}

# UK . faste priser findes ikke
{
  UK_emp <- read_excel("Data/UK_output_17ii.xlsx", sheet = "EMP") #Number of persons engaged (thousands)
  #UK_go <- read_excel("Data/UK_output_17ii.xlsx", sheet = "GO") #Gross Output at current basic prices (in millions of national currency)
  UK_gop <- read_excel("Data/UK_output_17ii.xlsx", sheet = "GO_P") #Gross Output at current basic prices (in millions of national currency)
  UK_comp <- read_excel("Data/UK_output_17ii.xlsx", sheet = "COMP") #Compensation of employees  (in millions of national currency)
  UK_lab <- read_excel("Data/UK_output_17ii.xlsx", sheet = "LAB") #Labour compensation (in millions of national currency)
  UK_va <- read_excel("Data/UK_output_17ii.xlsx", sheet = "VA") #Gross value added at current basic prices (in millions of national currency), svarer labour + capital compensation
  
  #Employment and productivty
  UK_ep = func_empprod(UK_emp, UK_gop,"UK", "EMP", "GO_P")
  
  #PLM analyse
  UK_tot <- UK_ep %>% filter(code=="TOT")
  UK_tot$TOT = UK_tot$EMP
  UK_tot <- UK_tot %>% select(year, country, TOT)
  
  UK <- UK_ep %>% filter(branche=="b-tot", year!="1975")
  UK = merge(UK,UK_tot, by=c("year", "country"), all.x = TRUE)
  
  UK$wgt = UK$EMP/UK$TOT
  
}

# Tyskland
{
  DE_emp <- read_excel("Data/DE_output_17ii.xlsx", sheet = "EMP") #Number of persons engaged (thousands)
  #DE_go <- read_excel("Data/DE_output_17ii.xlsx", sheet = "GO") #Gross Output at current basic prices (in millions of national currency)
  DE_gop <- read_excel("Data/DE_output_17ii.xlsx", sheet = "GO_P") #Gross Output at current basic prices (in millions of national currency)
  DE_comp <- read_excel("Data/DE_output_17ii.xlsx", sheet = "COMP") #Compensation of employees  (in millions of national currency)
  DE_lab <- read_excel("Data/DE_output_17ii.xlsx", sheet = "LAB") #Labour compensation (in millions of national currency)
  DE_va <- read_excel("Data/DE_output_17ii.xlsx", sheet = "VA") #Gross value added at current basic prices (in millions of national currency), svarer labour + capital compensation
  
  #Employment and productivty
  DE_ep = func_empprod(DE_emp, DE_gop,"DE", "EMP", "GO_P")
  
  #PLM analyse
  DE_tot <- DE_ep %>% filter(code=="TOT_MN")
  DE_tot$TOTmn = DE_tot$EMP
  DE_tot <- DE_tot %>% select(year, country, TOTmn)
  
  
  DE_ind = DE_ep %>% filter(sel_industries==1)
  
  DE <- DE_ep %>% filter(branche=="b-tot")
  DE = merge(DE,DE_tot, by=c("year", "country"), all.x = TRUE)
  DE$wgt = DE$EMP/DE$TOTmn
  DE$branche = DE$code
  DE$emp_logchanges_b = DE$emp_logchanges
  DE$prod_logchanges_b = DE$prod_logchanges
  DE = DE %>% select(year, branche,emp_logchanges_b,prod_logchanges_b,wgt)
  
  DE_ind = merge(DE_ind, DE, by=c("year", "branche"), all.x = TRUE)
  DE_ind = na.omit(DE_ind)
}

# Holland
{
  NL_emp <- read_excel("Data/NL_output_17ii.xlsx", sheet = "EMP") #Number of persons engaged (thoNLands)
  #NL_go <- read_excel("Data/NL_output_17ii.xlsx", sheet = "GO") #Gross Output at current basic prices (in millions of national currency)
  NL_gop <- read_excel("Data/NL_output_17ii.xlsx", sheet = "GO_P") #Gross Output at current basic prices (in millions of national currency)
  NL_comp <- read_excel("Data/NL_output_17ii.xlsx", sheet = "COMP") #Compensation of employees  (in millions of national currency)
  NL_lab <- read_excel("Data/NL_output_17ii.xlsx", sheet = "LAB") #Labour compensation (in millions of national currency)
  NL_va <- read_excel("Data/NL_output_17ii.xlsx", sheet = "VA") #Gross value added at current basic prices (in millions of national currency), svarer labour + capital compensation
  
  #Employment and productivty
  NL_ep = func_empprod(NL_emp, NL_gop,"NL", "EMP", "GO_P")
  
  #PLM analyse
  NL_tot <- NL_ep %>% filter(code=="TOT_MN")
  NL_tot$TOTmn = NL_tot$EMP
  NL_tot <- NL_tot %>% select(year, country, TOTmn)
  
  
  NL_ind = NL_ep %>% filter(sel_industries==1)
  
  NL <- NL_ep %>% filter(branche=="b-tot")
  NL = merge(NL,NL_tot, by=c("year", "country"), all.x = TRUE)
  NL$wgt = NL$EMP/NL$TOTmn
  NL$branche = NL$code
  NL$emp_logchanges_b = NL$emp_logchanges
  NL$prod_logchanges_b = NL$prod_logchanges
  NL = NL %>% select(year, branche,emp_logchanges_b,prod_logchanges_b,wgt)
  
  NL_ind = merge(NL_ind, NL, by=c("year", "branche"), all.x = TRUE)
  NL_ind = na.omit(NL_ind)
}

# Sverige
{
  SE_emp <- read_excel("Data/SE_output_17ii.xlsx", sheet = "EMP") #Number of persons engaged (thousands)
  #SE_go <- read_excel("Data/SE_output_17ii.xlsx", sheet = "GO") #Gross Output at current basic prices (in millions of national currency)
  SE_gop <- read_excel("Data/SE_output_17ii.xlsx", sheet = "GO_P") #Gross Output at current basic prices (in millions of national currency)
  SE_comp <- read_excel("Data/SE_output_17ii.xlsx", sheet = "COMP") #Compensation of employees  (in millions of national currency)
  SE_lab <- read_excel("Data/SE_output_17ii.xlsx", sheet = "LAB") #Labour compensation (in millions of national currency)
  SE_va <- read_excel("Data/SE_output_17ii.xlsx", sheet = "VA") #Gross value added at current basic prices (in millions of national currency), svarer labour + capital compensation
  
  #Employment and productivty
  SE_ep = func_empprod(SE_emp, SE_gop,"SE", "EMP", "GO_P")
  
  #PLM analyse
  SE_tot <- SE_ep %>% filter(code=="TOT_MN")
  SE_tot$TOTmn = SE_tot$EMP
  SE_tot <- SE_tot %>% select(year, country, TOTmn)
  
  
  SE_ind = SE_ep %>% filter(sel_industries==1)
  
  SE <- SE_ep %>% filter(branche=="b-tot")
  SE = merge(SE,SE_tot, by=c("year", "country"), all.x = TRUE)
  SE$wgt = SE$EMP/SE$TOTmn
  SE$branche = SE$code
  SE$emp_logchanges_b = SE$emp_logchanges
  SE$prod_logchanges_b = SE$prod_logchanges
  SE = SE %>% select(year, branche,emp_logchanges_b,prod_logchanges_b,wgt)
  
  SE_ind = merge(SE_ind, SE, by=c("year", "branche"), all.x = TRUE)
  SE_ind = na.omit(SE_ind)
}

# Østrig
{
  AT_emp = read_excel("Data/AT_output_17ii.xlsx", sheet = "EMP") #Number of persons engaged (thousands)
  #AT_go = read_excel("Data/AT_output_17ii.xlsx", sheet = "GO") #Gross Output at current basic prices (in millions of national currency)
  AT_gop = read_excel("Data/AT_output_17ii.xlsx", sheet = "GO_P") #Gross output, price indices, 2010 = 100
  AT_comp = read_excel("Data/AT_output_17ii.xlsx", sheet = "COMP") #Compensation of employees  (in millions of national currency)
  AT_lab = read_excel("Data/AT_output_17ii.xlsx", sheet = "LAB") #Labour compensation (in millions of national currency)
  AT_va = read_excel("Data/AT_output_17ii.xlsx", sheet = "VA") #Gross value added at current basic prices (in millions of national currency), svarer labour + capital compensation
 
  #Employment and productivty
  AT_ep = func_empprod(AT_emp, AT_gop,"AT", "EMP", "GO_P")
  
  #PLM analyse
  AT_tot <- AT_ep %>% filter(code=="TOT_MN")
  AT_tot$TOTmn = AT_tot$EMP
  AT_tot <- AT_tot %>% select(year, country, TOTmn)
  
  
  AT_ind = AT_ep %>% filter(sel_industries==1)
  
  AT <- AT_ep %>% filter(branche=="b-tot")
  AT = merge(AT,AT_tot, by=c("year", "country"), all.x = TRUE)
  AT$wgt = AT$EMP/AT$TOTmn
  AT$branche = AT$code
  AT$emp_logchanges_b = AT$emp_logchanges
  AT$prod_logchanges_b = AT$prod_logchanges
  AT = AT %>% select(year, branche,emp_logchanges_b,prod_logchanges_b,wgt)
  
  AT_ind = merge(AT_ind, AT, by=c("year", "branche"), all.x = TRUE)
  AT_ind = na.omit(AT_ind)
} 

# Belgium
{
  BE_emp = read_excel("Data/BE_output_17ii.xlsx", sheet = "EMP") #Number of persons engaged (thousands)
  #BE_go = read_excel("Data/BE_output_17ii.xlsx", sheet = "GO") #Gross Output at current basic prices (in millions of national currency)
  BE_gop = read_excel("Data/BE_output_17ii.xlsx", sheet = "GO_P") #Gross output, price indices, 2010 = 100
  BE_comp = read_excel("Data/BE_output_17ii.xlsx", sheet = "COMP") #Compensation of employees  (in millions of national currency)
  BE_lab = read_excel("Data/BE_output_17ii.xlsx", sheet = "LAB") #Labour compensation (in millions of national currency)
  BE_va = read_excel("Data/BE_output_17ii.xlsx", sheet = "VA") #Gross value added at current basic prices (in millions of national currency), svarer labour + capital compensation
  
  #Employment and productivty
  BE_ep = func_empprod(BE_emp, BE_gop,"BE", "EMP", "GO_P")
  
  #PLM analyse
  BE_tot <- BE_ep %>% filter(code=="TOT_MN")
  BE_tot$TOTmn = BE_tot$EMP
  BE_tot <- BE_tot %>% select(year, country, TOTmn)
  
  
  BE_ind = BE_ep %>% filter(sel_industries==1)
  
  BE <- BE_ep %>% filter(branche=="b-tot")
  BE = merge(BE,BE_tot, by=c("year", "country"), all.x = TRUE)
  BE$wgt = BE$EMP/BE$TOTmn
  BE$branche = BE$code
  BE$emp_logchanges_b = BE$emp_logchanges
  BE$prod_logchanges_b = BE$prod_logchanges
  BE = BE %>% select(year, branche,emp_logchanges_b,prod_logchanges_b,wgt)
  
  BE_ind = merge(BE_ind, BE, by=c("year", "branche"), all.x = TRUE)
  BE_ind = na.omit(BE_ind)
}

# Tjekkiet
{
  CZ_emp = read_excel("Data/CZ_output_17ii.xlsx", sheet = "EMP") #Number of persons engaged (thousands)
  #CZ_go = read_excel("Data/CZ_output_17ii.xlsx", sheet = "GO") #Gross Output at current basic prices (in millions of national currency)
  CZ_gop = read_excel("Data/CZ_output_17ii.xlsx", sheet = "GO_P") #Gross output, price indices, 2010 = 100
  CZ_comp = read_excel("Data/CZ_output_17ii.xlsx", sheet = "COMP") #Compensation of employees  (in millions of national currency)
  CZ_lab = read_excel("Data/CZ_output_17ii.xlsx", sheet = "LAB") #Labour compensation (in millions of national currency)
  CZ_va = read_excel("Data/CZ_output_17ii.xlsx", sheet = "VA") #Gross value added at current basic prices (in millions of national currency), svarer labour + capital compensation
  
  #Employment and productivty
  CZ_ep = func_empprod(CZ_emp, CZ_gop,"CZ", "EMP", "GO_P")
  
  #PLM analyse
  CZ_tot <- CZ_ep %>% filter(code=="TOT_MN")
  CZ_tot$TOTmn = CZ_tot$EMP
  CZ_tot <- CZ_tot %>% select(year, country, TOTmn)
  
  
  CZ_ind = CZ_ep %>% filter(sel_industries==1)
  
  CZ <- CZ_ep %>% filter(branche=="b-tot")
  CZ = merge(CZ,CZ_tot, by=c("year", "country"), all.x = TRUE)
  CZ$wgt = CZ$EMP/CZ$TOTmn
  CZ$branche = CZ$code
  CZ$emp_logchanges_b = CZ$emp_logchanges
  CZ$prod_logchanges_b = CZ$prod_logchanges
  CZ = CZ %>% select(year, branche,emp_logchanges_b,prod_logchanges_b,wgt)
  
  CZ_ind = merge(CZ_ind, CZ, by=c("year", "branche"), all.x = TRUE)
  CZ_ind = na.omit(CZ_ind)
}

# Finland
{
  FI_emp = read_excel("Data/FI_output_17ii.xlsx", sheet = "EMP") #Number of persons engaged (thousands)
  #FI_go = read_excel("Data/FI_output_17ii.xlsx", sheet = "GO") #Gross Output at current basic prices (in millions of national currency)
  FI_gop = read_excel("Data/FI_output_17ii.xlsx", sheet = "GO_P") #Gross output, price indices, 2010 = 100
  FI_comp = read_excel("Data/FI_output_17ii.xlsx", sheet = "COMP") #Compensation of employees  (in millions of national currency)
  FI_lab = read_excel("Data/FI_output_17ii.xlsx", sheet = "LAB") #Labour compensation (in millions of national currency)
  FI_va = read_excel("Data/FI_output_17ii.xlsx", sheet = "VA") #Gross value added at current basic prices (in millions of national currency), svarer labour + capital compensation
  
  #Employment and productivty
  FI_ep = func_empprod(FI_emp, FI_gop,"FI", "EMP", "GO_P")
  
  #PLM analyse
  FI_tot <- FI_ep %>% filter(code=="TOT_MN")
  FI_tot$TOTmn = FI_tot$EMP
  FI_tot <- FI_tot %>% select(year, country, TOTmn)
  
  
  FI_ind = FI_ep %>% filter(sel_industries==1)
  
  FI <- FI_ep %>% filter(branche=="b-tot")
  FI = merge(FI,FI_tot, by=c("year", "country"), all.x = TRUE)
  FI$wgt = FI$EMP/FI$TOTmn
  FI$branche = FI$code
  FI$emp_logchanges_b = FI$emp_logchanges
  FI$prod_logchanges_b = FI$prod_logchanges
  FI = FI %>% select(year, branche,emp_logchanges_b,prod_logchanges_b,wgt)
  
  FI_ind = merge(FI_ind, FI, by=c("year", "branche"), all.x = TRUE)
  FI_ind = na.omit(FI_ind)
}

# Frankrig
{
  FR_emp = read_excel("Data/FR_output_17ii.xlsx", sheet = "EMP") #Number of persons engaged (thousands)
  #FR_go = read_excel("Data/FR_output_17ii.xlsx", sheet = "GO") #Gross Output at current basic prices (in millions of national currency)
  FR_gop = read_excel("Data/FR_output_17ii.xlsx", sheet = "GO_P") #Gross output, price indices, 2010 = 100
  FR_comp = read_excel("Data/FR_output_17ii.xlsx", sheet = "COMP") #Compensation of employees  (in millions of national currency)
  FR_lab = read_excel("Data/FR_output_17ii.xlsx", sheet = "LAB") #Labour compensation (in millions of national currency)
  FR_va = read_excel("Data/FR_output_17ii.xlsx", sheet = "VA") #Gross value added at current basic prices (in millions of national currency), svarer labour + capital compensation
  
  #Employment and productivty
  FR_ep = func_empprod(FR_emp, FR_gop,"FR", "EMP", "GO_P")
  
  #PLM analyse
  FR_tot <- FR_ep %>% filter(code=="TOT_MN")
  FR_tot$TOTmn = FR_tot$EMP
  FR_tot <- FR_tot %>% select(year, country, TOTmn)
  
  
  FR_ind = FR_ep %>% filter(sel_industries==1)
  
  FR <- FR_ep %>% filter(branche=="b-tot")
  FR = merge(FR,FR_tot, by=c("year", "country"), all.x = TRUE)
  FR$wgt = FR$EMP/FR$TOTmn
  FR$branche = FR$code
  FR$emp_logchanges_b = FR$emp_logchanges
  FR$prod_logchanges_b = FR$prod_logchanges
  FR = FR %>% select(year, branche,emp_logchanges_b,prod_logchanges_b,wgt)
  
  FR_ind = merge(FR_ind, FR, by=c("year", "branche"), all.x = TRUE)
  FR_ind = na.omit(FR_ind)
}

# Grækenland
{
  EL_emp = read_excel("Data/EL_output_17ii.xlsx", sheet = "EMP") #Number of persons engaged (thousands)
  #EL_go = read_excel("Data/EL_output_17ii.xlsx", sheet = "GO") #Gross Output at current basic prices (in millions of national currency)
  EL_gop = read_excel("Data/EL_output_17ii.xlsx", sheet = "GO_P") #Gross output, price indices, 2010 = 100
  EL_comp = read_excel("Data/EL_output_17ii.xlsx", sheet = "COMP") #Compensation of employees  (in millions of national currency)
  EL_lab = read_excel("Data/EL_output_17ii.xlsx", sheet = "LAB") #Labour compensation (in millions of national currency)
  EL_va = read_excel("Data/EL_output_17ii.xlsx", sheet = "VA") #Gross value added at current basic prices (in millions of national currency), svarer labour + capital compensation
  
  #Employment and productivty
  EL_ep = func_empprod(EL_emp, EL_gop,"EL", "EMP", "GO_P")
  
  #PLM analyse
  EL_tot <- EL_ep %>% filter(code=="TOT_MN")
  EL_tot$TOTmn = EL_tot$EMP
  EL_tot <- EL_tot %>% select(year, country, TOTmn)
  
  
  EL_ind = EL_ep %>% filter(sel_industries==1)
  
  EL <- EL_ep %>% filter(branche=="b-tot")
  EL = merge(EL,EL_tot, by=c("year", "country"), all.x = TRUE)
  EL$wgt = EL$EMP/EL$TOTmn
  EL$branche = EL$code
  EL$emp_logchanges_b = EL$emp_logchanges
  EL$prod_logchanges_b = EL$prod_logchanges
  EL = EL %>% select(year, branche,emp_logchanges_b,prod_logchanges_b,wgt)
  
  EL_ind = merge(EL_ind, EL, by=c("year", "branche"), all.x = TRUE)
  EL_ind = na.omit(EL_ind)
}

# Italien
{
  IT_emp = read_excel("Data/IT_output_17ii.xlsx", sheet = "EMP") #Number of persons engaged (thousands)
  #IT_go = read_excel("Data/IT_output_17ii.xlsx", sheet = "GO") #Gross Output at current basic prices (in millions of national currency)
  IT_gop = read_excel("Data/IT_output_17ii.xlsx", sheet = "GO_P") #Gross output, price indices, 2010 = 100
  IT_comp = read_excel("Data/IT_output_17ii.xlsx", sheet = "COMP") #Compensation of employees  (in millions of national currency)
  IT_lab = read_excel("Data/IT_output_17ii.xlsx", sheet = "LAB") #Labour compensation (in millions of national currency)
  IT_va = read_excel("Data/IT_output_17ii.xlsx", sheet = "VA") #Gross value added at current basic prices (in millions of national currency), svarer labour + capital compensation

  #Employment and productivty
  IT_ep = func_empprod(IT_emp, IT_gop,"IT", "EMP", "GO_P")
  
  #PLM analysis
  IT_tot <- IT_ep %>% filter(code=="TOT_MN")
  IT_tot$TOTmn = IT_tot$EMP
  IT_tot <- IT_tot %>% select(year, country, TOTmn)
  
  
  IT_ind = IT_ep %>% filter(sel_industries==1)
  
  IT <- IT_ep %>% filter(branche=="b-tot")
  IT = merge(IT,IT_tot, by=c("year", "country"), all.x = TRUE)
  IT$wgt = IT$EMP/IT$TOTmn
  IT$branche = IT$code
  IT$emp_logchanges_b = IT$emp_logchanges
  IT$prod_logchanges_b = IT$prod_logchanges
  IT = IT %>% select(year, branche,emp_logchanges_b,prod_logchanges_b,wgt)
  
  IT_ind = merge(IT_ind, IT, by=c("year", "branche"), all.x = TRUE)
  IT_ind = na.omit(IT_ind)
}

# Letland
{
  LV_emp = read_excel("Data/LV_output_17ii.xlsx", sheet = "EMP") #Number of persons engaged (thousands)
  #LV_go = read_excel("Data/LV_output_17ii.xlsx", sheet = "GO") #Gross Output at current basic prices (in millions of national currency)
  LV_gop = read_excel("Data/LV_output_17ii.xlsx", sheet = "GO_P") #Gross output, price indices, 2010 = 100
  LV_comp = read_excel("Data/LV_output_17ii.xlsx", sheet = "COMP") #Compensation of employees  (in millions of national currency)
  LV_lab = read_excel("Data/LV_output_17ii.xlsx", sheet = "LAB") #Labour compensation (in millions of national currency)
  LV_va = read_excel("Data/LV_output_17ii.xlsx", sheet = "VA") #Gross value added at current basic prices (in millions of national currency), svarer labour + capital compensation
  
  #Employment and productivty
  LV_ep = func_empprod(LV_emp, LV_gop,"LV", "EMP", "GO_P")
  
  #PLM analyse
  LV_tot <- LV_ep %>% filter(code=="TOT_MN")
  LV_tot$TOTmn = LV_tot$EMP
  LV_tot <- LV_tot %>% select(year, country, TOTmn)
  
  
  LV_ind = LV_ep %>% filter(sel_industries==1)
  
  LV <- LV_ep %>% filter(branche=="b-tot")
  LV = merge(LV,LV_tot, by=c("year", "country"), all.x = TRUE)
  LV$wgt = LV$EMP/LV$TOTmn
  LV$branche = LV$code
  LV$emp_logchanges_b = LV$emp_logchanges
  LV$prod_logchanges_b = LV$prod_logchanges
  LV = LV %>% select(year, branche,emp_logchanges_b,prod_logchanges_b,wgt)
  
  LV_ind = merge(LV_ind, LV, by=c("year", "branche"), all.x = TRUE)
  LV_ind = na.omit(LV_ind)
}

# Luxenborg
{
  LU_emp = read_excel("Data/LU_output_17ii.xlsx", sheet = "EMP") #Number of persons engaged (thousands)
  #LU_go = read_excel("Data/LU_output_17ii.xlsx", sheet = "GO") #Gross Output at current basic prices (in millions of national currency)
  LU_gop = read_excel("Data/LU_output_17ii.xlsx", sheet = "GO_P") #Gross output, price indices, 2010 = 100
  LU_comp = read_excel("Data/LU_output_17ii.xlsx", sheet = "COMP") #Compensation of employees  (in millions of national currency)
  LU_lab = read_excel("Data/LU_output_17ii.xlsx", sheet = "LAB") #Labour compensation (in millions of national currency)
  LU_va = read_excel("Data/LU_output_17ii.xlsx", sheet = "VA") #Gross value added at current basic prices (in millions of national currency), svarer labour + capital compensation
  
  #Employment and productivty
  LU_ep = func_empprod(LU_emp, LU_gop,"LU", "EMP", "GO_P")
  
  #PLM analyse
  LU_tot <- LU_ep %>% filter(code=="TOT_MN")
  LU_tot$TOTmn = LU_tot$EMP
  LU_tot <- LU_tot %>% select(year, country, TOTmn)
  
  
  LU_ind = LU_ep %>% filter(sel_industries==1)
  
  LU <- LU_ep %>% filter(branche=="b-tot")
  LU = merge(LU,LU_tot, by=c("year", "country"), all.x = TRUE)
  LU$wgt = LU$EMP/LU$TOTmn
  LU$branche = LU$code
  LU$emp_logchanges_b = LU$emp_logchanges
  LU$prod_logchanges_b = LU$prod_logchanges
  LU = LU %>% select(year, branche,emp_logchanges_b,prod_logchanges_b,wgt)
  
  LU_ind = merge(LU_ind, LU, by=c("year", "branche"), all.x = TRUE)
  LU_ind = na.omit(LU_ind)
}

# Slovakiet
{
  SK_emp = read_excel("Data/SK_output_17ii.xlsx", sheet = "EMP") #Number of persons engaged (thousands)
  #SK_go = read_excel("Data/SK_output_17ii.xlsx", sheet = "GO") #Gross Output at current basic prices (in millions of national currency)
  SK_gop = read_excel("Data/SK_output_17ii.xlsx", sheet = "GO_P") #Gross output, price indices, 2010 = 100
  SK_comp = read_excel("Data/SK_output_17ii.xlsx", sheet = "COMP") #Compensation of employees  (in millions of national currency)
  SK_lab = read_excel("Data/SK_output_17ii.xlsx", sheet = "LAB") #Labour compensation (in millions of national currency)
  SK_va = read_excel("Data/SK_output_17ii.xlsx", sheet = "VA") #Gross value added at current basic prices (in millions of national currency), svarer labour + capital compensation
  
  #Employment and productivty
  SK_ep = func_empprod(SK_emp, SK_gop,"SK", "EMP", "GO_P")
  
  #PLM analyse
  SK_tot <- SK_ep %>% filter(code=="TOT_MN")
  SK_tot$TOTmn = SK_tot$EMP
  SK_tot <- SK_tot %>% select(year, country, TOTmn)
  
  
  SK_ind = SK_ep %>% filter(sel_industries==1)
  
  SK <- SK_ep %>% filter(branche=="b-tot")
  SK = merge(SK,SK_tot, by=c("year", "country"), all.x = TRUE)
  SK$wgt = SK$EMP/SK$TOTmn
  SK$branche = SK$code
  SK$emp_logchanges_b = SK$emp_logchanges
  SK$prod_logchanges_b = SK$prod_logchanges
  SK = SK %>% select(year, branche,emp_logchanges_b,prod_logchanges_b,wgt)
  
  SK_ind = merge(SK_ind, SK, by=c("year", "branche"), all.x = TRUE)
  SK_ind = na.omit(SK_ind)
}


# Slovenien
{
  SI_emp = read_excel("Data/SI_output_17ii.xlsx", sheet = "EMP") #Number of persons engaged (thousands)
  #SI_go = read_excel("Data/SI_output_17ii.xlsx", sheet = "GO") #Gross Output at current basic prices (in millions of national currency)
  SI_gop = read_excel("Data/SI_output_17ii.xlsx", sheet = "GO_P") #Gross output, price indices, 2010 = 100
  SI_comp = read_excel("Data/SI_output_17ii.xlsx", sheet = "COMP") #Compensation of employees  (in millions of national currency)
  SI_lab = read_excel("Data/SI_output_17ii.xlsx", sheet = "LAB") #Labour compensation (in millions of national currency)
  SI_va = read_excel("Data/SI_output_17ii.xlsx", sheet = "VA") #Gross value added at current basic prices (in millions of national currency), svarer labour + capital compensation
  
  #Employment and productivty
  SI_ep = func_empprod(SI_emp, SI_gop,"SI", "EMP", "GO_P")
  
  #PLM analyse
  SI_tot <- SI_ep %>% filter(code=="TOT_MN")
  SI_tot$TOTmn = SI_tot$EMP
  SI_tot <- SI_tot %>% select(year, country, TOTmn)
  
  
  SI_ind = SI_ep %>% filter(sel_industries==1)
  
  SI <- SI_ep %>% filter(branche=="b-tot")
  SI = merge(SI,SI_tot, by=c("year", "country"), all.x = TRUE)
  SI$wgt = SI$EMP/SI$TOTmn
  SI$branche = SI$code
  SI$emp_logchanges_b = SI$emp_logchanges
  SI$prod_logchanges_b = SI$prod_logchanges
  SI = SI %>% select(year, branche,emp_logchanges_b,prod_logchanges_b,wgt)
  
  SI_ind = merge(SI_ind, SI, by=c("year", "branche"), all.x = TRUE)
  SI_ind = na.omit(SI_ind)
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
ci_panel = ci_panel %>% select(year, country, code, desc, emp_logchanges, emp_logchanges_b, prod_logchanges, prod_logchanges_b, wgt)
ci_panel$id = ci_panel %>% group_indices(code, country)
ci_panel$prod_logchanges_wgt = ci_panel$prod_logchanges*ci_panel$wgt
ci_panel$emp_logchanges_wgt = ci_panel$emp_logchanges*ci_panel$wgt

#ci_panel = na.omit(ci_panel)

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

#Arellano

# Sammensætning af mikro og makroelasticiteter --------------------------------------------------

#hvad gør vi med lande hvor nogle industrier mangler?

sum_prod_yc <- ci_panel %>% group_by(year, country) %>% count(sum(prod_logchanges))
ci_panel = merge(ci_panel, sum_prod_yc, by=c( "year", "country"), all.x = TRUE)
ci_panel$avgLP_oi = (ci_panel$`sum(prod_logchanges)` - ci_panel$prod_logchanges)/(ci_panel$n - 1) #bør det vægtes

ci_panel = pdata.frame(ci_panel, index = c("id", "year"))
ci_panel$avgLP_oi_lag1 = lag(ci_panel$avgLP_oi, k = 1, shift = "time")
ci_panel$avgLP_oi_lag2 = lag(ci_panel$avgLP_oi, k = 2, shift = "time")
ci_panel$avgLP_oi_lag3 = lag(ci_panel$avgLP_oi, k = 3, shift = "time")
ci_panel = na.omit(ci_panel)


#model_linear2 = emp_logchanges ~ prod_logchanges + avgLP_oi + avgLP_oi_lag1 + avgLP_oi_lag2 + avgLP_oi_lag3

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







