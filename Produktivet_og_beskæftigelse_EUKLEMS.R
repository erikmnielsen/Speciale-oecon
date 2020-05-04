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

dataset_1 <- read_excel("DK_output_17ii.xlsx", sheet = "EMP")
dataset_2 <- read_excel("DK_output_17ii.xlsx", sheet = "GO")
measure_1="EMP"
measure_2="GO"
country="DK"

data_func <- function(dataset_1, dataset_2, country, measure_1="EMP", measure_2="GO") {
  
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
               value.name= measure_2)
  
  data = merge(dataset_1, dataset_2, by=c("desc","code", "year"), all.x = TRUE)
  
  data <- na.omit(data)
  
  #sapply(data, class)
  data$year <-  as.numeric(as.character(data[,"year"])) #ændres fordi "year" er en factor variabel
  data$GO <-  as.numeric(as.character(data[,"GO"]))
  data$code =gsub("-", "t", data[,2])
  data$country = country
  
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
  
  data$branche <- ifelse(data$code=="A", "b1",
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
  
  

   t2 <- data %>% filter(sector!=0) %>% 
    group_by(year,sector, sector_desc) %>%
    summarize(EMP=sum(EMP),GO=sum(GO))
 
   t3 <- data %>% filter(branche!=0) %>% 
    group_by(year, branche, branche_desc) %>%
    summarize(EMP=sum(EMP),GO=sum(GO))

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
  
  t <- data %>% filter(code=="TOT")
  t_A <- data %>% filter(code=="A")
  t_O <- data %>% filter(code=="O")
  t_T <- data %>% filter(code=="T")
  
  t$EMP = t$EMP -t_A$EMP - t_O$EMP - t_T$EMP
  t$desc = "TOTAL INDUSTRIES-AutorSalomons"
  t$code = "TOT_AS"
  
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
  
  data_fin <- rbind(data, data2, t)
  pdata = pdata.frame(data_fin, index = c("code", "year"))
  
  pdata$emp_log <- log(pdata$EMP)
  pdata$emp_diff = diff(pdata$EMP, lag = 1, shift = "time")
  pdata$emp_logdiff = diff(pdata$emp_log, lag = 1, shift = "time")
  pdata$emp_changes <- pdata$emp_diff/lag(pdata$EMP, k = 1, shift = "time")*100
  pdata$emp_logchanges <- pdata$emp_logdiff/lag(pdata$emp_log, k = 1, shift = "time")*100  
    
  pdata$prod <- pdata$GO/pdata$EMP
  pdata$prod_log <- log(pdata$prod)
  pdata$prod_diff <- diff(pdata$prod, lag = 1, shift = "time")
  pdata$prod_logdiff <- diff(pdata$prod_log, lag = 1, shift = "time")
  pdata$prod_changes <- pdata$prod_diff/lag(pdata$prod, k = 1, shift = "time")*100
  pdata$prod_changes2 <- pdata$prod_diff/lag(pdata$prod, k = 1, shift = "time")
  pdata$prod_logchanges <- pdata$prod_logdiff/lag(pdata$prod_log, k = 1, shift = "time")*100
  pdata$prod_logchanges2 <- pdata$prod_logdiff/lag(pdata$prod_log, k = 1, shift = "time")
  # pdata = pdata %>% group_by(code) %>% mutate(prod_CGR_logchanges =  order_by(year,cumprod(1+prod_logchanges2[-1])*100)) #metode 1
  
  CGR = function(x){
    sapply(1:length(x), function(y){
      prod(1+x[1:y]) - 1
    })
  }
  
  pdata = pdata %>% group_by(code) %>% mutate(prod_logCGR = order_by(year, CGR(prod_logchanges2[-1])*100)) #metode 2
  pdata = pdata %>% group_by(code) %>% mutate(prod_CGR= order_by(year, CGR(prod_changes2[-1])*100))
  #df = pdata %>% group_by(code) %>% mutate(cumsum = cumsum())
  
  pdata <- pdata %>% select(year, country, code, desc, sel_industries, sector, sector_desc, branche, branche_desc, EMP, emp_logchanges, GO, prod, prod_logchanges, prod_CGR, prod_logCGR) %>% 
    filter(code!="b0",code!="s0")
  
  pdata = pdata.frame(pdata, index = c("code", "year"))
  
  pdata$prod_logCGR <- lag(pdata$prod_logCGR, k=1, shift="time")
  pdata$prod_CGR <- lag(pdata$prod_CGR, k=1, shift="time")

  pdata
  

}


(0.2/0.1)^(1/2)-1

# PANEL DATA --------------------------------------------------------------

#key_table <- read_csv("EUklems-data-master/key_table.csv")

DK_emp <- read_excel("DK_output_17ii.xlsx", sheet = "EMP")
DK_go <- read_excel("DK_output_17ii.xlsx", sheet = "GO")


test = data_func(DK_emp, DK_go,"DK", "EMP", "GO")




# DESCRIPTIVE - Sectoral employment and productivty

b <- test %>% filter(branche=="b-tot")
b$year = lubridate::ymd(b$year, truncated = 2L)

s <- test %>% filter(sector=="s-tot")
s$year = lubridate::ymd(s$year, truncated = 2L)

tot = test %>% filter(code=="TOT" | code=="TOT_AS")
tot$year = lubridate::ymd(tot$year, truncated = 2L)


min <- as.Date("1975-1-1")
max <- NA


#Branchebeskæftigelse i DK
{ggplot(data=b, aes(x=year, y=EMP, group=desc, colour=desc)) + 
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
}
  
#Produktivitet- og beskæftigelsesvækst i DK 
{ggplot(data = tot) + 
  geom_line(aes(x = year, y = emp_logchanges), color = "blue") +
  geom_line(aes(x = year, y = prod_logchanges), color = "red") +
  xlab("Time") + ylab("") +
  ggtitle("Produktivitet- og beskæftigelsesvækst i DK") +
  guides(colour=guide_legend(title="Sector")) +
  theme_economist() +
  theme(legend.position="right") +
  scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
  scale_x_date(limits = c(min, max))
  }

#Beskæftigelsesvækst fordelt på brancher i DK
{ggplot(data=b, aes(x=year, y=emp_logchanges, group=desc, colour=desc)) +
  geom_point() + 
    geom_line() +
    xlab("Time") + ylab("") +
    ggtitle("Beskæftigelsesvækst fordelt på brancher") +
    guides(colour=guide_legend(title="Sector")) +
    theme_economist() +
    theme(legend.position="right") +
    scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
    scale_x_date(limits = c(min, max))
}

#Kumulativ produktivitetsvækst i DK
{ggplot(data=b, aes(x=year, y=prod_logCGR, group=desc, colour=desc)) + 
  geom_point() + 
  geom_line() +
  xlab("Time") + ylab(")") +
  ggtitle("Kumulativ produktivitetsvækst") +
  guides(colour=guide_legend(title="Sector")) +
  theme_economist() +
  theme(legend.position="right") +
  scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
  scale_x_date(limits = c(min, max))
}


  


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







