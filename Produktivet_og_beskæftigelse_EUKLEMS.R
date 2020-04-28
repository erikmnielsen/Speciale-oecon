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
library(ggplot2)
library(ggthemes)


test_func <- function(dataset, country, measure) {
  
  colnames(dataset) <- gsub(measure, "", colnames(dataset))
  
  test <- melt(dataset,
               # ID variables - all the variables to keep but not split apart on
               id.vars=c("desc", "code"),
               # The source columns (not necessary here) # measure.vars=c("1970","1971",...),
               # Name of the destination column that will identify the original column that the measurement came from
               variable.name="year",
               value.name= measure)
  
  # sapply(test, class)
  test$year <-  as.numeric(as.character(test[,"year"])) #ændres fordi "year" er en factor variabel
  
  test$code =gsub("-", "t", test[,2])
  test$country = country
  
  
  test$industry <-factor(ifelse( test$code %in% c("TOT", "MARKT", "A","C","G","H","J","OtU","O","R","S","T","U"), 0,1))
  
  test$sector <- factor(ifelse(test$code %in% c("B", "DtE", "F"), 1,
                         ifelse(test$code %in% c("10t12", "13t15", "16t18", "19", "20t21", "22t23","24t25", "26t27", "28", "29t30","31t33"), 2, #kan man ikke bare bruge C, Total Manufacturing?
                                ifelse(test$code %in% c("P","Q","RtS"), 3,
                                       ifelse(test$code %in% c("53", "58t60", "61", "62t63", "K", "MtN"), 4,
                                              ifelse(test$code %in% c("45", "46", "47", "49t52", "I", "L"), 5,
                                                     0))))))
  
  test$sector_desc <- ifelse(test$sector==1,"Mining, utilities, and construction", 
                             ifelse(test$sector==2,"Manufacturing", 
                                    ifelse(test$sector==3,"Education and health services", 
                                           ifelse(test$sector==4,"High-tech services",
                                                  ifelse(test$sector==5,"Low-tech services",
                                                         "Not relevant"
                                                  )))))
  

  test
  
}


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

# PANEL DATA --------------------------------------------------------------

key_table <- read_csv("EUklems-data-master/key_table.csv")

DK_emp <- read_excel("DK_output_17ii.xlsx", sheet = "EMP")
DK_go <- read_excel("DK_output_17ii.xlsx", sheet = "GO")

test = test_func(DK_emp, "DK", "EMP")




#Employment by sector

t2 <- test %>% filter(sector!=0, year>1974) %>% 
  group_by(year,sector, sector_desc) %>%
  summarize(EMP=sum(EMP))


ggplot(data=t2, aes(x=year, y=EMP, colour=sector_desc)) + 
  geom_point() + geom_line() +
  xlab("Time") + ylab("Number of persons engaged in work (thousands)") +
  ggtitle("Employment by Sector") +
  guides(colour=guide_legend(title="Sector")) +
  theme_economist() +
  theme(legend.position="right") +
  scale_color_economist()


test.ts <- ts(test, start=1975)
test.ts <- na.omit(test.ts)

autoplot(test.ts[,c("10t12","A")])



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







