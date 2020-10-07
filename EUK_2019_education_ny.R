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
library(sandwich)

CGR = function(x){
  sapply(1:length(x), function(y){
    prod(1+x[1:y]) - 1
  })
}

func_coefs <- function(regression, name, type) {
  
  options(scipen=999, digits=4) 
  #options(scipen=0, digits=7) #default
  
  if (type=="Country"){
    
    coef = coeftest(regression, vcovPL(regression, cluster = ~ country, kernel = "Bartlett"))
    
    siglvl = stars.pval(coef[,4])
    reg_coef = cbind(coef[,c(1,4)], siglvl)
    colnames(reg_coef) <- paste(name, colnames(reg_coef), sep = "_")
    
    reg_coef
    
  } else if (type=="Year"){
    
    coef = coeftest(regression, vcovPL(regression, cluster = ~ year, kernel = "Bartlett"))
    
    siglvl = stars.pval(coef[,4])
    reg_coef = cbind(coef[,c(1,4)], siglvl)
    colnames(reg_coef) <- paste(name, colnames(reg_coef), sep = "_")
    
    reg_coef
    
    
  }else if (type=="None"){
    
    coef = coeftest(regression, vcovPL(regression, cluster = NULL, kernel = "Bartlett"))
    
    siglvl = stars.pval(coef[,4])
    reg_coef = cbind(coef[,c(1,4)], siglvl)
    colnames(reg_coef) <- paste(name, colnames(reg_coef), sep = "_")
    
    reg_coef
    
    
  } else {
    
    NA
  }
  
}
  
#Datasæt konstrueres
{
  
  ### Data for produktivitet og beskæftigelse
  EUK_nationalaccounts <- readRDS("Data/Statistical_National-Accounts.rds")
  data = EUK_nationalaccounts %>% filter(code %in% c("B","C","D","E","F","G","H","I","J","K","L","M_N"))
  

  data_emp = data %>% filter(var=="EMP") %>% mutate(EMP=value) %>% select(-var, -value)
  data_go = data %>% filter(var=="GO_Q") %>% mutate(GO=value) %>% select(country, code, year, GO)
  data = merge(data_emp, data_go, by=c("country","code", "year"), all.x = TRUE)
  data = na.omit(data) #sletter rækker hvor enten GO eller EMP tal mangler
  
  #filtrering af lande/år
  test = data %>% group_by(country, year) %>% count() #492 obs
  data = merge(data, test, by=c("country", "year"), all.x = TRUE)
  data = data %>% filter(country %in% c("AT", "BE", "CZ", "DE", "DK", "EE","EL", "FI",
                                        "FR", "HR", "HU", "IT", "JP", "LV", "NL", "PL",
                                        "PT", "RO", "SE", "SI", "SK"), n>=12) # minus USA også
  
  
  ### Data for uddannelsesgrupper og andele af beskæftigelsen
  EUK_nationalaccountslabour <- read_csv("education_csv.csv") %>% select(-X1)
  data_educ = EUK_nationalaccountslabour
  data_educ$country = data_educ$Country
  data_educ$code = data_educ$Code
  data_educ$year = data_educ$variable
  data_educ = data_educ %>% select(country, code, year, Education, value)
  
  lowskill = data_educ %>% filter(Education=="1") %>% mutate(lowskill=value) %>% select(-Education, -value)
  midskill = data_educ %>% filter(Education=="2") %>% mutate(midskill=value) %>% select(-Education, -value)
  highskill = data_educ %>% filter(Education=="3") %>% mutate(highskill=value) %>% select(-Education, -value)
  
  data_educ = merge(lowskill, midskill, by=c("country","code", "year"), all.x = TRUE)
  data_educ = merge(data_educ, highskill, by=c("country","code", "year"), all.x = TRUE)
  
  ### Datasæt merges sammen 
  data = merge(data, data_educ, by=c("country","code", "year"), all.x = TRUE)
  data = na.omit(data) #sletter rækker hvor enten GO eller EMP tal mangler
  data = data %>% select(country, code, year, EMP, GO, lowskill, midskill, highskill)
  
  # Vægte
  gns = data %>% group_by(country, code) %>% summarize(EMP_gns=sum(EMP))
  gns_2 = data %>% group_by(country) %>% summarize(EMP_gns_2=sum(EMP))
  gns = merge(gns, gns_2, by=c("country"), all.x = TRUE)
  gns$wgt_i_avg = gns$EMP_gns/gns$EMP_gns_2
  gns = gns %>% select(country, code, wgt_i_avg)
  data = merge(data, gns, by=c("country","code"), all.x = TRUE)
  
  
  ### Paneldata laves 
  data$id_ci = data %>% group_indices(country, code)
  pdata = pdata.frame(data, index = c("id_ci", "year")) #Hvis R melder duplikater, hvilket bare skyldes at der er to variable for hver
  pdata$lowskill_changes = diff(pdata$lowskill, lag = 1, shift = "time")
  pdata$midskill_changes = diff(pdata$midskill, lag = 1, shift = "time")
  pdata$highskill_changes = diff(pdata$highskill, lag = 1, shift = "time")
  pdata$prod_logchanges = diff(log(pdata$GO/pdata$EMP), lag = 1, shift = "time")*100
  pdata = na.omit(pdata)
  
  
  skill_means = data %>% group_by(country, code) %>% summarize(lowskill_gns=mean(lowskill), midskill_gns=mean(midskill), highskill_gns=mean(highskill))
  # skill_means$tot = skill_means$lowskill_gns + skill_means$midskill_gns + skill_means$highskill_gns #tester om det summerer til 100
  skill_means_DK = skill_means %>% filter(country=="DK")
  
  saveRDS(skill_means_DK, file = "skill_means_DK_rds") # Save an object to a file
  
  #skal der bruges lags?
  pdata_lags = pdata
  pdata_lags$prod_logchanges_lag1 = lag(pdata_lags$prod_logchanges, k = 1, shift = "time")
  pdata_lags$prod_logchanges_lag2 = lag(pdata_lags$prod_logchanges, k = 2, shift = "time")
  pdata_lags$prod_logchanges_lag3 = lag(pdata_lags$prod_logchanges, k = 3, shift = "time")
  
  pdata_lags = na.omit(pdata_lags)
  
}

#uden lags
lowskill = lm(educshare_changes ~ prod_logchanges + factor(country) + factor(code) + factor(year), data=pdata, weights = wgt_i_avg)
lowskill_coef = func_coefs(lowskill, "low_skill", "Country")

midskill = lm(educshare_changes ~ prod_logchanges + factor(country) + factor(code) + factor(year), data=pdata, weights = wgt_i_avg)
midskill_coef = func_coefs(midskill, "mid_skill", "Country")

highskill = lm(educshare_changes ~ prod_logchanges + factor(country) + factor(code) + factor(year), data=pdata, weights = wgt_i_avg)
highskill_coef = func_coefs(highskill, "high_skill", "Country")

# med lags
lowskill_lags = lm(educshare_changes ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + factor(country) + factor(code) + factor(year), data=pdata_lags, weights = wgt_i_avg)
lowskill_lags_coef = func_coefs(lowskill_lags, "low_skill_lags", "Country")

midskill_lags  = lm(educshare_changes ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + factor(country) + factor(code) + factor(year), data=pdata_lags, weights = wgt_i_avg)
midskill_lags_coef = func_coefs(midskill_lags , "mid_skill_lags ", "Country")

highskill_lags  = lm(educshare_changes ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + factor(country) + factor(code) + factor(year), data=pdata_lags, weights = wgt_i_avg)
highskill_lags_coef = func_coefs(highskill_lags , "high_skill_lags ", "Country")

# eksporter
regoutput_share_panel <- Reduce(function(a,b){
  ans <- merge(a,b,by="row.names", all=T)
  row.names(ans) <- ans[,"Row.names"]
  ans[,!names(ans) %in% "Row.names"]
}, list(lowskill_coef, midskill_coef, highskill_coef, lowskill_lags_coef, midskill_lags_coef, highskill_lags_coef))

regoutput_share_panel$vars = row.names(regoutput_share_panel)
write_xlsx(regoutput_share_panel, "regoutput_share_panel.xlsx", col_names = TRUE)



table(data$country, data$year)


min = as.Date("2009-1-1")
max = NA

pdata$year <- as.Date(ISOdate(pdata$year, 1, 1))

pdata_dk = pdata %>% filter(country=="DK")

pdata_dk_low = pdata_dk %>% group_by(year) %>% summarise(tot = sum(EMP*(lowskill/100))) %>% mutate(educ="low")
pdata$lowskill_changes = diff(pdata$lowskill, lag = 1, shift = "time")

pdata_dk_mid = pdata_dk %>% group_by(year) %>% summarise(tot = sum(EMP*(midskill/100))) %>% mutate(educ="mid")
pdata_dk_high = pdata_dk %>% group_by(year) %>% summarise(tot = sum(EMP*(highskill/100))) %>% mutate(educ="high")
pdata_dk_all = pdata_dk %>% group_by(year) %>% summarise(tot = sum(EMP)) %>% mutate(educ="all")

pdata_dk_stat = rbind(pdata_dk_low, pdata_dk_mid, pdata_dk_high, pdata_dk_all)

ggplot(data=pdata_dk_stat, aes(x=year, y=tot, group=educ, colour=educ)) + 
  geom_point() + 
  geom_line() +
  xlab("Tid") + ylab("Kumulativ prædikterede ændring i beskæftigelse, pct.") +
  ggtitle("Kumulativ ændring i beskæftigelse i Danmark") +
  guides(colour=guide_legend(title="Branche")) +
  theme_economist() +
  theme(legend.position="right") +
  scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
  scale_x_date(limits = c(min, max))







