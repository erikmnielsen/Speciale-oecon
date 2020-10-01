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
  
func_empprod <- function(type) {
  
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
                                        "PT", "RO", "SE", "SI", "SK", "US" ), n>=12)
  
  
  ### Data for uddannelsesgrupper og andele af beskæftigelsen
  EUK_nationalaccountslabour <- read_csv("education_csv.csv") %>% select(-X1)
  data_educ = EUK_nationalaccountslabour
  data_educ$country = data_educ$Country
  data_educ$code = data_educ$Code
  data_educ$year = data_educ$variable
  data_educ = data_educ %>% select(country, code, year, Education, value)
  
  
  ### Datasæt merges sammen 
  data = merge(data, data_educ, by=c("country","code", "year"), all.x = TRUE)
  data = na.omit(data) #sletter rækker hvor enten GO eller EMP tal mangler
  data = data %>% select(country, code, year, EMP, GO, Education, value)
  
  # Vægte
  gns = data %>% group_by(country, code) %>% summarize(EMP_gns=sum(EMP))
  gns_2 = data %>% group_by(country) %>% summarize(EMP_gns_2=sum(EMP))
  gns = merge(gns, gns_2, by=c("country"), all.x = TRUE)
  gns$wgt_i_avg = gns$EMP_gns/gns$EMP_gns_2
  gns = gns %>% select(country, code, wgt_i_avg)
  data = merge(data, gns, by=c("country","code"), all.x = TRUE)
  
  
  ### Paneldata laves udfra education type
  data_type = data %>% filter(Education==type)
  data_type$id_ci = data_type %>% group_indices(country, code)
  pdata = pdata.frame(data_type, index = c("id_ci", "year")) #Hvis R melder duplikater, hvilket bare skyldes at der er to variable for hver
  
  pdata$educshare_changes = diff(pdata$value, lag = 1, shift = "time")
  pdata$prod_logchanges = diff(log(pdata$GO/pdata$EMP), lag = 1, shift = "time")*100
  
  pdata = na.omit(pdata)
  
  #skal der bruges lags?
  pdata$prod_logchanges_lag1 = lag(pdata$prod_logchanges, k = 1, shift = "time")
  pdata$prod_logchanges_lag2 = lag(pdata$prod_logchanges, k = 2, shift = "time")
  pdata$prod_logchanges_lag3 = lag(pdata$prod_logchanges, k = 3, shift = "time")
  
  pdata
  
}

# uden lags
panel_lowskill = func_empprod(1)
lowskill = lm(educshare_changes ~ prod_logchanges + factor(country) + factor(code) + factor(year), data=panel_lowskill, weights = wgt_i_avg)
lowskill_coef = func_coefs(lowskill, "low_skill", "Country")

panel_midskill = func_empprod(2)
midskill = lm(educshare_changes ~ prod_logchanges + factor(country) + factor(code) + factor(year), data=panel_midskill, weights = wgt_i_avg)
midskill_coef = func_coefs(midskill, "mid_skill", "Country")

panel_highskill = func_empprod(3)
highskill = lm(educshare_changes ~ prod_logchanges + factor(country) + factor(code) + factor(year), data=panel_highskill, weights = wgt_i_avg)
highskill_coef = func_coefs(highskill, "high_skill", "Country")

# med lags
panel_lowskill_lags = na.omit(panel_lowskill)
lowskill_lags = lm(educshare_changes ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + factor(country) + factor(code) + factor(year), data=panel_lowskill_lags, weights = wgt_i_avg)
lowskill_lags_coef = func_coefs(lowskill_lags, "low_skill_lags", "Country")

panel_midskill_lags  = na.omit(panel_midskill)
midskill_lags  = lm(educshare_changes ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + factor(country) + factor(code) + factor(year), data=panel_midskill_lags , weights = wgt_i_avg)
midskill_lags_coef = func_coefs(midskill_lags , "mid_skill_lags ", "Country")

panel_highskill_lags  = na.omit(panel_highskill)
highskill_lags  = lm(educshare_changes ~ prod_logchanges + prod_logchanges_lag1 + prod_logchanges_lag2 + prod_logchanges_lag3 + factor(country) + factor(code) + factor(year), data=panel_highskill_lags, weights = wgt_i_avg)
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


#### DESCRIPTIVE - Predicted cumulative percentage employment change 
{
  DK_effects = ci_panel_ss %>%  filter(country=="DK") %>% group_by(code) %>% mutate(baseyearEMP = EMP[year == 2009]) %>% select(country, year, code, branche, EMP, EMP_b, EMP_c, GO, prod_logchanges, baseyearEMP, 
                                                                                                                                dLP_BwoI_b1 , dLP_BwoI_b1_lag1 , dLP_BwoI_b1_lag2 , dLP_BwoI_b1_lag3 ,
                                                                                                                                dLP_BwoI_b2 , dLP_BwoI_b2_lag1 , dLP_BwoI_b2_lag2 , dLP_BwoI_b2_lag3 ,
                                                                                                                                dLP_BwoI_b3 , dLP_BwoI_b3_lag1 , dLP_BwoI_b3_lag2 , dLP_BwoI_b3_lag3 ,
                                                                                                                                dLP_BwoI_b4 , dLP_BwoI_b4_lag1 , dLP_BwoI_b4_lag2 , dLP_BwoI_b4_lag3)
  
  # INTERNAL EFFECT - Predicted cumulative percentage employment change from own-industry productivity growth originating in five sectors
  {
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
    
    DK_inteff = DK_inteff %>% group_by(branche, year) %>% summarise(sumEMPchange=sum(emp_basechange), EMP_c_base = 1736) #1999=1674, 2009=1736
    
    DK_inteff = as.data.frame(DK_inteff) %>% mutate(emp_basechange_pct = (sumEMPchange/EMP_c_base)*100)
    
    DK_inteff_all = DK_inteff %>% group_by(year) %>% summarise(sumEMPchange=sum(sumEMPchange),  EMP_c_base = 1736)
    DK_inteff_all = DK_inteff_all %>% mutate(emp_basechange_pct = (sumEMPchange/EMP_c_base)*100, branche="alle")
    DK_inteff_all = DK_inteff_all %>% select(branche, year, sumEMPchange, EMP_c_base, emp_basechange_pct)
    DK_inteff = rbind(DK_inteff,DK_inteff_all)
    
    
    
    min <- as.Date("1999-1-1")
    max <- NA
    
    DK_inteff$year <- as.Date(ISOdate(DK_inteff$year, 1, 1))
    
    ggplot(data=DK_inteff, aes(x=year, y=emp_basechange_pct, group=branche, colour=branche)) + 
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
  
  # EXTERNAL EFFECT - Predicted cumulative percentage employment change from spillovers of productivity growth originating in five sectors
  {
    #Meanwhile, the percentage annual employment change resulting from the external productivity effect is given by the sum of productivity change in each sector s in the current 
    #and past three years–leaving out the industry’s own productivity growth– multiplied by the respective sector-specific coefficients and their lags. This quantity is
    #in turn multiplied by total country-level employment in the base year , since these external effects operate on the entire economy. 
    
    
    #viser hvordan branchen bliver påvirket af sector spillovers
    DK_exeff_1 = DK_effects %>% mutate(emp_change = ifelse(year==1999,0, (dLP_BwoI_b1 * (exp(0.027)-1)) + (dLP_BwoI_b1_lag1 * (exp(0.054)-1)) + (dLP_BwoI_b1_lag2 * (exp(0.049)-1)) + (dLP_BwoI_b1_lag3 * (exp(0.040)-1)) + 
                                                             (dLP_BwoI_b2 * (exp(-0.056)-1)) + (dLP_BwoI_b2_lag1 * (exp(-0.032)-1)) + (dLP_BwoI_b2_lag2 * (exp(-0.063)-1)) + (dLP_BwoI_b2_lag3 * (exp(0.015)-1)) + 
                                                             (dLP_BwoI_b3 * (exp(0.079)-1))  + (dLP_BwoI_b3_lag1 * (exp(0.049)-1)) + (dLP_BwoI_b3_lag2 * (exp(-0.031)-1)) + (dLP_BwoI_b3_lag3 * (exp(-0.025)-1)) + 
                                                             (dLP_BwoI_b4 * (exp(0.130)-1)) + (dLP_BwoI_b4_lag1 * (exp(0.158)-1)) + (dLP_BwoI_b4_lag2 * (exp(0.097)-1)) + (dLP_BwoI_b4_lag3 * (exp(-0.027)-1))))
    
    DK_exeff_1 = DK_exeff_1 %>% group_by(code) %>%  mutate(cumsum_EMP = cumsum(emp_change))
    DK_exeff_1 = DK_exeff_1 %>% mutate(emp_basechange = baseyearEMP *(cumsum_EMP/100))
    
    DK_exeff_1 = DK_exeff_1 %>% group_by(branche, year) %>% summarise(sumEMPchange=sum(emp_basechange), EMP_c_base= 1736)
    DK_exeff_1 = as.data.frame(DK_exeff_1) %>% mutate(emp_basechange_pct = (sumEMPchange/EMP_c_base)*100)
    
    DK_exeff_1_all = DK_exeff_1 %>% group_by(year) %>% summarise(sumEMPchange=sum(sumEMPchange), EMP_c_base = 1736)
    DK_exeff_1_all = DK_exeff_1_all %>% mutate(emp_basechange_pct = (sumEMPchange/EMP_c_base)*100, branche="alle")
    DK_exeff_1_all = DK_exeff_1_all %>% select(branche, year, sumEMPchange, EMP_c_base, emp_basechange_pct)
    DK_exeff_1 = rbind(DK_exeff_1,DK_exeff_1_all)
    
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
    
    
    DK_neteff2 = merge(DK_inteff, DK_exeff_2_long, by = c("branche","year"))
    DK_neteff2$neteffect = DK_neteff2$emp_basechange_pct.x + DK_neteff2$emp_basechange_pct.y
    
    min = as.Date("1999-1-1")
    max = NA
    
    DK_neteff2$year <- as.Date(ISOdate(DK_neteff2$year, 1, 1))
    
    ggplot(data=DK_neteff2, aes(x=year, y=neteffect, group=branche, colour=branche)) + 
      geom_point() + 
      geom_line() +
      xlab("Tid") + ylab("Kumulativ prædikterede ændring i beskæftigelse, pct.") +
      ggtitle("Kumulativ ændring i beskæftigelse i Danmark") +
      guides(colour=guide_legend(title="Branche")) +
      theme_economist() +
      theme(legend.position="right") +
      scale_x_date(date_breaks = "5 year", date_labels = "%Y") +
      scale_x_date(limits = c(min, max))
    
    
    # Viser hvordan branchen påvirker økonomien - den vi bruger                                   
    DK_exeff_2 = DK_effects %>% mutate(emp_change_b1 = ifelse(year==1999,0, (dLP_BwoI_b1 * (exp(0.027)-1)) + (dLP_BwoI_b1_lag1 * (exp(0.054)-1)) + (dLP_BwoI_b1_lag2 * (exp(0.049)-1)) + (dLP_BwoI_b1_lag3 * (exp(0.040)-1))),
                                       emp_change_b2 = ifelse(year==1999,0, (dLP_BwoI_b2 * (exp(-0.056)-1)) + (dLP_BwoI_b2_lag1 * (exp(-0.032)-1)) + (dLP_BwoI_b2_lag2 * (exp(-0.063)-1)) + (dLP_BwoI_b2_lag3 * (exp(0.015)-1))),
                                       emp_change_b3 = ifelse(year==1999,0, (dLP_BwoI_b3 * (exp(0.079)-1)) + (dLP_BwoI_b3_lag1 * (exp(0.049)-1)) + (dLP_BwoI_b3_lag2 * (exp(-0.031)-1)) + (dLP_BwoI_b3_lag3 * (exp(-0.025)-1)) ),
                                       emp_change_b4 = ifelse(year==1999,0, (dLP_BwoI_b4 * (exp(0.130)-1)) + (dLP_BwoI_b4_lag1 * (exp(0.158)-1)) + (dLP_BwoI_b4_lag2 * (exp(0.097)-1)) + (dLP_BwoI_b4_lag3 * (exp(-0.027)-1))))
    
    
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
    
    ggplot(data=DK_exeff_2_long, aes(x=year, y=emp_basechange_pct, group=branche, colour=branche)) + 
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
  
  # TOTAL EFFECT
  
}

