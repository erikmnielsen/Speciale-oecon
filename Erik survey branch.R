library(survey)
library(dplyr)
library(haven)
library(ggplot2)
library(ggthemes)
library(GGally)
library(lmtest)
library(gtools)
#library(formattable)
library(openxlsx)
library(plm)

data <- read_csv("surveydata4.csv") %>% select(-X1)

# Kodning af variable --------------------------------------------------------

yes = TRUE

if (yes=FALSE){
  ## Alder
  data$Alder1839 = ifelse(data$aldergrp == 1, 1, 0)
  data$Alder4059 = ifelse(data$aldergrp == 2, 1, 0)
  data$Alder60 = ifelse(data$aldergrp == 3, 1, 0)
  
  ## Uddannelse
  data$udgrp1 = ifelse(data$udgrp == 1, 1, 0)
  data$udgrp2 = ifelse(data$udgrp == 2, 1, 0)
  data$udgrp3 = ifelse(data$udgrp == 3, 1, 0)
  
  ## Løn
  data$loen1 = ifelse(data$loengrp == 1, 1, 0) 
  data$loen2 = ifelse(data$loengrp == 2, 1, 0) 
  data$loen3 = ifelse(data$loengrp == 3, 1, 0) 
  data$loen4 = ifelse(data$loengrp == 4, 1, 0) 
  
  ## Funktioner
  data$Ledelsesarbejde = ifelse(data$Functions == 9, 1, 0)
  data$Højesteniveau = ifelse(data$Functions == 8, 1, 0)
  data$Mellemniveau = ifelse(data$Functions == 7, 1, 0)
  data$Kontorkundeservicearbejde = ifelse(data$Functions == 6, 1, 0)
  data$Servicesalgsarbejde = ifelse(data$Functions == 5, 1, 0)
  data$Landbrugskovbrugfiskeri_fun = ifelse(data$Functions == 4, 1, 0)
  data$Håndværksprægetarbejde = ifelse(data$Functions == 3, 1, 0)
  data$Operatørmonteringstransportarbejde = ifelse(data$Functions == 2, 1, 0)
  data$Andetmanueltarbejde = ifelse(data$Functions == 1, 1, 0)
  
  ## Brancher
  #data$Landbrugskovbrugfiskeri_bra = ifelse(data$bra10grp == "Landbrug, skovbrug og fiskeri", 1, 0)
  data$Industriråstofindvindingforsyningsvirksomhed = ifelse(data$bra10grp == "Industri, råstofindvinding og forsyningsvirksomhed", 1, 0)
  data$Byggeanlæg = ifelse(data$bra10grp == "Bygge og anlæg", 1, 0)
  data$Handeltransport = ifelse(data$bra10grp == "Handel og transport", 1, 0)
  data$Informationkommunikation = ifelse(data$bra10grp == "Information og kommunikation", 1, 0)
  data$Finansieringforsikring = ifelse(data$bra10grp == "Finansiering og forsikring", 1, 0)
  data$Ejendomshandeludlejning = ifelse(data$bra10grp == "Ejendomshandel og udlejning", 1, 0)
  data$Erhvervsservice = ifelse(data$bra10grp == "Erhvervsservice", 1, 0)
  #data$Offentligadministrationundervisningsundhed = ifelse(data$bra10grp == "Offentlig administration, undervisning og sundhed", 1, 0)
  #data$Kulturfritid = ifelse(data$bra10grp == "Kultur, fritid og anden service", 1, 0)
  
}

## Robotter
### Robot, samlet
data$Robotter = ifelse(data$F1 %in% c(1,2), 1, 
                       ifelse(data$F2 %in% c(1,2), 1,
                              0))

### Levere og modtage output
data$Leveremodtageoutput = ifelse(data$F1 %in% c(1,2), 1,
                                  0)

### Start, overvåge og stoppe robotter
data$Startovervågestopperobottter = ifelse(data$F2 %in% c(1,2), 1,
                                           0)


## Advancerede teknologier

### Advancerede teknologier, samlet

data$Advancerettek = ifelse(data$G1 %in% c(1,2), 1, 
                            ifelse(data$G2 %in% c(1,2), 1,
                                   ifelse(data$G3 %in% c(1,2), 1, 0)))

data$Advancerettek1 = ifelse(data$G1 %in% c(1,2), 1, 0)
data$Advancerettek2 = ifelse(data$G2 %in% c(1,2), 1, 0)
data$Advancerettek3 = ifelse(data$G3 %in% c(1,2), 1, 0)

## Sociale interaktioner
data$Socialinterationer = ifelse(data$E1 %in% c(1,2), 1, 
                                 ifelse(data$E2 %in% c(1,2), 1,
                                        ifelse(data$E3 %in% c(1,2), 1,
                                               ifelse(data$E4 %in% c(1,2), 1,
                                                      0))))





# Ændring til dummy variable ---------------

data$D2 = ifelse(data$D2 %in% c(1,2,3), 1, 0)
data$D1 = ifelse(data$D1 %in% c(1,2,3), 1, 0)
data$D3 = ifelse(data$D3 %in% c(1,2,3), 1, 0)
data$D4 = ifelse(data$D4 %in% c(1,2,3), 1, 0)

data$C1 = ifelse(data$C1==1, 1, 0)
data$C2 = ifelse(data$C2==1, 1, 0)
data$C3 = ifelse(data$C3==1, 1, 0)
data$C4 = ifelse(data$C4==1, 1, 0)
data$C5 = ifelse(data$C5==1, 1, 0)

data$B3 = ifelse(data$B3 %in% c(1,2),1,0)
data$B5 = ifelse(data$B5 %in% c(1,2),1,0)
data$B7 = ifelse(data$B7 %in% c(1,2),1,0)
data$B9 = ifelse(data$B9 %in% c(1,2),1,0)
data$B11 = ifelse(data$B11 %in% c(1,2),1,0)

data$E1 = ifelse(data$E1 %in% c(1,2), 1, 0)
data$E2 = ifelse(data$E2 %in% c(1,2), 1, 0)
data$E3 = ifelse(data$E3 %in% c(1,2), 1, 0)
data$E4 = ifelse(data$E4 %in% c(1,2), 1, 0)

data$E1a = ifelse(data$E1a==1, 1, 0)
data$E2a = ifelse(data$E2a==1, 1, 0)
data$E3a = ifelse(data$E3a==1, 1, 0)
data$E4a = ifelse(data$E4a==1, 1, 0)

data$F1 = ifelse(data$F1 %in% c(1,2), 1, 0)
data$F2 = ifelse(data$F2 %in% c(1,2), 1, 0)

data$F1a = ifelse(data$F1a==1, 1, 0)
data$F2a = ifelse(data$F2a==1, 1, 0)

data$G1 = ifelse(data$G1 %in% c(1,2), 1, 0)
data$G2 = ifelse(data$G2 %in% c(1,2), 1, 0)
data$G3 = ifelse(data$G3 %in% c(1,2), 1, 0)

data$G1a = ifelse(data$G1a==1, 1, 0)
data$G2a = ifelse(data$G2a==1, 1, 0)
data$G3a = ifelse(data$G3a==1, 1, 0)



# Dataforberedelse -----------------------------------------------------------------------

# Filtreret for A1=1 (Har du for tiden en lønnet hovedbeskæftigelse), samt branche 9 og 10

#data_A1 = data %>% filter(A1 == 1) %>% filter(Functions != "None") %>% filter(bra10grp != "Offentlig administration, undervisning og sundhed") %>% filter(bra10grp != "Kultur, fritid og anden service") %>% filter(bra10grp != "Landbrug, skovbrug og fiskeri")
data_A1 = data %>% filter(A1 == 1, Functions != "None")
#Functions != 4, bra10grp_code != 1, bra10grp_code != 9, bra10grp_code != 10

# Filtreret for A1=1, og A5=1 (Havde du i 2016 en lønnet hovedbeskæftigelse? ), samt branche 9 og 10
#data_A1A5 = data %>% filter(A1 == 1 & A5 == 1) %>% filter(Functions != "None") %>% filter(bra10grp != "Offentlig administration, undervisning og sundhed") %>% filter(bra10grp != "Kultur, fritid og anden service") %>% filter(bra10grp != "Landbrug, skovbrug og fiskeri")
data_A1A5 = data %>% filter(A1 == 1, A5 == 1, Functions != "None") 
#, bra10grp_code != 1, bra10grp_code != 9, bra10grp_code != 10

#Hvorfor er der forskel på de to metoder?


# Survey vægtning
svydesign_A1 = svydesign(id=~Resp_id1, weights = ~pervgt, data=data_A1, nest=TRUE)
svydesign_A1A5 = svydesign(id=~Resp_id1, weights = ~pervgt, data=data_A1A5, nest=TRUE)


#DESKRIPTIVT: Indhold af arbejde  --------------------------------------------------------


#Svar på spørgsmål C1-5

df.long1 = data_A1A5 %>% select(Resp_id1, C1, C2, C3, C4, C5) %>% gather(variable, value, -Resp_id1) %>% filter(value!=8,value!=9) 
df.long1_pct <- df.long1 %>% group_by(variable, value) %>% summarise(count=n()) %>% mutate(perc=count/sum(count))

ggplot(df.long1_pct, aes(x=variable, y = perc*100, fill=as.factor(value))) + 
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "Spørgsmål", y = "Procent", fill = "1=Oftere, 2=Sjældnere, 3=Uændret") +
  theme_economist() + scale_color_economist() # + theme_minimal(base_size = 14) + ggtitle("")


df.long1b = data_A1A5 %>% select(Resp_id1, B3, B5, B7, B9, B11) %>% gather(variable, value, -Resp_id1) %>% filter(value!=8,value!=9) 
df.long1b_pct <- df.long1b %>% group_by(variable, value) %>% summarise(count=n()) %>% mutate(perc=count/sum(count))

ggplot(df.long1b_pct, aes(x=variable, y = perc*100, fill=as.factor(value))) + 
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "Spørgsmål", y = "Procent", fill = "1=Altid, 2=Ofte, 3=Af og til, 4=Sjældent, 5=Aldrig") +
  theme_economist() + scale_color_economist() # + theme_minimal(base_size = 14) + ggtitle("")



#Svar på spørgsmål E1a-4a

df.long2 = data_A1A5 %>% select(Resp_id1, E1a, E2a, E3a, E4a) %>% gather(variable, value, -Resp_id1) %>% filter(value!=8,value!=9)
# ggplot(df.long2, aes(x=variable, fill=as.factor(value))) + geom_bar()
df.long2_pct <- df.long2 %>% group_by(variable, value) %>% summarise(count=n()) %>% mutate(perc=count/sum(count))

ggplot(df.long2_pct, aes(x=variable, y = perc*100, fill=as.factor(value))) + 
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "Spørgsmål", y = "Procent", fill = "1=Oftere, 2=Sjældnere, 3=Uændret") +
  theme_economist() + scale_color_economist() # + theme_minimal(base_size = 14) + ggtitle("")


df.long2b = data_A1A5 %>% select(Resp_id1, E1, E2, E3, E4) %>% gather(variable, value, -Resp_id1) %>% filter(value!=8,value!=9) 
df.long2b_pct <- df.long2b %>% group_by(variable, value) %>% summarise(count=n()) %>% mutate(perc=count/sum(count))

ggplot(df.long2b_pct, aes(x=variable, y = perc*100, fill=as.factor(value))) + 
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "Spørgsmål", y = "Procent", fill = "1=Hver dag, 2=Mindst én gang om ugen, 3=1-3 gange om måneden, 4=Sjældnere end én gang om måneden, 5=Aldrig") +
  theme_economist() + scale_color_economist() # + theme_minimal(base_size = 14) + ggtitle("")




#Svar på spørgsmål F1A, F2A, G1A, G3A

df.long3 = data_A1A5 %>% select(Resp_id1, F1a, F2a, G1a, G3a) %>% gather(variable, value, -Resp_id1) %>% filter(value!=8,value!=9)
df.long3_pct <- df.long3 %>% group_by(variable, value) %>% summarise(count=n()) %>% mutate(perc=count/sum(count))

ggplot(df.long3_pct, aes(x=variable, y = perc*100, fill=as.factor(value))) + 
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "Spørgsmål", y = "Procent", fill = "1=Oftere, 2=Sjældnere, 3=Uændret") +
  theme_economist() + scale_color_economist() # + theme_minimal(base_size = 14) + ggtitle("")

df.long3b = data_A1A5 %>% select(Resp_id1, F1, F2, G1, G3) %>% gather(variable, value, -Resp_id1) %>% filter(value!=8,value!=9) 
df.long3b_pct <- df.long3b %>% group_by(variable, value) %>% summarise(count=n()) %>% mutate(perc=count/sum(count))

ggplot(df.long3b_pct, aes(x=variable, y = perc*100, fill=as.factor(value))) + 
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "Spørgsmål", y = "Procent", fill = "1=Hver dag, 2=Mindst én gang om ugen, 3=1-3 gange om måneden, 4=Sjældnere end én gang om måneden, 5=Aldrig") +
  theme_economist() + scale_color_economist() # + theme_minimal(base_size = 14) + ggtitle("")



# REGRESSIONER: Indhold af arbejde  --------------------------------------------------------

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
      reg_coef = cbind(coeftest(regression, vcov. = vcovHC, type="HC0")[,c(1,4)], siglvl)
      #reg_coef = summary(regression)$coefficients[,c(1,4)]
      colnames(reg_coef) <- paste(name, colnames(reg_coef), sep = "_")
      
      
    } else if (method=="HC2") {
      
      siglvl = stars.pval(coeftest(regression, vcov. = vcovHC, type="HC2")[,4])
      reg_coef = cbind(coeftest(regression, vcov. = vcovHC, type="HC2")[,c(1,4)], siglvl)
      #reg_coef = summary(regression)$coefficients[,c(1,4)]
      colnames(reg_coef) <- paste(name, colnames(reg_coef), sep = "_")
      
      
    } else if (method=="HC3") {
      
      siglvl = stars.pval(coeftest(regression, vcov. = vcovHC, type="HC3")[,4])
      reg_coef = cbind(coeftest(regression, vcov. = vcovHC, type="HC3")[,c(1,4)], siglvl)
      #reg_coef = summary(regression)$coefficients[,c(1,4)]
      colnames(reg_coef) <- paste(name, colnames(reg_coef), sep = "_")
      
      
    } else if (method=="HC4") {
      
      siglvl = stars.pval(coeftest(regression, vcov. = vcovHC, type="HC4")[,4])
      reg_coef = cbind(coeftest(regression, vcov. = vcovHC, type="HC4")[,c(1,4)], siglvl)
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
  
# ORGANISERING  --------------

#Nuværende:
{
#Hvor ofte indebærer din hovedbeskæftigelse: At du løser uforudsete problemer på egen hånd?
regb3 = {svyglm(B3 ~ factor(bra10grp_code) + factor(udgrp) + factor(aldergrp) + factor(loengrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3,
               family=gaussian(), 
               design=svydesign_A1, 
               data=data_A1)}
summary(regb3)

regb3_coef_HC4 = func_coefs(regb3, "B3", "HC4")

#Hvor ofte indebærer din hovedbeskæftigelse: Komplekse opgaver?
regb5 = {svyglm(B5 ~ factor(bra10grp_code) + factor(udgrp) + factor(aldergrp) + factor(loengrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3,
                family=gaussian(), 
                design=svydesign_A1, 
                data=data_A1)}
summary(regb5)

#Hvor ofte indebærer din hovedbeskæftigelse: Korte, rutineprægede og gentagne arbejdsopgaver af en varighed på mindre end 10 minutter?
regb7 = {svyglm(B7 ~ factor(bra10grp_code) + factor(udgrp) + factor(aldergrp) + factor(loengrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3,
               family=gaussian(), 
               design=svydesign_A1, 
               data=data_A1)}
summary(regb7)
regb7_coef_HC4 = func_coefs(regb7, "B7", "HC4")

#Hvor ofte indebærer din hovedbeskæftigelse: At du er i stand til at vælge eller ændre dine arbejdsmetoder?
regb9 = {svyglm(B9 ~ factor(bra10grp_code) + factor(udgrp) + factor(aldergrp) + factor(loengrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3,
               family=gaussian(), 
               design=svydesign_A1, 
               data=data_A1)}
summary(regb9)

#Hvor ofte indebærer din hovedbeskæftigelse: At du arbejder i en gruppe eller et team, som har fælles opgaver og selv kan planlægge arbejdet?
regb11 = {svyglm(B11 ~ factor(bra10grp_code) + factor(udgrp) + factor(aldergrp) + factor(loengrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3,
                family=gaussian(), 
                design=svydesign_A1, 
                data=data_A1)}
summary(regb11)

}

#Nuværende vs 2016:

#Sammenlignet med din hovedbeskæftigelse i 2016: At du løser uforudsete problemer på egen hånd?
regc1 = {svyglm(C1 ~ factor(bra10grp_code) + factor(udgrp) + factor(aldergrp) + factor(loengrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3,
               family=gaussian(), 
               design=svydesign_A1A5, 
               data=data_A1A5)}


regc1_coef = func_coefs(regc1, "C1")
regc1_coef_HC0 = func_coefs(regc1, "C1", "HC0")
regc1_coef_HC1 = func_coefs(regc1, "C1", "HC1")
regc1_coef_HC2 = func_coefs(regc1, "C1", "HC2")
regc1_coef_HC3 = func_coefs(regc1, "C1", "HC3")
regc1_coef_HC4 = func_coefs(regc1, "C1", "HC4")


#Sammenlignet med din hovedbeskæftigelse i 2016: Komplekse problemer?
regc2 = {svyglm(C2 ~ factor(bra10grp_code) + factor(udgrp) + factor(aldergrp) + factor(loengrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3, 
               family=gaussian(), 
               design=svydesign_A1A5, 
               data=data_A1A5)}
regc2_coef = func_coefs(regc2, "C2")
regc2_coef_HC0 = func_coefs(regc2, "C2", "HC0")
regc2_coef_HC1 = func_coefs(regc2, "C2", "HC1")
regc2_coef_HC3 = func_coefs(regc2, "C2", "HC3")
regc2_coef_HC4 = func_coefs(regc2, "C2", "HC4")

#Sammenlignet med din hovedbeskæftigelse i 2016: Korte, rutineprægede og gentagne arbejdsopgaver af en varighed på mindre end 10 minutter?
regc3 = {svyglm(C3 ~ factor(bra10grp_code) + factor(udgrp) + factor(aldergrp) + factor(loengrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3,
               family=gaussian(), 
               design=svydesign_A1A5, 
               data=data_A1A5)}
regc3_coef = func_coefs(regc3, "C3")
regc3_coef_HC0 = func_coefs(regc3, "C3", "HC0")
regc3_coef_HC1 = func_coefs(regc3, "C3", "HC1")
regc3_coef_HC3 = func_coefs(regc3, "C3", "HC3")
regc3_coef_HC4 = func_coefs(regc3, "C3", "HC4")

#Sammenlignet med din hovedbeskæftigelse i 2016: At du er i stand til at vælge eller ændre dine arbejdsmetoder?
regc4 = {svyglm(C4 ~ factor(bra10grp_code) + factor(udgrp) + factor(aldergrp) + factor(loengrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3,
               family=gaussian(), 
               design=svydesign_A1A5, 
               data=data_A1A5)}
regc4_coef = func_coefs(regc4, "C4")
regc4_coef_HC0 = func_coefs(regc4, "C4", "HC0")
regc4_coef_HC1 = func_coefs(regc4, "C4", "HC1")
regc4_coef_HC3 = func_coefs(regc4, "C4", "HC3")
regc4_coef_HC4 = func_coefs(regc4, "C4", "HC4")

#Sammenlignet med din hovedbeskæftigelse i 2016: At du selv har mulighed for at ændre dit arbejdstempo?
regc5 = {svyglm(C5 ~ factor(bra10grp_code) + factor(udgrp) + factor(aldergrp) + factor(loengrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3,
               family=gaussian(), 
               design=svydesign_A1A5, 
               data=data_A1A5)}
regc5_coef = func_coefs(regc5, "C5")
regc5_coef_HC0 = func_coefs(regc5, "C5", "HC0")
regc5_coef_HC1 = func_coefs(regc5, "C5", "HC1")
regc5_coef_HC3 = func_coefs(regc5, "C5", "HC3")
regc5_coef_HC4 = func_coefs(regc5, "C5", "HC4")

#regoutput_org <- formattable(cbind(regc1_coef, regc2_coef, regc3_coef, regc4_coef, regc5_coef), digits = 4, format = "f") #indstillingerne bliver ikke overført til excel
regoutput_org = as.data.frame(cbind(regc1_coef, regc2_coef, regc3_coef, regc4_coef, regc5_coef))
write.xlsx(regoutput_org, "regoutput_org.xlsx", sheetName = "regoutput_org", col.names = TRUE, row.names = TRUE)

regoutput_org_HC0 = as.data.frame(cbind(regc1_coef_HC0, regc2_coef_HC0, regc3_coef_HC0, regc4_coef_HC0, regc5_coef_HC0))
write.xlsx(regoutput_org_HC0, "regoutput_org_HC0.xlsx", sheetName = "regoutput_org", col.names = TRUE, row.names = TRUE)

regoutput_org_HC1 = as.data.frame(cbind(regc1_coef_HC1, regc2_coef_HC1, regc3_coef_HC1, regc4_coef_HC1, regc5_coef_HC1))
write.xlsx(regoutput_org_HC1, "regoutput_org_HC1.xlsx", sheetName = "regoutput_org", col.names = TRUE, row.names = TRUE)

regoutput_org_HC3 = as.data.frame(cbind(regc1_coef_HC3, regc2_coef_HC3, regc3_coef_HC3, regc4_coef_HC3, regc5_coef_HC3))
write.xlsx(regoutput_org_HC3, "regoutput_org_HC3_1.xlsx", sheetName = "regoutput_org", col.names = TRUE, row.names = TRUE)

regoutput_org_HC4 = as.data.frame(cbind(regc1_coef_HC4, regc2_coef_HC4, regc3_coef_HC4, regc4_coef_HC4, regc5_coef_HC4))
write.xlsx(regoutput_org_HC4, "regoutput_org_HC4.xlsx", sheetName = "regoutput_org", col.names = TRUE, row.names = TRUE)



# SOCIALE INTERAKTIONER  --------------

#Nuværende:
{
#Hvor ofte plejer du i din hovedbeskæftigelse at rådgive, oplære, instruere eller undervise andre –individuelt eller i grupper?
rege3 = svyglm(E3 ~ factor(bra10grp_code) + factor(udgrp) + factor(aldergrp) + factor(loengrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3,
               data=data_A1)
summary(rege3)

#Hvor ofte plejer du i din hovedbeskæftigelse at sælge et produkt eller en tjenesteydelse?
rege4 = svyglm(E4 ~ factor(bra10grp_code) + factor(udgrp) + factor(aldergrp) + factor(loengrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3,
               family=gaussian(), 
               design=svydesign_A1, 
               data=data_A1)
summary(rege4)

#Hvor ofte plejer du i din hovedbeskæftigelse at varetage egentlig forhandlinger om kontrakter eller vilkår mere generelt med personer i eller uden for virksomheden eller organisationen?
rege1 = svyglm(E1 ~ factor(bra10grp_code) + factor(udgrp) + factor(aldergrp) + factor(loengrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3,
               family=gaussian(), 
               design=svydesign_A1, 
               data=data_A1)
summary(rege1)

#Hvor ofte plejer du i din hovedbeskæftigelse at dele arbejdsrelateret information med andre mennesker i eller uden for virksomheden eller organisationen?
rege2 = svyglm(E2 ~ factor(bra10grp_code) + factor(udgrp) + factor(aldergrp) + factor(loengrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3,
               family=gaussian(), 
               design=svydesign_A1, 
               data=data_A1)
summary(rege2)
}

#Nuværende vs 2016:

#Sammenlignet med din hovedbeskæftigelse i 2016: at rådgive, oplære, instruere eller undervise andre – individuelt eller i grupper?
reg_e3a = svyglm(E3a ~ factor(bra10grp_code) + factor(udgrp) + factor(aldergrp) + factor(loengrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3,
                family=gaussian(), 
                design=svydesign_A1A5, 
                data=data_A1A5)

reg_e3a_coef = func_coefs(reg_e3a, "E3a")
reg_e3a_coef_HC0 = func_coefs(reg_e3a, "E3a", "HC0")
reg_e3a_coef_HC1 = func_coefs(reg_e3a, "E3a", "HC1")
reg_e3a_coef_HC3 = func_coefs(reg_e3a, "E3a", "HC3")
reg_e3a_coef_HC4 = func_coefs(reg_e3a, "E3a", "HC4")



#Sammenlignet med din hovedbeskæftigelse i 2016: at sælge et produkt eller en tjenesteydelse?
reg_e4a = svyglm(E4a ~ factor(bra10grp_code) + factor(udgrp) + factor(aldergrp) + factor(loengrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3,
                family=gaussian(), 
                design=svydesign_A1A5, 
                data=data_A1A5)
reg_e4a_coef = func_coefs(reg_e4a, "E4a")
reg_e4a_coef_HC0 = func_coefs(reg_e4a, "E4a", "HC0")
reg_e4a_coef_HC1 = func_coefs(reg_e4a, "E4a", "HC1")
reg_e4a_coef_HC3 = func_coefs(reg_e4a, "E4a", "HC3")
reg_e4a_coef_HC4 = func_coefs(reg_e4a, "E4a", "HC4")

#Sammenlignet med din hovedbeskæftigelse i 2016: at forhandle med personer i eller uden for virksomheden eller organisationen?
reg_e1a = svyglm(E1a ~ factor(bra10grp_code) + factor(udgrp) + factor(aldergrp) + factor(loengrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3,
                family=gaussian(), 
                design=svydesign_A1A5, 
                data=data_A1A5)
reg_e1a_coef = func_coefs(reg_e1a, "E1a")
reg_e1a_coef_HC0 = func_coefs(reg_e1a, "E1a", "HC0")
reg_e1a_coef_HC1 = func_coefs(reg_e1a, "E1a", "HC1")
reg_e1a_coef_HC3 = func_coefs(reg_e1a, "E1a", "HC3")
reg_e1a_coef_HC4 = func_coefs(reg_e1a, "E1a", "HC4")

#Sammenlignet med din hovedbeskæftigelse i 2016: at dele arbejdsrelateret information med andre mennesker i eller uden for virksomheden eller organisationen?
reg_e2a = svyglm(E2a ~ factor(bra10grp_code) + factor(udgrp) + factor(aldergrp) + factor(loengrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3,
                family=gaussian(), 
                design=svydesign_A1A5, 
                data=data_A1A5)
reg_e2a_coef = func_coefs(reg_e2a, "E2a")
reg_e2a_coef_HC0 = func_coefs(reg_e2a, "E2a", "HC0")
reg_e2a_coef_HC1 = func_coefs(reg_e2a, "E2a", "HC1")
reg_e2a_coef_HC3 = func_coefs(reg_e2a, "E2a", "HC3")
reg_e2a_coef_HC4 = func_coefs(reg_e2a, "E2a", "HC4")


# Excel output
regoutput_soc = as.data.frame(cbind(reg_e3a_coef, reg_e4a_coef, reg_e1a_coef, reg_e2a_coef))
write.xlsx(regoutput_soc, "regoutput_soc.xlsx", sheetName = "regoutput_soc", col.names = TRUE, row.names = TRUE)

regoutput_soc_HC0 = as.data.frame(cbind(reg_e3a_coef_HC0, reg_e4a_coef_HC0, reg_e1a_coef_HC0, reg_e2a_coef_HC0))
write.xlsx(regoutput_soc_HC0, "regoutput_soc_HC0.xlsx", sheetName = "regoutput_soc_HC0", col.names = TRUE, row.names = TRUE)

regoutput_soc_HC1 = as.data.frame(cbind(reg_e3a_coef_HC1, reg_e4a_coef_HC1, reg_e1a_coef_HC1, reg_e2a_coef_HC1))
write.xlsx(regoutput_soc_HC1, "regoutput_soc_HC1.xlsx", sheetName = "regoutput_soc_HC1", col.names = TRUE, row.names = TRUE)

regoutput_soc_HC3 = as.data.frame(cbind(reg_e3a_coef_HC3, reg_e4a_coef_HC3, reg_e1a_coef_HC3, reg_e2a_coef_HC3))
write.xlsx(regoutput_soc_HC3, "regoutput_soc_HC3.xlsx", sheetName = "regoutput_soc_HC3", col.names = TRUE, row.names = TRUE)

regoutput_soc_HC4 = as.data.frame(cbind(reg_e3a_coef_HC4, reg_e4a_coef_HC4, reg_e1a_coef_HC4, reg_e2a_coef_HC4))
write.xlsx(regoutput_soc_HC4, "regoutput_soc_HC4.xlsx", sheetName = "regoutput_soc_HC4", col.names = TRUE, row.names = TRUE)



# BRUG AF TEKNOLOGI  --------------

#Nuværende:


#Nuværende vs 2016:

reg_f1a = svyglm(F1a ~ factor(bra10grp_code) + factor(udgrp) + factor(aldergrp) + factor(loengrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3,
                family=gaussian(), 
                design=svydesign_A1A5, 
                data=data_A1A5)
reg_f1a_coef = func_coefs(reg_f1a, "F1a")
reg_f1a_coef_HC0 = func_coefs(reg_f1a, "F1a", "HC0")
reg_f1a_coef_HC1 = func_coefs(reg_f1a, "F1a", "HC1")

reg_f2a = svyglm(F2a ~ factor(bra10grp_code) + factor(udgrp) + factor(aldergrp) + factor(loengrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3,
                family=gaussian(), 
                design=svydesign_A1A5, 
                data=data_A1A5)
reg_f2a_coef = func_coefs(reg_f2a, "F2a")
reg_f2a_coef_HC0 = func_coefs(reg_f2a, "F2a", "HC0")
reg_f2a_coef_HC1 = func_coefs(reg_f2a, "F2a", "HC1")

reg_g1a = svyglm(G1a ~ factor(bra10grp_code) + factor(udgrp) + factor(aldergrp) + factor(loengrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3,
                family=gaussian(), 
                design=svydesign_A1A5, 
                data=data_A1A5)
reg_g1a_coef = func_coefs(reg_g1a, "G1a")
reg_g1a_coef_HC0 = func_coefs(reg_g1a, "G1a", "HC0")
reg_g1a_coef_HC1 = func_coefs(reg_g1a, "G1a", "HC1")

reg_g2a = svyglm(G2a ~ factor(bra10grp_code) + factor(udgrp) + factor(aldergrp) + factor(loengrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3,
                family=gaussian(), 
                design=svydesign_A1A5, 
                data=data_A1A5)
reg_g2a_coef = func_coefs(reg_g2a, "G2a")
reg_g2a_coef_HC0 = func_coefs(reg_g2a, "G2a", "HC0")
reg_g2a_coef_HC1 = func_coefs(reg_g2a, "G2a", "HC1")




# Excel output
regoutput_tech = as.data.frame(cbind(reg_f1a_coef, reg_f2a_coef, reg_g1a_coef, reg_g2a_coef))
write.xlsx(regoutput_tech, "regoutput_tech.xlsx", sheetName = "regoutput_tech", col.names = TRUE, row.names = TRUE)

regoutput_tech_HC0 = as.data.frame(cbind(reg_f1a_coef_HC0, reg_f2a_coef_HC0, reg_g1a_coef_HC0, reg_g2a_coef_HC0))
write.xlsx(regoutput_tech_HC0, "regoutput_tech_HC0.xlsx", sheetName = "regoutput_tech_HC0", col.names = TRUE, row.names = TRUE)

regoutput_tech_HC1 = as.data.frame(cbind(reg_f1a_coef_HC1, reg_f2a_coef_HC1, reg_g1a_coef_HC1, reg_g2a_coef_HC1))
write.xlsx(regoutput_tech_HC1, "regoutput_tech_HC1.xlsx", sheetName = "regoutput_tech_HC1", col.names = TRUE, row.names = TRUE)


# JOBKVALITET  --------------

#Alt taget i betragtning, hvor tilfreds er du med arbejdsforholdene i din hovedbeskæftigelse?
reg_d1 = svyglm(D1 ~ factor(bra10grp_code) + factor(Functions) + factor(loengrp) + factor(udgrp) + factor(aldergrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3,
                family=gaussian(), 
                design=svydesign_A1, 
                data=data_A1)

reg_d1_coef_HC3 = func_coefs(reg_d1, "D1", "HC3")


#Jeg mister muligvis mit arbejde inden for de næste 6 måneder
reg_d2 = svyglm(D2 ~ factor(bra10grp_code) + factor(Functions) + factor(loengrp) + factor(udgrp) + factor(aldergrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3,
                family=gaussian(), 
                design=svydesign_A1, 
                data=data_A1)

summary(reg_d2)

reg_d2_coef_HC3 = func_coefs(reg_d2, "D2", "HC3")



#I betragtning af alle mine bestræbelser og resultater i mit job, føler jeg, at jeg bliver betalt behørigt
reg_d3 = svyglm(D3 ~ factor(bra10grp_code) + factor(Functions) + factor(loengrp) + factor(udgrp) + factor(aldergrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3,
                family=gaussian(), 
                design=svydesign_A1, 
                data=data_A1)

reg_d3_coef_HC3 = func_coefs(reg_d3, "D3", "HC3")

#Mit arbejde giver gode karrieremulighed
reg_d4 = svyglm(D4 ~ factor(bra10grp_code) + factor(Functions) + factor(loengrp) + factor(udgrp) + factor(aldergrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3,
                family=gaussian(), 
                design=svydesign_A1, 
                data=data_A1)

reg_d4_coef_HC3 = func_coefs(reg_d4, "D4", "HC3")


regoutput_kval_HC3 = as.data.frame(cbind(reg_d1_coef_HC3, reg_d2_coef_HC3, reg_d3_coef_HC3, reg_d4_coef_HC3))
write.xlsx(regoutput_kval_HC3, "regoutput_kval_HC3_2.xlsx", sheetName = "regoutput_kval_HC3", col.names = TRUE, row.names = TRUE, )




