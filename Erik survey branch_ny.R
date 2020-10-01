{
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
library(readr)
library(sjstats)
}

data = read_csv("surveydata.csv") %>% select(-X1)

# Kodning af variable --------------------------------------------------------
{
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
  data$Funktioner = ifelse(data$Functions %in% c(1,8), 1,
                               ifelse(data$Functions %in% c(6,7), 2,
                                      ifelse(data$Functions %in% c(4,5), 3, 
                                             ifelse(data$Functions %in% c(2,3,9), 4, 
                                             0))))
  
  data$Funktioner_1 = ifelse(data$Functions %in% c(1,8), 1, 0)
  data$Funktioner_2 = ifelse(data$Functions %in% c(6,7), 1, 0)
  data$Funktioner_3 = ifelse(data$Functions %in% c(4,5), 1, 0)
  data$Funktioner_4 = ifelse(data$Functions %in% c(2,3,9), 1, 0)

  
  
  data$bra10grp_code1 = ifelse(data$bra10grp_code %in% c(2,3), 1,
                               ifelse(data$bra10grp_code %in% c(5,6,8), 2,
                                      ifelse(data$bra10grp_code %in% c(4,7), 3, 0)))
  
  data$bra10grp_code1_1 = ifelse(data$bra10grp_code %in% c(2,3), 1, 0)
  data$bra10grp_code1_2 = ifelse(data$bra10grp_code %in% c(5,6,8), 1, 0)
  data$bra10grp_code1_3 = ifelse(data$bra10grp_code %in% c(4,7), 1, 0)
  
  }




{
## Robotter
data$Leveremodtageoutput = ifelse(data$F1 %in% c(1,2), 1, 0)
data$Startovervågestopperobottter = ifelse(data$F2 %in% c(1,2), 1, 0)
data$Robot = ifelse(data$F2 %in% c(1,2), 1,
                    ifelse(data$F1 %in% c(1,2),1,0))

## Advancerede teknologier
data$Advancerettek1 = ifelse(data$G1 %in% c(1,2), 1, 0)
data$Advancerettek2 = ifelse(data$G2 %in% c(1,2), 1, 0)
data$Advancerettek3 = ifelse(data$G3 %in% c(1,2), 1, 0)
}
# Ændring til dummy variable ---------------
{
data$D2 = ifelse(data$D2 %in% c(1,2,3), 1, 0)
data$D1 = ifelse(data$D1 %in% c(1,2), 1, 0)
data$D3 = ifelse(data$D3 %in% c(1,2), 1, 0)
data$D4 = ifelse(data$D4 %in% c(1,2), 1, 0)

data$C1 = ifelse(data$C1==1, 1, 0)
data$C2 = ifelse(data$C2==1, 1, 0)
data$C3 = ifelse(data$C3==1, 1, 0)
data$C4 = ifelse(data$C4==1, 1, 0)
data$C5 = ifelse(data$C5==1, 1, 0)

data$B3 = ifelse(data$B3 %in% c(1,2),1,0)
data$B5 = ifelse(data$B5 %in% c(1,2),1,0)
data$B7 = ifelse(data$B7 %in% c(1,2),1,0)
data$B9 = ifelse(data$B9 %in% c(1,2),1,0)
data$B10 = ifelse(data$B11 %in% c(1,2),1,0)
}

data$F1 = ifelse(data$F1 %in% c(1,2), 1, 0)
data$F2 = ifelse(data$F2 %in% c(1,2), 1, 0)
data$G1 = ifelse(data$G1 %in% c(1,2), 1, 0)
data$G2 = ifelse(data$G2 %in% c(1,2), 1, 0)
data$G3 = ifelse(data$G3 %in% c(1,2), 1, 0)

x = data_A1 %>% filter(F2 == 1)


# Dataforberedelse -----------------------------------------------------------------------

# Filtreret for A1=1 (Har du for tiden en lønnet hovedbeskæftigelse), samt branche 9 og 10

#data_A1 = data %>% filter(A1 == 1) %>% filter(Functions != "None") %>% filter(bra10grp != "Offentlig administration, undervisning og sundhed") %>% filter(bra10grp != "Kultur, fritid og anden service") %>% filter(bra10grp != "Landbrug, skovbrug og fiskeri")
data_A1 = data %>% filter(A1 == 1, Functions != "None", bra10grp_code != 1, bra10grp_code != 9, bra10grp_code != 10)

# Filtreret for A1=1, og A5=1 (Havde du i 2016 en lønnet hovedbeskæftigelse? ), samt branche 9 og 10
data_A1A5 = data %>% filter(A1 == 1, A5 == 1, Functions != "None", bra10grp_code != 1, bra10grp_code != 9, bra10grp_code != 10)

# Survey vægtning
svydesign_A1 = svydesign(id=~Resp_id1, weights = ~pervgt, data=data_A1, nest=TRUE)
svydesign_A1A5 = svydesign(id=~Resp_id1, weights = ~pervgt, data=data_A1A5, nest=TRUE)

# ORGANISERING  --------------

#Nuværende:

#Hvor ofte indebærer din hovedbeskæftigelse: At du løser uforudsete problemer på egen hånd?
regb3 = {svyglm(B3 ~ factor(bra10grp_code1) + factor(Funktioner) + factor(aldergrp) + factor(loengrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3,
               family=gaussian(), 
               design=svydesign_A1, 
               data=data_A1)}

#Hvor ofte indebærer din hovedbeskæftigelse: Komplekse opgaver?
regb5 = {svyglm(B5 ~ factor(bra10grp_code1) + factor(Funktioner) + factor(aldergrp) + factor(loengrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3,
                family=gaussian(), 
                design=svydesign_A1, 
                data=data_A1)}

#Hvor ofte indebærer din hovedbeskæftigelse: Korte, rutineprægede og gentagne arbejdsopgaver af en varighed på mindre end 10 minutter?
regb7 = {svyglm(B7 ~ factor(bra10grp_code1) + factor(Funktioner) + factor(aldergrp) + factor(loengrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3,
               family=gaussian(), 
               design=svydesign_A1, 
               data=data_A1)}

#Hvor ofte indebærer din hovedbeskæftigelse: At du er i stand til at vælge eller ændre dine arbejdsmetoder?
regb9 = {svyglm(B9 ~ factor(bra10grp_code1) + factor(Funktioner) + factor(aldergrp) + factor(loengrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3,
               family=gaussian(), 
               design=svydesign_A1, 
               data=data_A1)}

#Hvor ofte indebærer din hovedbeskæftigelse: At du arbejder i en gruppe eller et team, som har fælles opgaver og selv kan planlægge arbejdet?
regb10 = {svyglm(B10 ~ factor(bra10grp_code1) + factor(Funktioner) + factor(aldergrp) + factor(loengrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3,
                family=gaussian(), 
                design=svydesign_A1, 
                data=data_A1)}


#Nuværende vs 2016:

{
#Sammenlignet med din hovedbeskæftigelse i 2016: At du løser uforudsete problemer på egen hånd?
regc1 = {svyglm(C1 ~ factor(bra10grp_code1) + factor(udgrp) + factor(Funktioner) + factor(aldergrp) + factor(loengrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3,
               family=gaussian(), 
               design=svydesign_A1A5, 
               data=data_A1A5)}

data_A1A5$lo
  
summary({svyglm(B3 ~ 0 + factor(loengrp),
                  family=gaussian(), 
                  design=svydesign_A1A5, 
                  data=data_A1A5)})  


library(lmtest)
step1 = {svyglm(C1 ~ factor(bra10grp_code1) + factor(udgrp) + factor(Funktioner) + factor(aldergrp) + factor(loengrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3,
                family=gaussian(), 
                design=svydesign_A1A5, 
                data=data_A1A5)}
res = residuals(step1)
step2 = {svyglm(C1 ~ factor(bra10grp_code1) + factor(Funktioner) + factor(aldergrp) + factor(loengrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3 + res,
                        family=gaussian(), 
                        design=svydesign_A1A5, 

                
                                        data=data_A1A5)}
data_A1A5$res = res

coeftest(step2)



  
#Sammenlignet med din hovedbeskæftigelse i 2016: Komplekse problemer?
regc2 = {svyglm(C2 ~ factor(bra10grp_code1) + + factor(udgrp) + factor(Funktioner) + factor(aldergrp) + factor(loengrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3, 
               family=gaussian(), 
               design=svydesign_A1A5, 
               data=data_A1A5)}

#Sammenlignet med din hovedbeskæftigelse i 2016: Korte, rutineprægede og gentagne arbejdsopgaver af en varighed på mindre end 10 minutter?
regc3 = {svyglm(C3 ~ factor(bra10grp_code1) + factor(Funktioner) + factor(aldergrp) + factor(loengrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3,
               family=gaussian(), 
               design=svydesign_A1A5, 
               data=data_A1A5)}


regc3_alt = {svyglm(C3 ~ bra10grp_code1_1 + bra10grp_code1_2 + bra10grp_code1_3 + Funktioner_1 + Funktioner_2 + Funktioner_3 + Funktioner_4 + Alder1839 + Alder4059 + Alder60 + loen1 + loen2 + loen3 + loen4 + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3,
                family=gaussian(), 
                design=svydesign_A1A5, 
                data=data_A1A5)}

#Sammenlignet med din hovedbeskæftigelse i 2016: At du er i stand til at vælge eller ændre dine arbejdsmetoder?
regc4 = {svyglm(C4 ~ factor(bra10grp_code1) + factor(Funktioner)  + factor(aldergrp) + factor(loengrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3,
               family=gaussian(), 
               design=svydesign_A1A5, 
               data=data_A1A5)}

#Sammenlignet med din hovedbeskæftigelse i 2016: At du selv har mulighed for at ændre dit arbejdstempo?
regc5 = {svyglm(C5 ~ factor(bra10grp_code1) + factor(Funktioner) + factor(aldergrp) + factor(loengrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3,
               family=gaussian(), 
               design=svydesign_A1A5, 
               data=data_A1A5)}

#regoutput_org <- formattable(cbind(regc1_coef, regc2_coef, regc3_coef, regc4_coef, regc5_coef), digits = 4, format = "f") #indstillingerne bliver ikke overført til excel
regoutput_org = as.data.frame(cbind(regc1_coef, regc2_coef, regc3_coef, regc4_coef, regc5_coef))
write.xlsx(regoutput_org, "regoutput_org.xlsx", sheetName = "regoutput_org", col.names = TRUE, row.names = TRUE)

regoutput_org_HC3 = as.data.frame(cbind(regc1_coef_HC3, regc2_coef_HC3, regc3_coef_HC3, regc4_coef_HC3, regc5_coef_HC3))
write.xlsx(regoutput_org_HC3, "regoutput_org_HC3_3.xlsx", sheetName = "regoutput_org", col.names = TRUE, row.names = TRUE)

}

# JOBKVALITET  --------------

#Alt taget i betragtning, hvor tilfreds er du med arbejdsforholdene i din hovedbeskæftigelse?
reg_d1 = svyglm(D1 ~ factor(bra10grp_code1) + factor(aldergrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3,
                family=gaussian(), 
                design=svydesign_A1, data=data_A1)


#Jeg mister muligvis mit arbejde inden for de næste 6 måneder
reg_d2 = svyglm(D2 ~ factor(bra10grp_code1) + factor(Funktioner) + factor(loengrp) + factor(aldergrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3 + B7,
                family=gaussian(), 
                design=svydesign_A1, 
                data=data_A1)#I betragtning af alle mine bestræbelser og resultater i mit job, føler jeg, at jeg bliver betalt behørigt

reg_d3 = svyglm(D3 ~ factor(bra10grp_code1) + factor(Funktioner) + factor(loengrp) + factor(aldergrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3,
                family=gaussian(), 
                design=svydesign_A1, 
                data=data_A1)


#Mit arbejde giver gode karrieremulighed
reg_d4 = svyglm(D4 ~ factor(bra10grp_code1) + factor(Funktioner) + factor(loengrp)  + factor(aldergrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3 + B7,
                family=gaussian(), 
                design=svydesign_A1, 
                data=data_A1)

#Excel output
regoutput_kval_HC3 = as.data.frame(cbind(reg_d1_coef_HC3, reg_d2_coef_HC3, reg_d3_coef_HC3, reg_d4_coef_HC3))
write.xlsx(regoutput_kval_HC3, "regoutput_kval_HC3_1.xlsx", sheetName = "regoutput_kval_HC3", col.names = TRUE, row.names = TRUE, )

#Modeltjek

prob.table(table(data_A1A5$Functions, data_A1A5$C1))

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



