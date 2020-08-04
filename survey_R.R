library(survey)
library(dplyr)
library(haven)
library(ggplot2)
library(ggthemes)
library(GGally)

data <- read_csv("surveydata.csv") %>% select(-X1)

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
data$Ledelsesarbejde = ifelse(data$Functions == 1, 1, 0)
data$Højesteniveau = ifelse(data$Functions == 2, 1, 0)
data$Mellemniveau = ifelse(data$Functions == 3, 1, 0)
data$Kontorkundeservicearbejde = ifelse(data$Functions == 4, 1, 0)
data$Servicesalgsarbejde = ifelse(data$Functions == 5, 1, 0)
data$Landbrugskovbrugfiskeri_fun = ifelse(data$Functions == 6, 1, 0)
data$Håndværksprægetarbejde = ifelse(data$Functions == 7, 1, 0)
data$Operatørmonteringstransportarbejde = ifelse(data$Functions == 8, 1, 0)
data$Andetmanueltarbejde = ifelse(data$Functions == 9, 1, 0)

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



# Meget enig eller enig i "Jeg mister muligvis mit job indenfor de næste 6 måneder"
data$D2 = ifelse(data$D2 %in% c(1,2), 1, 0)
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
data_A1 = data %>% filter(A1 == 1, Functions != "None", bra10grp_code != 1, bra10grp_code != 9, bra10grp_code != 10)

# Filtreret for A1=1, og A5=1 (Havde du i 2016 en lønnet hovedbeskæftigelse? ), samt branche 9 og 10
data_A1A5 = data %>% filter(A1 == 1 & A5 == 1) %>% filter(Functions != "None") %>% filter(bra10grp != "Offentlig administration, undervisning og sundhed") %>% filter(bra10grp != "Kultur, fritid og anden service") %>% filter(bra10grp != "Landbrug, skovbrug og fiskeri")
data_A1A5 = data %>% filter(A1 == 1, A5 == 1, Functions != "None", bra10grp_code != 1, bra10grp_code != 9, bra10grp_code != 10) 

#Hvorfor er der forskel på de to metoder?


# Survey vægtning
svydesign_A1 = svydesign(id=~Resp_id1, weights = ~pervgt, data=data_A1, nest=TRUE)
svydesign_A1A5 = svydesign(id=~Resp_id1, weights = ~pervgt, data=data_A1A5, nest=TRUE)


#DESKRIPTIVT: Indhold af arbejde  --------------------------------------------------------

#Arbejdsfunktioner
ggplot(data_A1A5, aes(Functions)) + 
  geom_histogram(binwidth = 0.5, color="black") + 
  scale_x_discrete(limits=c("Ledelsesarbejde","Højeste niveau","Mellemniveau","Kontor- og kundeservicearbejde","Service- og salgsarbejde","Landbrug, skovbrug og fiskeri","Håndværkspræget arbejde","Operatør-, monterings- og transportarbejde","Andet manuelt arbejde","9999")) + 
  theme_economist() + scale_color_economist() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ylab("") +  
  xlab("") + 
  ggtitle("Arbejdsfunktioner")




# REGRESSIONER: Indhold af arbejde  --------------------------------------------------------

#Hvor ofte indebærer din hovedbeskæftigelse: At du løser uforudsete problemer på egen hånd?
regb3 = svyglm(B3 ~ factor(bra10grp_code) + factor(udgrp) + factor(aldergrp) + factor(loengrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3,
              family=gaussian(), 
              design=svydesign_A1, 
              data=data_A1)
summary(regb3)

#Hvor ofte indebærer din hovedbeskæftigelse: Komplekse opgaver?
regb5 = {svyglm(B5 ~ factor(bra10grp_code) + factor(udgrp) + factor(aldergrp) + factor(loengrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3,
               family=gaussian(), 
               design=svydesign_A1, 
               data=data_A1)}
summary(regb5)

#Hvor ofte indebærer din hovedbeskæftigelse: Korte, rutineprægede og gentagne arbejdsopgaver af en varighed på mindre end 10 minutter?
regb7 = svyglm(B7 ~ factor(bra10grp_code) + factor(udgrp) + factor(aldergrp) + factor(loengrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3,
               family=gaussian(), 
               design=svydesign_A1, 
               data=data_A1)
summary(regb7)

#Hvor ofte indebærer din hovedbeskæftigelse: At du er i stand til at vælge eller ændre dine arbejdsmetoder?
regb9 = svyglm(B9 ~ factor(bra10grp_code) + factor(udgrp) + factor(aldergrp) + factor(loengrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3,
               family=gaussian(), 
               design=svydesign_A1, 
               data=data_A1)
summary(regb9)

#Hvor ofte indebærer din hovedbeskæftigelse: At du arbejder i en gruppe eller et team, som har fælles opgaver og selv kan planlægge arbejdet?
regb11 = svyglm(B11 ~ factor(bra10grp_code) + factor(udgrp) + factor(aldergrp) + factor(loengrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3,
               family=gaussian(), 
               design=svydesign_A1, 
               data=data_A1)
summary(regb11)


#Sammenlignet med din hovedbeskæftigelse i 2016: At du løser uforudsete problemer på egen hånd?
regc1 = svyglm(C1 ~ factor(bra10grp_code) + factor(udgrp) + factor(aldergrp) + factor(loengrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3,
               family=gaussian(), 
               design=svydesign_A1A5, 
               data=data_A1A5)

summary(regc1)

#Sammenlignet med din hovedbeskæftigelse i 2016: Komplekse problemer?
regc2 = svyglm(C2 ~ factor(bra10grp_code) + factor(udgrp) + factor(aldergrp) + factor(loengrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3, 
               family=gaussian(), 
               design=svydesign_A1A5, 
               data=data_A1A5)
summary(regc2)

#Sammenlignet med din hovedbeskæftigelse i 2016: Korte, rutineprægede og gentagne arbejdsopgaver af en varighed på mindre end 10 minutter?
regc3 = svyglm(C3 ~ factor(bra10grp_code) + factor(udgrp) + factor(aldergrp) + factor(loengrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3,
               family=gaussian(), 
               design=svydesign_A1A5, 
               data=data_A1A5)
summary(regc3)

#Sammenlignet med din hovedbeskæftigelse i 2016: At du er i stand til at vælge eller ændre dine arbejdsmetoder?
regc4 = svyglm(C4 ~ factor(bra10grp_code) + factor(udgrp) + factor(aldergrp) + factor(loengrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3,
               family=gaussian(), 
               design=svydesign_A1A5, 
               data=data_A1A5)
summary(regc4)

#Sammenlignet med din hovedbeskæftigelse i 2016: At du selv har mulighed for at ændre dit arbejdstempo?
regc5 = svyglm(C5 ~ factor(bra10grp_code) + factor(udgrp) + factor(aldergrp) + factor(loengrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3,
               family=gaussian(), 
               design=svydesign_A1A5, 
               data=data_A1A5)
summary(regc5)

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

#Sammenlignet med din hovedbeskæftigelse i 2016: at rådgive, oplære, instruere eller undervise andre – individuelt eller i grupper?
rege3a = svyglm(E3a ~ factor(bra10grp_code) + factor(udgrp) + factor(aldergrp) + factor(loengrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3,
               family=gaussian(), 
               design=svydesign_A1A5, 
               data=data_A1A5)
summary(rege3a)

#Sammenlignet med din hovedbeskæftigelse i 2016: at sælge et produkt eller en tjenesteydelse?
rege4a = svyglm(E4a ~ factor(bra10grp_code) + factor(udgrp) + factor(aldergrp) + factor(loengrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3,
               family=gaussian(), 
               design=svydesign_A1A5, 
               data=data_A1A5)
summary(regc5)

#Sammenlignet med din hovedbeskæftigelse i 2016: at forhandle med personer i eller uden for virksomheden eller organisationen?
rege1a = svyglm(E1a ~ factor(bra10grp_code) + factor(udgrp) + factor(aldergrp) + factor(loengrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3,
               family=gaussian(), 
               design=svydesign_A1A5, 
               data=data_A1A5)
summary(regc5)

#Sammenlignet med din hovedbeskæftigelse i 2016: at dele arbejdsrelateret information med andre mennesker i eller uden for virksomheden eller organisationen?
rege2a = svyglm(E2a ~ factor(bra10grp_code) + factor(udgrp) + factor(aldergrp) + factor(loengrp) + Leveremodtageoutput + Startovervågestopperobottter + Advancerettek1 + Advancerettek2 + Advancerettek3,
               family=gaussian(), 
               design=svydesign_A1A5, 
               data=data_A1A5)
summary(rege2a)


