library(survey)
library(dplyr)
library(haven)
library(ggplot2)
library(ggthemes)
library(GGally)

dok1 = "surveydata.csv"
data = read.csv(dok1)
data = data %>% select(c(-X))

## Alder
data$Alder1839 = ifelse(data$aldergrp == 1, 1, 0)
data$Alder4059 = ifelse(data$aldergrp == 2, 1, 0)
data$Alder60 = ifelse(data$aldergrp == 3, 1, 0)

## Uddannelse
data$udgrp1 = ifelse(data$udgrp == 1, 1, 0)
data$udgrp2 = ifelse(data$udgrp == 2, 1, 0)
data$udgrp3 = ifelse(data$udgrp == 3, 1, 0)

## Løn
data$Undermiddel = ifelse(data$loengrp == 1, 1, 
                          ifelse(data$loengrp == 2, 2, 
                                 0))
                          
data$Overmiddel = ifelse(data$loengrp == 3, 1, 
                         ifelse(data$loegrp == 4, 1,
                                0))

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
data$Landbrugskovbrugfiskeri_bra = ifelse(data$bra10grp == "Landbrug, skovbrug og fiskeri", 1, 0)
data$Industriråstofindvindingforsyningsvirksomhed = ifelse(data$bra10grp == "Industri, råstofindvinding og forsyningsvirksomhed", 1, 0)
data$Byggeanlæg = ifelse(data$bra10grp == "Bygge og anlæg", 1, 0)
data$Handeltransport = ifelse(data$bra10grp == "Handel og transport", 1, 0)
data$Informationkommunikation = ifelse(data$bra10grp == "Information og kommunikation", 1, 0)
data$Finansieringforsikring = ifelse(data$bra10grp == "Finansiering og forsikring", 1, 0)
data$Ejendomshandeludlejning = ifelse(data$bra10grp == "Ejendomshandel og udlejning", 1, 0)
data$Erhvervsservice = ifelse(data$bra10grp == "Erhvervsservice", 1, 0)
data$Offentligadministrationundervisningsundhed = ifelse(data$bra10grp == "Offentlig administration, undervisning og sundhed", 1, 0)
data$Kulturfritid = ifelse(data$bra10grp == "Kultur, fritid og anden service", 1, 0)

## Robotter

### Robot, samlet
data$Robotter = ifelse(data$F1 %in% c(1,2, 3), 1, 
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
                                   ifelse(data$G3 %in% c(1,2), 1,
                                   0)))

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

# Filtreret for A1=1
data_A1 = data %>% filter(A1 == 1) %>% filter(Functions != "None")


# Survey vægtning
svydesign = svydesign(id=~Resp_id1, weights = ~pervgt, data=data_A1, nest=TRUE)

# Tabeller
prop.table(svytable(~D2
         , design = svydesign))

#proc.table


# Filtreret for A1=1 og A5=1
data_A1A5 = data %>% filter(A1 == 1 & A5 == 1) %>% filter(Functions != "None")
table(data_A1A5$G3a)

# Filtreret for F
data_F = data %>% filter(A1 == 1 & F1 %in% c(1,2,3))

print(data_A1)

# Regression på "Alt taget i betragtning, hvor tilfreds er du med arbejdsforholdene i din hovedbeskæftigelse?"
reg1 = svyglm(D4 ~ 1 + Landbrugskovbrugfiskeri_bra + Byggeanlæg + Handeltransport +
                Informationkommunikation + Finansieringforsikring + Ejendomshandeludlejning +
                Erhvervsservice + Offentligadministrationundervisningsundhed + Kulturfritid +
                Højesteniveau + Mellemniveau + Kontorkundeservicearbejde + Servicesalgsarbejde +
                Landbrugskovbrugfiskeri_fun + Håndværksprægetarbejde + Operatørmonteringstransportarbejde +
                Andetmanueltarbejde +
                udgrp1 + udgrp2 +
                Alder4059 + Alder60 + 
                Undermiddel, 
              family=gaussian, 
              design=svydesign, 
              data=data_A1)
summary(reg1)
