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
  library(regclass)
}


#Indlæs data
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
  
  #Funktioner
  data$Funktioner = ifelse(data$Functions %in% c(8,9), 1,
                               ifelse(data$Functions %in% c(6,7), 2,
                                      ifelse(data$Functions %in% c(4,5), 3, 
                                             ifelse(data$Functions %in% c(1,2,3), 4, 
                                             0))))

  
  #Brancher  
  data$bra10grp_code1 = ifelse(data$bra10grp_code %in% c(2), 1, 0)
  data$bra10grp_code2 = ifelse(data$bra10grp_code %in% c(3), 1, 0)
  data$bra10grp_code3 = ifelse(data$bra10grp_code %in% c(5,6,8), 1, 0)
  data$bra10grp_code4 = ifelse(data$bra10grp_code %in% c(4,7), 1, 0)

  #Robotter
  data$Leveremodtageoutput = ifelse(data$F1 %in% c(1,2), 1, 0)
  data$Startovervågestopperobottter = ifelse(data$F2 %in% c(1,2), 1, 0)
  data$Robot = ifelse(data$F2 %in% c(1,2), 1,
                    ifelse(data$F1 %in% c(1,2),1,0))

  #Advancerede teknologier
  data$Advancerettek1 = ifelse(data$G1 %in% c(1,2), 1, 0)
  data$Advancerettek2 = ifelse(data$G2 %in% c(1,2), 1, 0)
  data$Advancerettek3 = ifelse(data$G3 %in% c(1,2), 1, 0)
  data$Adv= ifelse(data$G3 %in% c(1,2), 1, 
                 ifelse(data$G2 %in% c(1,2), 1,
                        ifelse(data$G1 %in% c(1,2), 1,0)))

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
data$virk = ifelse(data$strata %in% c(1,3,5,7,9), 0, 1)

# Dataforberedelse -----------------------------------------------------------------------

# Filtreret for A1=1 (Har du for tiden en lønnet hovedbeskæftigelse), samt branche 9 og 10

#data_A1 = data %>% filter(A1 == 1) %>% filter(Functions != "None") %>% filter(bra10grp != "Offentlig administration, undervisning og sundhed") %>% filter(bra10grp != "Kultur, fritid og anden service") %>% filter(bra10grp != "Landbrug, skovbrug og fiskeri")
data_A1 = data %>% filter(A1 == 1, Functions != "None", bra10grp_code != 1, bra10grp_code != 1, bra10grp_code != 9, bra10grp_code != 10)
# Filtreret for A1=1, og A5=1 (Havde du i 2016 en lønnet hovedbeskæftigelse? ), samt branche 9 og 10
data_A1A5 = data %>% filter(A1 == 1, A5 == 1, Functions != "None", bra10grp_code != 1, bra10grp_code != 9, bra10grp_code != 10)

# Survey vægtning
svydesign_A1 = svydesign(id=~Resp_id1, weights = ~pervgt, data=data_A1, strata = ~strata)
svydesign_A1A5 = svydesign(id=~Resp_id1, weights = ~pervgt, data=data_A1A5, strata = ~strata)

# ORGANISERING  --------------
#At du løser uforudsete problemer på egen hånd?
regb3 = {svyglm(B3 ~ bra10grp_code2 +  bra10grp_code3 + bra10grp_code4 + udgrp + factor(Funktioner) + aldergrp + loengrp + F1 + F2 + G1 + G2 + G3 + virk,
                family=gaussian(),
                design=svydesign_A1, 
                data=data_A1)}


#Komplekse problemer?
regb5 = {svyglm(B5 ~ bra10grp_code2 +  bra10grp_code3 + bra10grp_code4 + udgrp + factor(Funktioner) + aldergrp + loengrp + F1 + F2 + G1 + G2 + G3 + virk,
                family=gaussian(),
                design=svydesign_A1, 
                data=data_A1)}

#Sammenlignet med din hovedbeskæftigelse i 2016: Korte, rutineprægede og gentagne arbejdsopgaver af en varighed på mindre end 10 minutter?
regb7 = {svyglm(B7 ~ bra10grp_code2 +  bra10grp_code3 + bra10grp_code4 + udgrp + factor(Funktioner) + aldergrp + loengrp + F1 + F2 + G1 + G2 + G3 + virk,
                family=gaussian(),
                design=svydesign_A1, 
                data=data_A1)}

#Sammenlignet med din hovedbeskæftigelse i 2016: At du løser uforudsete problemer på egen hånd?
regc1 = {svyglm(C1 ~ bra10grp_code2 +  bra10grp_code3 + bra10grp_code4 + udgrp + factor(Funktioner) + aldergrp + loengrp + F1 + F2 + G1 + G2 + G3 + virk,
               family=gaussian(),
               design=svydesign_A1A5, 
               data=data_A1A5)}


#Sammenlignet med din hovedbeskæftigelse i 2016: Komplekse problemer?
regc2 = {svyglm(C2 ~ bra10grp_code2 +  bra10grp_code3 + bra10grp_code4 + udgrp + factor(Funktioner) + aldergrp + loengrp + F1 + F2 + G1 + G2 + G3 + virk,
                family=gaussian(),
                design=svydesign_A1A5, 
                data=data_A1A5)}

#Sammenlignet med din hovedbeskæftigelse i 2016: Korte, rutineprægede og gentagne arbejdsopgaver af en varighed på mindre end 10 minutter?
regc3 = {svyglm(C3 ~ bra10grp_code2 +  bra10grp_code3 + bra10grp_code4 + udgrp + factor(Funktioner) + aldergrp + loengrp + F1 + F2 + G1 + G2 + G3 + virk,
                family=gaussian(),
                design=svydesign_A1A5, 
                data=data_A1A5)}


#Jeg mister muligvis mit arbejde inden for de næste 6 måneder
regd2 = {svyglm(D2 ~ bra10grp_code2 +  bra10grp_code3 + bra10grp_code4 + udgrp + factor(Funktioner) + aldergrp + loengrp + F1 + F2 + G1 + G2 + G3 + virk,
                family=gaussian(),
                design=svydesign_A1, 
                data=data_A1)}

#Mit arbejde giver gode karrieremulighed
regd4 = {svyglm(D4 ~ bra10grp_code2 +  bra10grp_code3 + bra10grp_code4 + udgrp + factor(Funktioner) + aldergrp + loengrp + F1 + F2 + G1 + G2 + G3 + virk,
                family=gaussian(),
                design=svydesign_A1, 
                data=data_A1)}


#MODELTJEK --------------------------------------------------------
#Multikollinaritets-test
VIF(regb3)
VIF(regb5)
VIF(regb7)
VIF(regd2)
VIF(regd4)
VIF(regc1)
VIF(regc2)
VIF(regc3)

#Ingen værdier der er betydeligt høje. Dette kan være svært at vurdere.

resettest(regb3, type="fitted")
resettest(regb5, type="fitted")
resettest(regb7, type="fitted")
resettest(regd2, type="fitted")
resettest(regd4, type="fitted")
resettest(regc1, type="fitted")
resettest(regc2, type="fitted")
resettest(regc3, type="fitted")

#DESKRIPTIVT: --------------------------------------------------------

data_A1$Jobusikkerhed = data_A1$D2
data_A1$Karrieremuligheder = data_A1$D4
data_A1$'Oftere: Komplekse opgaver' = data_A1$C2
df.long1 = data_A1 %>% select(Resp_id1, C1, 'Oftere: Komplekse opgaver', C3) %>% gather(variable, value, -Resp_id1) %>% filter(value!=8,value!=9) 

df.long1_pct <- df.long1 %>% group_by(variable, value) %>% summarise(count=n()) %>% mutate(perc=count/sum(count))

ggplot(df.long1_pct, aes(x=variable, y = perc*100, fill=as.factor(value))) + 
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "", y = "Pct.", fill = "") +
  theme_economist() + scale_color_economist()# + theme_minimal(base_size = 14) + ggtitle("")


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
library (ggplot2)
library(reshape2)

a = c("Arbejder (lav)","Arbejder (høj) ","Funktionær (lav)","Funktionær (høj)")
b = c(0.7358, 0.8868, 0.7982, 0.9045)
c = c(0.4057, 0.6226, 0.3814, 0.7672)
d = c(0.3491, 0.3585, 0.4495, 0.2657)
df = melt(data.frame (a,b,c,d))
df$variable = c("Uforudsete problemer","Uforudsete problemer", "Uforudsete problemer", "Uforudsete problemer","Komplekse opgaver","Komplekse opgaver", "Komplekse opgaver", "Komplekse opgaver","Rutineopgaver", "Rutineopgaver", "Rutineopgaver", "Rutineopgaver")

ggplot (df, aes(x=a, y=value, fill=variable)) + geom_bar (stat="identity", position ="dodge") + labs(x = "", y = "Pct.")

a = c("Arbejder (lav)","Arbejder (høj) ","Funktionær (lav)","Funktionær (høj)")
b = c(0.1415, 0.1321,  0.0275, 0.0896)
c = c(0.1415, 0.1132, 0.00, 0.0537)
d = c(0.2075, 0.1509, 0.2752, 0.2417)
e = c(0.2547, 0.0943, 0.1468, 0.1045)
f = c(0.1792, 0.0755, 0.1376, 0.1284)
df = melt(data.frame (a,b,c,d,e,f))
df$variable = c("Robot (input/output)","Robot (input/output)","Robot (input/output)","Robot (input/output)","Robot (kontrol)","Robot (kontrol)","Robot (kontrol)","Robot (kontrol)","AI (data)","AI (data)","AI (data)","AI (data)","AI (ordre)","AI (ordre)","AI (ordre)","AI (ordre)","ML","ML","ML","ML")

a = c("Arbejder (lav)","Arbejder (høj) ","Funktionær (lav)","Funktionær (høj)")
b = c(0.1415, 0.1321,  0.0275, 0.0896)
c = c(0.1415, 0.1132, 0.00, 0.0537)
d = c(0.2075, 0.1509, 0.2752, 0.2417)
e = c(0.2547, 0.0943, 0.1468, 0.1045)
f = c(0.1792, 0.0755, 0.1376, 0.1284)
df = melt(data.frame (a,b,c,d,e,f))
df$variable = c("Input/output fra robot","Input/output fra robot","Input/output fra robot","Input/output fra robot","Kontrollere robot","Kontrollere robot","Kontrollere robot","Kontrollere robot","Anvende information fra computer","Anvende information fra computer","Anvende information fra computer","Anvende information fra computer","Ordre fra computer","Ordre fra computer","Ordre fra computer","Ordre fra computer","Maskinlæring","Maskinlæring","Maskinlæring","Maskinlæring")

a = c("Arbejder (lav)","Arbejder (høj) ","Funktionær (lav)","Funktionær (høj)")
b = c(0.2547, 0.1132, 0.2018, 0.1612)
c = c(0.2075, 0.4151, 0.4862, 0.6119)
df = melt(data.frame (a,b,c))
df$variable = c("Jobusikkerhed","Jobusikkerhed","Jobusikkerhed","Jobusikkerhed","Karrieremuligheder","Karrieremuligheder","Karrieremuligheder","Karrieremuligheder")

a = c("Industri","Bygge og anlæg","Højteknologisk","Lavteknologisk")
b = c(0.2317,0.2156,0.0676,0.2375)
c = c(0.1158,0.5294,0.0096, 0.0276)
d = c(0.0976, 0.03921, 0.15458, 0.3259)
e = c(0.5548, 0.2156, 0.7681, 0.4088)
df = melt(data.frame (a,b,c,d,e))
df$variable = c("Arbejder (lav)","Arbejder (lav)","Arbejder (lav)","Arbejder (lav)","Arbejder (høj)","Arbejder (høj)","Arbejder (høj)","Arbejder (høj)","Funktionær (lav)","Funktionær (lav)","Funktionær (lav)","Funktionær (lav)","Funktionær (høj)","Funktionær (høj)","Funktionær (høj)","Funktionær (høj)")



a = c("Industri","Bygge og anlæg","Højteknologisk","Lavteknologisk")
b = c(0.1585, 0.0196, 0.0870, 0.0552)
c = c(0.1585, 0.00, 0.0338, 0.0331)
d = c(0.2682, 0.0392, 0.2608, 0.2265)
e = c(0.1768, 0.0196, 0.1353, 0.1381)
f = c(0.1646, 0.0392, 0.1691, 0.0939)
df = melt(data.frame (a,b,c,d,e,f))
df$variable = c("Robot (input/output)","Robot (input/output)","Robot (input/output)","Robot (input/output)","Robot (kontrol)","Robot (kontrol)","Robot (kontrol)","Robot (kontrol)","AI (data)","AI (data)","AI (data)","AI (data)","AI (ordre)","AI (ordre)","AI (ordre)","AI (ordre)","ML","ML","ML","ML")

a = c("Industri","Bygge og anlæg","Højteknologisk","Lavteknologisk")
b = c(0.0641, 0.0434, 0.0638, 0.0244)
c = c(0.0769, 0.00, 0.0319, 0.0244)
d = c(0.1474, 0.00, 0.1809, 0.1037)
e = c(0.1090, 0.00, 0.0798, 0.0732)
f = c(0.0705, 0.0217, 0.1011, 0.0610)
df = melt(data.frame (a,b,c,d,e,f))
df$variable = c("Oftere: Robot (input/output)","Oftere: Robot (input/output)","Oftere: Robot (input/output)","Oftere: Robot (input/output)","Oftere: Robot (kontrol)","Oftere: Robot (kontrol)","Oftere: Robot (kontrol)","Oftere: Robot (kontrol)","Oftere: AI (data)","Oftere: AI (data)","Oftere: AI (data)","Oftere: AI (data)","Oftere: AI (ordre)","Oftere: AI (ordre)","Oftere: AI (ordre)","Oftere: AI (ordre)", "Oftere: ML","Oftere: ML","Oftere: ML","Oftere: ML")

a = c("Industri","Bygge og anlæg","Højteknologisk","Lavteknologisk")
b = c(0.8717, 0.8695, 0.9042, 0.8231)
c = c(0.6795, 0.6522, 0.7248, 0.5060)
d = c(0.3397, 0.3261, 0.25, 0.3780)
df = melt(data.frame (a,b,c,d))
df$variable = c("Uforudsete problemer","Uforudsete problemer","Uforudsete problemer","Uforudsete problemer","Kompelske opgaver","Kompelske opgaver","Kompelske opgaver","Kompelske opgaver","Rutineopgaver","Rutineopgaver","Rutineopgaver","Rutineopgaver")

a = c("Industri","Bygge og anlæg","Højteknologisk","Lavteknologisk")
b = c(0.3356, 0.3261, 0.2713, 0.3232)
c = c(0.3012, 0.2609, 0.2447, 0.2622)
d = c(0.1025, 0.0869, 0.0744, 0.1159)
df = melt(data.frame (a,b,c,d))
df$variable = c("Oftere: Uforudsete problemer","Oftere: Uforudsete problemer","Oftere: Uforudsete problemer","Oftere: Uforudsete problemer","Oftere: Kompelske opgaver","Oftere: Kompelske opgaver","Oftere: Kompelske opgaver","Oftere: Kompelske opgaver","Oftere: Rutineopgaver","Oftere: Rutineopgaver","Oftere: Rutineopgaver","Oftere: Rutineopgaver")


a = c("Arbejder (lav)","Arbejder (høj) ","Funktionær (lav)","Funktionær (høj)")
b = c(0.2887, 0.3333, 0.3263, 0.2962)
c = c(0.1856, 0.3125, 0.2842, 0.2803)
d = c(0.1340, 0.1250, 0.1368, 0.0669)
df = melt(data.frame (a,b,c,d))
df$variable = c("Oftere: Uforudsete problemer","Oftere: Uforudsete problemer", "Oftere: Komplekse opgaver","Oftere: Komplekse opgaver", "Oftere: Rutineopgaver", "Oftere: Rutineopgaver")

a = c("Arbejder (lav)","Arbejder (høj) ","Funktionær (lav)","Funktionær (høj)")
b = c(0.0619, 0.0417, 0.0316, 0.0541)
c = c(0.0722, 0.0833, 0.0105, 0.0318)
d = c(0.1031, 0.0833, 0.0947, 0.1624)
e = c(0.1134, 0.0625, 0.0737, 0.0732)
f = c(0.0928, 0.0208, 0.0526, 0.0828)
df = melt(data.frame (a,b,c,d,e,f))
df$variable = c("Oftere: Robot (input/output)","Oftere: Robot (input/output)","Oftere: Robot (input/output)","Oftere: Robot (input/output)","Oftere: Robot (kontrol)","Oftere: Robot (kontrol)","Oftere: Robot (kontrol)","Oftere: Robot (kontrol)","Oftere: AI (data)","Oftere: AI (data)","Oftere: AI (data)","Oftere: AI (data)","Oftere: AI (ordre)","Oftere: AI (ordre)","Oftere: AI (ordre)","Oftere: AI (ordre)", "Oftere: ML","Oftere: ML","Oftere: ML","Oftere: ML")

a = c("Arbejder (lav)","Arbejder (høj) ","Funktionær (lav)","Funktionær (høj)")
b = c(0.2547, 0.1132, 0.2018, 0.1612)
c = c(0.2075, 0.40, 0.4862, 0.6119)
df = melt(data.frame (a,b,c))
df$variable = c("Jobusikkerhed", "Jobusikkerhed", "Jobusikkerhed", "Jobusikkerhed", "Karrieremuligheder","Karrieremuligheder","Karrieremuligheder","Karrieremuligheder") 

a = c("Industri","Bygge og anlæg","Højteknologisk","Lavteknologisk")
b = c(0.2547, 0.1132, 0.2018, 0.1612)
c = c(0.2075, 0.40, 0.4862, 0.6119)
df = melt(data.frame (a,b,c))
df$variable = c("Jobusikkerhed", "Jobusikkerhed", "Jobusikkerhed", "Jobusikkerhed", "Karrieremuligheder","Karrieremuligheder","Karrieremuligheder","Karrieremuligheder") 


newdataset = as.data.frame(newdataset)
newdataset$branche = c(1,2,3,4,1,2,3,4,1,2,3,4)
newdataset$opgaver = c(0.7358, 0.8868, 0.7982, 0.9045, 0.4057, 0.6226, 0.3814, 0.7672,0.3491, 0.3585, 0.4495, 0.2657)

ggplot(newdataset, aes(factor(branche), fill = factor(opgaver))) + geom_bar(position = "dodge2")

df.long3 = data_A1A5 %>% select(Resp_id1, F1a, F2a, G1a, G3a) %>% gather(variable, value, -Resp_id1) %>% filter(value!=8,value!=9)
df.long3_pct <- df.long3 %>% group_by(variable, value) %>% summarise(count=n()) %>% mutate(perc=count/sum(count))

ggplot(df.long3_pct, aes(x=variable, y = perc*100, fill=as.factor(value))) + 
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "Spørgsmål", y = "Procent", fill = "1=Oftere, 2=Sjældnere, 3=Uændret") +
  theme_economist() + scale_color_economist() # + theme_minimal(base_size = 14) + ggtitle("")

mtcars$cyl

ggplot(newdataset, aes(x=branche)) + geom_bar(position = "dodge")


#Svar på spørgsmål F1A, F2A, G1A, G3A

data_A1A5$'Oftere: ML' = data_A1A5$G3a
data_A1A5$'Oftere: AI(ordre)' = data_A1A5$G2a
data_A1A5$'Oftere: AI(data)' = data_A1A5$G1a
data_A1A5$'Oftere: Robot(i/o)' = data_A1A5$F1a
data_A1A5$'Oftere: Robot(kontrol)' = data_A1A5$F2a
data_A1$'Uforudsete problemer' = data_A1$B3
data_A1$'Komplekse opgaver' = data_A1$B5
data_A1$'Rutineprægede opgaver' = data_A1$B7
data_A1A5$'Oftere: Uforudsete problemer' = data_A1A5$C1
data_A1A5$'Oftere: Komplekse opgaver' = data_A1A5$C2
data_A1A5$'Oftere: Rutineprægede opgaver' = data_A1A5$C3
data_A1$'Jobusikkerhed' = data_A1$D2
data_A1$'Karrieremuligheder' = data_A1$D4
data_A1$'Avanceret teknologi' = data_A1$Adv
data_A1$'Robot' = data_A1$Robot

library(tidyr)
df.long3b = data_A1A5 %>% select(Resp_id1, 'Oftere: Uforudsete problemer','Oftere: Rutineprægede opgaver', 'Oftere: Komplekse opgaver') %>% gather(variable, value, -Resp_id1) %>% filter(value!=8,value!=9)
df.long3_pct <- df.long3b %>% group_by(variable, value) %>% summarise(count=n()) %>% mutate(perc=count/sum(count))

ggplot(df.long3_pct, aes(x=variable, y = perc*100, fill=as.factor(value))) + 
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "Spørgsmål", y = "Procent", fill = "") +
  theme_economist() + scale_color_economist() # + theme_minimal(base_size = 14) + ggtitle("")

df.long3b = data_A1A5 %>% select(Resp_id1, F1, F2, G1, G3) %>% gather(variable, value, -Resp_id1) %>% filter(value!=8,value!=9) 
df.long3b_pct <- df.long3b %>% group_by(variable, value) %>% summarise(count=n()) %>% mutate(perc=count/sum(count))

ggplot(df.long3b_pct, aes(x=variable, y = perc*100, fill=as.factor(value))) + 
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "Spørgsmål", y = "Pct.", fill="") +
  theme_economist() + scale_color_economist() # + theme_minimal(base_size = 14) + ggtitle("")

x = data_A1 %>% filter(D4 == 0)
table(x$ISCO_NU)

table(data_A1$ISCO_NU)

merge()
