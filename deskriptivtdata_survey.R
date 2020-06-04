library(survey)
library(dplyr)
library(haven)
library(ggplot2)
library(ggthemes)

dok1 = "surveydata.csv"
data = read.csv(dok1)
data = data %>% select(c(-X))

#Deskriptivt
data$Functions = as.numeric(data$Functions)
data$bra10grp = as.numeric(data$bra10grp)
data$aldergrp = as.numeric(data$aldergrp)
data$udgrp = as.numeric(data$udgrp)
data$loengrp = as.numeric(data$loengrp)
data$strata = as.numeric(data$strata)


ggplot(data, aes(Functions)) + 
  geom_histogram(binwidth = 0.5, color="black") + 
  scale_x_discrete(limits=c("Ledelsesarbejde","Højeste niveau","Mellemniveau","Kontor- og kundeservicearbejde","Service- og salgsarbejde","Landbrug, skovbrug og fiskeri","Håndværkspræget arbejde","Operatør-, monterings- og transportarbejde","Andet manuelt arbejde","9999")) + 
  theme_economist() + scale_color_economist() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ylab("") +  
  xlab("") + 
  ggtitle("Arbejdsfunktioner")

ggplot(data, aes(bra10grp)) + 
  geom_histogram(binwidth = 0.5, color="black") + 
  scale_x_discrete(limits=c("Bygge og anlæg","Ejendomshandel og udlejning","Erhvervsservice","Finansiering og forsikring","Handel og transport","Industri, råstofindvinding og forsyningsvirksomhed","Information og kommunikation","Kultur, fritid og anden service","Landbrug, skovbrug og fiskeri","Offentlig administration, undervisning og sundhed")) + 
  theme_economist() + 
  scale_color_economist() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ylab("") + 
  xlab("") + 
  ggtitle("Brancher")


ggplot(data, aes(strata)) + 
  geom_histogram(binwidth = 0.5, color="black") + 
  scale_x_discrete(limits=c("Nordjylland, 5-49","Nordjylland, +50","Midtjylland, 5-49","Midtjylland, +50","Sydjylland, 5-49","Sydjylland, +50","Hovedstaden, 5-49","Hovedstaden, +50","Sjælland, 5-49","Sjælland, +50")) + 
  theme_economist() + 
  scale_color_economist() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ylab("") + xlab("") + 
  ggtitle("Strata")

ggplot(data, aes(aldergrp)) + 
  geom_histogram(binwidth = 0.5, color="black") + 
  ylab("") + 
  xlab("") + 
  theme_economist() + 
  scale_color_economist() +
  scale_x_discrete(limits=c("18-39 år", "40-59 år", "60 år og derover")) + 
  theme(axis.text.x = element_text(angle = 360, hjust = 0.5)) + 
  ggtitle("Alder, grupperet")

ggplot(data, aes(fill = udgrp)) + 
  geom_bar(x = data$udgrp, 
           y = (total)/sum(total)) + 
  ylab("") + 
  xlab("") + 
  theme_economist() + 
  scale_color_economist() +
  scale_x_discrete(limits=c("Grundskole og uoplyst", "Kort udd", "Mellemlang og lang udd")) + 
  theme(axis.text.x = element_text(angle = 360, hjust = 0.5)) + 
  scale_y_continuous(labels = percent) +
  ggtitle("Uddannelse")

ggplot(data, aes(loengrp)) + 
  geom_histogram(binwidth = 0.5, color="black") + 
  ylab("") + 
  xlab("") + 
  ggtitle("Løn, grupperet") + 
  theme_economist() + 
  scale_color_economist() +
  scale_x_discrete(limits=c("-22.767", "22.767-32.950", "32.950-39.406", "+39.406")) + 
  theme(axis.text.x = element_text(angle = 360, hjust = 0.5)) 
