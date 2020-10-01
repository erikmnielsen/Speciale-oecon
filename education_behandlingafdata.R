library(readr)
library(readxl)
library(writexl)
library(tidyverse)
library(dplyr)
library(tidyr)

data =  read_csv("C:/Users/Emma/Desktop/QueryDownload_Statistical_Labour-Accounts-_2020-10-01-12_19_58.csv")

data = data %>% select(c(-Code_desc, -Gender, -Age, -Database, -Variable_desc, -IndNr, -Sort_ID)) %>% filter(Country %in% c("AT", "BE", "CZ", "DE", "DK", "EE", "EL", "FI", "FR", "HR", "HU", "IT", "JP", "LV", "NL", "PL", "PT", "RO", "SE", "SI", "SK")) %>% filter(Variable == "H_shares") %>% select(-Variable) %>% filter(Code %in% c("B","C","D","E","F","G","H","I","J","K","L","MtN")) 
  
long = melt(setDT(data), id.vars = c("Country","Code", "Education"))

long = na.omit(long)

data = long %>% group_by(Country, variable, Education, Code) %>% summarize(value = sum(value))


write.csv(data, "education_csv.csv")
