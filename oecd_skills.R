library(readr)
library(tidyverse)

data = read_csv("C:/Users/Emma/Desktop/prgdnkp1.csv")

data = data %>% select(c("AGE_R",
                         "B_Q01a", "B_Q01a3", "B_Q01b", 
                         "B_Q02a", "B_Q02b","B_Q02c", 
                         "B_Q04a",
                         "B_Q05a", "B_Q05b", "B_Q05c",
                         "B_Q10a", "B_Q10b", "B_Q10c",
                         "B_Q11",
                         "B_Q12c", "B_Q12e", "B_Q12g",
                         "B_Q13",
                         "B_Q14a", "B_Q14b",
                         "B_Q15a", "B_Q15b", "B_Q15c",
                         "B_Q16",
                         "B_Q17",
                         "B_Q18a",
                         "B_Q19a",
                         "B_Q20a", "B_Q20b",
                         "B_Q26a", "B_Q26b",
                         "D_Q04",
                         "D_Q06a", "D_Q06b", "D_Q06c",
                         "D_Q12a", "D_Q12b", "D_Q12c",
                         "D_Q13a", "D_Q13b", "D_Q13c",
                         "D_Q14",
                         "E_Q03",
                         "E_Q04",
                         "E_Q06",
                         "E_Q10",
                         "F_Q02b", "F_Q02c", "F_Q02d", "F_Q02e",
                         "F_Q03a", "F_Q03b","F_Q03c",
                         "F_Q04a", "F_Q04b",
                         "F_Q05a", "F_Q05b",
                         "F_Q06b", "F_Q06c",
                         "F_Q07a", "F_Q07b",
                         "G_Q01a", "G_Q01b", "G_Q01c", "G_Q01d", "G_Q01e", "G_Q01f", "G_Q01g", "G_Q01h",
                         "G_Q02a", "G_Q02b", "G_Q02c", "G_Q02d",
                         "G_Q03b", "G_Q03c", "G_Q03d", "G_Q03f", "G_Q03g", "G_Q03h",
                         "G_Q04",
                         "G_Q05a", "G_Q05c", "G_Q05d", "G_Q05e", "G_Q05f", "G_Q05g", "G_Q05h",
                         "G_Q06",
                         "G_Q07",
                         "G_Q08"))

#Alder
data$AGE_R[data$AGE_R <= 39] ="1"
data$AGE_R[data$AGE_R > 39 & data$AGE_R <= 59] ="2"
data$AGE_R[data$AGE_R > 59] ="3"

#Gennemført uddannelse
data$B_Q01a3[data$B_Q01a3 %in% c("1", "2", "3", "4")] = "1"
data$B_Q01a3[data$B_Q01a3 %in% c("5", "6", "7", "8", "9", "10")] = "2"
data$B_Q01a3[data$B_Q01a3 %in% c("11", "12", "13", "14")] = "3"

data$B_Q01a[data$B_Q01a %in% c("1", "2", "3", "4", "15") | data$B_Q01a3 %in% c("1", "2", "3", "4")] = "1"
data$B_Q01a[data$B_Q01a %in% c("5", "6", "7", "8", "9", "10") | data$B_Q01a3 %in% c("5", "6", "7", "8", "9", "10")] = "2"
data$B_Q01a[data$B_Q01a %in% c("11", "12", "13", "14") | data$B_Q01a3 %in% c("11", "12", "13", "14")] = "3"

#Under uddannelse
table(data$B_Q02a)

#Under uddannelse
data$B_Q02b[data$B_Q02b %in% c("1", "2", "3", "4")] = "1"
data$B_Q02b[data$B_Q02b %in% c("5", "6", "7", "8", "9", "10")] = "2"
data$B_Q02b[data$B_Q02b %in% c("11", "12", "13", "14")] = "3"

#Branche
table(data$B_Q02c)

#Under uddannelse, sidste 12 måneder
table(data$B_Q04a)

#Under uddannelse, sidste 12 måneder
data$B_Q05a[data$B_Q05a %in% c("1", "2", "3", "4")] = "1"
data$B_Q05a[data$B_Q05a %in% c("5", "6", "7", "8", "9", "10")] = "2"
data$B_Q05a[data$B_Q05a %in% c("11", "12", "13", "14")] = "3"

#Grunden til at studere
table(data$B_Q05c)



