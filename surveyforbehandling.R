
library(survey)
library(dplyr)
library(haven)
library(ggplot2)
library(ggthemes)
library(haven)

#test-erik

dok1 = "aau_job_2019_svar.sas7bdat"
aau_job_2019_svar = read_sas(dok1, NULL)
#View(aau_job_2019_svar)

dok2 = "disco.sas7bdat"
disco = read_sas(dok2, NULL)
#View(disco)



## Disco

#Chief executives, senior officials and legislators
disco$ISCO_NU[disco$ISCO_NU == "111" ] = "Chief executives, senior officials and legislators"
disco$ISCO_NU[disco$ISCO_NU == "1111" ] = "Chief executives, senior officials and legislators"
disco$ISCO_NU[disco$ISCO_NU == "1112" ] = "Chief executives, senior officials and legislators"
disco$ISCO_NU[disco$ISCO_NU == "1114" ] = "Chief executives, senior officials and legislators"
disco$ISCO_NU[disco$ISCO_NU == "112" ] = "Chief executives, senior officials and legislators"
disco$ISCO_NU[disco$ISCO_NU == "1120" ] = "Chief executives, senior officials and legislators"

#Administrative and commercial managers
disco$ISCO_NU[disco$ISCO_NU == "121" ] = "Administrative and commercial managers"
disco$ISCO_NU[disco$ISCO_NU == "1211" ] = "Administrative and commercial managers"
disco$ISCO_NU[disco$ISCO_NU == "1212" ] = "Administrative and commercial managers"
disco$ISCO_NU[disco$ISCO_NU == "1213" ] = "Administrative and commercial managers"
disco$ISCO_NU[disco$ISCO_NU == "1219" ] = "Administrative and commercial managers"
disco$ISCO_NU[disco$ISCO_NU == "122" ] = "Administrative and commercial managers"
disco$ISCO_NU[disco$ISCO_NU == "1221" ] = "Administrative and commercial managers"
disco$ISCO_NU[disco$ISCO_NU == "1222" ] = "Administrative and commercial managers"
disco$ISCO_NU[disco$ISCO_NU == "1223" ] = "Administrative and commercial managers"

#Production and specialised services managers
disco$ISCO_NU[disco$ISCO_NU == "131" ] = "Production and specialised services managers"
disco$ISCO_NU[disco$ISCO_NU == "1311" ] = "Production and specialised services managers"
disco$ISCO_NU[disco$ISCO_NU == "1312" ] = "Production and specialised services managers"
disco$ISCO_NU[disco$ISCO_NU == "132" ] = "Production and specialised services managers"
disco$ISCO_NU[disco$ISCO_NU == "1321" ] = "Production and specialised services managers"
disco$ISCO_NU[disco$ISCO_NU == "1322" ] = "Production and specialised services managers"
disco$ISCO_NU[disco$ISCO_NU == "1323" ] = "Production and specialised services managers"
disco$ISCO_NU[disco$ISCO_NU == "1324" ] = "Production and specialised services managers"
disco$ISCO_NU[disco$ISCO_NU == "133" ] = "Production and specialised services managers"
disco$ISCO_NU[disco$ISCO_NU == "1330" ] = "Production and specialised services managers"
disco$ISCO_NU[disco$ISCO_NU == "134" ] = "Production and specialised services managers"
disco$ISCO_NU[disco$ISCO_NU == "1341" ] = "Production and specialised services managers"
disco$ISCO_NU[disco$ISCO_NU == "1342" ] = "Production and specialised services managers"
disco$ISCO_NU[disco$ISCO_NU == "1343" ] = "Production and specialised services managers"
disco$ISCO_NU[disco$ISCO_NU == "1344" ] = "Production and specialised services managers"
disco$ISCO_NU[disco$ISCO_NU == "1345" ] = "Production and specialised services managers"
disco$ISCO_NU[disco$ISCO_NU == "1346" ] = "Production and specialised services managers"
disco$ISCO_NU[disco$ISCO_NU == "1349" ] = "Production and specialised services managers"

#Hospitality, retail and other services managers
disco$ISCO_NU[disco$ISCO_NU == "141" ] = "Hospitality, retail and other services managers"
disco$ISCO_NU[disco$ISCO_NU == "1411" ] = "Hospitality, retail and other services managers"
disco$ISCO_NU[disco$ISCO_NU == "1412" ] = "Hospitality, retail and other services managers"
disco$ISCO_NU[disco$ISCO_NU == "142" ] = "Hospitality, retail and other services managers"
disco$ISCO_NU[disco$ISCO_NU == "1420" ] = "Hospitality, retail and other services managers"
disco$ISCO_NU[disco$ISCO_NU == "143" ] = "Hospitality, retail and other services managers"
disco$ISCO_NU[disco$ISCO_NU == "1431" ] = "Hospitality, retail and other services managers"
disco$ISCO_NU[disco$ISCO_NU == "1439" ] = "Hospitality, retail and other services managers"

#Science and engineering professionals
disco$ISCO_NU[disco$ISCO_NU == "211" ] = "Science and engineering professionals"
disco$ISCO_NU[disco$ISCO_NU == "2111" ] = "Science and engineering professionals"
disco$ISCO_NU[disco$ISCO_NU == "2112" ] = "Science and engineering professionals"
disco$ISCO_NU[disco$ISCO_NU == "2113" ] = "Science and engineering professionals"
disco$ISCO_NU[disco$ISCO_NU == "2114" ] = "Science and engineering professionals"
disco$ISCO_NU[disco$ISCO_NU == "212" ] = "Science and engineering professionals"
disco$ISCO_NU[disco$ISCO_NU == "2120" ] = "Science and engineering professionals"
disco$ISCO_NU[disco$ISCO_NU == "2120" ] = "Science and engineering professionals"
disco$ISCO_NU[disco$ISCO_NU == "213" ] = "Science and engineering professionals"
disco$ISCO_NU[disco$ISCO_NU == "2131" ] = "Science and engineering professionals"
disco$ISCO_NU[disco$ISCO_NU == "2132" ] = "Science and engineering professionals"
disco$ISCO_NU[disco$ISCO_NU == "2133" ] = "Science and engineering professionals"
disco$ISCO_NU[disco$ISCO_NU == "214" ] = "Science and engineering professionals"
disco$ISCO_NU[disco$ISCO_NU == "2141" ] = "Science and engineering professionals"
disco$ISCO_NU[disco$ISCO_NU == "2142" ] = "Science and engineering professionals"
disco$ISCO_NU[disco$ISCO_NU == "2143" ] = "Science and engineering professionals"
disco$ISCO_NU[disco$ISCO_NU == "2144" ] = "Science and engineering professionals"
disco$ISCO_NU[disco$ISCO_NU == "2145" ] = "Science and engineering professionals"
disco$ISCO_NU[disco$ISCO_NU == "2146" ] = "Science and engineering professionals"
disco$ISCO_NU[disco$ISCO_NU == "2149" ] = "Science and engineering professionals"
disco$ISCO_NU[disco$ISCO_NU == "215" ] = "Science and engineering professionals"
disco$ISCO_NU[disco$ISCO_NU == "2151" ] = "Science and engineering professionals"
disco$ISCO_NU[disco$ISCO_NU == "2152" ] = "Science and engineering professionals"
disco$ISCO_NU[disco$ISCO_NU == "2153" ] = "Science and engineering professionals"
disco$ISCO_NU[disco$ISCO_NU == "216" ] = "Science and engineering professionals"
disco$ISCO_NU[disco$ISCO_NU == "2161" ] = "Science and engineering professionals"
disco$ISCO_NU[disco$ISCO_NU == "2162" ] = "Science and engineering professionals"
disco$ISCO_NU[disco$ISCO_NU == "2163" ] = "Science and engineering professionals"
disco$ISCO_NU[disco$ISCO_NU == "2164" ] = "Science and engineering professionals"
disco$ISCO_NU[disco$ISCO_NU == "2165" ] = "Science and engineering professionals"
disco$ISCO_NU[disco$ISCO_NU == "2166" ] = "Science and engineering professionals"

#Health professionals
disco$ISCO_NU[disco$ISCO_NU == "221" ] = "Health professionals"
disco$ISCO_NU[disco$ISCO_NU == "2211" ] = "Health professionals"
disco$ISCO_NU[disco$ISCO_NU == "2212" ] = "Health professionals"
disco$ISCO_NU[disco$ISCO_NU == "222" ] = "Health professionals"
disco$ISCO_NU[disco$ISCO_NU == "2221" ] = "Health professionals"
disco$ISCO_NU[disco$ISCO_NU == "2222" ] = "Health professionals"
disco$ISCO_NU[disco$ISCO_NU == "223" ] = "Health professionals"
disco$ISCO_NU[disco$ISCO_NU == "2230" ] = "Health professionals"
disco$ISCO_NU[disco$ISCO_NU == "224" ] = "Health professionals"
disco$ISCO_NU[disco$ISCO_NU == "2240" ] = "Health professionals"
disco$ISCO_NU[disco$ISCO_NU == "225" ] = "Health professionals"
disco$ISCO_NU[disco$ISCO_NU == "2250" ] = "Health professionals"
disco$ISCO_NU[disco$ISCO_NU == "226" ] = "Health professionals"
disco$ISCO_NU[disco$ISCO_NU == "2261" ] = "Health professionals"
disco$ISCO_NU[disco$ISCO_NU == "2262" ] = "Health professionals"
disco$ISCO_NU[disco$ISCO_NU == "2263" ] = "Health professionals"
disco$ISCO_NU[disco$ISCO_NU == "2264" ] = "Health professionals"
disco$ISCO_NU[disco$ISCO_NU == "2265" ] = "Health professionals"
disco$ISCO_NU[disco$ISCO_NU == "2266" ] = "Health professionals"
disco$ISCO_NU[disco$ISCO_NU == "2267" ] = "Health professionals"
disco$ISCO_NU[disco$ISCO_NU == "2269" ] = "Health professionals"

#Teaching professionals
disco$ISCO_NU[disco$ISCO_NU == "231" ] = "Teaching professionals"
disco$ISCO_NU[disco$ISCO_NU == "2310" ] = "Teaching professionals"
disco$ISCO_NU[disco$ISCO_NU == "232" ] = "Teaching professionals"
disco$ISCO_NU[disco$ISCO_NU == "2320" ] = "Teaching professionals"
disco$ISCO_NU[disco$ISCO_NU == "233" ] = "Teaching professionals"
disco$ISCO_NU[disco$ISCO_NU == "2330" ] = "Teaching professionals"
disco$ISCO_NU[disco$ISCO_NU == "234" ] = "Teaching professionals"
disco$ISCO_NU[disco$ISCO_NU == "2341" ] = "Teaching professionals"
disco$ISCO_NU[disco$ISCO_NU == "2342" ] = "Teaching professionals"
disco$ISCO_NU[disco$ISCO_NU == "2343" ] = "Teaching professionals"
disco$ISCO_NU[disco$ISCO_NU == "235" ] = "Teaching professionals"
disco$ISCO_NU[disco$ISCO_NU == "2351" ] = "Teaching professionals"
disco$ISCO_NU[disco$ISCO_NU == "2352" ] = "Teaching professionals"
disco$ISCO_NU[disco$ISCO_NU == "2353" ] = "Teaching professionals"
disco$ISCO_NU[disco$ISCO_NU == "2354" ] = "Teaching professionals"
disco$ISCO_NU[disco$ISCO_NU == "2355" ] = "Teaching professionals"
disco$ISCO_NU[disco$ISCO_NU == "2356" ] = "Teaching professionals"
disco$ISCO_NU[disco$ISCO_NU == "2357" ] = "Teaching professionals"
disco$ISCO_NU[disco$ISCO_NU == "2359" ] = "Teaching professionals"

#Business and administration professionals
disco$ISCO_NU[disco$ISCO_NU == "241" ] = "Business and administration professionals"
disco$ISCO_NU[disco$ISCO_NU == "2411" ] = "Business and administration professionals"
disco$ISCO_NU[disco$ISCO_NU == "2412" ] = "Business and administration professionals"
disco$ISCO_NU[disco$ISCO_NU == "2413" ] = "Business and administration professionals"
disco$ISCO_NU[disco$ISCO_NU == "242" ] = "Business and administration professionals"
disco$ISCO_NU[disco$ISCO_NU == "2421" ] = "Business and administration professionals"
disco$ISCO_NU[disco$ISCO_NU == "2422" ] = "Business and administration professionals"
disco$ISCO_NU[disco$ISCO_NU == "2423" ] = "Business and administration professionals"
disco$ISCO_NU[disco$ISCO_NU == "2424" ] = "Business and administration professionals"
disco$ISCO_NU[disco$ISCO_NU == "243" ] = "Business and administration professionals"
disco$ISCO_NU[disco$ISCO_NU == "2431" ] = "Business and administration professionals"
disco$ISCO_NU[disco$ISCO_NU == "2432" ] = "Business and administration professionals"
disco$ISCO_NU[disco$ISCO_NU == "2433" ] = "Business and administration professionals"
disco$ISCO_NU[disco$ISCO_NU == "2434" ] = "Business and administration professionals"

#Information and communications technology
disco$ISCO_NU[disco$ISCO_NU == "251" ] = "Information and communications technology"
disco$ISCO_NU[disco$ISCO_NU == "2511" ] = "Information and communications technology"
disco$ISCO_NU[disco$ISCO_NU == "2512" ] = "Information and communications technology"
disco$ISCO_NU[disco$ISCO_NU == "2513" ] = "Information and communications technology"
disco$ISCO_NU[disco$ISCO_NU == "2514" ] = "Information and communications technology"
disco$ISCO_NU[disco$ISCO_NU == "2519" ] = "Information and communications technology"
disco$ISCO_NU[disco$ISCO_NU == "252" ] = "Information and communications technology"
disco$ISCO_NU[disco$ISCO_NU == "2521" ] = "Information and communications technology"
disco$ISCO_NU[disco$ISCO_NU == "2522" ] = "Information and communications technology"
disco$ISCO_NU[disco$ISCO_NU == "2523" ] = "Information and communications technology"
disco$ISCO_NU[disco$ISCO_NU == "2529" ] = "Information and communications technology"

#Legal, social and cultural professionals
disco$ISCO_NU[disco$ISCO_NU == "261" ] = "Legal, social and cultural professionals"
disco$ISCO_NU[disco$ISCO_NU == "2611" ] = "Legal, social and cultural professionals"
disco$ISCO_NU[disco$ISCO_NU == "2612" ] = "Legal, social and cultural professionals"
disco$ISCO_NU[disco$ISCO_NU == "2619" ] = "Legal, social and cultural professionals"
disco$ISCO_NU[disco$ISCO_NU == "262" ] = "Legal, social and cultural professionals"
disco$ISCO_NU[disco$ISCO_NU == "2621" ] = "Legal, social and cultural professionals"
disco$ISCO_NU[disco$ISCO_NU == "2622" ] = "Legal, social and cultural professionals"
disco$ISCO_NU[disco$ISCO_NU == "263" ] = "Legal, social and cultural professionals"
disco$ISCO_NU[disco$ISCO_NU == "2631" ] = "Legal, social and cultural professionals"
disco$ISCO_NU[disco$ISCO_NU == "2632" ] = "Legal, social and cultural professionals"
disco$ISCO_NU[disco$ISCO_NU == "2633" ] = "Legal, social and cultural professionals"
disco$ISCO_NU[disco$ISCO_NU == "2634" ] = "Legal, social and cultural professionals"
disco$ISCO_NU[disco$ISCO_NU == "2635" ] = "Legal, social and cultural professionals"
disco$ISCO_NU[disco$ISCO_NU == "2636" ] = "Legal, social and cultural professionals"
disco$ISCO_NU[disco$ISCO_NU == "264" ] = "Legal, social and cultural professionals"
disco$ISCO_NU[disco$ISCO_NU == "2641" ] = "Legal, social and cultural professionals"
disco$ISCO_NU[disco$ISCO_NU == "2642" ] = "Legal, social and cultural professionals"
disco$ISCO_NU[disco$ISCO_NU == "2643" ] = "Legal, social and cultural professionals"
disco$ISCO_NU[disco$ISCO_NU == "265" ] = "Legal, social and cultural professionals"
disco$ISCO_NU[disco$ISCO_NU == "2651" ] = "Legal, social and cultural professionals"
disco$ISCO_NU[disco$ISCO_NU == "2652" ] = "Legal, social and cultural professionals"
disco$ISCO_NU[disco$ISCO_NU == "2653" ] = "Legal, social and cultural professionals"
disco$ISCO_NU[disco$ISCO_NU == "2654" ] = "Legal, social and cultural professionals"
disco$ISCO_NU[disco$ISCO_NU == "2655" ] = "Legal, social and cultural professionals"
disco$ISCO_NU[disco$ISCO_NU == "2656" ] = "Legal, social and cultural professionals"
disco$ISCO_NU[disco$ISCO_NU == "2659" ] = "Legal, social and cultural professionals"

#Science and engineering associate professionals
disco$ISCO_NU[disco$ISCO_NU == "311" ] = "Science and engineering associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3111" ] = "Science and engineering associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3112" ] = "Science and engineering associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3113" ] = "Science and engineering associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3114" ] = "Science and engineering associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3115" ] = "Science and engineering associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3116" ] = "Science and engineering associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3117" ] = "Science and engineering associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3118" ] = "Science and engineering associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3119" ] = "Science and engineering associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "312" ] = "Science and engineering associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3121" ] = "Science and engineering associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3122" ] = "Science and engineering associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3123" ] = "Science and engineering associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "313" ] = "Science and engineering associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3131" ] = "Science and engineering associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3132" ] = "Science and engineering associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3133" ] = "Science and engineering associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3134" ] = "Science and engineering associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3135" ] = "Science and engineering associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3139" ] = "Science and engineering associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "314" ] = "Science and engineering associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3141" ] = "Science and engineering associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3142" ] = "Science and engineering associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3143" ] = "Science and engineering associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "315" ] = "Science and engineering associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3151" ] = "Science and engineering associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3152" ] = "Science and engineering associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3153" ] = "Science and engineering associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3154" ] = "Science and engineering associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3155" ] = "Science and engineering associate professionals"

#Health associate professionals
disco$ISCO_NU[disco$ISCO_NU == "321" ] = "Health associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3211" ] = "Health associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3212" ] = "Health associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3213" ] = "Health associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3214" ] = "Health associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "322" ] = "Health associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3221" ] = "Health associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3222" ] = "Health associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "323" ] = "Health associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3230" ] = "Health associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "324" ] = "Health associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3240" ] = "Health associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "325" ] = "Health associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3251" ] = "Health associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3252" ] = "Health associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3253" ] = "Health associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3254" ] = "Health associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3255" ] = "Health associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3256" ] = "Health associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3257" ] = "Health associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3258" ] = "Health associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3259" ] = "Health associate professionals"

#Business and administration associate professionals
disco$ISCO_NU[disco$ISCO_NU == "331" ] = "Business and administration associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3311" ] = "Business and administration associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3312" ] = "Business and administration associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3313" ] = "Business and administration associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3314" ] = "Business and administration associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3315" ] = "Business and administration associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "332" ] = "Business and administration associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3321" ] = "Business and administration associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3322" ] = "Business and administration associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3323" ] = "Business and administration associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3324" ] = "Business and administration associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "333" ] = "Business and administration associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3331" ] = "Business and administration associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3332" ] = "Business and administration associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3333" ] = "Business and administration associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3334" ] = "Business and administration associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3339" ] = "Business and administration associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "334" ] = "Business and administration associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3341" ] = "Business and administration associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3342" ] = "Business and administration associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3343" ] = "Business and administration associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3344" ] = "Business and administration associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "335" ] = "Business and administration associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3351" ] = "Business and administration associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3352" ] = "Business and administration associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3353" ] = "Business and administration associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3354" ] = "Business and administration associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3355" ] = "Business and administration associate professionals"
disco$ISCO_NU[disco$ISCO_NU == "3359" ] = "Business and administration associate professionals"

#Legal, social, cultural and related associate
disco$ISCO_NU[disco$ISCO_NU == "341" ] = "Legal, social, cultural and related associate"
disco$ISCO_NU[disco$ISCO_NU == "3411" ] = "Legal, social, cultural and related associate"
disco$ISCO_NU[disco$ISCO_NU == "3412" ] = "Legal, social, cultural and related associate"
disco$ISCO_NU[disco$ISCO_NU == "3413" ] = "Legal, social, cultural and related associate"
disco$ISCO_NU[disco$ISCO_NU == "342" ] = "Legal, social, cultural and related associate"
disco$ISCO_NU[disco$ISCO_NU == "3421" ] = "Legal, social, cultural and related associate"
disco$ISCO_NU[disco$ISCO_NU == "3422" ] = "Legal, social, cultural and related associate"
disco$ISCO_NU[disco$ISCO_NU == "3423" ] = "Legal, social, cultural and related associate"
disco$ISCO_NU[disco$ISCO_NU == "343" ] = "Legal, social, cultural and related associate"
disco$ISCO_NU[disco$ISCO_NU == "3431" ] = "Legal, social, cultural and related associate"
disco$ISCO_NU[disco$ISCO_NU == "3432" ] = "Legal, social, cultural and related associate"
disco$ISCO_NU[disco$ISCO_NU == "3433" ] = "Legal, social, cultural and related associate"
disco$ISCO_NU[disco$ISCO_NU == "3434" ] = "Legal, social, cultural and related associate"
disco$ISCO_NU[disco$ISCO_NU == "3435" ] = "Legal, social, cultural and related associate"

#Information and communications technicians
disco$ISCO_NU[disco$ISCO_NU == "351" ] = "Information and communications technicians"
disco$ISCO_NU[disco$ISCO_NU == "3511" ] = "Information and communications technicians"
disco$ISCO_NU[disco$ISCO_NU == "3512" ] = "Information and communications technicians"
disco$ISCO_NU[disco$ISCO_NU == "3513" ] = "Information and communications technicians"
disco$ISCO_NU[disco$ISCO_NU == "3514" ] = "Information and communications technicians"
disco$ISCO_NU[disco$ISCO_NU == "352" ] = "Information and communications technicians"
disco$ISCO_NU[disco$ISCO_NU == "3521" ] = "Information and communications technicians"
disco$ISCO_NU[disco$ISCO_NU == "3522" ] = "Information and communications technicians"

#General and keyboard clerks
disco$ISCO_NU[disco$ISCO_NU == "411" ] = "General and keyboard clerks"
disco$ISCO_NU[disco$ISCO_NU == "4110" ] = "General and keyboard clerks"
disco$ISCO_NU[disco$ISCO_NU == "4120" ] = "General and keyboard clerks"
disco$ISCO_NU[disco$ISCO_NU == "413" ] = "General and keyboard clerks"
disco$ISCO_NU[disco$ISCO_NU == "4131" ] = "General and keyboard clerks"
disco$ISCO_NU[disco$ISCO_NU == "4132" ] = "General and keyboard clerks"

#Customer services clerks
disco$ISCO_NU[disco$ISCO_NU == "421" ] = "Customer services clerks"
disco$ISCO_NU[disco$ISCO_NU == "4211" ] = "Customer services clerks"
disco$ISCO_NU[disco$ISCO_NU == "4212" ] = "Customer services clerks"
disco$ISCO_NU[disco$ISCO_NU == "4213" ] = "Customer services clerks"
disco$ISCO_NU[disco$ISCO_NU == "4214" ] = "Customer services clerks"
disco$ISCO_NU[disco$ISCO_NU == "422" ] = "Customer services clerks"
disco$ISCO_NU[disco$ISCO_NU == "4221" ] = "Customer services clerks"
disco$ISCO_NU[disco$ISCO_NU == "4222" ] = "Customer services clerks"
disco$ISCO_NU[disco$ISCO_NU == "4223" ] = "Customer services clerks"
disco$ISCO_NU[disco$ISCO_NU == "4224" ] = "Customer services clerks"
disco$ISCO_NU[disco$ISCO_NU == "4225" ] = "Customer services clerks"
disco$ISCO_NU[disco$ISCO_NU == "4226" ] = "Customer services clerks"
disco$ISCO_NU[disco$ISCO_NU == "4227" ] = "Customer services clerks"
disco$ISCO_NU[disco$ISCO_NU == "4229" ] = "Customer services clerks"

#Numerical and material recording clerks
disco$ISCO_NU[disco$ISCO_NU == "431" ] = "Numerical and material recording clerks"
disco$ISCO_NU[disco$ISCO_NU == "4311" ] = "Numerical and material recording clerks"
disco$ISCO_NU[disco$ISCO_NU == "4312" ] = "Numerical and material recording clerks"
disco$ISCO_NU[disco$ISCO_NU == "4313" ] = "Numerical and material recording clerks"
disco$ISCO_NU[disco$ISCO_NU == "432" ] = "Numerical and material recording clerks"
disco$ISCO_NU[disco$ISCO_NU == "4321" ] = "Numerical and material recording clerks"
disco$ISCO_NU[disco$ISCO_NU == "4322" ] = "Numerical and material recording clerks"
disco$ISCO_NU[disco$ISCO_NU == "4323" ] = "Numerical and material recording clerks"

#Other clerical support workers
disco$ISCO_NU[disco$ISCO_NU == "441" ] = "Other clerical support workers"
disco$ISCO_NU[disco$ISCO_NU == "4411" ] = "Other clerical support workers"
disco$ISCO_NU[disco$ISCO_NU == "4412" ] = "Other clerical support workers"
disco$ISCO_NU[disco$ISCO_NU == "4413" ] = "Other clerical support workers"
disco$ISCO_NU[disco$ISCO_NU == "4415" ] = "Other clerical support workers"
disco$ISCO_NU[disco$ISCO_NU == "4416" ] = "Other clerical support workers"
disco$ISCO_NU[disco$ISCO_NU == "4419" ] = "Other clerical support workers"

#Personal service workers
disco$ISCO_NU[disco$ISCO_NU == "511" ] = "Personal service workers"
disco$ISCO_NU[disco$ISCO_NU == "5111" ] = "Personal service workers"
disco$ISCO_NU[disco$ISCO_NU == "5112" ] = "Personal service workers"
disco$ISCO_NU[disco$ISCO_NU == "5113" ] = "Personal service workers"
disco$ISCO_NU[disco$ISCO_NU == "512" ] = "Personal service workers"
disco$ISCO_NU[disco$ISCO_NU == "5120" ] = "Personal service workers"
disco$ISCO_NU[disco$ISCO_NU == "513" ] = "Personal service workers"
disco$ISCO_NU[disco$ISCO_NU == "5131" ] = "Personal service workers"
disco$ISCO_NU[disco$ISCO_NU == "5132" ] = "Personal service workers"
disco$ISCO_NU[disco$ISCO_NU == "514" ] = "Personal service workers"
disco$ISCO_NU[disco$ISCO_NU == "5141" ] = "Personal service workers"
disco$ISCO_NU[disco$ISCO_NU == "5142" ] = "Personal service workers"
disco$ISCO_NU[disco$ISCO_NU == "515" ] = "Personal service workers"
disco$ISCO_NU[disco$ISCO_NU == "5151" ] = "Personal service workers"
disco$ISCO_NU[disco$ISCO_NU == "5152" ] = "Personal service workers"
disco$ISCO_NU[disco$ISCO_NU == "5153" ] = "Personal service workers"
disco$ISCO_NU[disco$ISCO_NU == "516" ] = "Personal service workers"
disco$ISCO_NU[disco$ISCO_NU == "5161" ] = "Personal service workers"
disco$ISCO_NU[disco$ISCO_NU == "5163" ] = "Personal service workers"
disco$ISCO_NU[disco$ISCO_NU == "5164" ] = "Personal service workers"
disco$ISCO_NU[disco$ISCO_NU == "5165" ] = "Personal service workers"
disco$ISCO_NU[disco$ISCO_NU == "5169" ] = "Personal service workers"

#Sales workers
disco$ISCO_NU[disco$ISCO_NU == "521" ] = "Sales workers"
disco$ISCO_NU[disco$ISCO_NU == "5211" ] = "Sales workers"
disco$ISCO_NU[disco$ISCO_NU == "5212" ] = "Sales workers"
disco$ISCO_NU[disco$ISCO_NU == "522" ] = "Sales workers"
disco$ISCO_NU[disco$ISCO_NU == "5221" ] = "Sales workers"
disco$ISCO_NU[disco$ISCO_NU == "5222" ] = "Sales workers"
disco$ISCO_NU[disco$ISCO_NU == "5223" ] = "Sales workers"
disco$ISCO_NU[disco$ISCO_NU == "523" ] = "Sales workers"
disco$ISCO_NU[disco$ISCO_NU == "5230" ] = "Sales workers"
disco$ISCO_NU[disco$ISCO_NU == "524" ] = "Sales workers"
disco$ISCO_NU[disco$ISCO_NU == "5241" ] = "Sales workers"
disco$ISCO_NU[disco$ISCO_NU == "5242" ] = "Sales workers"
disco$ISCO_NU[disco$ISCO_NU == "5244" ] = "Sales workers"
disco$ISCO_NU[disco$ISCO_NU == "5245" ] = "Sales workers"
disco$ISCO_NU[disco$ISCO_NU == "5246" ] = "Sales workers"
disco$ISCO_NU[disco$ISCO_NU == "5249" ] = "Sales workers"

#Personal care workers
disco$ISCO_NU[disco$ISCO_NU == "531" ] = "Personal care workers"
disco$ISCO_NU[disco$ISCO_NU == "5311" ] = "Personal care workers"
disco$ISCO_NU[disco$ISCO_NU == "5312" ] = "Personal care workers"
disco$ISCO_NU[disco$ISCO_NU == "532" ] = "Personal care workers"
disco$ISCO_NU[disco$ISCO_NU == "5321" ] = "Personal care workers"
disco$ISCO_NU[disco$ISCO_NU == "5322" ] = "Personal care workers"
disco$ISCO_NU[disco$ISCO_NU == "5329" ] = "Personal care workers"

#Personal care workers
disco$ISCO_NU[disco$ISCO_NU == "541" ] = "Protective services workers"
disco$ISCO_NU[disco$ISCO_NU == "5411" ] = "Protective services workers"
disco$ISCO_NU[disco$ISCO_NU == "5412" ] = "Protective services workers"
disco$ISCO_NU[disco$ISCO_NU == "5413" ] = "Protective services workers"
disco$ISCO_NU[disco$ISCO_NU == "5414" ] = "Protective services workers"
disco$ISCO_NU[disco$ISCO_NU == "5419" ] = "Protective services workers"

#Market-oriented skilled agricultural workers
disco$ISCO_NU[disco$ISCO_NU == "611" ] = "Market-oriented skilled agricultural workers"
disco$ISCO_NU[disco$ISCO_NU == "6111" ] = "Market-oriented skilled agricultural workers"
disco$ISCO_NU[disco$ISCO_NU == "6112" ] = "Market-oriented skilled agricultural workers"
disco$ISCO_NU[disco$ISCO_NU == "6113" ] = "Market-oriented skilled agricultural workers"
disco$ISCO_NU[disco$ISCO_NU == "6114" ] = "Market-oriented skilled agricultural workers"
disco$ISCO_NU[disco$ISCO_NU == "612" ] = "Market-oriented skilled agricultural workers"
disco$ISCO_NU[disco$ISCO_NU == "6121" ] = "Market-oriented skilled agricultural workers"
disco$ISCO_NU[disco$ISCO_NU == "6122" ] = "Market-oriented skilled agricultural workers"
disco$ISCO_NU[disco$ISCO_NU == "6123" ] = "Market-oriented skilled agricultural workers"
disco$ISCO_NU[disco$ISCO_NU == "6129" ] = "Market-oriented skilled agricultural workers"
disco$ISCO_NU[disco$ISCO_NU == "613" ] = "Market-oriented skilled agricultural workers"
disco$ISCO_NU[disco$ISCO_NU == "6130" ] = "Market-oriented skilled agricultural workers"

#Market-oriented skilled forestry, fishery and hunting
disco$ISCO_NU[disco$ISCO_NU == "621" ] = "Market-oriented skilled forestry, fishery and hunting"
disco$ISCO_NU[disco$ISCO_NU == "6210" ] = "Market-oriented skilled forestry, fishery and hunting"
disco$ISCO_NU[disco$ISCO_NU == "622" ] = "Market-oriented skilled forestry, fishery and hunting"
disco$ISCO_NU[disco$ISCO_NU == "6221" ] = "Market-oriented skilled forestry, fishery and hunting"
disco$ISCO_NU[disco$ISCO_NU == "6222" ] = "Market-oriented skilled forestry, fishery and hunting"
disco$ISCO_NU[disco$ISCO_NU == "6223" ] = "Market-oriented skilled forestry, fishery and hunting"
disco$ISCO_NU[disco$ISCO_NU == "6224" ] = "Market-oriented skilled forestry, fishery and hunting"

#Subsistence farmers, fishers, hunters and gatherers
disco$ISCO_NU[disco$ISCO_NU == "631" ] = "Subsistence farmers, fishers, hunters and gatherers"
disco$ISCO_NU[disco$ISCO_NU == "6310" ] = "Subsistence farmers, fishers, hunters and gatherers"
disco$ISCO_NU[disco$ISCO_NU == "6320" ] = "Subsistence farmers, fishers, hunters and gatherers"
disco$ISCO_NU[disco$ISCO_NU == "633" ] = "Subsistence farmers, fishers, hunters and gatherers"
disco$ISCO_NU[disco$ISCO_NU == "6330" ] = "Subsistence farmers, fishers, hunters and gatherers"
disco$ISCO_NU[disco$ISCO_NU == "634" ] = "Subsistence farmers, fishers, hunters and gatherers"
disco$ISCO_NU[disco$ISCO_NU == "6340" ] = "Subsistence farmers, fishers, hunters and gatherers"

#Building and related trades workers, excluding electricians
disco$ISCO_NU[disco$ISCO_NU == "711" ] = "Building and related trades workers, excluding electricians"
disco$ISCO_NU[disco$ISCO_NU == "7112" ] = "Building and related trades workers, excluding electricians"
disco$ISCO_NU[disco$ISCO_NU == "7113" ] = "Building and related trades workers, excluding electricians"
disco$ISCO_NU[disco$ISCO_NU == "7114" ] = "Building and related trades workers, excluding electricians"
disco$ISCO_NU[disco$ISCO_NU == "7115" ] = "Building and related trades workers, excluding electricians"
disco$ISCO_NU[disco$ISCO_NU == "7119" ] = "Building and related trades workers, excluding electricians"
disco$ISCO_NU[disco$ISCO_NU == "712" ] = "Building and related trades workers, excluding electricians"
disco$ISCO_NU[disco$ISCO_NU == "7121" ] = "Building and related trades workers, excluding electricians"
disco$ISCO_NU[disco$ISCO_NU == "7122" ] = "Building and related trades workers, excluding electricians"
disco$ISCO_NU[disco$ISCO_NU == "7123" ] = "Building and related trades workers, excluding electricians"
disco$ISCO_NU[disco$ISCO_NU == "7124" ] = "Building and related trades workers, excluding electricians"
disco$ISCO_NU[disco$ISCO_NU == "7125" ] = "Building and related trades workers, excluding electricians"
disco$ISCO_NU[disco$ISCO_NU == "7126" ] = "Building and related trades workers, excluding electricians"
disco$ISCO_NU[disco$ISCO_NU == "7127" ] = "Building and related trades workers, excluding electricians"
disco$ISCO_NU[disco$ISCO_NU == "713" ] = "Building and related trades workers, excluding electricians"
disco$ISCO_NU[disco$ISCO_NU == "7131" ] = "Building and related trades workers, excluding electricians"
disco$ISCO_NU[disco$ISCO_NU == "7132" ] = "Building and related trades workers, excluding electricians"
disco$ISCO_NU[disco$ISCO_NU == "7133" ] = "Building and related trades workers, excluding electricians"

#Metal, machinery and related trades workers
disco$ISCO_NU[disco$ISCO_NU == "721" ] = "Metal, machinery and related trades workers"
disco$ISCO_NU[disco$ISCO_NU == "7211" ] = "Metal, machinery and related trades workers"
disco$ISCO_NU[disco$ISCO_NU == "7212" ] = "Metal, machinery and related trades workers"
disco$ISCO_NU[disco$ISCO_NU == "7213" ] = "Metal, machinery and related trades workers"
disco$ISCO_NU[disco$ISCO_NU == "7214" ] = "Metal, machinery and related trades workers"
disco$ISCO_NU[disco$ISCO_NU == "7215" ] = "Metal, machinery and related trades workers"
disco$ISCO_NU[disco$ISCO_NU == "722" ] = "Metal, machinery and related trades workers"
disco$ISCO_NU[disco$ISCO_NU == "7221" ] = "Metal, machinery and related trades workers"
disco$ISCO_NU[disco$ISCO_NU == "7222" ] = "Metal, machinery and related trades workers"
disco$ISCO_NU[disco$ISCO_NU == "7223" ] = "Metal, machinery and related trades workers"
disco$ISCO_NU[disco$ISCO_NU == "7224" ] = "Metal, machinery and related trades workers"
disco$ISCO_NU[disco$ISCO_NU == "723" ] = "Metal, machinery and related trades workers"
disco$ISCO_NU[disco$ISCO_NU == "7231" ] = "Metal, machinery and related trades workers"
disco$ISCO_NU[disco$ISCO_NU == "7232" ] = "Metal, machinery and related trades workers"
disco$ISCO_NU[disco$ISCO_NU == "7233" ] = "Metal, machinery and related trades workers"
disco$ISCO_NU[disco$ISCO_NU == "7234" ] = "Metal, machinery and related trades workers"

#Handicraft and printing workers
disco$ISCO_NU[disco$ISCO_NU == "731" ] = "Handicraft and printing workers"
disco$ISCO_NU[disco$ISCO_NU == "7311" ] = "Handicraft and printing workers"
disco$ISCO_NU[disco$ISCO_NU == "7312" ] = "Handicraft and printing workers"
disco$ISCO_NU[disco$ISCO_NU == "7313" ] = "Handicraft and printing workers"
disco$ISCO_NU[disco$ISCO_NU == "7314" ] = "Handicraft and printing workers"
disco$ISCO_NU[disco$ISCO_NU == "7315" ] = "Handicraft and printing workers"
disco$ISCO_NU[disco$ISCO_NU == "7316" ] = "Handicraft and printing workers"
disco$ISCO_NU[disco$ISCO_NU == "7317" ] = "Handicraft and printing workers"
disco$ISCO_NU[disco$ISCO_NU == "7318" ] = "Handicraft and printing workers"
disco$ISCO_NU[disco$ISCO_NU == "7319" ] = "Handicraft and printing workers"
disco$ISCO_NU[disco$ISCO_NU == "732" ] = "Handicraft and printing workers"
disco$ISCO_NU[disco$ISCO_NU == "7321" ] = "Handicraft and printing workers"
disco$ISCO_NU[disco$ISCO_NU == "7322" ] = "Handicraft and printing workers"
disco$ISCO_NU[disco$ISCO_NU == "7323" ] = "Handicraft and printing workers"

#Electrical and electronic trades workers
disco$ISCO_NU[disco$ISCO_NU == "741" ] = "Electrical and electronic trades workers"
disco$ISCO_NU[disco$ISCO_NU == "7411" ] = "Electrical and electronic trades workers"
disco$ISCO_NU[disco$ISCO_NU == "7412" ] = "Electrical and electronic trades workers"
disco$ISCO_NU[disco$ISCO_NU == "7413" ] = "Electrical and electronic trades workers"
disco$ISCO_NU[disco$ISCO_NU == "742" ] = "Electrical and electronic trades workers"
disco$ISCO_NU[disco$ISCO_NU == "7421" ] = "Electrical and electronic trades workers"
disco$ISCO_NU[disco$ISCO_NU == "7422" ] = "Electrical and electronic trades workers"

#Food processing, wood working, garment and other craft and related trades workers
disco$ISCO_NU[disco$ISCO_NU == "751" ] = "Food processing, wood working, garment and other craft and related trades workers"
disco$ISCO_NU[disco$ISCO_NU == "7511" ] = "Food processing, wood working, garment and other craft and related trades workers"
disco$ISCO_NU[disco$ISCO_NU == "7512" ] = "Food processing, wood working, garment and other craft and related trades workers"
disco$ISCO_NU[disco$ISCO_NU == "7513" ] = "Food processing, wood working, garment and other craft and related trades workers"
disco$ISCO_NU[disco$ISCO_NU == "7514" ] = "Food processing, wood working, garment and other craft and related trades workers"
disco$ISCO_NU[disco$ISCO_NU == "7515" ] = "Food processing, wood working, garment and other craft and related trades workers"
disco$ISCO_NU[disco$ISCO_NU == "7516" ] = "Food processing, wood working, garment and other craft and related trades workers"
disco$ISCO_NU[disco$ISCO_NU == "752" ] = "Food processing, wood working, garment and other craft and related trades workers"
disco$ISCO_NU[disco$ISCO_NU == "7521" ] = "Food processing, wood working, garment and other craft and related trades workers"
disco$ISCO_NU[disco$ISCO_NU == "7522" ] = "Food processing, wood working, garment and other craft and related trades workers"
disco$ISCO_NU[disco$ISCO_NU == "7523" ] = "Food processing, wood working, garment and other craft and related trades workers"
disco$ISCO_NU[disco$ISCO_NU == "753" ] = "Food processing, wood working, garment and other craft and related trades workers"
disco$ISCO_NU[disco$ISCO_NU == "7531" ] = "Food processing, wood working, garment and other craft and related trades workers"
disco$ISCO_NU[disco$ISCO_NU == "7532" ] = "Food processing, wood working, garment and other craft and related trades workers"
disco$ISCO_NU[disco$ISCO_NU == "7533" ] = "Food processing, wood working, garment and other craft and related trades workers"
disco$ISCO_NU[disco$ISCO_NU == "7534" ] = "Food processing, wood working, garment and other craft and related trades workers"
disco$ISCO_NU[disco$ISCO_NU == "7535" ] = "Food processing, wood working, garment and other craft and related trades workers"
disco$ISCO_NU[disco$ISCO_NU == "7536" ] = "Food processing, wood working, garment and other craft and related trades workers"
disco$ISCO_NU[disco$ISCO_NU == "754" ] = "Food processing, wood working, garment and other craft and related trades workers"
disco$ISCO_NU[disco$ISCO_NU == "7541" ] = "Food processing, wood working, garment and other craft and related trades workers"
disco$ISCO_NU[disco$ISCO_NU == "7542" ] = "Food processing, wood working, garment and other craft and related trades workers"
disco$ISCO_NU[disco$ISCO_NU == "7543" ] = "Food processing, wood working, garment and other craft and related trades workers"
disco$ISCO_NU[disco$ISCO_NU == "7544" ] = "Food processing, wood working, garment and other craft and related trades workers"
disco$ISCO_NU[disco$ISCO_NU == "7549" ] = "Food processing, wood working, garment and other craft and related trades workers"

#Stationary plant and machine operators
disco$ISCO_NU[disco$ISCO_NU == "811" ] = "Stationary plant and machine operators"
disco$ISCO_NU[disco$ISCO_NU == "8111" ] = "Stationary plant and machine operators"
disco$ISCO_NU[disco$ISCO_NU == "8112" ] = "Stationary plant and machine operators"
disco$ISCO_NU[disco$ISCO_NU == "8113" ] = "Stationary plant and machine operators"
disco$ISCO_NU[disco$ISCO_NU == "8114" ] = "Stationary plant and machine operators"
disco$ISCO_NU[disco$ISCO_NU == "812" ] = "Stationary plant and machine operators"
disco$ISCO_NU[disco$ISCO_NU == "8121" ] = "Stationary plant and machine operators"
disco$ISCO_NU[disco$ISCO_NU == "8122" ] = "Stationary plant and machine operators"
disco$ISCO_NU[disco$ISCO_NU == "813" ] = "Stationary plant and machine operators"
disco$ISCO_NU[disco$ISCO_NU == "8131" ] = "Stationary plant and machine operators"
disco$ISCO_NU[disco$ISCO_NU == "8132" ] = "Stationary plant and machine operators"
disco$ISCO_NU[disco$ISCO_NU == "814" ] = "Stationary plant and machine operators"
disco$ISCO_NU[disco$ISCO_NU == "8141" ] = "Stationary plant and machine operators"
disco$ISCO_NU[disco$ISCO_NU == "8142" ] = "Stationary plant and machine operators"
disco$ISCO_NU[disco$ISCO_NU == "8143" ] = "Stationary plant and machine operators"
disco$ISCO_NU[disco$ISCO_NU == "815" ] = "Stationary plant and machine operators"
disco$ISCO_NU[disco$ISCO_NU == "8151" ] = "Stationary plant and machine operators"
disco$ISCO_NU[disco$ISCO_NU == "8152" ] = "Stationary plant and machine operators"
disco$ISCO_NU[disco$ISCO_NU == "8153" ] = "Stationary plant and machine operators"
disco$ISCO_NU[disco$ISCO_NU == "8154" ] = "Stationary plant and machine operators"
disco$ISCO_NU[disco$ISCO_NU == "8155" ] = "Stationary plant and machine operators"
disco$ISCO_NU[disco$ISCO_NU == "8156" ] = "Stationary plant and machine operators"
disco$ISCO_NU[disco$ISCO_NU == "8157" ] = "Stationary plant and machine operators"
disco$ISCO_NU[disco$ISCO_NU == "8159" ] = "Stationary plant and machine operators"
disco$ISCO_NU[disco$ISCO_NU == "816" ] = "Stationary plant and machine operators"
disco$ISCO_NU[disco$ISCO_NU == "8160" ] = "Stationary plant and machine operators"
disco$ISCO_NU[disco$ISCO_NU == "817" ] = "Stationary plant and machine operators"
disco$ISCO_NU[disco$ISCO_NU == "8171" ] = "Stationary plant and machine operators"
disco$ISCO_NU[disco$ISCO_NU == "8172" ] = "Stationary plant and machine operators"
disco$ISCO_NU[disco$ISCO_NU == "818" ] = "Stationary plant and machine operators"
disco$ISCO_NU[disco$ISCO_NU == "8181" ] = "Stationary plant and machine operators"
disco$ISCO_NU[disco$ISCO_NU == "8182" ] = "Stationary plant and machine operators"
disco$ISCO_NU[disco$ISCO_NU == "8183" ] = "Stationary plant and machine operators"
disco$ISCO_NU[disco$ISCO_NU == "8189" ] = "Stationary plant and machine operators"

#Assemblers
disco$ISCO_NU[disco$ISCO_NU == "821" ] = "Assemblers"
disco$ISCO_NU[disco$ISCO_NU == "8211" ] = "Assemblers"
disco$ISCO_NU[disco$ISCO_NU == "8212" ] = "Assemblers"
disco$ISCO_NU[disco$ISCO_NU == "8219" ] = "Assemblers"

#Drivers and mobile plant operators
disco$ISCO_NU[disco$ISCO_NU == "831" ] = "Drivers and mobile plant operators"
disco$ISCO_NU[disco$ISCO_NU == "8311" ] = "Drivers and mobile plant operators"
disco$ISCO_NU[disco$ISCO_NU == "8312" ] = "Drivers and mobile plant operators"
disco$ISCO_NU[disco$ISCO_NU == "832" ] = "Drivers and mobile plant operators"
disco$ISCO_NU[disco$ISCO_NU == "8321" ] = "Drivers and mobile plant operators"
disco$ISCO_NU[disco$ISCO_NU == "8322" ] = "Drivers and mobile plant operators"
disco$ISCO_NU[disco$ISCO_NU == "833" ] = "Drivers and mobile plant operators"
disco$ISCO_NU[disco$ISCO_NU == "8331" ] = "Drivers and mobile plant operators"
disco$ISCO_NU[disco$ISCO_NU == "8332" ] = "Drivers and mobile plant operators"
disco$ISCO_NU[disco$ISCO_NU == "834" ] = "Drivers and mobile plant operators"
disco$ISCO_NU[disco$ISCO_NU == "8341" ] = "Drivers and mobile plant operators"
disco$ISCO_NU[disco$ISCO_NU == "8342" ] = "Drivers and mobile plant operators"
disco$ISCO_NU[disco$ISCO_NU == "8343" ] = "Drivers and mobile plant operators"
disco$ISCO_NU[disco$ISCO_NU == "8344" ] = "Drivers and mobile plant operators"
disco$ISCO_NU[disco$ISCO_NU == "835" ] = "Drivers and mobile plant operators"
disco$ISCO_NU[disco$ISCO_NU == "8350" ] = "Drivers and mobile plant operators"

#Cleaners and helpers
disco$ISCO_NU[disco$ISCO_NU == "911" ] = "Cleaners and helpers"
disco$ISCO_NU[disco$ISCO_NU == "9111" ] = "Cleaners and helpers"
disco$ISCO_NU[disco$ISCO_NU == "9112" ] = "Cleaners and helpers"
disco$ISCO_NU[disco$ISCO_NU == "912" ] = "Cleaners and helpers"
disco$ISCO_NU[disco$ISCO_NU == "9121" ] = "Cleaners and helpers"
disco$ISCO_NU[disco$ISCO_NU == "9122" ] = "Cleaners and helpers"
disco$ISCO_NU[disco$ISCO_NU == "9123" ] = "Cleaners and helpers"
disco$ISCO_NU[disco$ISCO_NU == "9129" ] = "Cleaners and helpers"

#Agricultural, forestry and fishery labourers
disco$ISCO_NU[disco$ISCO_NU == "921" ] = "Agricultural, forestry and fishery labourers"
disco$ISCO_NU[disco$ISCO_NU == "9211" ] = "Agricultural, forestry and fishery labourers"
disco$ISCO_NU[disco$ISCO_NU == "9212" ] = "Agricultural, forestry and fishery labourers"
disco$ISCO_NU[disco$ISCO_NU == "9213" ] = "Agricultural, forestry and fishery labourers"
disco$ISCO_NU[disco$ISCO_NU == "9214" ] = "Agricultural, forestry and fishery labourers"
disco$ISCO_NU[disco$ISCO_NU == "9215" ] = "Agricultural, forestry and fishery labourers"
disco$ISCO_NU[disco$ISCO_NU == "9216" ] = "Agricultural, forestry and fishery labourers"

#Labourers in mining, construction, manufacturing and transport
disco$ISCO_NU[disco$ISCO_NU == "931" ] = "Labourers in mining, construction, manufacturing and transport"
disco$ISCO_NU[disco$ISCO_NU == "9311" ] = "Labourers in mining, construction, manufacturing and transport"
disco$ISCO_NU[disco$ISCO_NU == "9312" ] = "Labourers in mining, construction, manufacturing and transport"
disco$ISCO_NU[disco$ISCO_NU == "9313" ] = "Labourers in mining, construction, manufacturing and transport"
disco$ISCO_NU[disco$ISCO_NU == "932" ] = "Labourers in mining, construction, manufacturing and transport"
disco$ISCO_NU[disco$ISCO_NU == "9321" ] = "Labourers in mining, construction, manufacturing and transport"
disco$ISCO_NU[disco$ISCO_NU == "9329" ] = "Labourers in mining, construction, manufacturing and transport"
disco$ISCO_NU[disco$ISCO_NU == "933" ] = "Labourers in mining, construction, manufacturing and transport"
disco$ISCO_NU[disco$ISCO_NU == "9331" ] = "Labourers in mining, construction, manufacturing and transport"
disco$ISCO_NU[disco$ISCO_NU == "9332" ] = "Labourers in mining, construction, manufacturing and transport"
disco$ISCO_NU[disco$ISCO_NU == "9333" ] = "Labourers in mining, construction, manufacturing and transport"
disco$ISCO_NU[disco$ISCO_NU == "9334" ] = "Labourers in mining, construction, manufacturing and transport"

#Food preparation assistants
disco$ISCO_NU[disco$ISCO_NU == "941" ] = "Food preparation assistants"
disco$ISCO_NU[disco$ISCO_NU == "9411" ] = "Food preparation assistants"
disco$ISCO_NU[disco$ISCO_NU == "9412" ] = "Food preparation assistants"

#Street and related sales and service workers
disco$ISCO_NU[disco$ISCO_NU == "951" ] = "Street and related sales and service workers"
disco$ISCO_NU[disco$ISCO_NU == "9510" ] = "Street and related sales and service workers"
disco$ISCO_NU[disco$ISCO_NU == "952" ] = "Street and related sales and service workers"
disco$ISCO_NU[disco$ISCO_NU == "9520" ] = "Street and related sales and service workers"

#Refuse workers and other elementary workers
disco$ISCO_NU[disco$ISCO_NU == "961" ] = "Refuse workers and other elementary workers"
disco$ISCO_NU[disco$ISCO_NU == "9611" ] = "Refuse workers and other elementary workers"
disco$ISCO_NU[disco$ISCO_NU == "9612" ] = "Refuse workers and other elementary workers"
disco$ISCO_NU[disco$ISCO_NU == "9613" ] = "Refuse workers and other elementary workers"
disco$ISCO_NU[disco$ISCO_NU == "962" ] = "Refuse workers and other elementary workers"
disco$ISCO_NU[disco$ISCO_NU == "9621" ] = "Refuse workers and other elementary workers"
disco$ISCO_NU[disco$ISCO_NU == "9622" ] = "Refuse workers and other elementary workers"
disco$ISCO_NU[disco$ISCO_NU == "9623" ] = "Refuse workers and other elementary workers"
disco$ISCO_NU[disco$ISCO_NU == "9629" ] = "Refuse workers and other elementary workers"

#Commissioned armed forces officers
disco$ISCO_NU[disco$ISCO_NU == "011" ] = "Commissioned armed forces officers"
disco$ISCO_NU[disco$ISCO_NU == "0110" ] = "Commissioned armed forces officers"

#Non-commissioned armed forces officers
disco$ISCO_NU[disco$ISCO_NU == "021" ] = "Non-commissioned armed forces officers"
disco$ISCO_NU[disco$ISCO_NU == "0210" ] = "Non-commissioned armed forces officers"

#Armed forces occupations, other ranks
disco$ISCO_NU[disco$ISCO_NU == "031" ] = "Armed forces occupations, other ranks"
disco$ISCO_NU[disco$ISCO_NU == "0110" ] = "Armed forces occupations, other ranks"

#None
disco$ISCO_NU[disco$ISCO_NU == "310" ] = "None"
disco$ISCO_NU[disco$ISCO_NU == "9701" ] = "None"
disco$ISCO_NU[disco$ISCO_NU == "9705" ] = "None"
disco$ISCO_NU[disco$ISCO_NU == "9999" ] = "None"

disco["Functions"] = NA

#Functions
disco$Functions[disco$ISCO_NU == "Chief executives, senior officials and legislators"] = 1
disco$Functions[disco$ISCO_NU == "Administrative and commercial managers"] = 1
disco$Functions[disco$ISCO_NU == "Production and specialised services managers"] = 1
disco$Functions[disco$ISCO_NU == "Hospitality, retail and other services managers"] = 1
disco$Functions[disco$ISCO_NU == "Science and engineering professionals"] = 2
disco$Functions[disco$ISCO_NU == "Health professionals"] = 2
disco$Functions[disco$ISCO_NU == "Teaching professionals"] = 2
disco$Functions[disco$ISCO_NU == "Business and administration professionals"] = 2
disco$Functions[disco$ISCO_NU == "Information and communications technology"] = 2
disco$Functions[disco$ISCO_NU == "Legal, social and cultural professionals"] = 2
disco$Functions[disco$ISCO_NU == "Science and engineering associate professionals"] = 3
disco$Functions[disco$ISCO_NU == "Health associate professionals"] = 3
disco$Functions[disco$ISCO_NU == "Business and administration associate professionals"] = 3
disco$Functions[disco$ISCO_NU == "Legal, social, cultural and related associate"] = 3
disco$Functions[disco$ISCO_NU == "Information and communications technicians"] = 3
disco$Functions[disco$ISCO_NU == "General and keyboard clerks"] = 4
disco$Functions[disco$ISCO_NU == "Customer services clerks"] = 4
disco$Functions[disco$ISCO_NU == "Numerical and material recording clerks"] = 4
disco$Functions[disco$ISCO_NU == "Other clerical support workers"] = 4
disco$Functions[disco$ISCO_NU == "Personal service workers"] = 5
disco$Functions[disco$ISCO_NU == "Sales workers"] = 5
disco$Functions[disco$ISCO_NU == "Personal care workers"] = 5
disco$Functions[disco$ISCO_NU == "Protective services workers"] = 5
disco$Functions[disco$ISCO_NU == "Market-oriented skilled agricultural workers"] = 6
disco$Functions[disco$ISCO_NU == "Market-oriented skilled forestry, fishery and hunting"] = 6
disco$Functions[disco$ISCO_NU == "Subsistence farmers, fishers, hunters and gatherers"] = 6
disco$Functions[disco$ISCO_NU == "Building and related trades workers, excluding electricians"] = 7
disco$Functions[disco$ISCO_NU == "Metal, machinery and related trades workers"] = 7
disco$Functions[disco$ISCO_NU == "Handicraft and printing workers"] = 7
disco$Functions[disco$ISCO_NU == "Electrical and electronic trades workers"] = 7
disco$Functions[disco$ISCO_NU == "Food processing, wood working, garment and other craft and related trades workers"] = 7
disco$Functions[disco$ISCO_NU == "Stationary plant and machine operators"] = 8
disco$Functions[disco$ISCO_NU == "Assemblers"] = 8
disco$Functions[disco$ISCO_NU == "Drivers and mobile plant operators"] = 8
disco$Functions[disco$ISCO_NU == "Cleaners and helpers"] = 9
disco$Functions[disco$ISCO_NU == "Agricultural, forestry and fishery labourers"] = 9
disco$Functions[disco$ISCO_NU == "Labourers in mining, construction, manufacturing and transport"] = 9
disco$Functions[disco$ISCO_NU == "Food preparation assistants"] = 9
disco$Functions[disco$ISCO_NU == "Street and related sales and service workers"] = 9
disco$Functions[disco$ISCO_NU == "Refuse workers and other elementary workers"] = 9
disco$Functions[disco$ISCO_NU == "Food preparation assistants"] = 9
disco$Functions[disco$ISCO_NU == "Commissioned armed forces officers"] = 0
disco$Functions[disco$ISCO_NU == "Non-commissioned armed forces officers"] = 0
disco$Functions[disco$ISCO_NU == "Armed forces occupations, other ranks"] = 0
disco$Functions[disco$ISCO_NU == "None"] = "None"

aau_job_2019_svar$bra10grp[aau_job_2019_svar$bra10grp == "1 Landbrug, skovbrug og fiskeri"] = "Landbrug, skovbrug og fiskeri"
aau_job_2019_svar$bra10grp[aau_job_2019_svar$bra10grp == "2 Industri, råstofindvinding og forsyningsvirksomhed"] = "Industri, råstofindvinding og forsyningsvirksomhed"
aau_job_2019_svar$bra10grp[aau_job_2019_svar$bra10grp == "3 Bygge og anlæg"] = "Bygge og anlæg"
aau_job_2019_svar$bra10grp[aau_job_2019_svar$bra10grp == "4 Handel og transport mv."] = "Handel og transport"
aau_job_2019_svar$bra10grp[aau_job_2019_svar$bra10grp == "5 Information og kommunikation"] = "Information og kommunikation"
aau_job_2019_svar$bra10grp[aau_job_2019_svar$bra10grp == "6 Finansiering og forsikring"] = "Finansiering og forsikring"
aau_job_2019_svar$bra10grp[aau_job_2019_svar$bra10grp == "7 Ejendomshandel og udlejning"] = "Ejendomshandel og udlejning"
aau_job_2019_svar$bra10grp[aau_job_2019_svar$bra10grp == "8 Erhvervsservice"] = "Erhvervsservice"
aau_job_2019_svar$bra10grp[aau_job_2019_svar$bra10grp == "9 Offentlig administration, undervisning og sundhed"] = "Offentlig administration, undervisning og sundhed"
aau_job_2019_svar$bra10grp[aau_job_2019_svar$bra10grp == "10 Kultur, fritid og anden service"] = "Kultur, fritid og anden service"

aau_job_2019_svar = aau_job_2019_svar %>% mutate(bra10grp_code = ifelse(bra10grp=="Landbrug, skovbrug og fiskeri",3,
                                                                        ifelse(bra10grp=="Industri, råstofindvinding og forsyningsvirksomhed",2,
                                                                               ifelse(bra10grp=="Bygge og anlæg",1,
                                                                                      ifelse(bra10grp=="Handel og transport",4,
                                                                                             ifelse(bra10grp=="Information og kommunikation",5,
                                                                                                    ifelse(bra10grp=="Finansiering og forsikring",6,
                                                                                                           ifelse(bra10grp=="Ejendomshandel og udlejning",7,
                                                                                                                  ifelse(bra10grp=="Erhvervsservice",8,
                                                                                                                         ifelse(bra10grp=="Offentlig administration, undervisning og sundhed",9,
                                                                                                                                ifelse(bra10grp=="Kultur, fritid og anden service",10,
                                                                                                                                       NA)))))))))))



aau_job_2019_svar = aau_job_2019_svar[,c(1,2,3,85,4:(ncol(aau_job_2019_svar)-1))] 

aau_job_2019_svar = aau_job_2019_svar %>% filter(bra10grp != "11 Uoplyst")

aau_job_2019_svar = aau_job_2019_svar %>% select(c(-int_resultat, -metode))

disco = disco %>% select(c(Resp_ID1, ISCO_NU, Functions)) %>% rename(Resp_id1 = Resp_ID1)

data = merge(disco, aau_job_2019_svar)

write.csv(data, "surveydata.csv")



