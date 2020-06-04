library(haven)

path = file.path("C:/Users/Emma/Downloads", "ISSP 2005 Danskernes arbejdsliv.sav")
dataset = read_sav(path)

path1 = file.path("C:/Users/Emma/Downloads", "ISSP_DK_97.sav")
dataset1 = read_sav(path1)

path2 = file.path("C:/Users/Emma/Downloads", "379437_issp_2015_dda--1-.sav")
dataset2 = read_sav(path2)


a = prop.table(table(dataset2$s_66))
b = prop.table(table(dataset2$s_67))
c = prop.table(table(dataset2$s_68))
d = prop.table(table(dataset2$s_69))
e = prop.table(table(dataset2$s_70))
f = prop.table(table(dataset2$s_71))
g = prop.table(table(dataset2$s_72))
h = prop.table(table(dataset2$s_73))

a1 = 0.31488550 + 0.48727735
b1 = 0.094783715 + 0.493638677
c1 = 0.06157793 + 0.33996151
d1 = 0.585086042 + 0.377947737
e1 = 0.443456163 + 0.440279543
f1 = 0.25871909 + 0.45339252
g1 = 0.20995533 + 0.43714103
h1 = 0.17691820 + 0.41915029

a0 = 0.36943907 + 0.413926500
b0 = 0.09477756 + 0.45841393
c0 = 0.08317215 + 0.35203095
d0 = 0.599613153 + 0.351063830
e0 = 0.47582205 + 0.41779497
f0 = 0.34719536 + 0.45551257
g0 = 0.26982592 + 0.47582205
h0 = 0.16150870 + 0.34235977


a2 = 0.34137291 + 0.51205937
b2 = 0.082560297 + 0.468460111
c2 = 0.08163265 + 0.36270872
d2 = 0.555658627 + 0.393320965
e2 = 0.386827458 + 0.443413729
f2 = 0.255102041 + 0.451762523
g2 = 0.24304267 + 0.44155844
h2 = 0.15120594 + 0.37755102


values = c(a0,a1,a2,b0,b1,b2,c0,c1,c2,d0,d1,d2,e0,e1,e2,f0,f1,f2,g0,g1,g2,h0,h1,h2)
year = c(1997,2005,2010,1997,2005,2010,1997,2005,2010,1997,2005,2010,1997,2005,2010,1997,2005,2010,1997,2005,2010,1997,2005,2010)
group = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)

data = data.frame(values, year, group)

{ggplot(data=data, aes(x=group, y=values)) + 
    geom_histogram(stat="identity") +
    theme_economist() +
    
}
