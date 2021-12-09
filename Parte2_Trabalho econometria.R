install.packages("pastecs")
library(pastecs)

install.packages("plm")
library(plm)

install.packages("ggplot2")
library(ggplot2)

getwd()  #retorna a pasta diretório 



dtpd = read.csv("driving.csv", sep = ";", header = T)
datapd = data.frame (dtpd, index = c("n", "year"))
attach(datapd)

pdata = pdata.frame(dtpd, index=c("n","year"))

summary(pdata)

head (datapd)
attach(datapd)
scores_1 = cbind (totfatpvm, nghtfatpvm, wkndfatpvm, statepop, totfatrte, nghtfatrte, wkndfatrte, vehicmiles, unem, perc14_24)
scores_2 = cbind(state, sl55, sl65, sl70, sl75, slnone, seatbelt, minage, zerotol, gdl, bac10, bac08, perse)
stat.desc(scores)

#Statisticas descritivas
options(scipen=100)
options(digits=2)
stat.desc(scores_1)
stat.desc(scores_1, basic=F)

options(scipen=100)
options(digits=2)
stat.desc(scores_2)
stat.desc(scores_2, basic=F)


colnames(dtpd)
by_year = group_by(dtpd, year)
nhsdf = summarize(dtpd, mean_by_year = mean(totfatpvm, na.rm = TRUE))



# Pooled OLS estimator
pooling = plm(Y ~ X, data=pdata, model= "pooling")
summary(pooling)


