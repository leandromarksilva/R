install.packages("pastecs")
library(pastecs)
install.packages("doBy")
library(doBy)
install.packages("plm")
library(plm)
install.packages("lfe")
library(lfe)
install.packages("ggplot2")
library(ggplot2)

getwd()  #retorna a pasta diretório  (para desencargo de consciência) 

## ------------------------------------------------------------------------
#{Dados 
dtpd_1980 = read.csv("driving_1980.csv", sep = ";", header = T)
datapd_1980 = data.frame (dtpd_1980, index = c("n", "year"))
attach(datapd_1980)


dtpd_1982 = read.csv("driving_1982.csv", sep = ";", header = T)
datapd_1982 = data.frame (dtpd_1982, index = c("n", "year"))
attach(datapd_1982)


dtpd_2004 = read.csv("driving_2004.csv", sep = ";", header = T)
datapd_2004 = data.frame (dtpd_2004, index = c("n", "year"))
attach(datapd_2004)
#end}

## ------------------------------------------------------------------------
#{Statisticas descritivas

scores_1980 = cbind (bac10, bac08, totfatrte, nghtfatrte, wkndfatrte, vehicmiles, unem, perc14_24)
stat.desc(scores_1980)
options(scipen=100)
options(digits=3)
stat.desc(scores_1980)
stat.desc(scores_1980, basic=F)

scores_1982 = cbind (bac10, bac08, totfatrte, nghtfatrte, wkndfatrte, vehicmiles, unem, perc14_24)
stat.desc(scores_1982)
options(scipen=100)
options(digits=3)
stat.desc(scores_1982)
stat.desc(scores_1982, basic=F)

scores_2004 = cbind (bac10, bac08, totfatrte, nghtfatrte, wkndfatrte, vehicmiles, unem, perc14_24)
stat.desc(scores_2004)
options(scipen=100)
options(digits=3)
stat.desc(scores_2004)
stat.desc(scores_2004, basic=F)

##} ------------------------------------------------------------------------

#{Dados em painel
dtpd = read.csv("driving.csv", sep = ";", header = T)

pdata = pdata.frame(dtpd, index=c("state","year"))
attach(pdata)

Y = cbind(totfatrte)
X = cbind(log(vehicmiles), log(unem), log(perc14_24))
P = cbind(bac10, bac08, sl70plus, sbprim, sbsecon)

l_dummy = cbind(d81, d82, d83, d84, d85, d86, d87, d88, d89, d90, d91, d92, d93, d94, 
             d95, d96, d97, d98, d99, d00, d01, d02, d03, d04)

# Estatisticas Descritivas - Generalizando para todos os anos
summaryBy(cbind(totfatrte, nghtfatrte, wkndfatrte, vehicmiles, unem, perc14_24) ~ year, data=pdata,
          FUN=c(mean, var, sd), na.rm=TRUE)


#Reg Pooled MQO 
plm_pooling = plm(log(Y) ~ X + l_dummy, data=pdata, model= "pooling")
summary(plm_pooling)

#Reg Fixed effects ou within estimator
plm_fixed = plm(log(totfatrte) ~ vehicmiles + unem + perc14_24, 
                data=pdata, index=c("state", "year"), model= "within")
summary(plm_fixed)

#Reg Random effects 
plm_random = plm(log(totfatrte) ~ vehicmiles + unem + perc14_24, 
                 data=pdata, index=c("state", "year"), model="random")
summary(plm_random)

# Between estimator
plm_between = plm(log(totfatrte) ~ vehicmiles + unem + perc14_24 + P, 
                  data=pdata, index=c("state", "year"), model= "within", effect="twoways")
summary(plm_between)

#Reg First differences 
plm_firstdiff = plm(log(Y) ~ X + l_dummy, data=pdata, model= "fd")
summary(plm_firstdiff)


#Reg Time-fixed effects 

fixed.time = plm(log(totfatrte) ~ vehicmiles + unem + perc14_24 + factor(year), 
                 data=pdata, model= "within")
summary(fixed.time)

#}------------------------------------------------------------------------
#Avaliação da Regressão de dados em painel

# LM test for fixed effects and MQO 
pFtest(plm_fixed, plm_pooling)

# Test between MQO and random effects (não quer rodar!)
plmtest(plm_pooling, type="bp")

# Hausman test for fixed versus random effects model
phtest(plm_random, plm_fixed)


#}------------------------------------------------------------------------
# Diagnóstico do modelo

# Test for serial correlation
pbgtest(plm_random)

#Normalidade dos resíduos
residuos = plm_random$residuals
hist(residuos,col="blue",main="Histograma dos resíduos")
shapiro.test(residuos)$p.value

#Homocedasticidade dos resíduos
library(lmtest)
bptest(plm_random)$p.value



