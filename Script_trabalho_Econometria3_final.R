install.packages("pastecs")
library(pastecs)

getwd()  #retorna a pasta diretório 

dt= read.csv("traffic2.csv", sep = ";", header = T)

head (dt)
attach(dt)
scores = cbind (totacc, fatacc, t, unem)
stat.desc(scores)

#Statisticas descritivas
options(scipen=100)
options(digits=2)
stat.desc(scores)
stat.desc(scores, basic=F)

#Matrix de correlação
res = cor(totacc, fatacc, t, unem)
round(res, 2)

  install.packages("corrplot")
  library(corrplot)
  corrplot(res, type = "upper", order = "hclust", 
           tl.col = "black", tl.srt = 45)



#Time Series

install.packages("dynlm")
library(dynlm)


totacc.ts = ts(totacc,start=c(1981,1),end=c(1989,12),frequency=12)
fatacc.ts = ts(fatacc,start=c(1981,1),end=c(1989,12),frequency=12)
unem.ts = ts(unem,start=c(1981,1),end=c(1989,12),frequency=12)

plot(totacc.ts)
plot(unem.ts)
plot(fatacc.ts)


# Seasonal decomposition
fit_01 = stl(totacc.ts, s.window="period")
fit_02 = stl(fatacc.ts, s.window="period")
fit_03 = stl(unem.ts, s.window="period")


plot(fit)
monthplot(totacc)
library(forecast)
seasonplot(totacc.ts)



# Efeito da spdlaw =1 after 65 mph in effect 
# Efeito da beltlaw =1 after seatbelt law
# Efeito da wkends;  weekends in month

mr_05 = lm(log(totacc.ts) ~ unem.ts + spdlaw +beltlaw + wkends + t + feb + mar +
             apr + may + jun + jul + aug + sep + oct + nov + dec)
summary(mr_05)



# Efeito da spdlaw =1 after 65 mph in effect
# Efeito da beltlaw =1 after seatbelt law
# Efeito da wkends;  weekends in month

mr_06 = lm(log(fatacc.ts) ~ unem.ts + spdlaw + beltlaw + wkends + t  + feb + mar +
             apr + may + jun + jul + aug + sep + oct + nov + dec)
summary(mr_06)



# Efeito da spdlaw =1 after 65 mph in effect
# Efeito da beltlaw =1 after seatbelt law
# Efeito da wkends;  weekends in month

mr_07 = lm(log(unem.ts) ~ fatacc.ts + spdlaw + beltlaw + wkends + t + feb + mar +
             apr + may + jun + jul + aug + sep + oct + nov + dec)
summary(mr_07)







