## Trabalho Econometria Series Temporais ###
##questao 6 

##carregar pctes
library(openxlsx)
library(forecast)
library(tidyverse)
library(ggmatplot)
library(sarima)

#importar
dados_SIM2 = openxlsx::read.xlsx("Doutorado/2022-1/Econometria I  2022-1/Listas/Sim2.xlsx")

View(dados_SIM2)

#transformar dataframe em ts
Y_1 = ts(dados_SIM2$Y1)
Y_2 = ts(dados_SIM2$Y2)
Y_3 = ts(dados_SIM2$Y3)

## a - plotar grafico

par(mfrow=c(2,2))
plot.ts(Y_1, main="Y1")
plot.ts(Y_2, main="Y2")
plot.ts(Y_3, main="Y3")
# - Séries y2 e y3 parecem estacionárias, y1 não estacionária

## b - FAC e FACPF
par(mfrow=c(1,2))
acf(Y_1,main="ACF Y1")
pacf(Y_1,main="PACF Y1")

par(mfrow=c(1,2))
acf(Y_2,main="ACF Y2")
pacf(Y_2,main="PACF Y2")

par(mfrow=c(1,2))
acf(Y_3,main="ACF Y3")
pacf(Y_3,main="PACF Y3")


####Questao 07######

# Organização do processo
n = 100
e = rnorm(n)
a = 0
a_1 = 0.9 
Y = rep(0,n)

Y[1] = rnorm(100,0,1)

for(t in 2:n) {
    Y[t] = a + a_1*Y[t-1] + e[t]
    }

Y = abs(Y)

#########
n = 100
e = rnorm(n)
a = 20
a_1 = 0.75 
Y = rep(0,n)

Y[1] = rnorm(100,0,1)

for(t in 2:n) {
    Y[t] = a + a_1*Y[t-1] + e[t]
}

Ya = abs(Y)


###########
n = 100
e = rnorm(n)
a = 0.2
a_1 = 1 
Y = rep(0,n)

Y[1] = rnorm(100,0,1)

for(t in 2:n) {
    Y[t] = a + a_1*Y[t-1] + e[t]
}

Yb = abs(Y)

#############


n = 100
e = rnorm(n)
a = 0
a_1 = -1.1 
Y = rep(0,n)

Y[1] = rnorm(100,0,1)

for(t in 2:n) {
    Y[t] = a + a_1*Y[t-1] + e[t]
}

Yc = abs(Y)






##Plots
par(mfrow=c(2,2))
A = matplot(Y,type='l', lty=1, col=rgb(0, 0.4, 1, 0.6), xlab="")
B = matplot(Ya,type='l', lty=1, col=rgb(0, 0.4, 1, 0.6), xlab="")
C = matplot(Yb,type='l', lty=1, col=rgb(0, 0.4, 1, 0.6), xlab="")
D = matplot(Yc,type='l', lty=1, col=rgb(0, 0.4, 1, 0.6), xlab="")



par(mfrow=c(2,2))
acf(Y,main="ACF Y1")
acf(Ya,main="ACF Y2")


par(mfrow=c(2,2))
acf(Yb,main="ACF Y3")
acf(Yc,main="ACF Y4")













# Simulate AutoRegressive model with 0.5 slope
yt = arima.sim(list(order=c(1,0,0), ar=0.9), n=100)
yt_2 = arima.sim(list(order=c(1,0,0), ar=0.75), n=100)
yt_3 = arima.sim(list(order=c(1,0,0), ar=1), n=100)
yt_4 = arima.sim(list(order=c(1,0,0), ar=-1.1), n=100)



matplot(yt,type='l', lty=1, col=rgb(0, 0.4, 1, 0.6), xlab="")


# Plot
par(mfrow=c(1,2))
matplot(1:(N-1),yt,type='l', lty=1, col=rgb(0, 0.4, 1, 0.6), xlab="")





