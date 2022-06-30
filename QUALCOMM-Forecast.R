
##Bibliotecas utilizadas##
library(quantmod)
library(plyr)
library(xts)
library(quantmod)
library(tseries)
library(timeSeries)
library(quadprog)
library(forecast)
require(forecast)


##Transferência dos dados do S&P 500 (YahooFinance)##

getSymbols('QCOM', from='2010-01-01', to='2019-12-31')

spclose_0110 = QCOM[,4]
spclose_0110

##Calculo do log dos retornos##

qclog = diff(log(spclose_0110),lag = 1)
qclog = spclose_0110[!is.na(spclose_0110)]

qclog

##Plotting log returns##
matplot(qclog,type='l', lty=1, col=rgb(0, 0.4, 1, 0.6), 
        xlab="Percentual de retornos diários do SP-500", ylab = "Retornno Diário (%)")

##Verificação da estacionaridade ## Unit Root Tests
##Augumented Dickey Fuller Test##
ADF = print(adf.test(qclog))

pp_P = pp.test(qclog, type = "Z_rho", lag.short = TRUE, output = TRUE)
kpss_S = kpss.test(qclog ,lag.short = TRUE, output = TRUE)


##Box-Ljung test##
Box.test(qclog, lag = 10, type = "Ljung-Box")
#Rejeitamos a hipótese nula/Não há autocorrelação


#Divisão do conjunto de dados em duas partes - treinamento e testes##
breakpyt = floor(nrow(qclog)*(9.5/10))

##aplicação das funções ACF e PACF em dados truncados usando o breakpoint##
par(mfrow = c(1,2))
acf.qclog = acf(qclog[c(1:breakpyt),], main='ACF Plot',lag.max = 100)
pacf.qclog = pacf(qclog[c(1:breakpyt),], main='PACF Plot',lag.max = 100)

##Não é necessário diferenciar, pois os dados são estacionários##


stargazer(ADF, no.space = TRUE, type = "text")
stargazer(pp_P, kpss_S, no.space = TRUE, type = "text", style='aer')


#ARIMA(1)
arima(qclog, order = c(0,1,1)) 


##Cross checking com AutoArima##
#Auto arima 
auto.arima(qclog) #(0,1,1)

##Fitting arima model##
arima_fit = arima(qclog, order = c(0,1,1))
arima_fit
fto = forecast(arima_fit, h=1000)
par(mfrow = c(1,1))
plot(fto)




############################################################
#1- Projetando toda a série de retornos a partir do breakpoint.
#2- Within this loop we will forecast returns for each data point from the test dataset.

##Inicialização de um objeto xts para retornos reais de log##

serie_atual = xts(0,as.Date("2014-08-10","%Y-%m-%d"))


#3- Primeiro iniciamos uma série que irá armazenar os retornos reais e outra série para armazenar os retornos previstos.
#4- No Loop (recursivo), primeiro formamos o conjunto de dados de treinamento e o conjunto de dados de teste com base no ponto de quebra dinâmico.



##Iniciando um dataframe para a série de retornos previstos##
forecasted_ts = data.frame(Forecasted = numeric())


for (b in breakpyt:(nrow(qclog)-1)) {
    
    qclog_train = qclog[1:b, ]
    qclog_test = qclog[(b+1):nrow(qclog), ]
    
    
    #5- fitting model, parâmetros ARIMA (0,1,1)

    ##Resumo do modelo ARIMA utilizando os parâmetros determinados (p,d,q)##
    fit = arima(qclog_train, order = c(0, 1, 1),include.mean=FALSE)
    summary(fit)
    
    ##Plot acf dos residuos##
    acf(fit$residuals,main="Residuos")

    
    
    
    #6- Previsão, o nível de confiança é de 99% , para melhorar o modelo. 
    ##Projeção dos retornos dos logs##
    arima_forecast = forecast(fit, h = 1,level = 99)
    summary(arima_forecast)

    ##Plotting da previsão##
    par(mfrow=c(1,1))
    plot(arima_forecast, main = "Previsão ARIMA")  

    ##Criando uma série de retornos previstos para o período projetado##
    forecasted_ts = rbind(forecasted_ts,arima_forecast$mean[1])
    colnames(forecasted_ts) = c("Forecasted")    

    ##Criação de uma série de retornos reais para o período projetado##
    Actual_return = qclog[(b+1),]
    serie_atual = c(serie_atual,xts(Actual_return))
    rm(Actual_return)
    
    print(qclog[(b+1),])
    print(qclog[(b+2),])
    
}    


###########################################################
##Ajustar o comprimento da série de retorno real##
serie_atual = serie_atual[-1]


##Criação de um objeto de série temporal da série prevista##
forecasted_ts = xts(forecasted_ts,index(serie_atual))
forecasted_ts = forecasted_ts


library(dygraphs)
serie_atual = serie_atual
serie_atual
dygraph(serie_atual)



plot(serie_atual,type='l',main='Retornos Realizados Vs Retorno Previstos')
lines(forecasted_ts,lwd=1.8,col='#FC724F')
legend('bottomright',c("Atual","Previsto"),lty=c(1,1),lwd=c(1.5,1.5),col=c('black','red'))



##Elaboração de uma tabela para a acurácia da previsão##
comparison = merge(serie_atual,forecasted_ts)
comparison$Accuracy = sign(comparison$serie_atual)==sign(comparison$Forecasted)

print(comparison)

dygraph(comparison)



##Cálculo da porcentagem de precisão métrica##
Accuracy_percentual = sum(comparison$Accuracy == 1)*100/length(comparison$Accuracy)
print(Accuracy_percentual)


fit_res = residuals(arima_fit)



#diagnósticos #Check autocorrelação #aleatoriedade dos resíduos
Box.test(fit_res, lag = 10, type = "Ljung-Box")


#Desde que o P-valor seja > 0.05, indicando que os resíduos são aleatórios
#e que o modelo fornece um ajuste adequado aos dados. 
    

tsdiag(arima_fit)


#Previsão dos retornos
pred = predict(arima_fit, n.ahead = 2)
pred


###End Code###
