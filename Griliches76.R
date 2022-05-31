#Returns from Schooling. Griliches (1976)

#packages
library(tidyverse)
library(AER)
library(stargazer)
library(knitr)
library(estimatr)
library(openxlsx)
library(lmtest)
library(sandwich)
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(cache = TRUE)


## Leitura diretamente do Excel

dados_GRILC = openxlsx::read.xlsx("C:/2022-1/Econometria I  2022-1/grilic.xlsx")
str(dados_GRILC)

options(scipen=4)

to_summarize = dados_GRILC %>% select(AGE, S, LW, KWW, IQ, EXPR)
stargazer(as.data.frame(to_summarize), type = "text")



#create dummy variables
year_66 <- ifelse(dados_GRILC$YEAR == '66', 1, 0)
year_67 <- ifelse(dados_GRILC$YEAR == '67', 1, 0)
year_68 <- ifelse(dados_GRILC$YEAR == '68', 1, 0)
year_69 <- ifelse(dados_GRILC$YEAR == '69', 1, 0)
year_70 <- ifelse(dados_GRILC$YEAR == '70', 1, 0)
year_71 <- ifelse(dados_GRILC$YEAR == '71', 1, 0)
year_73 <- ifelse(dados_GRILC$YEAR == '73', 1, 0)



#Data frame geral para regressao
df_regress <- data.frame(RNS = dados_GRILC$RNS, RNS80 = dados_GRILC$RNS80,
                         MRT = dados_GRILC$MRT, MRT80 = dados_GRILC$MRT80,
                         SMSA = dados_GRILC$SMSA, SMSA80 = dados_GRILC$SMSA80,
                         MED = dados_GRILC$MED, IQ = dados_GRILC$IQ, 
                         KWW = dados_GRILC$KWW, AGE = dados_GRILC$AGE,
                         AGE80 = dados_GRILC$AGE80, S = dados_GRILC$S,
                         S80 = dados_GRILC$S80, EXPR = dados_GRILC$EXPR, 
                         EXPR80 = dados_GRILC$EXPR80, TENURE = dados_GRILC$TENURE,
                         TENURE80 = dados_GRILC$TENURE80, LW = dados_GRILC$LW,
                         LW80 = dados_GRILC$LW80,
                         year_66 = year_66, year_67 = year_67, 
                         year_68 = year_68,year_69 = year_69,
                         year_70 = year_70, year_71= year_71,
                         year_73 = year_73)


#######################################################  

#2SLS

# 1st stage

stage_01 = lm(S ~ IQ + EXPR + TENURE + MED + KWW + MRT + AGE
              ,data=df_regress)
summary(stage_01)

df_regress$x_hat = fitted.values(stage_01)

# 2st stage
stage_02 = lm(LW ~ x_hat + S + EXPR + TENURE +RNS + SMSA +MED+KWW
              +year_66+year_67+year_68+year_69
              +year_70+year_71
              ,data = df_regress)
summary(stage_02)  


summary(stage_01, robust = TRUE)
summary(stage_02, robust = TRUE)


stargazer(stage_01, stage_02, type = "text")


#######################################################  
#2SLS direto: 
#######################################################


First_SLS = ivreg(LW ~ S + IQ + EXPR + TENURE +RNS + SMSA
                  +year_66+year_67+year_68+year_69
                  +year_70+year_71+year_73|
                      S + EXPR + TENURE + RNS + SMSA 
                  +year_66 + year_67 + year_68 + year_69
                  +year_70 + year_71
                  + MED +KWW + MRT + AGE, data = df_regress, method ="MM")



ToSLS = ivreg(LW ~ S + IQ + EXPR + TENURE +RNS + SMSA
              +year_66+year_67+year_68+year_69
              +year_70+year_71+year_73|
                S + EXPR + TENURE + RNS + SMSA 
              +year_66 + year_67 + year_68 + year_69
              +year_70 + year_71
              + MED +KWW + MRT + AGE, data = df_regress)
summary(ToSLS)


#E se eu retirar o intercepto?

To2SLS = ivreg(LW ~ S + IQ + EXPR + TENURE +RNS + SMSA
              +year_66+year_67+year_68+year_69
              +year_70+year_71+ year_73 +0|
                  S + EXPR + TENURE + RNS + SMSA 
              +year_66 + year_67 + year_68 + year_69
              +year_70 + year_71
              + MED +KWW + MRT + AGE, data = df_regress)
summary(To2SLS)



stargazer(ToSLS, type = "text",header=FALSE,no.space=TRUE,
          title="2SLS",omit.table.layout="nl" , out = "2SLSregress.txt")



###2. Sargan

summary(ToSLS,diagnostics = TRUE)
#summary(to_SLS,diagnostics = TRUE)

#Ou seja, temos a estatistica de 87.655 sobre 745 graus de liberdade. O p-Valor
# zero o que n?o fornece provas suficientes contra a hipotese nula.
#Portanto, o teste Sargan sugere que as variaveis do instrumento nao 
#estão relacionadas aos residuos.



###2. Hausman


# Com ou sem "IQ"?
#=======================================
reg_03IV= ivreg(LW ~ S + EXPR + TENURE +RNS + SMSA
                +year_66+year_67+year_68+year_69
                +year_70+year_71+year_73|
                  S + EXPR + TENURE + RNS + SMSA 
                +year_66 + year_67 + year_68 + year_69
                +year_70 + year_71   
                + MED + KWW + MRT + AGE, data = df_regress)     
summary(reg_03IV)
#summary(reg_03IV,diagnostics = TRUE)

#stargazer(ToSLS, reg_03IV,type = "text", out = "IVregressCompare.txt")


# Hausmann

error = reg_03IV$res
reg4  = lm(LW ~ S + IQ + EXPR + TENURE +RNS + SMSA
            +year_66+year_67+year_68+year_69
            +year_70+year_71+year_73 + error, data = dados_GRILC)
summary(reg4)





# 5
# MRT exclude
ToSLS002 = ivreg(LW ~ S + IQ + EXPR + TENURE +RNS + SMSA
              +year_66+year_67+year_68+year_69
              +year_70+year_71+year_73|
                S + EXPR + TENURE + RNS + SMSA 
              +year_66 + year_67 + year_68 + year_69
              +year_70 + year_71
              + MED +KWW + AGE, data = df_regress)
summary(ToSLS_02)


# AGE exclude
ToSLS003 = ivreg(LW ~ S + IQ + EXPR + TENURE +RNS + SMSA
                 +year_66+year_67+year_68+year_69
                 +year_70+year_71+year_73|
                   S + EXPR + TENURE + RNS + SMSA 
                 +year_66 + year_67 + year_68 + year_69
                 +year_70 + year_71
                 + MED +KWW, data = df_regress)
summary(ToSLS_03)



summary(ToSLS002,diagnostics = TRUE)
summary(ToSLS003,diagnostics = TRUE)

# Na primeira especificao passa, porem na segunda nao.

gaze.coeft = function(x, col="Std. Error"){
    stopifnot(is.list(x))
    out = lapply(x, function(y){
        y[ , col]
    })
    return(out)
}
gaze.coeft(list(rob.ToSLS002, rob.ToSLS003))
gaze.coeft(list(rob.ToSLS002, rob.ToSLS003), col=2)


gaze.lines.ivreg.diagn = function(x, col="p-value", row=1:3, digits=2){
    stopifnot(is.list(x))
    out = lapply(x, function(y){
        stopifnot(class(y)=="summary.ivreg")
        y$diagnostics[row, col, drop=FALSE]
    })
    out = as.list(data.frame(t(as.data.frame(out)), check.names = FALSE))
    for(i in 1:length(out)){
        out[[i]] = c(names(out)[i], round(out[[i]], digits=digits))
    }
    return(out)
}
gaze.lines.ivreg.diagn(list(summ.ToSLS002, summ.ToSLS003), row=1:2)
gaze.lines.ivreg.diagn(list(summ.ToSLS002, summ.ToSLS003), col=4, row=1:2, digits=4)


stargazer(ToSLS002, ToSLS003, type = "text", 
          se = gaze.coeft(list(rob.ToSLS002, rob.ToSLS003)), 
          add.lines = gaze.lines.ivreg.diagn(list(summ.ToSLS002, summ.ToSLS003), row=1:2))
