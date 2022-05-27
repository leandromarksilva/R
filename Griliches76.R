#Returns from Schooling. Griliches (1976)

#packages
library(tidyverse)
library(AER)
library(car)
library(carData)
library(stargazer)
library(knitr)
library(estimatr)
library(openxlsx)
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(cache = TRUE)


## Leitura diretamente do Excel

dados_GRILC = openxlsx::read.xlsx("C:/Users/Doutorado/2022-1/Econometria I  2022-1/grilic.xlsx")
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

stage_01 = lm(LW~S+EXPR+TENURE+IQ, data=dados_GRILC)
summary(ols)


# 2st stage
stage_02 = lm(LW ~ fitted(stage_01) +RNS + SMSA 
              + AGE + MRT, data = dados_GRILC)
summary(stage_02)  



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

ToSLS = ivreg(LW ~ S + IQ + EXPR + TENURE +RNS + SMSA
              +year_66+year_67+year_68+year_69
              +year_70+year_71+year_73 +0|
                  S + EXPR + TENURE + RNS + SMSA 
              +year_66 + year_67 + year_68 + year_69
              +year_70 + year_71
              + MED +KWW + MRT + AGE, data = df_regress)
summary(ToSLS)



stargazer(ToSLS, type = "text", out = "IVregress.txt")



###2. Sargan

summary(ToSLS,diagnostics = TRUE)
#summary(to_SLS,diagnostics = TRUE)

#Ou seja, temos a estistica de 87.655 sobre 745 graus de liberdade. O p-Valor
# zero o que n?o fornece provas suficientes contra a hip?tese nula.
#Portanto, o teste Sargan sugere que as vari?veis do instrumento n?o 
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
ToSLS_02 = ivreg(LW ~ S + IQ + EXPR + TENURE +RNS + SMSA
              +year_66+year_67+year_68+year_69
              +year_70+year_71+year_73|
                S + EXPR + TENURE + RNS + SMSA 
              +year_66 + year_67 + year_68 + year_69
              +year_70 + year_71
              + MED +KWW + AGE, data = df_regress)
summary(ToSLS_02)


# AGE exclude
ToSLS_03 = ivreg(LW ~ S + IQ + EXPR + TENURE +RNS + SMSA
                 +year_66+year_67+year_68+year_69
                 +year_70+year_71+year_73|
                   S + EXPR + TENURE + RNS + SMSA 
                 +year_66 + year_67 + year_68 + year_69
                 +year_70 + year_71
                 + MED +KWW, data = df_regress)
summary(ToSLS_03)



summary(ToSLS_02,diagnostics = TRUE)
summary(ToSLS_03,diagnostics = TRUE)

# Na primeira especificao passa, porem na segunda nao.
