#Importando Bibliotecas
library("tidyverse")
library("openxlsx")
library("Matrix")
library("gdata")

## Leitura diretamente do Excel
dados_KT = openxlsx::read.xlsx("C:/Doutorado/2022-1/Econometria I  2022-1/Sample3.5.xlsx")


# change character to numeric
dados_KT$Ability = as.numeric(dados_KT$Ability)
dados_KT$ln.Wage = as.numeric(dados_KT$ln.Wage)
#str(dados_KT)

options(scipen=4)  # Set scipen = 0 to get back to default



#def da matriz de variaveis
Y = matrix(data = NA, nrow=nrow(dados_KT) , ncol= 1 )
R = matrix(data = NA, nrow=nrow(dados_KT) , ncol=ncol(dados_KT))
X_1 = matrix(data = NA, nrow=nrow(dados_KT) , ncol=4)
X_2 = matrix(data = NA, nrow=nrow(dados_KT) , ncol=3)


Y = dados_KT$ln.Wage
X_1[,1] = 1
X_1[,2] = dados_KT$Education
X_1[,3] = dados_KT$Experience
X_1[,4] = dados_KT$Ability
X_2[,1] = dados_KT$`Mother’s.Education`
X_2[,2] = dados_KT$`Father’s.Education`
X_2[,3] = dados_KT$Siblings

exploratoria = summary(dados_KT)
exploratoria

R = cbind(X_1 , X_2)


#Modelo regressão em X_1
X_under1 = cbind(dados_KT$Education, dados_KT$Experience, dados_KT$Ability)
X_under2 = cbind(dados_KT$`Mother’s.Education`, dados_KT$`Father’s.Education`, dados_KT$Siblings)



mdl_X2 = lm (ln.Wage ~ `Mother’s.Education` + `Father’s.Education` + Siblings, data = dados_KT)
summary(mdl_X2)


#Letra A. Solução Matricial em X_1

mdl_X1 = lm (ln.Wage ~ Education + Experience + Ability, data = dados_KT)
summary(mdl_X1)

  #analiticamente

n = nrow(dados_KT)
p = length(coef(mdl_X1))
X_01000001 = cbind(rep(1, n),dados_KT$Education, dados_KT$Experience,dados_KT$Ability)
y = dados_KT$ln.Wage
#(beta_hat = solve(t(X_01000001) %*% X_01000001) %*% t(X_01000001) %*% y)



#Letra B. Solução Matricial em X_1 e X_2
X_01000010 = cbind(rep(1, n),dados_KT$Education, dados_KT$Experience,dados_KT$Ability,
             dados_KT$`Mother’s.Education`, dados_KT$`Father’s.Education`, dados_KT$Siblings)

(beta_hat = solve(t(X_01000010) %*% X_01000010) %*% t(X_01000010) %*% y)



#Letra C.


X_2asterisco = diag(dim((X_1 %*% solve(crossprod(X_1))) %*% t(X_1))[1]) -
  ((X_1 %*% solve(crossprod(X_1))) %*% t(X_1))
X_2asterisco

E_X2eX1 = matrix(data = NA , nrow = 1, ncol = 3)
rownames(E_X2eX1) = c('Medias')
colnames(E_X2eX1) = c('mothered','fathered','sibms')
for (k in 1:3){
  E_X2eX1[,k] = mean(X_2asterisco %*% X_2[,k])
}
print(E_X2eX1)

#Ao criar um vector para os valores residuais,a diferença das medias tem soma zero. 
#Isso se deve ao vetor variáveis que são ortogonais para as coluna X1. 



#Letra D.

Rquadrado = lm(ln.Wage ~ X_under1 + X_under2, data = dados_KT)
summary(Rquadrado)

  #Analiticamente R2 sem a constante em X1

lts = matrix(data = 1, nrow=nrow(dados_KT) , ncol= 1)
M_rs = diag(dim(lts)[1]) - (lts %*% solve(crossprod(lts)) %*% t(lts))

X_sem_cons = R[,-1]
r_sem_cons = solve(crossprod(X_sem_cons)) %*% (t(X_sem_cons) %*% Y)
Rquadrado_sem_cons = (t(r_sem_cons) %*% t(X_sem_cons) %*% M_rs %*% X_sem_cons %*% r_sem_cons) / (t(Y) %*% M_rs %*% Y)
Rquadrado_sem_cons


#Letra E.

R2_ajustado = lm(ln.Wage ~ X_under1 + X_under2, data = dados_KT)
summary(R2_ajustado)


#Letra F.

X_f = cbind(X_1, X_2asterisco %*% X_2 )
B_f = solve(crossprod(X_f)) %*% (t(X_f) %*% Y)
rownames(B_f) = c ('Cons','EDUC','EXP','ABILITY','MOTHERED','FATHERED','SIBLINGS')
colnames(B_f) =( 'Coef' )
print(B_f)

#Agora basta comparar com a regressao multipla original. Dá na mesma.

mdl_multiplo = lm (ln.Wage ~ Education + Experience + Ability + X_2asterisco, data = dados_KT)
summary(mdl_multiplo)
