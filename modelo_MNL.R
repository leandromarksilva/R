################################################
## ECONOMETRIA II - Doutorado                 ##
## Atividade Empírica I                       ##
## Aluno: Leandro Marques                     ##
################################################

###############################
## Importing useful packages ##
###############################

if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2,ggthemes, nnet, margins,performance,gtsummary,survival,reshape2,DescTools,pROC,
               psych,car,rms,lmtest, AER, mlogit, MNP, sandwich, gmnl, dfidx, Formula, stargazer,kableExtra,
               gridExtra,stringi)

# Carregar conjunto de dados
dados_br = read.csv(".../dados_brDF.csv")


##--------------------------------------------------------------------------------------------------###
# Modelo Probit Binário #

# Filtra o conjunto de dados para incluir apenas as observações em que a escolha é "carro" ou "transporte_publico"
dados_br_probit = dados_br[which(dados_br$escolha == "1" | dados_br$escolha == "2"),]

# Verifica se o conjunto de dados 'dados_br_probit' foi criado corretamente
head(dados_br_probit)

# Converte a variável de escolha em uma variável binária
dados_br_probit$choice_bin = ifelse(dados_br_probit$escolha == "1", 0, 1)

# Ajusta o modelo Probit
probit_model = glm(choice_bin ~ tempo_carro + tempo_transporte_publico + tempo_bicicleta + custo_carro +
                     custo_transporte_publico + custo_bicicleta + renda + idade + genero + educacao_fundamental +
                     educacao_medio + ocupacao_empregado + ocupacao_autonomo + possui_carro,
                   data = dados_br_probit, family = binomial(link = "probit"))

# Exibi os resultados
summary(probit_model)

# Gera tabela resumo dos resultados
stargazer(probit_model,
          title = "Resultados do Modelo Probit",
          type = "latex", out = "tabela_resumo.tex",
          header = FALSE, digits = 3)

#Pseudo R-squared (McFadden R-squared):
null_model = glm(choice_bin ~ 1, data = dados_br_probit, family = binomial(link = "probit"))
null_deviance = null_model$deviance
model_deviance = probit_model$deviance
pseudo_r_squared = 1 - (model_deviance / null_deviance)
show(pseudo_r_squared)

# Análise de resíduos para o modelo Probit:
# dataframe com os resíduos
residuos_probit_df = data.frame(residuos = resid(probit_model, type = "deviance"))
# gráfico de dispersão com o tema theme_par
plot_residuos_probit = ggplot(residuos_probit_df, aes(x = 1:length(residuos), y = residuos)) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuos do Modelo Probit", x = "Indice", y = "Residuos") +
  theme_par() +
  theme(plot.title = element_text(face = "bold", size = 14, margin = margin(b = 10)),
        axis.title = element_text(face = "bold", size = 12, margin = margin(t = 10, r = 10, b = 10, l = 10)),
        axis.text = element_text(size = 10),
        panel.grid.major = element_line(color = "gray", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
# Exibe o gráfico
print(plot_residuos_probit)








# modelo linear sem a variável de resposta
linear_model = lm(choice_bin ~ tempo_carro + tempo_transporte_publico + tempo_bicicleta + custo_carro +
                     custo_transporte_publico + custo_bicicleta + renda + idade + genero + educacao_fundamental +
                     educacao_medio + ocupacao_empregado + ocupacao_autonomo + possui_carro,
                   data = dados_br_probit)
# Calcule os valores VIF para cada variável
vif_values = vif(linear_model)
# Exiba os valores VIF
vif_values

# Calcula efeitos marginais
marginal_effects = margins(probit_model)
summary(marginal_effects)

# Realiza previsões com base no modelo ajustado
predicted_probabilities = predict(probit_model, type = "response")

# Adiciona as probabilidades previstas ao conjunto de dados
dados_br_probit$predicted_probabilities = predicted_probabilities

# Visualizando as primeiras linhas do conjunto de dados com as probabilidades previstas
head(dados_br_probit)


# Calculando a taxa de acertos e a matriz de confusão
dados_br_probit$predicted_choice = ifelse(dados_br_probit$predicted_probabilities > 0.5, 1, 0)
confusion_matrix = table(dados_br_probit$choice_bin, dados_br_probit$predicted_choice)
accuracy = sum(diag(confusion_matrix)) / sum(confusion_matrix)

# Imprimi a taxa de acertos e a matriz de confusão
print(confusion_matrix)
print(accuracy)


# Criando a curva ROC e calcular a AUC
roc_obj = roc(dados_br_probit$choice_bin, dados_br_probit$predicted_probabilities)
auc_value = auc(roc_obj)

# Plotar a curva ROC com o theme_par
roc_plot <- ggplot() +
  geom_line(aes(x = 1 - roc_obj$specificities, y = roc_obj$sensitivities, color = "Modelo Probit"), linetype = "solid") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = sprintf("Curva ROC\nAUC = %.2f", auc_value),
       x = "1 - Especificidade",
       y = "Sensibilidade") +
  scale_color_manual(values = "blue") +
  theme_par()

# Exibir o gráfico
print(roc_plot)


# Gráfico de dispersão das probabilidades previstas em relação a "tempo_carro"
scatter_plot_tempo_carro <- ggplot(dados_br_probit, aes(x = tempo_carro, y = predicted_probabilities)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed", color = "blue") +
  labs(title = "Probabilidades Previstas vs Tempo Carro",
       x = "Tempo Carro",
       y = "Probabilidades Previstas") +
  theme_wsj()

# Exibir o gráfico
print(scatter_plot_tempo_carro)


# Criar a tabela resumo com as informações relevantes
stargazer(probit_model,
          title = "Resultados do Modelo Probit",
          type = "latex", out = "tabela_resumo_beamer.tex",
          header = FALSE, digits = 3,
          keep.stat = c("n", "rsq", "f", "ser"),
          column.sep.width = "5pt",
          font.size = "small",
          summary = FALSE)




##--------------------------------------------------------------------------------------------------###
# Modelo Logit Binário #

# Filtra o conjunto de dados para incluir apenas as observações em que a escolha é "carro" ou "transporte_publico"
dados_br_logit = dados_br[which(dados_br$escolha == "1" | dados_br$escolha == "2"),]

# Verifica se o conjunto de dados 'dados_br_logit' foi criado corretamente
head(dados_br_logit)

# Converte a variável de escolha em uma variável binária
dados_br_logit$choice_bin = ifelse(dados_br_logit$escolha == "1", 0, 1)

# Ajusta o modelo Logit
logit_model = glm(choice_bin ~ tempo_carro + tempo_transporte_publico + tempo_bicicleta + custo_carro +
                     custo_transporte_publico + custo_bicicleta + renda + idade + genero + educacao_fundamental +
                     educacao_medio + ocupacao_empregado + ocupacao_autonomo + possui_carro,
                   data = dados_br_logit, family = binomial(link = "logit"))

# Exibe os resultados
summary(logit_model)

# Gera tabela resumo dos resultados
stargazer(logit_model,
          title = "Resultados do Modelo Logit",
          type = "latex", out = "tabela_resumo_logit.tex",
          header = FALSE, digits = 3)

# Realiza previsões com base no modelo ajustado
predicted_probabilities_logit = predict(logit_model, type = "response")

# Adiciona as probabilidades previstas ao conjunto de dados
dados_br_logit$predicted_probabilities = predicted_probabilities_logit

# Visualizando as primeiras linhas do conjunto de dados com as probabilidades previstas
head(dados_br_logit)


# Pseudo R-squared (McFadden R-squared) para o modelo Logit:
null_model_logit = glm(choice_bin ~ 1, data = dados_br_logit, family = binomial(link = "logit"))
null_deviance_logit = null_model_logit$deviance
model_deviance_logit = logit_model$deviance
pseudo_r_squared_logit = 1 - (model_deviance_logit / null_deviance_logit)
show(pseudo_r_squared_logit)

# Análise de resíduos para o modelo Logit:
# dataframe com os resíduos
residuos_logit_df = data.frame(residuos = resid(logit_model, type = "deviance"))
# gráfico de dispersão com o tema theme_par
plot_residuos_logit = ggplot(residuos_logit_df, aes(x = 1:length(residuos), y = residuos)) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuos do Modelo Logit", x = "Indice", y = "Residuos") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", size = 14, margin = margin(b = 10)),
        axis.title = element_text(face = "bold", size = 12, margin = margin(t = 10, r = 10, b = 10, l = 10)),
        axis.text = element_text(size = 10),
        panel.grid.major = element_line(color = "gray", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
# Exibe o gráfico
print(plot_residuos_logit)


# Calculando VIF para o modelo Logit
linear_model_logit = lm(choice_bin ~ tempo_carro + tempo_transporte_publico + tempo_bicicleta + custo_carro +
                     custo_transporte_publico + custo_bicicleta + renda + idade + genero + educacao_fundamental +
                     educacao_medio + ocupacao_empregado + ocupacao_autonomo + possui_carro,
                   data = dados_br_logit)
# Calcule os valores VIF para cada variável
vif_values_logit = vif(linear_model_logit)
# Exiba os valores VIF
vif_values_logit

# Calcula efeitos marginais para o modelo Logit
marginal_effects_logit = margins(logit_model)
summary(marginal_effects_logit)

# Calculando a taxa de acertos e a matriz de confusão para o modelo Logit
dados_br_logit$predicted_choice = ifelse(dados_br_logit$predicted_probabilities > 0.5, 1, 0)
confusion_matrix_logit = table(dados_br_logit$choice_bin, dados_br_logit$predicted_choice)
accuracy_logit = sum(diag(confusion_matrix_logit)) / sum(confusion_matrix_logit)

# Imprime a taxa de acertos e a matriz de confusão para o modelo Logit
print(confusion_matrix_logit)
print(accuracy_logit)

# Criando a curva ROC e calcular a AUC para o modelo Logit
roc_obj_logit = roc(dados_br_logit$choice_bin, dados_br_logit$predicted_probabilities)
auc_value_logit = auc(roc_obj_logit)

# Imprime o valor AUC para o modelo Logit
print(auc_value_logit)
# Plotar a curva ROC para o modelo Logit
plot(roc_obj_logit, main = sprintf("Curva ROC\nAUC = %.2f", auc_value_logit))


# Gráfico de dispersão das probabilidades previstas em relação a "tempo_carro" para o modelo Logit
scatter_plot_tempo_carro_logit <- ggplot(dados_br_logit, aes(x = tempo_carro, y = predicted_probabilities)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed", color = "blue") +
  labs(title = "Probabilidades Previstas vs Tempo Carro - Modelo Logit",
       x = "Tempo Carro",
       y = "Probabilidades Previstas") +
  theme_par()

# Exibir o gráfico para o modelo Logit
print(scatter_plot_tempo_carro_logit)





########----------------------------------------------------------------####

stargazer(probit_model, logit_model,
          title = "Comparacao dos Modelos Probit e Logit",
          type = "latex", out = "tabela_resumo_probit_logit.tex",
          header = FALSE, digits = 3,
          keep.stat = c("n", "rsq", "f", "ser"),
          column.sep.width = "3pt",
          font.size = "small",
          summary = FALSE,
          covariate.labels = c("Tempo de Carro", "Tempo de Transporte Publico", "Tempo de Bicicleta",
                               "Custo do Carro", "Custo do Transporte Publico", "Custo da Bicicleta",
                               "Renda", "Idade", "Genero", "Ensino Fundamental", "Ensino Medio",
                               "Empregado", "Autonomo", "Possui Carro"),
          dep.var.caption = "Escolha de Transporte",
          dep.var.labels.include = FALSE)




#----------------------------------------------------------------------------#
# Modelo Probit Multinomial #
#----------------------------------------------------------------------------#

# Converter conjunto de dados em formato longo
dados_br_long = dados_br %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = starts_with(c("tempo", "custo")),
               names_to = c(".value", "alternative"),
               names_pattern = "(.*)_(.*)")

# Criar variável de escolha
dados_br_long = dados_br_long %>%
  mutate(choice = as.integer(alternative == escolha)) %>%
  mutate(choice = ifelse(choice == 0, alternative, choice))

# Ajuste do modelo multinomial probit
model_mlogit = multinom(formula = choice ~ tempo + custo,
                        data = dados_br_long)

# Resumir resultados do modelo
summary(model_mlogit)
# Mostrar coeficientes do modelo
coef(model_mlogit)

#head(dados_br_long)


# Adicionar probabilidades ajustadas ao conjunto de dados
# Calcular as probabilidades de escolha ajustadas
fitted_probs = fitted(model_mlogit, type = "probabilities")

dados_br_long = dados_br_long %>%
  mutate(choice_id = as.integer(alternative))

# Converter o objeto matrix em um data.frame
fitted_probs_df = as.data.frame(fitted_probs) %>%
  rownames_to_column(var = "id") %>%
  mutate(id = as.integer(id)) %>%
  pivot_longer(cols = -id, names_to = "choice_id", values_to = "prob_mlogit") %>%
  mutate(choice_id = as.integer(choice_id))

# Juntar as probabilidades ajustadas ao conjunto de dados
dados_br_long <- dados_br_long %>%
  left_join(fitted_probs_df, by = c("id", "choice_id"))

# Calcular efeitos marginais para as variáveis tempo e custo
dados_br_long = dados_br_long %>%
  mutate(mfx_tempo = coef(model_mlogit)[2] * prob_mlogit * (1 - prob_mlogit),
         mfx_custo = coef(model_mlogit)[3] * prob_mlogit * (1 - prob_mlogit))


# Codifique a variável 'choice' como um fator
dados_br_long$choice <- factor(dados_br_long$choice, levels = c("carro", "publico", "bicicleta"))
# Crie a variável 'choice_id' a partir da variável 'choice'
dados_br_long$choice_id <- as.integer(dados_br_long$choice)
# Verifique se a variável 'choice_id' foi criada corretamente
head(dados_br_long)


# Visualizar densidade dos efeitos marginais

# Criar o gráfico de densidade
densidade_grafico <- dados_br_long %>%
  filter(is.finite(mfx_tempo) & is.finite(mfx_custo)) %>%
  pivot_longer(cols = c("mfx_tempo", "mfx_custo"), names_to = "variavel", values_to = "efeito_marginal") %>%
  ggplot(aes(x = efeito_marginal)) +
  geom_density(fill = "steelblue", alpha = 0.8) +
  facet_wrap(~ variavel, scales = "free_x", labeller = as_labeller(c(mfx_tempo = "Efeito marginal do tempo",
                                                                    mfx_custo = "Efeito marginal do custo"))) +
  xlab('Efeito Marginal') +
  ylab('Densidade') +
  ggtitle("Densidade dos Efeitos Marginais") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14, face = "bold"),
        strip.background = element_blank())
# Visualizar o gráfico
print(densidade_grafico)

# Salvar o gráfico como uma imagem PNG
ggsave(filename = "densidade_grafico.png", plot = densidade_grafico, width = 10, height = 6, dpi = 300)


# Verifique a ausência de multicolinearidade com um gráfico de pares
psych::pairs.panels(dados_br_long[, c("tempo", "custo")])

# Ajuste modelo linear para calcular o fator de inflação da variância (VIF)
mod = lm(choice_id ~ tempo + custo, data = dados_br_long)
#o coeficiente estimado para tempo indica que, mantendo o custo constante,
# um aumento de uma unidade em tempo está associado a uma diminuição estimada de choice_id
# (a variável resposta) de 0.04668728 unidades, em média. Da mesma forma, o coeficiente estimado para custo indica que,
# mantendo o tempo constante, um aumento de uma unidade em custo está associado a um aumento estimado
# de choice_id de 0.68675631 unidades, em média.
car::vif(mod)
#os VIFs para tempo e custo são iguais a 1.148956,
#o que indica que não há evidência de multicolinearidade entre essas variáveis.
#Ou seja, a relação entre a escolha e o tempo e custo são independentes.



# Adicionar probabilidades ajustadas ao conjunto de dados

dados_br_long <- dados_br_long %>%
  left_join(fitted_probs_df, by = c("id", "choice_id")) %>%
  rename(prob_mlogit = prob)

dados_br_long = dados_br_long %>%
  mutate(mfx_tempo = coef(model_mlogit)[2] * prob_mlogit * (1 - prob_mlogit),
         mfx_custo = coef(model_mlogit)[3] * prob_mlogit * (1 - prob_mlogit))


# Calculando as probabilidades de escolha ajustadas
fitted_probs = fitted(model_mlogit, type = "probabilities")

dados_br_long = dados_br_long %>%
  mutate(choice = as.integer(alternative == escolha),
         choice_id = as.integer(alternative))

#BEM AQUI

# Calcula efeitos marginais para o modelo MNL
marg_eff_multinom_probit = margins(fitted_probs)
summary(marg_eff_multinom_probit )



# Converte o objeto matrix em um data.frame
fitted_probs_df = as.data.frame(fitted_probs) %>%
  rownames_to_column(var = "id") %>%
  mutate(id = as.integer(id)) %>%
  pivot_longer(cols = -id, names_to = "choice_id", values_to = "prob_mlogit") %>%
  mutate(choice_id = ifelse(choice_id == "0", 1, 2))


# Calcula efeitos marginais para as variáveis tempo e custo
dados_br_long = dados_br_long %>%
  mutate(mfx_tempo = coef(model_mlogit)[2] * prob_mlogit * (1 - prob_mlogit),
         mfx_custo = coef(model_mlogit)[3] * prob_mlogit * (1 - prob_mlogit))


# Juntando as probabilidades ajustadas ao conjunto de dados
dados_br_long = dados_br_long %>%
  left_join(fitted_probs_df, by = c("id", "choice_id"))

# Visualizar densidade dos efeitos marginais

dados_br_long = dados_br_long %>%
  filter(is.finite(mfx_tempo) & is.finite(mfx_custo))

dados_br_long %>%
  pivot_longer(cols = c("mfx_tempo", "mfx_custo"), names_to = "variavel", values_to = "efeito_marginal") %>%
  ggplot(aes(x = efeito_marginal)) +
  geom_density() +
  facet_wrap(~ variavel, scales = "free_x", labeller = as_labeller(c(mfx_tempo = "Efeito marginal do tempo",
                                                                    mfx_custo = "Efeito marginal do custo"))) +
  xlab('Efeito Marginal') +
  ylab('Densidade')


#Testes de hipóteses (testes t):
# Ajuste o modelo multinomial
modelo_multinom = multinom(choice ~ tempo + custo, data = dados_br_long)
# Sumário do modelo
modelo_sumario = summary(modelo_multinom)
# Extraia os valores z do sumário do modelo
valores_z = modelo_sumario$coefficients / modelo_sumario$standard.errors
# Calcule os p-valores usando a distribuição normal padrão
p_valores = 2 * (1 - pnorm(abs(valores_z)))
# Mostre os p-valores
print(p_valores)


# Pseudo R-quadrado
# Modelo nulo (apenas intercepto)
modelo_nulo = multinom(choice ~ 1, data = dados_br_long)
# Calcule a estatística de verossimilhança para o modelo ajustado e o modelo nulo
loglik_modelo = logLik(modelo_multinom)
loglik_nulo = logLik(modelo_nulo)
# Calcule o pseudo R² (McFadden)
pseudo_R2 = 1 - (loglik_modelo / loglik_nulo)
# Mostre o pseudo R²
print(pseudo_R2)


# Critério de informação de Akaike (AIC) e critério de informação bayesiano (BIC)
AIC(modelo_multinom)
BIC(modelo_multinom)


#Análise de resíduos:

# Resíduos do modelo
residuos = residuals(modelo_multinom, type = "pearson")

# Criar um data.frame com os valores ajustados e resíduos
residuos_vs_ajustados = data.frame(
  ajustados = fitted(modelo_multinom),
  residuos = residuos
)

# Criar o gráfico de dispersão usando ggplot2
grafico_residuos = ggplot(data = residuos_vs_ajustados, aes(x = ajustados, y = residuos)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Grafico de Dispersao dos Residuos",
    x = "Residuos",
    y = "Valores Ajustados"
  ) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "gray"),
        axis.title = element_text(face = "bold", size = 12),
        axis.text = element_text(size = 10)) +
  scale_x_reverse() +
  ggtitle("Grafico de Dispersao dos Residuos") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))

# Exibir o gráfico
print(grafico_residuos)




# Ajuste FINAL do modelo multinomial probit
# (supondo que você já ajustou o modelo e ele é chamado de modelo_mnl)

# Obtenha os coeficientes do modelo
coeficientes = coef(model_mnl)

# Calcule as razões de chance (odds ratios) exponenciando os coeficientes
odds_ratios = exp(coeficientes)

# Obtenha os intervalos de confiança de 95% para os coeficientes
conf_int = confint(model_mnl, level = 0.95)

# Exponencie os limites inferior e superior para obter os intervalos de confiança das razões de chance
conf_int_odds_ratios = exp(conf_int)

# Apresente os resultados em um data.frame
resultado = data.frame(Odds_Ratio = odds_ratios, Lower_CI = conf_int_odds_ratios[, 1],
                       Upper_CI = conf_int_odds_ratios[, 2])
resultado




#---------------------------------------------------------
# Obter coeficientes, erros padrão, valores-z e p-valores para o modelo multinomial logit
coeficientes_mlogit = coef(summary(model_mnl))
std_error_mlogit = summary(model_mlogit)$standard.errors
z_values_mlogit = coeficientes_mlogit / std_error_mlogit
p_values_mlogit = 2 * (1 - pnorm(abs(z_values_mlogit)))

# Obter coeficientes, erros padrão, valores-t e p-valores para o modelo linear
coef_mod = summary(mod)$coefficients[, "Estimate"]
std_error_mod = summary(mod)$coefficients[, "Std. Error"]
t_values_mod = summary(mod)$coefficients[, "t value"]
p_values_mod = summary(mod)$coefficients[, "Pr(>|t|)"]

# Combinar resultados em um único data.frame
results_combined <- data.frame(
  Model = rep(c("Modelo Multinomial Logit", "Modelo Linear"), each = 2),
  Variable = rep(c("tempo", "custo"), 2),
  Coefficient = c(coeficientes_mlogit[2], coeficientes_mlogit[3], coef_mod[2], coef_mod[3]),
  Std_Error = c(std_error_mlogit[2], std_error_mlogit[3], std_error_mod[2], std_error_mod[3]),
  z_t_Value = c(z_values_mlogit[2], z_values_mlogit[3], t_values_mod[2], t_values_mod[3]),
  p_Value = c(p_values_mlogit[2], p_values_mlogit[3], p_values_mod[2], p_values_mod[3])
)
results_combined


# Criar a tabela LaTeX
latex_table <- results_combined %>%
  kable("latex", booktabs = TRUE, align = "c") %>%
  kable_styling(latex_options = "scale_down", position = "center") %>%
  add_header_above(c(" " = 1, "Modelo Multinomial Logit" = 2, "Modelo Linear" = 2), align = "c") %>%
  column_spec(1, bold = TRUE, width = "2cm") %>%
  column_spec(2:5, width = "1.5cm")

# Salvar a tabela LaTeX em um arquivo .tex
cat(latex_table, file = "results_table.tex")




#FIM


unique(dados_br_long$choice)
response_matrix <- model.matrix(~factor(choice, levels = c("carro", "publico")) - 1, data = dados_br_long)
cat("Dimensoes de response_matrix:", dim(response_matrix), "\n")

probs_logit_matrix <- cbind(1 - probs_logit, probs_logit)
probs_probit_matrix <- cbind(1 - probs_probit, probs_probit)
probs_multinom_probit_matrix <- cbind(1 - probs_multinom_probit, probs_multinom_probit)

cat("Dimensões de probs_logit_matrix:", dim(probs_logit_matrix), "\n")
cat("Dimensões de probs_probit_matrix:", dim(probs_probit_matrix), "\n")
cat("Dimensões de probs_multinom_probit_matrix:", dim(probs_multinom_probit_matrix), "\n")

roc_logit <- evalmod(scores = probs_logit_matrix, labels = response_matrix)
roc_probit <- evalmod(scores = probs_probit_matrix, labels = response_matrix)
roc_multinom_probit <- evalmod(scores = probs_multinom_probit_matrix, labels = response_matrix)


set.seed(123)
train_idx <- sample(1:nrow(dados_br_long), 0.8 * nrow(dados_br_long))
test_idx <- setdiff(1:nrow(dados_br_long), train_idx)

response_matrix_train <- response_matrix[train_idx, ]
response_matrix_test <- response_matrix[test_idx, ]

roc_logit <- evalmod(scores = probs_logit_matrix, labels = response_matrix_test)
roc_probit <- evalmod(scores = probs_probit_matrix, labels = response_matrix_test)
roc_multinom_probit <- evalmod(scores = probs_multinom_probit_matrix, labels = response_matrix_test)









# Criar um dataframe com os efeitos marginais dos modelos Logit e Probit
marginal_effects_df <- data.frame(
  Factor = rownames(marginal_effects_logit$coef),
  Logit_AME = marginal_effects_logit$AME,
  Probit_AME = marginal_effects$AME
)

# Converter o dataframe de formato largo para formato longo
marginal_effects_long <- reshape2::melt(marginal_effects_df, id.vars = "Factor", variable.name = "Model",
                                        value.name = "AME")

# Criar o gráfico de barras
bar_plot <- ggplot(marginal_effects_long, aes(x = Factor, y = AME, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  theme_economist() +
  labs(
    title = "Comparação dos Efeitos Marginais dos Modelos Logit e Probit",
    x = "Fatores",
    y = "Efeitos Marginais",
    fill = "Modelo"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Exibir o gráfico
print(bar_plot)



