# Dados densidade Gustavo contato Nésio
# R 4.2.1
# @BragatteMAS

#Instalando pacotes
packs = c("tidyverse", "Amelia")
lapply(packs, install.packages, character.only = TRUE)
lapply(packs, require, character.only = TRUE)

##importanto tabela
geral <- read_xlsx("C:/Users/bragatte/Downloads/geral.xlsx")
view(geral)

## Checando dado
summary(geral)

### chamando biblioteca especifica
library(Amelia)
missmap(geral)

### chamando biblioteca especifica
library(dplyr)

## retirando linhas com dados incompletos
mini_geral <- slice(geral, 1:12)
view(mini_geral)

## Cálculos T1MOEs
T1MOE_mean <- mean (mini_geral$T1MOE) #média
T1MOE_var <- var (mini_geral$T1MOE) #variância
T1MOE_sd <- sd (mini_geral$T1MOE) #desvio padrão
T1MOE_coe <- (sd(mini_geral$T1MOE)/mean(mini_geral$T1MOE))*100 #coeficiente de variação
T1MOE_quanti <- quantile(mini_geral$T1MOE) #quartis sobre mediana

## Cálculos T2MOEs
T2MOE_mean <- mean (mini_geral$T2MOE) #média
T2MOE_var <- var (mini_geral$T2MOE) #variância
T2MOE_sd <- sd (mini_geral$T2MOE) #desvio padrão
T2MOE_coe <- (sd(mini_geral$T2MOE)/mean(mini_geral$T2MOE))*100 #coeficiente de variação
T2MOE_quanti <- quantile(mini_geral$T2MOE) #quartis sobre mediana

## Cálculo T3MOEs
T3MOE_mean <- mean (mini_geral$T3MOE) #média
T3MOE_var <- var (mini_geral$T3MOE) #variância
T3MOE_sd <- sd (mini_geral$T3MOE) #desvio padrão
T3MOE_coe <- (sd(mini_geral$T3MOE)/mean(mini_geral$T3MOE))*100 #coeficiente de variação
T3MOE_quanti <- quantile(mini_geral$T3MOE) #quartis sobre mediana

## Cálculos T4MOEs
T4MOE_mean <- mean (mini_geral$T4MOE) #média
T4MOE_var <- var (mini_geral$T4MOE) #variância
T4MOE_sd <- sd (mini_geral$T4MOE) #desvio padrão
T4MOE_coe <- (sd(mini_geral$T4MOE)/mean(mini_geral$T4MOE))*100 #coeficiente de variação
T4MOE_quanti <- quantile(mini_geral$T4MOE) #quartis sobre mediana

## Cálculos T5MOEs
T5MOE_mean <- mean (mini_geral$T5MOE) #média
T5MOE_var <- var (mini_geral$T5MOE) #variância
T5MOE_sd <- sd (mini_geral$T5MOE) #desvio padrão
T5MOE_coe <- (sd(mini_geral$T5MOE)/mean(mini_geral$T5MOE))*100 #coeficiente de variação
T5MOE_quanti <- quantile(mini_geral$T5MOE) #quartis sobre mediana

## Boxplots TMOES
boxplot(T1MOE_quanti)
boxplot(T2MOE_quanti)
boxplot(T3MOE_quanti)
boxplot(T4MOE_quanti)
boxplot(T5MOE_quanti)

all_boxplots <- boxplot(T1MOE_quanti,T2MOE_quanti,T3MOE_quanti,T4MOE_quanti,T5MOE_quanti)

## Calculos de regressão
regressão_T1 <- lm(mini_geral$T1~ mini_geral$T1MOE,data=mini_geral) 
regressão_T2 <- lm(mini_geral$T2~ mini_geral$T2MOE,data=mini_geral) 
regressão_T3 <- lm(mini_geral$T3~ mini_geral$T3MOE,data=mini_geral) 
regressão_T4 <- lm(mini_geral$T4~ mini_geral$T4MOE,data=mini_geral) 
regressão_T5 <- lm(mini_geral$T5~ mini_geral$T5MOE,data=mini_geral) 
