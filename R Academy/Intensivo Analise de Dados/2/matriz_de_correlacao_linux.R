################################################
## script que  plota  matriz de correlação  ####
## dos dados atmosferiocos para cidade de   ####
## Porto Alegre                             ####
##                                          ####
## desenvolvido por: Mercel Santos          ####
## contato: contato@mercelsantos.com        ####
## instagram: @mercelsantos                 ####
################################################

rm(list = ls())
library(ggplot2)
library(ggcorrplot)
library(readxl)

######################################
##    leitura de dados              ##
######################################

#Diretorio de trabalho
setwd('/home/merce/mega/work/banners/matrizCorrelacao')

dados <- read_excel("dados/esta_porto_alegre.xls")
dados.ok <- dados[,4:8] #Seleciona colunas de dados

##########################################
## Determinando a matrix de correlacao  ##
## e truncando os valores              ###
##########################################

cor.data <- round(cor(dados.ok),1)

##########################################
## Plotando e salvando os dados      #####
##########################################

ggcorrplot(cor.data, #Definindo os dados 
           hc.order = TRUE, #Organiza os dados usando agrupamento 
           type = "lower", #Mostra apenas a diagonal inferior da matriz
           legend.title = "Coeficiente de \n Correlação", #Título da legenda
           lab = TRUE, #Adiciona o coeficiente de correlação no gráfico
           lab_size = 3,#Definindo o tamanho dos rótulos 
           method="circle", #Definindo o tipo de pontos 
           colors = c("firebrick", "white", "dodgerblue4"), #Definindo as cores 
           title="Correlação entre Variáveis", #Definindo o título principal
           ggtheme=theme_bw) #Definindo o tema


ggsave(filename ='figuras/correlacao.png', #Definindo o diretório
       height =4, #Definindo a altura
       width = 5) #Definindo a largura

