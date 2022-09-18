#Exercitando medidas de tendência central e dispersão
#Marcos V C Vital, Academia do R
#Script feito em R 4.0.3

#Pacote necessário:
library(sciplot)

#Dados (não se esqueça de verificar arquivos e pasta de trabalho):
fragmentos <- read.csv ("abelhas em fragmentos.csv", sep=",", dec=".", stringsAsFactors=T)

#Conferindo:
summary (fragmentos)


#Calculando média, variância e desvio:
mean (fragmentos$Riqueza)     #Média
var (fragmentos$Riqueza)      #Variância
sd (fragmentos$Riqueza)       #Desvio padrão

#Máximo, mínimo e a amplitude
max (fragmentos$Riqueza)      #Máximo
min (fragmentos$Riqueza)      #Mínimo
max (fragmentos$Riqueza) - min (fragmentos$Riqueza)  #Amplitude


#Coeficiente de variação:
(sd (fragmentos$Riqueza) / mean (fragmentos$Riqueza)) * 100


########################################
#Comparando o número de espécies entre ambientes:

tapply (fragmentos$Riqueza, fragmentos$Ambiente, mean)
tapply (fragmentos$Riqueza, fragmentos$Ambiente, sd)

#Colocando em um gráfico (médias com desvio padrão):
lineplot.CI (fragmentos$Ambiente, fragmentos$Riqueza, type="p", las=1, xlab="Tipo de Ambiente", ylab="Número de espécies", ci.fun= function(x) c(mean(x)-sd(x), mean(x)+sd(x)))
