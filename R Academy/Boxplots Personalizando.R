# Boxplots Personalizando
# R 4.2.0
# Refactor @BragatteMAS

## Library
data("iris")

## edit data 
edit(iris)

## Check
summary(iris)

# Boxplot
grafico.box <- boxplot(iris$Petal.Length ~ iris$Species, range = 0)

## which class
class(grafico.box)

## help
?boxplot

## list
grafico.box

#check values of interest
percentis.setosa = quantile(iris$Petal.Length[iris$Species=="setosa"], probs = c(0.1, 0.9))

percentis.versicolor = quantile(iris$Petal.Length[iris$Species=="versicolor"], probs = c(0.1, 0.9))

percentis.virginica = quantile(iris$Petal.Length[iris$Species=="virginica"], probs = c(0.1, 0.9))

# obj save inside the graph
grafico.box$stats[1,1] <- percentis.setosa[1]
grafico.box$stats[5,1] <- percentis.setosa[2]

grafico.box$stats[1,2] <- percentis.versicolor[1]
grafico.box$stats[5,2] <- percentis.versicolor[2]

grafico.box$stats[1,3] <- percentis.virginica[1]
grafico.box$stats[5,3] <- percentis.virginica[2]

grafico.box

### bxp

bxp(grafico.box, xlab="Espécie", ylab="Comprimento da pétala",)

install.packages("installr")
library(installr)
