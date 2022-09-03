# Histogramas com médias
# R 4.2.0
# Refactor @BragatteMAS

data("iris")

summary(iris)

#Hist com linha de média
hist(
  iris$Petal.Length,
  main = "",
  las = 1,
  ylim = c(0, 40),
  xlab = "Comprimento da pétala",
  ylab = "Frequência"
)
abline(v=mean(iris$Petal.Length), lty=2, col="red")
text(x = 4.25, y = 30, labels=mean(iris$Petal.Length), col="red")
box()

###############
#Versão por espécie
par(mfrow = c(1,3))

hist(
  iris$Petal.Length[iris$Species=="setosa"],
  main = "Setosa",
  las = 1,
  ylim = c(0, 25),
  cex = 1.0,
  xlab = "",
  ylab = "Frequência"
)
box()
abline(v=mean(iris$Petal.Lengt[iris$Species=="setosa"]), lty=2, col="red")


hist(
  iris$Petal.Length[iris$Species=="versicolor"],
  main = "Versicolor",
  las = 1,
  ylim = c(0, 25),
  cex = 1.0,
  xlab = "Comprimento",
  ylab = ""
)
box()
abline(v=mean(iris$Petal.Lengt[iris$Species=="versicolor"]), lty=2, col="red")

hist(
  iris$Petal.Length[iris$Species=="virginica"],
  main = "virginica",
  las = 1,
  ylim = c(0, 25),
  cex = 1.0,
  xlab = "",
  ylab = ""
)
box()
abline(v=mean(iris$Petal.Lengt[iris$Species=="virginica"]), lty=2, col="red")
