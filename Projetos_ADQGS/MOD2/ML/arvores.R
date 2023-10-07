## Arvores de decisão em machine learning
## R 4.3.0
## Bragatte

## Exemplo Bertolde
## Instalar e carregar a biblioteca
pacman::p_load(party)

## verificar dados
head(iris)

## arvore de decisão
modelo <- ctree(Species ~ ., data = iris)

## desfecho modelo preditivo
table(predict(modelo), iris$Species)

## plotar arvore
plot(modelo)


#################################################
# Exemplo outra biblioteca
## Instalar e carregar a biblioteca
pacman::p_load(rpart,rpart.plot)

## Carregar os dados
data(iris)

## Criar a árvore de decisão
arvore <- rpart(Species ~ ., data = iris)

## Imprimir a árvore de decisão
printcp(arvore)

## Plotar a árvore de decisão
plot(arvore, uniform = TRUE, main = "Árvore de Decisão")
text(arvore,
     use.n = TRUE,
     all = TRUE,
     cex = .8)

## Explicação
### Este código cria uma Árvore de Decisão que nos ajuda a decidir a espécie de uma flor Iris com base em suas características.

## Plotar a árvore de decisão usando rpart.plot
rpart.plot(arvore, type=4, extra=101)


