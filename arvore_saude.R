## Arvores de decisão na saude em machine learning
## R 4.3.0
## Bragatte
#################################################

## Instalar as bibliotecas
pacman::p_load(mlbench,rpart, rpart.plot)

## Este conjunto de dados inclui informações sobre saúde de mulheres de ascendência Pima Indian
data(PimaIndiansDiabetes)

## Criar a árvore de decisão
arvore_saude <- rpart(diabetes ~ ., data = PimaIndiansDiabetes)

## Imprimir a árvore de decisão
printcp(arvore_saude)

## Plotar a árvore de decisão
plot(arvore_saude, uniform = TRUE, main = "Árvore de Decisão para Diabetes")
text(arvore_saude,
     use.n = TRUE,
     all = TRUE,
     cex = .8)

## Plotar a árvore de decisão com rpart.plot
### Esta função cria uma visualização bonita e fácil de entender da árvore de decisão
rpart.plot(arvore_saude, type=4, extra=101)

rpart.plot(arvore_saude, type=3, box.palette="auto", shadow.col="gray", nn=TRUE)