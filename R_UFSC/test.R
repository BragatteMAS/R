## Carregar o pacote necessário
library(foreign)

## Caminho para o arquivo .dbf (altere isso para o caminho do seu arquivo)
filename <- "R_UFSC/NINDINET.dbf"

## Ler o arquivo .dbf
data <- read.dbf(filename)

## Exibir as primeiras linhas do conjunto de dados
print(head(data))

## Descrição básica do conjunto de dados
print(summary(data))

## Supondo que 'variavel' é uma coluna no conjunto de dados, aqui está um histograma
hist(data$CLASSI_FIN, main="Histograma da variavel", xlab="Valores da class_final")

## Supondo que 'variavel1' e 'variavel2' são colunas no conjunto de dados, aqui está um scatter plot
plot(data$variavel1, data$variavel2, main="Scatter plot de variavel1 vs variavel2", xlab="variavel1", ylab="variavel2")

## Salvar o conjunto de dados em um arquivo .csv
write.csv(data, file = "R_UFSC//NINDINET.csv")


## Contar o número de valores faltantes por coluna
print(summarize_all(data, funs(sum(is.na(.)))))

## Exibir a estrutura do conjunto de dados
print(str(data))

## Se 'variavel' for uma coluna numérica no conjunto de dados, exibir um histograma
ggplot(data, aes(x=data$CLASSI_FIN)) +
  geom_histogram(binwidth=10, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  theme_minimal()

## Se 'variavel1' e 'variavel2' forem colunas no conjunto de dados, exibir um gráfico de dispersão
ggplot(data, aes(x=variavel1, y=variavel2)) +
  geom_point() +
  theme_minimal()