# Unindo bancos de dados
# R. 4.3.0
# Curso ADQGS Mod 3

## Carregar biblioteca padrão para ter acesso aos datasets
library(datasets)

## Carregar os datasets 'airquality' e 'ToothGrowth'
dados1 <- airquality[1:10, ]  ## Usar apenas as primeiras 10 linhas
dados2 <- ToothGrowth[1:8, ]  ## Usar apenas as primeiras 8 linhas

## Adicionar uma coluna de ID aos dois datasets
dados1$id <- 1:10  ## IDs de 1 a 10 para dados1
dados2$id <- 5:12  ## IDs de 5 a 12 para dados2

## Left Join: Todas as linhas de dados1, matching de dados2
left_join <- merge(dados1, dados2, by="id", all.x=TRUE)

## Right Join: Todas as linhas de dados2, matching de dados1
right_join <- merge(dados1, dados2, by="id", all.y=TRUE)

## Inner Join: Apenas linhas com IDs correspondentes em ambos os datasets
inner_join <- merge(dados1, dados2, by="id")

## Full Join: Todas as linhas de ambos os datasets
full_join <- merge(dados1, dados2, by="id", all=TRUE)

## Salvar os resultados em arquivos CSV
write.csv(left_join, "left_join_health.csv") ## juntar a esquerda 
write.csv(right_join, "right_join_health.csv") ## juntar a direita
write.csv(inner_join, "inner_join_health.csv") ## juntar match
write.csv(full_join, "full_join_health.csv") ## juntar tudo
