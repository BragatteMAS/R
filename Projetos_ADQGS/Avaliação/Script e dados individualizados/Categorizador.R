## Análise de Dados para a Qualificação da Gestão na Saúde - Módulo 2
## "Categorizador" de dados, conjunto de dados de 'câncer de pâncreas'
## Bragatte
## Script criado em R 4.2.3

## Bibliotecas
pacman::p_load(tidyverse, vroom)

## Ler os dados
dados <-
    vroom("Avaliação/Script e dados individualizados/dados.txt")

## Substituindo os valores nas colunas|Transformar "sexo", "cafe", "cigarro" e "cancerpancreas" em fatores
dados_cancer <- dados |>
    mutate(
        idade = as.integer(idade),
        idademdecenios = as.integer(idademdecenios),
        sexo = ifelse(sexo == 0, "Masculino", "Feminino"),
        cafe = ifelse(cafe == 0, "Não", "Sim"),
        cigarro = ifelse(cigarro == 0, "Não", "Sim"),
        cancerpancreas = ifelse(cancerpancreas == 0, "Não", "Sim")
    )
## verificar dados
glimpse(dados_cancer)

# Salvar arquivo CSV
write.csv(dados_cancer, "Avaliação/Script e dados individualizados/dados_transformados.csv", row.names = FALSE)
