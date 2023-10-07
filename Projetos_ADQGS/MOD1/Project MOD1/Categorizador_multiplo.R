## Análise de Dados para a Qualificação da Gestão na Saúde - Módulo 2
## "Categorizador" de dados, conjunto de dados de 'câncer de pâncreas'
## Bragatte
## Script criado em R 4.2.3

## Bibliotecas
pacman::p_load(tidyverse, vroom)

## Define local da pasta
setwd(dir ="Avaliação/Script e dados individualizados/arquivos_n/")

# Lista todos os arquivos que terminam com ".csv" na pasta de trabalho
arquivos <- list.files(pattern = "\\.csv$")

# Loop para iterar sobre todos os arquivos
for (arquivo in arquivos) {
    # Lê o arquivo
    dados <- read.csv(arquivo, header = TRUE)

    # Transformando as colunas "sexo", "cafe", "cigarro" e "cancerpancreas" em fatores
    dados <- dados %>%
        mutate(sexo = if_else(sexo == 0, "Masculino", "Feminino"),
               cafe = if_else(cafe == 0, "Não", "Sim"),
               cigarro = if_else(cigarro == 0, "Não", "Sim"),
               cancerpancreas = if_else(cancerpancreas == 0, "Não", "Sim"))

    # # Transformando as colunas "idade" e "idademdecenios" em inteiros
    # dados <- dados %>%
    #     mutate(idade = as.integer(idade),
    #            idademdecenios = as.integer(idademdecenios))

    # Salva o arquivo transformado com o sufixo "_sujo.csv"
    nome_saida <- sub(".csv", ".csv", arquivo)
    write.csv(dados, nome_saida, row.names = FALSE)

    # Imprime uma mensagem informando o nome do arquivo transformado
    cat(paste("Arquivo transformado salvo em:", nome_saida, "\n"))
}
