# Script para leitura de dados no R
# Prof Marcos Vital, para Maratona de análise de Biodiversidade da Academia do R
# Script criado em 14-12-2021

# Confira se o arquivo a ser lido está na pasta do projeto no qual você está trabalhando
# Ao usar arquivos csv, confira se o seu computador o salva com vírgula ou com ponto-e-vírgula

#Lendo o arquivo:
dados <- read.csv ("insetos.csv", dec=".", sep=",", stringsAsFactors = T, row.names = 1)

# dec determina o tipo de separador decimal
# sep determina o separador de colunas
# stringsAsFactors = T faz com que o R trate as variáveis categóricas como fatores
# row.names = 1 determina a primeira coluna como nomes das unidades amostrais

#Vamos conferir se foi tudo lido corretamente:
summary (dados)

