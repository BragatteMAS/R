# Editar tabelas no R
#R 4.2.1
#@BragatteMAS

## bibliotecas
pacotes <- c("DataEditR", "editData")
#lapply(pacotes, install.packages,character.only = TRUE) #caso ainda não tenha instalado tirar a # do inicio
lapply(pacotes, require, character.only = TRUE)

## Documentação na internet
browseVignettes('DataEditR')

#### edição básica no R
print(mtcars)
edit(mtcars)

novo_mpg <- edit(mtcars) #salvando em nova tabela

## Salvando alterações no arquivo existente em .R
data_edit (mtcars, code = TRUE)

## Salvando alterações em novo arquivo .R
data_edit (mtcars, code = "novo_mpg.R")

## Salvando alterações em novo arquivo .csv
data_edit (mtcars, save_as = "mpg_editado.csv")

### Pacote alternativo para editar rápido dentro do R
editData(iris)