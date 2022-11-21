---
title: "Projeto Corr"
author: "@BragatteMAS"
format: html
editor: visual
version: 4.2.1
---

# 1. Documentar
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, collapse = TRUE, error = FALSE, warning = FALSE, message = FALSE)
```

Para cada novo script criado pode seguir estes passos:

    1.  Nomear meu arquivo de modo fácil para lembrar
2.  Instalar pacotes necessários para tarefa\
`install.packages("*nome pacote*")`
3.  Carregar as bibliotecas dos pacotes\
`library("*nome pacote*")`
4.  Definir ambiente de trabalho ou criar projeto\
`setwd()`
5.  Ler os dados
6.  Analisar...um univeRso de possibilidades\
Sript de exemplo com dados reais para...

# 2. Instalar pacotes

**Descomentar** para instalar os pacotes.

Descomentar é tirar o `##` da frente\
`##` duas hastags ou forquilhas quando forem comentários ou saídas

`#` uma hastag ou forquila quando for código

```{r}
#install.packages("readr")
#install.packages("tidytable")
```

# 3. Carregar bibliotecas

```{r}
library(readr)
library(tidytable)
```

# 4. Definir local e salvar arquivo

Salvar local onde terão os arquivos para meu projeto usar

**COLOCAR O NOME DA PASTA DO SEU COMPUTADOR aqui**

    ```{r}
pasta_covid <-"/Users/sabrinaguedes/Documents/Bragatte/"
```

Definir local para buscar os dados

```{r}
setwd(pasta_covid)
```

# 5. Ler os dados

```{r}
covid_mundo <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")

#covid_mundo
```

# 6. Analisar Como calcular:

Fazendo uma tabela menor para o exercício:

    ```{r results='asis'}
covid_br <- covid_mundo %>%                                 ##qual arquivo
    select. (location, date, new_cases, new_deaths) %>%       ##quais colunas eu quero
    filter. (location == "Brazil",
             between(date, as.Date('2022-01-01'),
                     as.Date('2022-06-01')))                  ##quais filtros quero aplicar

## substituindo valores NA por 0
covid_br[is.na(covid_br)] <- 0

#covid_br
```

## Média

O resultado da soma de todos os valores ou elementos, divido pelo número de unidades.

Simplificando: se considerarmos uma coluna seria a soma dos números em cada linha, dividido pelo número de linhas.

```{r results='asis'}
## calculando média
mean (covid_br$new_cases)

## atribuindo nome para a média de casos de covid primeiro semestre de 2022
media_casos <- mean (covid_br$new_cases)
media_casos
## arredondamento
round(media_casos)

## atribuindo nome para a média de casos de covid primeiro semestre de 2022
media_obitos <- mean (covid_br$new_deaths)
media_obitos
## arredondamento
round(media_obitos)
```

## Mediana

Valor central da distribuição, por exemplo se tivermos uma coluna com 20 linhas, seria o valor da linha número 10, desde que a coluna esteja com os valores ordenados (1,2,3,4,5,6,7,8,9,**10**,11,12,13,14,15,16,17,18,19,20) .

```{r results='asis'}
median(covid_br$new_cases)

## atribuindo nome para a mediana de casos de covid primeiro semestre de 2022
mediana_casos <- median (covid_br$new_cases)
mediana_casos

## atribuindo nome para a mediana de casos de covid primeiro semestre de 2022
mediana_obitos <- median (covid_br$new_deaths)
mediana_obitos

## deixar número arredondado
round (mediana_obitos)
```

## Moda

Valor que mais se repete na distribuição, por exemplo uma coluna do meu banco de dados.

A moda não tem um pacote padrão no R como média e mediana, então usamos outro pacote. Lembrem-se se for um cálculo "comum", alguém já deve ter feito um pacote.

```{r results='asis'}
## instalar pacote para calcular moda - descomentar para instalar
# install.packages ("modeest")
## chamar biblioteca
library (modeest)
```

```{r results='asis'}
mlv(covid_br$new_deaths)
modeest::chisqMode(covid_br$new_deaths)
```

## Referência

[Introdução a R para Visualização e Apresentação de Dados](http://sillasgonzaga.com/material/curso_visualizacao/apresentacao-de-dados-com-relatorios.html)
