rm(list = ls())
## Bibliotecas
library(geobr)
library(ggplot2)
library(httpgd)

# gerar gráficos na internet
hgd()
hgd_browse()

## Dados
estados <- read_state(code_state = "all")

## sf biblioteca para o gráfico do Brasil
## gerar gráficos dos estados original
## dado
ggplot(estados) +
    ## mapa e geometria
    geom_sf(aes(fill = code_state))

#########
## usando o import dataset para corrigir os dados de óbitos infantis do ES

library(readr)
obitos_infantis <- read_delim(
    "A172242189_28_143_208.csv",
    delim = ";",
    escape_double = FALSE,
    locale = locale(encoding = "WINDOWS-1252"),
    skip = 3
)
