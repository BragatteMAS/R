# ID: 202009182242
# R: 4.2.1
#title: Arqgis
# Refactor: @BragatteMAS

##Install and Library
packs  <- c("readr","readxl", "dplyr","tidytable", "ggplot2", "geobr", "writexl")
lapply(packs, install.packages, character.only = TRUE)
lapply(packs, require, character.only = TRUE)

## Define path
setwd("/Users/sabrinaguedes/Documents/Bragatte/GitHub/R/R Academy/Intensivo Analise de Dados/maps/")

## Read data
### exemple COVID
owid <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv", h = T)

owid_br <- owid %>% 
  select.(date,new_cases,new_deaths, population, location) %>% 
  filter.(owid$date >= '2022-09', owid$location == "Brazil")

### example gas BR
### "https://www.gov.br/anp/pt-br/centrais-de-conteudo/dados-abertos/arquivos/shpc/dsas/ca/ca-2021-02.csv"
gas_br <- read.csv(file ="ca-2021-02.csv",header = TRUE, sep = ";", dec = ",", encoding = "ISO-8859-2")

gas_dados <- gas_br %>% 
  select.(Regiao_Sigla, CNPJ_da_Revenda,Valor_de_Venda)

write_xlsx(gas_dados, path = "maps/gas_dados.xlsx")

ggplot(gas_dados,aes(x = Regiao_Sigla, y = Valor_de_Venda))+
  geom_boxplot()+
  theme_minimal()

#########################################
### Map analyses | geobr
### state data
estados_su2 <- read_state() %>% tidytable::filter.(name_region == "Sudeste")

ggplot(estados_su)+
  geom_sf()+
  geom_sf(aes(fill=code_state))

