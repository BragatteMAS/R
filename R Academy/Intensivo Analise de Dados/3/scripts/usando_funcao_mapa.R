library('sf')

setwd("/Users/sabrinaguedes/Documents/Bragatte/GitHub/R/R Academy/Intensivo Analise de Dados/3/")

source("scripts/mapa_relevo.R")

meu_mapa('SP')

# i variávvel combine são vetores
for (i in c('ES','SP','RS')) {
  # comandos para automatizar
  ## exemplo com print
  ### print(i)
  meu_mapa(i)
  ggsave(filename = paste0('figuras/',i,'.png'))
}
