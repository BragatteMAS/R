#Projeto final Módulo 01 Curso ADQGS
#Script por Isaac Schrarstzhaupt
#Script criado na versão 4.2.1 do R

#Ler os dados originais
vac_ES <- read.csv("Proposta 02 - Desigualdade vacinal/processed_ES.csv",
                   header=T,
                   stringsAsFactors = T)

#Filtrar apenas a terceira dose
vac_ES_D3 <- vac_ES[which(vac_ES$dose=="3"),]

#Fazer a soma das terceiras doses aplicadas por municipio
vac_ES_D3_soma <- tapply(vac_ES_D3$count,vac_ES_D3$city,sum)

#Transformar o vetor da soma em um data frame
vac_ES_D3_soma_df <- as.data.frame(vac_ES_D3_soma)

#Recuperar a coluna do municipio
vac_ES_D3_soma_df$municipio <- rownames(vac_ES_D3_soma)

#Fazer o agrupamento da populacaoo por municipio
vac_ES_D3_pop <- tapply(vac_ES_D3$pop2021,vac_ES_D3$city,max)

#Transformar o vetor da população em um data frame
vac_ES_D3_pop_df <- as.data.frame(vac_ES_D3_pop)

#Recuperar a coluna do municipio para o objeto da populacao
vac_ES_D3_pop_df$municipio <- rownames(vac_ES_D3_pop)

#Juntar os dataframes das doses e da populacao em um so
vac_ES_D3_ok <- cbind(vac_ES_D3_soma_df,vac_ES_D3_pop_df$vac_ES_D3_pop)

#Ajustar os nomes das colunas
colnames(vac_ES_D3_ok)[1]="dosesaplicadas"
colnames(vac_ES_D3_ok)[2]="municipio"
colnames(vac_ES_D3_ok)[3]="populacao"

#Reordenar as colunas para facilitar a visualizacao
vac_ES_D3_ok <- vac_ES_D3_ok[,c(2,1,3)]

#Calcular a cobertura vacinal
vac_ES_D3_ok$cobertura <- vac_ES_D3_ok$dosesaplicadas/vac_ES_D3_ok$populacao

#Multiplicar a cobertura por 100 para facilitar a visualizacao
vac_ES_D3_ok$cobertura <- vac_ES_D3_ok$cobertura*100

#Arredondar a cobertura vacinal para dois digitos depois da virgula
vac_ES_D3_ok$cobertura <- round(vac_ES_D3_ok$cobertura,2)

#Remover o "/ES" do final do nome de cada municipio
vac_ES_D3_ok$municipio <- substr(vac_ES_D3_ok$municipio,
                                 1,
                                 nchar(vac_ES_D3_ok$municipio)-3)

#Ordenar o objeto com doses e cobertura por ordem alfabetica de municipio
vac_ES_D3_ok <- vac_ES_D3_ok[order(vac_ES_D3_ok$municipio),]

#Montar o mapa
#Carregar o pacote sf
library(sf)

#Criar um novo objeto com o mapa em si
mapa_ES <- read_sf("Proposta 02 - Desigualdade vacinal/Limite_Municipal_2018.shp")

#Ordenar o objeto do mapa por ordem alfabetica e municipio
mapa_ES <- mapa_ES[order(mapa_ES$nome),]

#Juntar a coluna de cobertura vacinal com o objeto do mapa
mapa_ES <- cbind(mapa_ES,vac_ES_D3_ok$cobertura)

#Gerar o mapa de cobertura vacinal para ver a desigualdade (caso exista)
plot(mapa_ES["vac_ES_D3_ok.cobertura"],main = "COBERTURA VACINAL COVID-19 3a DOSE ES")
