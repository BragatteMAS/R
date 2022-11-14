# Datasus extraction
# R 4.2.1
#@BragatteMAS
#########################################

# Microdados SUS
#install.packages("devtools")
#install.packages("remotes")
#remotes::install_github("rfsaldanha/microdatasus")

# Libraries
packs = c("read.dbc", "microdatasus", "geobr", "sf")
#lapply(packs, install.packages, character.only = TRUE)
lapply(packs, require, character.only = TRUE)

# Download DataSUS 
dados_2 <- fetch_datasus(
  year_start = 2020,
  month_start = 1,
  year_end = 2022,
  month_end = 7,
  uf = "SP",
  information_system = "SINAN-ZIKA"
) #"SIM-DO"

# Save Data
write.csv(dados, file = "SINAN_DENGUE.csv")

#########################################
# Ref
#https://github.com/rfsaldanha/microdatasus
#https://datasus.saude.gov.br/transferencia-de-arquivos/#

