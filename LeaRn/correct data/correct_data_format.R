# Correct data format
#R 4.2.1
#@BragatteMAS
#########################################
# install
packs = c("vroom","lubridate" )
#lapply(packs, install.packages, character.only = TRUE)

# libraries
lapply(packs, require, character.only = TRUE)

## sifilis file
pasta_covid <- "~/Documents/GitHub/R/LeaRn/correct data"
setwd(pasta_covid)

sif <- vroom("sisab_pe_cid_sifilis.csv")

#add day 01 and change format
sif$mes <- lubridate::as_date(paste("01", sif$mes), format = "%d_%b_%y")

write.csv(sif, file = "sif.csv")
