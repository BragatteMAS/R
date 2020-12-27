# Description
#Ref.:

#install
packs = c("tidyr", "geobr")
lapply(packs, install.packages, character.only = TRUE)

#libraries
lapply(packs, require, character.only = TRUE)

#file
df <- read.csv("", header = TRUE, stringsAsFactors = FALSE)
#RDS
saveRDS(df, ".rds") #save as RDS
df2 <- readRDS(".rds") #read as RDS
