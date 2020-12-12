#Using readRDS
#Native R data type "RDS" (faster loading times)
#Ref.: https://www.notion.so/bragatte/readRDS-8c9564245f474232af9fabdf4f2bc041

#original_dataset <- read.csv("filename.csv") #read original
df <- read.csv("https://query.data.world/s/bwkvpbwy2ae3bb777phhzhb6jivlbm", header=TRUE, stringsAsFactors=FALSE)
#
saveRDS(df, "df_test.rds") #save as RDS
df2 <- readRDS("df_test.rds") #read as RDS

#install
packs=c("microbenchmark", "ggplot2") 
lapply(packs, install.packages, character.only=TRUE)
#libraries
lapply(packs, require, character.only=TRUE)

microbenchmark::microbenchmark({read.csv("https://query.data.world/s/bwkvpbwy2ae3bb777phhzhb6jivlbm", header=TRUE, stringsAsFactors=FALSE)},
                               {readRDS("df_test.rds")}, times = 10L)

ggplot2::autoplot(
    microbenchmark::microbenchmark(
        {read.csv("https://query.data.world/s/bwkvpbwy2ae3bb777phhzhb6jivlbm", header=TRUE, stringsAsFactors=FALSE)},
        {readRDS("df_test.rds")}, times = 10L))