# Análise exploratório de Dados
##R 4.2.1
##@BragatteMAS

# Bibliotecas
pacotes <- c("DataExplorer", "nycflights13")
#lapply(pacotes, install.packages, character.only = TRUE) # se necessário instalar algum pacote
lapply(pacotes, require, character.only = TRUE)

## Dataset para explorar no exemplo da documentação
#install.packages("nycflights13")
#library(nycflights13)

## Documentação na internet com passo a passo
browseVignettes("DataExplorer")


### selecionando colunas para explorar
data_list <- list(airlines, airports, flights, planes, weather)
plot_str(data_list)

### unificando "merge" todos os dados em um só dataframe
merge_airlines <-
  merge(flights, airlines, by = "carrier", all.x = TRUE)
merge_planes <-
  merge(
    merge_airlines,
    planes,
    by = "tailnum",
    all.x = TRUE,
    suffixes = c("_flights", "_planes")
  )
merge_airports_origin <-
  merge(
    merge_planes,
    airports,
    by.x = "origin",
    by.y = "faa",
    all.x = TRUE,
    suffixes = c("_carrier", "_origin")
  )
final_data <-
  merge(
    merge_airports_origin,
    airports,
    by.x = "dest",
    by.y = "faa",
    all.x = TRUE,
    suffixes = c("_origin", "_dest")
  )

### CRIANDO RELATÓRIO COM UMA LINHA DE COMANDO
create_report(final_data)

## Explorando dados automaticamente
### Análise introdutória
introduce(final_data) # resultados no terminal
plot_intro(final_data) # resultados em gráfico ggplot

plot_missing(final_data) # dados faltantes - principal culpado coluna speed
final_data <-
  drop_columns(final_data, "speed") # removendo coluna speed

### Distribuição de Frequências
plot_bar(final_data)

### Exemplos para limpar os dados
final_data[which(final_data$manufacturer == "AIRBUS INDUSTRIE"), ]$manufacturer <-
  "AIRBUS"
final_data[which(final_data$manufacturer == "CANADAIR LTD"), ]$manufacturer <-
  "CANADAIR"
final_data[which(
  final_data$manufacturer %in% c("MCDONNELL DOUGLAS AIRCRAFT CO", "MCDONNELL DOUGLAS CORPORATION")
), ]$manufacturer <- "MCDONNELL DOUGLAS"

plot_bar(final_data$manufacturer) # visualizando os dados em barras

final_data <-
  drop_columns(final_data,
               c("dst_origin", "tzone_origin", "year_flights", "tz_origin")) # colunas com apenas um dado sendo removidas

plot_bar(final_data, with = "arr_delay")

### análise para variáveis discretas
plot_bar(final_data, by = "origin")

### histogramas para variáveis contínuas
plot_histogram(final_data)

### análise de correlação
plot_correlation(na.omit(final_data), maxcat = 5L)

plot_correlation(na.omit(final_data), type = "c")

### PCA - Análise de componentes principais
pca_df <-
  na.omit(final_data[, c("origin",
                         "dep_delay",
                         "arr_delay",
                         "air_time",
                         "year_planes",
                         "seats")])

plot_prcomp(pca_df,
            variance_cap = 0.9,
            nrow = 2L,
            ncol = 2L)

### Box Plots
##Reduce data size for demo purpose
arr_delay_df <-
  final_data[, c(
    "arr_delay",
    "month",
    "day",
    "hour",
    "minute",
    "dep_delay",
    "distance",
    "year_planes",
    "seats"
  )]

##Call boxplot function
plot_boxplot(arr_delay_df, by = "arr_delay")
