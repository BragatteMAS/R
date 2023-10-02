#########################################
## chama bibliotecas
pacman::p_load(tidyverse, vroom)

## 24_resp
combined <-
  # vroom("~/Documents/23_respat/results/combined.tsv")
  vroom("~/Documents/20230927_relatorio24_resp/respat/results/combined.tsv")

## test_result by labs
table(combined$SC2_test_result, combined$lab_id)

## test_kit by labs
table(combined$test_kit, combined$lab_id)

## Test by state
table(combined$state, combined$SC2_test_result)

## test_lab by last week
last_week <- combined[combined$epiweek == "2023-09-23", ]
table(last_week$lab_id, last_week$epiweek)

## Pos Neg by last week
table(last_week$lab_id, last_week$SC2_test_result)

## state by last week
table(last_week$state_code, last_week$epiweek)

## save new combined
write.table(combined, file = "combined_cache.tsv", sep = "\t", row.names = FALSE, quote = FALSE)

#########################################

##Extractiong lab_id == HILAB
hilab <- vroom("~/Documents/23_respat/results/combined.tsv")

## Filtrar o dataframe com base nas condições
hilab_uniq <-
  unique(hilab$Exame[hilab$Exame == "Covid-19 Antígeno"], hilab$`Código Da Cápsula`)

## select Covid
df_hi <- hilab |>
  filter(hilab$Exame == 'Covid-19 Antígeno')
## select Flu A
df_hi_flu <- hilab |>
  filter(hilab$Exame == 'Influenza A')
## select FLu B
df_hi_flu <- hilab |>
  filter(hilab$Exame == 'Influenza B')

# Selecionar as colunas desejadas
sel <- hilab[, c("Código Da Cápsula", "Exame", "Paciente")]

sel_cov <- df_hi[, c("Código Da Cápsula", "Exame", "Paciente")]

sel_fluA <- df_hi_flu[, c("Código Da Cápsula", "Exame", "Paciente")]

sel_fluB <- df_hi_flu[, c("Código Da Cápsula", "Exame", "Paciente")]


# Obter os valores únicos das colunas selecionadas
valores_unicos <- unique(sel)

val_uniq_cov <- unique(sel_cov)

val_uniq_flua <- unique(sel_fluA)

val_uniq_fluB <- unique(sel_fluB)

#########################################

## Replace names
# combined$state[combined$state == "RIO DE JANEIRO"] <- "Rio de Janeiro"

## Replace Positive for Pos
# combined$SC2_test_result[combined$SC2_test_result == "Positive"] <- "Pos"

######################################### 
# Report

# Leia o arquivo
data <- vroom("~/Documents/20230927_relatorio24_resp/respat/results/country/combined_matrix_country_posneg_allpat_weeks.tsv")

# Filtra os dados do último ano com base na última semana disponível
last_week <- max(data$epi_week)
data_last_year <- data %>% filter(epi_week >= (last_week - 52))

# Soma os casos Pos e Neg
total_cases <- sum(data_last_year$Pos, data_last_year$Neg)

# Calcula a taxa de casos positivos
positive_rate <- sum(data_last_year$Pos) / total_cases * 100

# Exibe a taxa de casos positivos
print(paste("Taxa de casos positivos no último ano:", round(positive_rate, 2), "%"))
