#########################################
## chama bibliotecas
pacman::p_load(tidyverse, vroom)

## 22_resp
combined22 <-
  vroom("~/Documents/22_respat/results/combined.tsv")

## test_result by labs
table(combined22$SC2_test_result, combined22$lab_id)

## test_kit by labs
table(combined22$test_kit, combined22$lab_id)

## Replace names
combined22$SC2_test_result[combined22$SC2_test_result == "Negative"] <- "Neg"
combined22$SC2_test_result[combined22$SC2_test_result == "Positive"] <- "Pos"

## save new combined
write.table(combined22, file = "output.tsv", sep = "\t", row.names = FALSE, quote = FALSE)

#########################################

##Extractiong lab_id == HILAB
hilab <- vroom("~/Documents/22_respat/results/combined.tsv")

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