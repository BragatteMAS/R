#########################################
## chama bibliotecas
pacman::p_load(tidyverse, vroom)

## chama dados Lab
combined <-
  vroom("~/Documents/respat_new/data/20230630_combined.tsv")
combined21 <-
  vroom("~/Documents/20230620_relatorio21_resp/results/combined.tsv")
combined22_230802 <-
  vroom("~/Documents/20230802_relatorio22_resp/results/combined.tsv")
## _230809_
combined22 <-
  vroom("~/Documents/20230809_relatorio22_resp/results/combined.tsv")

## test_result by labs
table(combined22$SC2_test_result, combined22$lab_id)

## test_kit by labs
table(combined22$test_kit, combined22$lab_id)

##. HILAB
hilab <- vroom("~/Documents/respat_new/data/20230630_combined.tsv")
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