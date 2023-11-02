
# Leia o arquivo
allpat <- vroom("~/Documents/20231025_relatorio27_resp/results/country/combined_matrix_country_posneg_allpat_weeks.tsv")

# Filtra os dados do último ano com base na última semana disponível
last_week <- max(allpat$epi_week)
data_last_year <- allpat %>% filter(epi_week >= (last_week - 52))

# Soma os casos Pos e Neg
total_cases <- sum(data_last_year$Pos, data_last_year$Neg)

# Calcula a taxa de casos positivos
positive_rate <- sum(data_last_year$Pos) / total_cases * 100

# Exibe a taxa de casos positivos
print(paste("Taxa de casos positivos no último ano:", round(positive_rate, 2), "%"))
## Quantidade de testes por estado na última semana
# state by last week
table(last_week$state, last_week$epiweek)
