require(ggplot2)

data_pib <- data.frame(read.csv2('PIB_SAUDE_ES.csv'))

pib <- ggplot(
  data_pib,
  aes(
    as.factor(ANO),
    PIB_ASPS_T,
    group = 1,
    label = PIB_ASPS_T,
    col = PIB_ASPS_T,
    fill = PIB_ASPS_T
  )
) +
  
  geom_col() +
  geom_label(col = 'black') +
  labs(
    title = 'Evolução da Relação % PIB aplicação em saúde pública no Estado do Espírito/BR Santo',
    subtitle = 'Série Histórica - 2011-2021',
    y = '',
    x = '',
    caption = 'Fonte: Indicador de PIB trimestral do Espírito Santo - Instituto Jones dos Santos Neves e SIOPS/MS. Twitter: dr_nesio'
  ) +
  theme_update() +
  theme(legend.position = "none") +
  geom_line(aes(y = c(ASPS_ES_EE / 350000000)), size = 1, col = 'pink')

pib

pib + annotate("text",
           x = c("2011","2019","2021"),
           y = c("3,14%","4,10%","4,80%"),
           label = c("R$1,7bi","R$2,8bi","3,7bi")
         )
