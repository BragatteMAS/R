

pacotes <- c("tidyverse",
             "tidytable",
             "scales",
             "readxl",
             "zoo",
             "lubridate",
             "DataEditR",
             "esquisse"
             )
#lapply(pacotes, install.packages,character.only = TRUE) #caso ainda não tenha instalado tirar a # do inicio
lapply(pacotes, require, character.only = TRUE)

#### Apenas para mostrar - ESTARÁ APAGADO no Sciropt final
library(tidyverse)
library(zoo)
library(lubridate)
library(readxl)
library(DataEditR)
library(esquisse)

covid_br <-
    covid_mundo |>                                  ##qual arquivo
    select.(location, date, new_cases, new_deaths)  |>        ##quais colunas eu quero
    filter.(location == "Brazil",
            between(date, as.Date('2022-01-01'),
                    as.Date('2022-11-19'))) |>

    mutate.(m.movel.7 = rollmean(Total, k = 7, fill = NA)) |>
    mutate.(m.movel.15 = rollmean(Total, k = 15, fill = NA))


vacina <- read_xlsx('Proposta 05 - Corr/doses_diabr.xlsx') |>
    mutate.(m.movel.7 = rollmean(Total, k = 7, fill = NA)) |>
    mutate.(m.movel.15 = rollmean(Total, k = 15, fill = NA))


vacina$Dia <- as.Date(vacina$Dia, format = "%m/%d/%Y")

esquisser(vacina)

ggplot(vacina, aes(x = Dia, y = m.movel.7, col = 'Média Móvel 7 dias')) +
    geom_hline(
        yintercept = 100000,
        color = "red",
        linetype = "dashed",
        alpha = 0.4
    ) +
    geom_hline(
        yintercept = 500000,
        color = "red",
        linetype = "dashed",
        alpha = 0.4
    ) +
    geom_hline(
        yintercept = 1000000,
        color = "red",
        linetype = "dashed",
        alpha = 0.4
    ) +
    geom_hline(
        yintercept = 1500000,
        color = "red",
        linetype = "dashed",
        alpha = 0.4
    ) +
    geom_hline(
        yintercept = 2000000,
        color = "red",
        linetype = "dashed",
        alpha = 0.4
    ) +
    geom_hline(
        yintercept = 2500000,
        color = "red",
        linetype = "dashed",
        alpha = 0.4
    ) +
    geom_vline(
        xintercept = base::as.Date("2022-04-22"),
        color = "black",
        linetype = "dashed",
        alpha = 0.4
    ) +
    geom_vline(
        xintercept = base::as.Date("2022-08-15"),
        color = "black",
        linetype = "dashed",
        alpha = 0.4
    ) +
    geom_vline(
        xintercept = base::as.Date("2021-12-28"),
        color = "black",
        linetype = "dashed",
        alpha = 0.4
    ) +
    geom_vline(
        xintercept = base::as.Date("2022-03-01"),
        color = "black",
        linetype = "dashed",
        alpha = 0.4
    ) +
    geom_line(aes(y = Total, col = 'Casos Diários'), lwd = 0.4) +
    geom_line(lwd = 1.1) +
    labs(
        title = '💉 Aplicação de Vacinas contra COVID19 no Brasil 🇧🇷',
        subtitle = 'Período de descrição: 668 dias de vacinação.',
        x = 'Ano/Mês',
        y = 'Doses Aplicadas',
        caption = 'Fonte: LocalizaSUS/MS, acesso em 18/11/22
       👨🏻‍⚕️ Twitter: @dr_nesio; #Rstats',
       colour = "Vacinação"
    ) +
    scale_y_continuous(labels = comma_format(big.mark = ".",
                                             decimal.mark = ","),
                       n.breaks = 6) +
    scale_x_date(date_labels = "%Y%m", breaks = "months") +
    theme(
        axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        plot.subtitle = element_text(hjust = 0.5, face = 'italic'),
        legend.position = c(.9, .9),
        panel.background = element_rect(fill = "#faf9d1"),
        legend.title = element_text('Legenda', face = 2),
        legend.background = element_rect(fill = "#ffebeb",
                                         colour = 1),
        legend.title.align = 0.5
    ) +
    scale_color_manual(
        labels = c("Diária", "Média Móvel 7 dias"),
        values = c("green1", "blue")
    ) +
    annotate(
        'text',
        x = base::as.Date("2022-04-22"),
        y = 1800000,
        label = '️Anúnicio do "Fim do Estado de Emergênci️a"',
        size = 4,
        fontface = 'bold',
        angle = 90,
        vjust = 1.5,
        col = 'tomato'
    ) +
    annotate(
        'text',
        x = base::as.Date("2022-08-15"),
        y = 1600000,
        label = 'Início das Eleições Gerais',
        size = 4,
        fontface = 'bold',
        angle = 90,
        vjust = 1.5,
        col = 'tomato'
    ) +
    annotate(
        'text',
        x = base::as.Date("2021-12-28"),
        y = 2000000,
        label = 'Festas de Fim de Ano (Natal/Ano Novo)',
        size = 4,
        fontface = 'bold',
        angle = 90,
        vjust = 1.5,
        col = 'tomato'
    ) +
    annotate(
        'text',
        x = base::as.Date("2022-03-01"),
        y = 2300000,
        label = 'Festas de Carnaval',
        size = 4,
        fontface = 'bold',
        angle = 90,
        vjust = 1.5,
        col = 'tomato'
    ) +
    annotate(
        "rect",
        fill = "red",
        alpha = 0.1,
        xmin = base::as.Date('2021-01-01'),
        xmax = base::as.Date('2021-06-01'),
        ymin = 0,
        ymax = 1500000
    ) +
    annotate(
        "rect",
        fill = "red",
        alpha = 0.1,
        xmin = base::as.Date('2021-06-01'),
        xmax = base::as.Date('2021-12-31'),
        ymin = 0,
        ymax = 2500000
    ) +
    annotate(
        "rect",
        fill = "#f91fdc",
        alpha = 0.1,
        xmin = base::as.Date('2022-01-01'),
        xmax = base::as.Date('2022-04-22'),
        ymin = 0,
        ymax = 1500000
    ) +
    annotate(
        "rect",
        fill = "#f91fdc",
        alpha = 0.1,
        xmin = base::as.Date('2022-04-22'),
        xmax = base::as.Date('2022-08-15'),
        ymin = 0,
        ymax = 1000000
    ) +
    annotate(
        "rect",
        fill = "#f91fdc",
        alpha = 0.1,
        xmin = base::as.Date('2022-08-15'),
        xmax = base::as.Date('2022-12-31'),
        ymin = 0,
        ymax = 500000
    ) +
    annotate(
        'text',
        x = base::as.Date("2021-03-01"),
        y = 2200000,
        label = 'Durante o Ano de 2021 foram aplicadas 340.552.344 doses de vacinas',
        size = 4.5,
        vjust = 1.5,
        fontface = 'bold',
        col = '#9e4278'
        ) +
    annotate(
        'text',
        x = base::as.Date("2022-11-01"),
        y = 490000,
        label = 'Durante o Ano de 2022 foram aplicadas 149.720.878 doses de vacinas',
        fontface = 'bold',
        size = 4.5,
        vjust = -0.1,
        col = '#9e4278'
        )
