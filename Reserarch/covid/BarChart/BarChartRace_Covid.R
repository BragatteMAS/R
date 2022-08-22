#Bar Chart Race COVID
#https://github.com/viniciusbgodinho/bar_chat_race_covid/blob/main/bar_chart.R
con <- gzcon(url(paste("https://data.brasil.io/dataset/covid19/caso_full.csv.gz", sep=",")))
txt <- readLines(con)
dados <- read.csv(textConnection(txt))


library("dplyr")
library("tidyr")
library("lubridate")
library("ggplot2")
library("devtools")
library("flexdashboard")
library("gifski")
library('gganimate')

colnames(dados)[9] <-'confirmed'
colnames(dados)[13] <-'deaths'
colnames(dados)[10] <- 'confirmed_per_100k'


df_regiao <- dados %>%
    filter(place_type == "state") %>%
    arrange(date)



library(tidyverse)
library(gganimate)
library(gifski)
ggplot2::theme_set(theme_classic())




setwd("E:\\Covid-19\\rede analise\\bar chart race")


library('av')



####capitais




dados_capitais <- dados %>%
    filter(place_type=="city",  city_ibge_code=="1100205" | city_ibge_code=="1302603"|
               city_ibge_code=="1200401"| city_ibge_code=="5002704"| city_ibge_code=="1600303"|  
               city_ibge_code=="5300108"| city_ibge_code=="1400100"| city_ibge_code=="5103403"| 
               city_ibge_code=="1721000"| city_ibge_code=="2211001"| city_ibge_code=="3550308"| 
               city_ibge_code=="3304557"| city_ibge_code=="1501402"| city_ibge_code=="2111300"|
               city_ibge_code=="5208707"| city_ibge_code=="2927408"| city_ibge_code=="2704302"| 
               city_ibge_code=="4314902"| city_ibge_code=="4106902"| city_ibge_code=="4205407"| 
               city_ibge_code=="3106200"| city_ibge_code=="2304400"| city_ibge_code=="2611606"| 
               city_ibge_code=="2507507"| city_ibge_code=="2800308"| city_ibge_code=="2408102"| 
               city_ibge_code=="3205309") 







dados_capitais$date <- as.Date(dados_capitais$date)



ggplot2::theme_set(theme_classic())



table_deaths_capitais <- dados_capitais %>%
    select(city,deaths,date)


# generate top n ranking by year group

anim_table_deaths_city <- table_deaths_capitais %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-deaths) * 1,
        Value_rel = deaths / deaths[rank == 1],
        Value_lbl = paste0(" ", deaths)
    ) %>%
    filter(date >= "2020-04-01") %>%
    dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_city, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = deaths / 2,
        height = deaths,
        width = 0.9,
        fill= 'black'
    ), alpha = 0.8, color = NA) +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = deaths, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19: �bitos Acumulados Capitais", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_capitais.gif"))


###casos confirmados

table_confirmed_capitais <- dados_capitais %>%
    select(city,confirmed,date)


# generate top n ranking by year group

anim_table_confirmed_city <- table_confirmed_capitais %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-confirmed) * 1,
        Value_rel = confirmed / confirmed[rank == 1],
        Value_lbl = paste0(" ", confirmed)
    ) %>%
    filter(date >= "2020-04-01") %>%
    dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_confirmed_city, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = confirmed / 2,
        height = confirmed,
        width = 0.9
    ), alpha = 0.8, colour = NA) +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = confirmed, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19: Casos Acumulados Capitais", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_capitais.gif"))




####munic�pios por estado


##minas gerais

df_mg <- dados %>%
    filter(place_type == "city", state == "MG") %>%
    arrange(date)




###casos confirmados

table_confirmed_mg <- df_mg %>%
    select(city,confirmed,date) %>%
    filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_mg <- table_confirmed_mg %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-confirmed) * 1,
        Value_rel = confirmed / confirmed[rank == 1],
        Value_lbl = paste0(" ", confirmed)
    ) %>%
    filter(date >= "2020-04-01") %>%
    dplyr::filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_mg, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = confirmed / 2,
        height = confirmed,
        width = 0.9
    ), alpha = 0.8, color = "black") +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = confirmed, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 Casos Acumulados: Minas Gerais ", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_mg.gif"))


##�bitos

table_deaths_mg <- df_mg %>%
    select(city,deaths,date) %>%
    filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_mg <- table_deaths_mg %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-deaths) * 1,
        Value_rel = deaths / deaths[rank == 1],
        Value_lbl = paste0(" ", deaths)
    ) %>%
    filter(date >= "2020-04-01") %>%
    filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_mg, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = deaths / 2,
        height = deaths,
        width = 0.9,
        fill = "darkred"
    ), alpha = 0.8, color = NA) +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = deaths, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 �bitos Acumulados: Minas Gerais", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_mg.gif"))


##Tocantins

df_to <- dados %>%
    filter(place_type == "city", state == "TO") %>%
    arrange(date)




###casos confirmados

table_confirmed_to <- df_to %>%
    select(city,confirmed,date) %>%
    filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_to <- table_confirmed_to %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-confirmed) * 1,
        Value_rel = confirmed / confirmed[rank == 1],
        Value_lbl = paste0(" ", confirmed)
    ) %>%
    filter(date >= "2020-05-01") %>%
    dplyr::filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_to, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = confirmed / 2,
        height = confirmed,
        width = 0.9
    ), alpha = 0.8, color = "black") +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = confirmed, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 Casos Acumulados: Tocantins ", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_to.gif"))


##�bitos

table_deaths_to <- df_to %>%
    select(city,deaths,date) %>%
    filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_to <- table_deaths_to %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-deaths) * 1,
        Value_rel = deaths / deaths[rank == 1],
        Value_lbl = paste0(" ", deaths)
    ) %>%
    filter(date >= "2020-05-01") %>%
    filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_to, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = deaths / 2,
        height = deaths,
        width = 0.9,
        fill = "darkred"
    ), alpha = 0.8, color = NA) +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = deaths, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 �bitos Acumulados: Tocantins", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_to.gif"))


##Sergipe

df_se <- dados %>%
    filter(place_type == "city", state == "SE") %>%
    arrange(date)




###casos confirmados

table_confirmed_se <- df_se %>%
    select(city,confirmed,date) %>%
    filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_se <- table_confirmed_se %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-confirmed) * 1,
        Value_rel = confirmed / confirmed[rank == 1],
        Value_lbl = paste0(" ", confirmed)
    ) %>%
    filter(date >= "2020-05-01") %>%
    dplyr::filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_se, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = confirmed / 2,
        height = confirmed,
        width = 0.9
    ), alpha = 0.8, color = "black") +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = confirmed, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 Casos Acumulados: Sergipe ", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_se.gif"))


##�bitos

table_deaths_se <- df_se %>%
    select(city,deaths,date) %>%
    filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_se <- table_deaths_se %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-deaths) * 1,
        Value_rel = deaths / deaths[rank == 1],
        Value_lbl = paste0(" ", deaths)
    ) %>%
    filter(date >= "2020-05-01") %>%
    filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_se, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = deaths / 2,
        height = deaths,
        width = 0.9,
        fill = "darkred"
    ), alpha = 0.8, color = NA) +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = deaths, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 �bitos Acumulados: Sergipe", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_se.gif"))


##S�o Paulo

df_sp <- dados %>%
    filter(place_type == "city", state == "SP") %>%
    arrange(date)




###casos confirmados

table_confirmed_sp <- df_sp %>%
    select(city,confirmed,date) %>%
    filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_sp <- table_confirmed_sp %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-confirmed) * 1,
        Value_rel = confirmed / confirmed[rank == 1],
        Value_lbl = paste0(" ", confirmed)
    ) %>%
    filter(date >= "2020-04-01") %>%
    dplyr::filter(rank <= 20) %>%
    dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_sp, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = confirmed / 2,
        height = confirmed,
        width = 0.9
    ), alpha = 0.8, color = "black") +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = confirmed, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 Casos Acumulados: S�o Paulo ", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_sp1.gif"))


##�bitos

table_deaths_sp <- df_sp %>%
    select(city,deaths,date) %>%
    filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_sp <- table_deaths_sp %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-deaths) * 1,
        Value_rel = deaths / deaths[rank == 1],
        Value_lbl = paste0(" ", deaths)
    ) %>%
    filter(date >= "2020-04-01") %>%
    filter(rank <= 20) %>%
    dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_sp, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = deaths / 2,
        height = deaths,
        width = 0.9,
        fill = "darkred"
    ), alpha = 0.8, color = NA) +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = deaths, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 �bitos Acumulados: S�o Paulo", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_sp1.gif"))


##Santa Catarina

df_sc <- dados %>%
    filter(place_type == "city", state == "SC") %>%
    arrange(date)




###casos confirmados

table_confirmed_sc <- df_sc %>%
    select(city,confirmed,date) %>%
    filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_sc <- table_confirmed_sc %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-confirmed) * 1,
        Value_rel = confirmed / confirmed[rank == 1],
        Value_lbl = paste0(" ", confirmed)
    ) %>%
    filter(date >= "2020-05-01") %>%
    dplyr::filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_sc, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = confirmed / 2,
        height = confirmed,
        width = 0.9
    ), alpha = 0.8, color = "black") +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = confirmed, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 Casos Acumulados: Santa Catarina ", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_sc.gif"))


##�bitos

table_deaths_sc <- df_sc %>%
    select(city,deaths,date) %>%
    filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_sc <- table_deaths_sc %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-deaths) * 1,
        Value_rel = deaths / deaths[rank == 1],
        Value_lbl = paste0(" ", deaths)
    ) %>%
    filter(date >= "2020-05-01") %>%
    filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_sc, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = deaths / 2,
        height = deaths,
        width = 0.9,
        fill = "darkred"
    ), alpha = 0.8, color = NA) +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = deaths, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 �bitos Acumulados: Santa Catarina", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_sc.gif"))


##Roraima

df_rr <- dados %>%
    filter(place_type == "city", state == "RR") %>%
    arrange(date)




###casos confirmados

table_confirmed_rr <- df_rr %>%
    select(city,confirmed,date) %>%
    filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_rr <- table_confirmed_rr %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-confirmed) * 1,
        Value_rel = confirmed / confirmed[rank == 1],
        Value_lbl = paste0(" ", confirmed)
    ) %>%
    filter(date >= "2020-05-01") %>%
    dplyr::filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_rr, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = confirmed / 2,
        height = confirmed,
        width = 0.9
    ), alpha = 0.8, color = "black") +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = confirmed, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 Casos Acumulados: Roraima ", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_rr.gif"))


##�bitos

table_deaths_rr <- df_rr %>%
    select(city,deaths,date) %>%
    filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_rr <- table_deaths_rr %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-deaths) * 1,
        Value_rel = deaths / deaths[rank == 1],
        Value_lbl = paste0(" ", deaths)
    ) %>%
    filter(date >= "2020-05-01") %>%
    filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_rr, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = deaths / 2,
        height = deaths,
        width = 0.9,
        fill = "darkred"
    ), alpha = 0.8, color = NA) +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = deaths, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 �bitos Acumulados: Roraima", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_rr.gif"))


##Rondonia

df_ro <- dados %>%
    filter(place_type == "city", state == "ro") %>%
    arrange(date)




###casos confirmados

table_confirmed_ro <- df_ro %>%
    select(city,confirmed,date) %>%
    filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_ro <- table_confirmed_ro %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-confirmed) * 1,
        Value_rel = confirmed / confirmed[rank == 1],
        Value_lbl = paste0(" ", confirmed)
    ) %>%
    filter(date >= "2020-05-01") %>%
    dplyr::filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_ro, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = confirmed / 2,
        height = confirmed,
        width = 0.9
    ), alpha = 0.8, color = "black") +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = confirmed, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 Casos Acumulados: Rondonia ", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_ro.gif"))


##�bitos

table_deaths_ro <- df_ro %>%
    select(city,deaths,date) %>%
    filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_ro <- table_deaths_ro %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-deaths) * 1,
        Value_rel = deaths / deaths[rank == 1],
        Value_lbl = paste0(" ", deaths)
    ) %>%
    filter(date >= "2020-05-01") %>%
    filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_ro, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = deaths / 2,
        height = deaths,
        width = 0.9,
        fill = "darkred"
    ), alpha = 0.8, color = NA) +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = deaths, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 �bitos Acumulados: Rondonia", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_ro.gif"))


##Rio Grande do Sul

df_rs <- dados %>%
    filter(place_type == "city", state == "RS") %>%
    arrange(date)




###casos confirmados

table_confirmed_rs <- df_rs %>%
    select(city,confirmed,date) %>%
    filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_rs <- table_confirmed_rs %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-confirmed) * 1,
        Value_rel = confirmed / confirmed[rank == 1],
        Value_lbl = paste0(" ", confirmed)
    ) %>%
    filter(date >= "2020-04-01") %>%
    dplyr::filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_rs, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = confirmed / 2,
        height = confirmed,
        width = 0.9
    ), alpha = 0.8, color = "black") +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = confirmed, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 Casos Acumulados: Rio Grande do Sul", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_rs.gif"))


##�bitos

table_deaths_rs <- df_rs %>%
    select(city,deaths,date) %>%
    filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_rs <- table_deaths_rs %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-deaths) * 1,
        Value_rel = deaths / deaths[rank == 1],
        Value_lbl = paste0(" ", deaths)
    ) %>%
    filter(date >= "2020-05-01") %>%
    filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_rs, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = deaths / 2,
        height = deaths,
        width = 0.9,
        fill = "darkred"
    ), alpha = 0.8, color = NA) +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = deaths, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 �bitos Acumulados: Rio Grande do Sul", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_rs.gif"))


##Rio Grande do Norte

df_rn <- dados %>%
    filter(place_type == "city", state == "rn") %>%
    arrange(date)




###casos confirmados

table_confirmed_rn <- df_rn %>%
    select(city,confirmed,date) %>%
    filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_rn <- table_confirmed_rn %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-confirmed) * 1,
        Value_rel = confirmed / confirmed[rank == 1],
        Value_lbl = paste0(" ", confirmed)
    ) %>%
    filter(date >= "2020-04-01") %>%
    dplyr::filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_rn, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = confirmed / 2,
        height = confirmed,
        width = 0.9
    ), alpha = 0.8, color = "black") +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = confirmed, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 Casos Acumulados: Rio Grande do Norte ", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_rn.gif"))


##�bitos

table_deaths_rn <- df_rn %>%
    select(city,deaths,date) %>%
    filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_rn <- table_deaths_rn %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-deaths) * 1,
        Value_rel = deaths / deaths[rank == 1],
        Value_lbl = paste0(" ", deaths)
    ) %>%
    filter(date >= "2020-04-01") %>%
    filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_rn, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = deaths / 2,
        height = deaths,
        width = 0.9,
        fill = "darkred"
    ), alpha = 0.8, color = NA) +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = deaths, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 �bitos Acumulados: Rio Grande do Norte", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_rn.gif"))


##Rio de Janeiro

df_rj <- dados %>%
    filter(place_type == "city", state == "RJ") %>%
    arrange(date)




###casos confirmados

table_confirmed_rj <- df_rj %>%
    select(city,confirmed,date) %>%
    filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_rj <- table_confirmed_rj %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-confirmed) * 1,
        Value_rel = confirmed / confirmed[rank == 1],
        Value_lbl = paste0(" ", confirmed)
    ) %>%
    filter(date >= "2020-04-01") %>%
    dplyr::filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_rj, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = confirmed / 2,
        height = confirmed,
        width = 0.9
    ), alpha = 0.8, color = "black") +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = confirmed, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 Casos Acumulados: Rio de Janeiro ", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_rj.gif"))


##�bitos

table_deaths_rj <- df_rj %>%
    select(city,deaths,date) %>%
    filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_rj <- table_deaths_rj %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-deaths) * 1,
        Value_rel = deaths / deaths[rank == 1],
        Value_lbl = paste0(" ", deaths)
    ) %>%
    filter(date >= "2020-04-01") %>%
    filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_rj, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = deaths / 2,
        height = deaths,
        width = 0.9,
        fill = "darkred"
    ), alpha = 0.8, color = NA) +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = deaths, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 �bitos Acumulados: Rio de Janeiro", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_rj.gif"))


##Piau�

df_pi <- dados %>%
    filter(place_type == "city", state == "PI") %>%
    arrange(date)




###casos confirmados

table_confirmed_pi <- df_pi %>%
    select(city,confirmed,date) %>%
    filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_pi <- table_confirmed_pi %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-confirmed) * 1,
        Value_rel = confirmed / confirmed[rank == 1],
        Value_lbl = paste0(" ", confirmed)
    ) %>%
    filter(date >= "2020-05-01") %>%
    dplyr::filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_pi, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = confirmed / 2,
        height = confirmed,
        width = 0.9
    ), alpha = 0.8, color = "black") +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = confirmed, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 Casos Acumulados: Piau� ", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_pi.gif"))


##�bitos

table_deaths_pi <- df_pi %>%
    select(city,deaths,date) %>%
    filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_pi <- table_deaths_pi %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-deaths) * 1,
        Value_rel = deaths / deaths[rank == 1],
        Value_lbl = paste0(" ", deaths)
    ) %>%
    filter(date >= "2020-05-01") %>%
    filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_pi, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = deaths / 2,
        height = deaths,
        width = 0.9,
        fill = "darkred"
    ), alpha = 0.8, color = NA) +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = deaths, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 �bitos Acumulados: Piau�", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_pi.gif"))


##Pernambuco

df_pe <- dados %>%
    filter(place_type == "city", state == "PE") %>%
    arrange(date)




###casos confirmados

table_confirmed_pe <- df_pe %>%
    select(city,confirmed,date) %>%
    filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_pe <- table_confirmed_pe %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-confirmed) * 1,
        Value_rel = confirmed / confirmed[rank == 1],
        Value_lbl = paste0(" ", confirmed)
    ) %>%
    filter(date >= "2020-04-01") %>%
    dplyr::filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_pe, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = confirmed / 2,
        height = confirmed,
        width = 0.9
    ), alpha = 0.8, color = "black") +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = confirmed, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 Casos Acumulados: Pernambuco ", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_pe.gif"))


##�bitos

table_deaths_pe <- df_pe %>%
    select(city,deaths,date) %>%
    filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_pe <- table_deaths_pe %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-deaths) * 1,
        Value_rel = deaths / deaths[rank == 1],
        Value_lbl = paste0(" ", deaths)
    ) %>%
    filter(date >= "2020-04-01") %>%
    filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_pe, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = deaths / 2,
        height = deaths,
        width = 0.9,
        fill = "darkred"
    ), alpha = 0.8, color = NA) +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = deaths, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 �bitos Acumulados: Pernambuco", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_pe.gif"))


##Paran�

df_pr <- dados %>%
    filter(place_type == "city", state == "PR") %>%
    arrange(date)




###casos confirmados

table_confirmed_pr <- df_pr %>%
    select(city,confirmed,date) %>%
    filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_pr <- table_confirmed_pr %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-confirmed) * 1,
        Value_rel = confirmed / confirmed[rank == 1],
        Value_lbl = paste0(" ", confirmed)
    ) %>%
    filter(date >= "2020-05-01") %>%
    dplyr::filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_pr, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = confirmed / 2,
        height = confirmed,
        width = 0.9
    ), alpha = 0.8, color = "black") +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = confirmed, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 Casos Acumulados: Paran� ", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_pr.gif"))


##�bitos

table_deaths_pr <- df_pr %>%
    select(city,deaths,date) %>%
    filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_pr <- table_deaths_pr %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-deaths) * 1,
        Value_rel = deaths / deaths[rank == 1],
        Value_lbl = paste0(" ", deaths)
    ) %>%
    filter(date >= "2020-05-01") %>%
    filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_pr, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = deaths / 2,
        height = deaths,
        width = 0.9,
        fill = "darkred"
    ), alpha = 0.8, color = NA) +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = deaths, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 �bitos Acumulados: Paran�", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_pr.gif"))


##Para�ba 

df_pb <- dados %>%
    filter(place_type == "city", state == "PB") %>%
    arrange(date)




###casos confirmados

table_confirmed_pb <- df_pb %>%
    select(city,confirmed,date) %>%
    filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_pb <- table_confirmed_pb %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-confirmed) * 1,
        Value_rel = confirmed / confirmed[rank == 1],
        Value_lbl = paste0(" ", confirmed)
    ) %>%
    filter(date >= "2020-04-01") %>%
    dplyr::filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_pb, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = confirmed / 2,
        height = confirmed,
        width = 0.9
    ), alpha = 0.8, color = "black") +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = confirmed, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 Casos Acumulados: Para�ba ", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_pb.gif"))


##�bitos

table_deaths_pb <- df_pb %>%
    select(city,deaths,date) %>%
    filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_pb <- table_deaths_pb %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-deaths) * 1,
        Value_rel = deaths / deaths[rank == 1],
        Value_lbl = paste0(" ", deaths)
    ) %>%
    filter(date >= "2020-04-01") %>%
    filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_pb, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = deaths / 2,
        height = deaths,
        width = 0.9,
        fill = "darkred"
    ), alpha = 0.8, color = NA) +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = deaths, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 �bitos Acumulados: Para�ba", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_pb.gif"))


##Par�

df_pa <- dados %>%
    filter(place_type == "city", state == "PA") %>%
    arrange(date)




###casos confirmados

table_confirmed_pa <- df_pa %>%
    select(city,confirmed,date) %>%
    filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_pa <- table_confirmed_pa %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-confirmed) * 1,
        Value_rel = confirmed / confirmed[rank == 1],
        Value_lbl = paste0(" ", confirmed)
    ) %>%
    filter(date >= "2020-04-01") %>%
    dplyr::filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_pa, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = confirmed / 2,
        height = confirmed,
        width = 0.9
    ), alpha = 0.8, color = "black") +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = confirmed, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 Casos Acumulados: Par� ", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_pa.gif"))


##�bitos

table_deaths_pa <- df_pa %>%
    select(city,deaths,date) %>%
    filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_pa <- table_deaths_pa %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-deaths) * 1,
        Value_rel = deaths / deaths[rank == 1],
        Value_lbl = paste0(" ", deaths)
    ) %>%
    filter(date >= "2020-04-01") %>%
    filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_pa, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = deaths / 2,
        height = deaths,
        width = 0.9,
        fill = "darkred"
    ), alpha = 0.8, color = NA) +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = deaths, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 �bitos Acumulados: Par�", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_pa.gif"))


##Mato Grosso do Sul

df_ms <- dados %>%
    filter(place_type == "city", state == "MS") %>%
    arrange(date)




###casos confirmados

table_confirmed_ms <- df_ms %>%
    select(city,confirmed,date) %>%
    filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_ms <- table_confirmed_ms %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-confirmed) * 1,
        Value_rel = confirmed / confirmed[rank == 1],
        Value_lbl = paste0(" ", confirmed)
    ) %>%
    filter(date >= "2020-05-01") %>%
    dplyr::filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_ms, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = confirmed / 2,
        height = confirmed,
        width = 0.9
    ), alpha = 0.8, color = "black") +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = confirmed, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 Casos Acumulados: Mato Grosso do Sul ", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_ms.gif"))


##�bitos

table_deaths_ms <- df_ms %>%
    select(city,deaths,date) %>%
    filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_ms <- table_deaths_ms %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-deaths) * 1,
        Value_rel = deaths / deaths[rank == 1],
        Value_lbl = paste0(" ", deaths)
    ) %>%
    filter(date >= "2020-05-01") %>%
    filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_ms, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = deaths / 2,
        height = deaths,
        width = 0.9,
        fill = "darkred"
    ), alpha = 0.8, color = NA) +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = deaths, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 �bitos Acumulados: Mato Grosso do Sul", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_ms.gif"))


##Mato Grosso

df_mt <- dados %>%
    filter(place_type == "city", state == "MT") %>%
    arrange(date)




###casos confirmados

table_confirmed_mt <- df_mt %>%
    select(city,confirmed,date) %>%
    filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_mt <- table_confirmed_mt %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-confirmed) * 1,
        Value_rel = confirmed / confirmed[rank == 1],
        Value_lbl = paste0(" ", confirmed)
    ) %>%
    filter(date >= "2020-05-01") %>%
    dplyr::filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_mt, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = confirmed / 2,
        height = confirmed,
        width = 0.9
    ), alpha = 0.8, color = "black") +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = confirmed, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 Casos Acumulados: Mato Grosso ", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_mt.gif"))


##�bitos

table_deaths_mt <- df_mt %>%
    select(city,deaths,date) %>%
    filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_mt <- table_deaths_mt %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-deaths) * 1,
        Value_rel = deaths / deaths[rank == 1],
        Value_lbl = paste0(" ", deaths)
    ) %>%
    filter(date >= "2020-05-01") %>%
    filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_mt, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = deaths / 2,
        height = deaths,
        width = 0.9,
        fill = "darkred"
    ), alpha = 0.8, color = NA) +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = deaths, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 �bitos Acumulados: Mato Grosso", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_mt.gif"))


##Maranh�o

df_ma <- dados %>%
    filter(place_type == "city", state == "MA") %>%
    arrange(date)




###casos confirmados

table_confirmed_ma <- df_ma %>%
    select(city,confirmed,date) %>%
    filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_ma <- table_confirmed_ma %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-confirmed) * 1,
        Value_rel = confirmed / confirmed[rank == 1],
        Value_lbl = paste0(" ", confirmed)
    ) %>%
    filter(date >= "2020-04-01") %>%
    dplyr::filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_ma, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = confirmed / 2,
        height = confirmed,
        width = 0.9
    ), alpha = 0.8, color = "black") +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = confirmed, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 Casos Acumulados: Maranh�o ", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_ma.gif"))


##�bitos

table_deaths_ma <- df_ma %>%
    select(city,deaths,date) %>%
    filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_ma <- table_deaths_ma %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-deaths) * 1,
        Value_rel = deaths / deaths[rank == 1],
        Value_lbl = paste0(" ", deaths)
    ) %>%
    filter(date >= "2020-04-01") %>%
    filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_ma, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = deaths / 2,
        height = deaths,
        width = 0.9,
        fill = "darkred"
    ), alpha = 0.8, color = NA) +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = deaths, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 �bitos Acumulados: Maranh�o", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_ma.gif"))


##Goi�s

df_go <- dados %>%
    filter(place_type == "city", state == "GO") %>%
    arrange(date)




###casos confirmados

table_confirmed_go <- df_go %>%
    select(city,confirmed,date) %>%
    filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_go <- table_confirmed_go %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-confirmed) * 1,
        Value_rel = confirmed / confirmed[rank == 1],
        Value_lbl = paste0(" ", confirmed)
    ) %>%
    filter(date >= "2020-05-01") %>%
    dplyr::filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_go, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = confirmed / 2,
        height = confirmed,
        width = 0.9
    ), alpha = 0.8, color = "black") +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = confirmed, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 Casos Acumulados: Goi�s ", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_go.gif"))


##�bitos

table_deaths_go <- df_go %>%
    select(city,deaths,date) %>%
    filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_go <- table_deaths_go %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-deaths) * 1,
        Value_rel = deaths / deaths[rank == 1],
        Value_lbl = paste0(" ", deaths)
    ) %>%
    filter(date >= "2020-05-01") %>%
    filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_go, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = deaths / 2,
        height = deaths,
        width = 0.9,
        fill = "darkred"
    ), alpha = 0.8, color = NA) +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = deaths, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 �bitos Acumulados: Goi�s", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_go.gif"))


##Esp�rito Santo

df_es <- dados %>%
    filter(place_type == "city", state == "ES") %>%
    arrange(date)




###casos confirmados

table_confirmed_es <- df_es %>%
    select(city,confirmed,date) %>%
    filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_es <- table_confirmed_es %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-confirmed) * 1,
        Value_rel = confirmed / confirmed[rank == 1],
        Value_lbl = paste0(" ", confirmed)
    ) %>%
    filter(date >= "2020-05-01") %>%
    dplyr::filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_es, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = confirmed / 2,
        height = confirmed,
        width = 0.9
    ), alpha = 0.8, color = "black") +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = confirmed, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 Casos Acumulados: Esp�rito Santo ", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_es.gif"))


##�bitos

table_deaths_es <- df_es %>%
    select(city,deaths,date) %>%
    filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_es <- table_deaths_es %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-deaths) * 1,
        Value_rel = deaths / deaths[rank == 1],
        Value_lbl = paste0(" ", deaths)
    ) %>%
    filter(date >= "2020-05-01") %>%
    filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_es, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = deaths / 2,
        height = deaths,
        width = 0.9,
        fill = "darkred"
    ), alpha = 0.8, color = NA) +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = deaths, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 �bitos Acumulados: Esp�rito Santo", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_es.gif"))


##Distrito Federal

df_df <- dados %>%
    filter(place_type == "city", state == "DF") %>%
    arrange(date)




###casos confirmados

table_confirmed_df <- df_df %>%
    select(city,confirmed,date) %>%
    filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_df <- table_confirmed_df %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-confirmed) * 1,
        Value_rel = confirmed / confirmed[rank == 1],
        Value_lbl = paste0(" ", confirmed)
    ) %>%
    filter(date >= "2020-05-01") %>%
    dplyr::filter(rank <= 5) %>%
    dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_df, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = confirmed / 2,
        height = confirmed,
        width = 0.9
    ), alpha = 0.8, color = "black") +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = confirmed, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 Casos Acumulados: Distrito Federal ", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_df.gif"))


##�bitos

table_deaths_df <- df_df %>%
    select(city,deaths,date) %>%
    filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_df <- table_deaths_df %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-deaths) * 1,
        Value_rel = deaths / deaths[rank == 1],
        Value_lbl = paste0(" ", deaths)
    ) %>%
    filter(date >= "2020-05-01") %>%
    filter(rank <= 5) %>%
    dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_df, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = deaths / 2,
        height = deaths,
        width = 0.9,
        fill = "darkred"
    ), alpha = 0.8, color = NA) +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = deaths, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 �bitos Acumulados: Distrito Federal", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_df.gif"))


##Cear�

df_ce <- dados %>%
    filter(place_type == "city", state == "CE") %>%
    arrange(date)




###casos confirmados

table_confirmed_ce <- df_ce %>%
    select(city,confirmed,date) %>%
    filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_ce <- table_confirmed_ce %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-confirmed) * 1,
        Value_rel = confirmed / confirmed[rank == 1],
        Value_lbl = paste0(" ", confirmed)
    ) %>%
    filter(date >= "2020-04-01") %>%
    dplyr::filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_ce, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = confirmed / 2,
        height = confirmed,
        width = 0.9
    ), alpha = 0.8, color = "black") +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = confirmed, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 Casos Acumulados: Cear� ", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_ce.gif"))


##�bitos

table_deaths_ce <- df_ce %>%
    select(city,deaths,date) %>%
    filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_ce <- table_deaths_ce %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-deaths) * 1,
        Value_rel = deaths / deaths[rank == 1],
        Value_lbl = paste0(" ", deaths)
    ) %>%
    filter(date >= "2020-04-01") %>%
    filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_ce, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = deaths / 2,
        height = deaths,
        width = 0.9,
        fill = "darkred"
    ), alpha = 0.8, color = NA) +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = deaths, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 �bitos Acumulados: Cear�", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_ce.gif"))


##Bahia

df_ba <- dados %>%
    filter(place_type == "city", state == "BA") %>%
    arrange(date)




###casos confirmados

table_confirmed_ba <- df_ba %>%
    select(city,confirmed,date) %>%
    filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_ba <- table_confirmed_ba %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-confirmed) * 1,
        Value_rel = confirmed / confirmed[rank == 1],
        Value_lbl = paste0(" ", confirmed)
    ) %>%
    filter(date >= "2020-05-01") %>%
    dplyr::filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_ba, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = confirmed / 2,
        height = confirmed,
        width = 0.9
    ), alpha = 0.8, color = "black") +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = confirmed, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 Casos Acumulados: Bahia ", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_ba.gif"))


##�bitos

table_deaths_ba <- df_ba %>%
    select(city,deaths,date) %>%
    filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_ba <- table_deaths_ba %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-deaths) * 1,
        Value_rel = deaths / deaths[rank == 1],
        Value_lbl = paste0(" ", deaths)
    ) %>%
    filter(date >= "2020-05-01") %>%
    filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_ba, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = deaths / 2,
        height = deaths,
        width = 0.9,
        fill = "darkred"
    ), alpha = 0.8, color = NA) +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = deaths, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 �bitos Acumulados: Bahia", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_ba.gif"))


##Amazonas

df_am <- dados %>%
    filter(place_type == "city", state == "AM") %>%
    arrange(date)




###casos confirmados

table_confirmed_am <- df_am %>%
    select(city,confirmed,date) %>%
    filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_am <- table_confirmed_am %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-confirmed) * 1,
        Value_rel = confirmed / confirmed[rank == 1],
        Value_lbl = paste0(" ", confirmed)
    ) %>%
    filter(date >= "2020-04-01") %>%
    dplyr::filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_am, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = confirmed / 2,
        height = confirmed,
        width = 0.9
    ), alpha = 0.8, color = "black") +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = confirmed, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 Casos Acumulados: Amazonas", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_am.gif"))


##�bitos

table_deaths_am <- df_am %>%
    select(city,deaths,date) %>%
    filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_am <- table_deaths_am %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-deaths) * 1,
        Value_rel = deaths / deaths[rank == 1],
        Value_lbl = paste0(" ", deaths)
    ) %>%
    filter(date >= "2020-04-01") %>%
    filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_am, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = deaths / 2,
        height = deaths,
        width = 0.9,
        fill = "darkred"
    ), alpha = 0.8, color = NA) +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = deaths, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 �bitos Acumulados: Amazonas", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_am.gif"))


##Amap�

df_ap <- dados %>%
    filter(place_type == "city", state == "AP") %>%
    arrange(date)




###casos confirmados

table_confirmed_ap <- df_ap %>%
    select(city,confirmed,date) %>%
    filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_ap <- table_confirmed_ap %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-confirmed) * 1,
        Value_rel = confirmed / confirmed[rank == 1],
        Value_lbl = paste0(" ", confirmed)
    ) %>%
    filter(date >= "2020-05-01") %>%
    dplyr::filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_ap, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = confirmed / 2,
        height = confirmed,
        width = 0.9
    ), alpha = 0.8, color = "black") +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = confirmed, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 Casos Acumulados: Amap� ", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_ap.gif"))


##�bitos

table_deaths_ap <- df_ap %>%
    select(city,deaths,date) %>%
    filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_ap <- table_deaths_ap %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-deaths) * 1,
        Value_rel = deaths / deaths[rank == 1],
        Value_lbl = paste0(" ", deaths)
    ) %>%
    filter(date >= "2020-05-01") %>%
    filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_ap, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = deaths / 2,
        height = deaths,
        width = 0.9,
        fill = "darkred"
    ), alpha = 0.8, color = NA) +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = deaths, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 �bitos Acumulados: Amap�", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_ap.gif"))


##Acre

df_ac <- dados %>%
    filter(place_type == "city", state == "AC") %>%
    arrange(date)




###casos confirmados

table_confirmed_ac <- df_ac %>%
    select(city,confirmed,date) %>%
    filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_ac <- table_confirmed_ac %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-confirmed) * 1,
        Value_rel = confirmed / confirmed[rank == 1],
        Value_lbl = paste0(" ", confirmed)
    ) %>%
    filter(date >= "2020-05-01") %>%
    dplyr::filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_ac, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = confirmed / 2,
        height = confirmed,
        width = 0.9
    ), alpha = 0.8, color = "black") +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = confirmed, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 Casos Acumulados: Acre ", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_ac.gif"))


##�bitos

table_deaths_ac <- df_ac %>%
    select(city,deaths,date) %>%
    filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_ac <- table_deaths_ac %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-deaths) * 1,
        Value_rel = deaths / deaths[rank == 1],
        Value_lbl = paste0(" ", deaths)
    ) %>%
    filter(date >= "2020-05-01") %>%
    filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_ac, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = deaths / 2,
        height = deaths,
        width = 0.9,
        fill = "darkred"
    ), alpha = 0.8, color = NA) +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = deaths, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 �bitos Acumulados: Acre", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_ac.gif"))

####munic�pios por estado

##Alagoas

df_al <- dados %>%
    filter(place_type == "city", state == "AL") %>%
    arrange(date)




###casos confirmados

table_confirmed_al <- df_al %>%
    select(city,confirmed,date) %>%
    filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_al <- table_confirmed_al %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-confirmed) * 1,
        Value_rel = confirmed / confirmed[rank == 1],
        Value_lbl = paste0(" ", confirmed)
    ) %>%
    filter(date >= "2020-05-01") %>%
    dplyr::filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_al, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = confirmed / 2,
        height = confirmed,
        width = 0.9
    ), alpha = 0.8, color = "black") +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = confirmed, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 Casos Acumulados: Alagoas ", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_al.gif"))


##�bitos

table_deaths_al <- df_al %>%
    select(city,deaths,date) %>%
    filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_al <- table_deaths_al %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-deaths) * 1,
        Value_rel = deaths / deaths[rank == 1],
        Value_lbl = paste0(" ", deaths)
    ) %>%
    filter(date >= "2020-05-01") %>%
    filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_al, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = deaths / 2,
        height = deaths,
        width = 0.9,
        fill = "darkred"
    ), alpha = 0.8, color = NA) +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = deaths, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 �bitos Acumulados: Alagoas", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_al.gif"))



####munic�pios por estado

##Rio Grande do Norte

df_rn <- dados %>%
    filter(place_type == "city", state == "RN") %>%
    arrange(date)




###casos confirmados

table_confirmed_rn <- df_rn %>%
    select(city,confirmed,date) %>%
    filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_rn <- table_confirmed_rn %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-confirmed) * 1,
        Value_rel = confirmed / confirmed[rank == 1],
        Value_lbl = paste0(" ", confirmed)
    ) %>%
    filter(date >= "2020-04-01") %>%
    dplyr::filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_rn, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = confirmed / 2,
        height = confirmed,
        width = 0.9
    ), alpha = 0.8, color = "black") +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = confirmed, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 Casos Acumulados: Rio Grande do Norte ", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_rn.gif"))


##�bitos

table_deaths_rn <- df_rn %>%
    select(city,deaths,date) %>%
    filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_rn <- table_deaths_rn %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-deaths) * 1,
        Value_rel = deaths / deaths[rank == 1],
        Value_lbl = paste0(" ", deaths)
    ) %>%
    filter(date >= "2020-04-01") %>%
    filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_rn, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = deaths / 2,
        height = deaths,
        width = 0.9,
        fill = "darkred"
    ), alpha = 0.8, color = NA) +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = deaths, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 �bitos Acumulados: Rio Grande do Norte", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_rn.gif"))



####munic�pios por estado

##Rond�nia

df_ro <- dados %>%
    filter(place_type == "city", state == "RO") %>%
    arrange(date)




###casos confirmados

table_confirmed_ro <- df_ro %>%
    select(city,confirmed,date) %>%
    filter(city != "Importados/Indefinidos")


# generate top n ranking by year group

anim_table_confirmed_ro <- table_confirmed_ro %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-confirmed) * 1,
        Value_rel = confirmed / confirmed[rank == 1],
        Value_lbl = paste0(" ", confirmed)
    ) %>%
    filter(date >= "2020-05-01") %>%
    dplyr::filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated bar chart

p <- ggplot2::ggplot(anim_table_confirmed_ro, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = confirmed / 2,
        height = confirmed,
        width = 0.9
    ), alpha = 0.8, color = "black") +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = confirmed, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 Casos Acumulados: Rond�nia", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_confirmed_ro.gif"))


##�bitos

table_deaths_ro <- df_ro %>%
    select(city,deaths,date) %>%
    filter(city != "Importados/Indefinidos")




# generate top n ranking by year group

anim_table_deaths_ro <- table_deaths_ro %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
        rank = row_number(-deaths) * 1,
        Value_rel = deaths / deaths[rank == 1],
        Value_lbl = paste0(" ", deaths)
    ) %>%
    filter(date >= "2020-05-01") %>%
    filter(rank <= 10) %>%
    dplyr::ungroup()





# create animated barchart

p <- ggplot2::ggplot(anim_table_deaths_ro, aes(rank)) +
    ggplot2::geom_tile(aes(
        y = deaths / 2,
        height = deaths,
        width = 0.9,
        fill = "darkred"
    ), alpha = 0.8, color = NA) +
    ggplot2::geom_text(aes(y = 0, label = paste(city, " ")), size = 8, vjust = 0.2, hjust = 1) +
    ggplot2::geom_text(aes(y = deaths, label = Value_lbl, hjust = 0), size = 8) +
    ggplot2::coord_flip(clip = "off", expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::labs(
        title = "Covid-19 �bitos Acumulados: Rond�nia", x = "", y = "",
        subtitle = "{closest_state}",
        caption = "Dados: SES via https://data.brasil.io/. Elaborado: Rede An�lise COVID-19 por Vin�cius Godinho"
    ) +
    ggplot2::theme(title = element_text(size=30),
                   axis.ticks.y = element_blank(),
                   axis.text.y = element_blank(),
                   plot.margin = margin(2, 2, 1, 16, "cm")
    ) +
    gganimate::transition_states(date, transition_length = 4, state_length = 1) +
    gganimate::ease_aes("cubic-in-out")

# save as preferred rendered format

gganimate::animate(p, 200, fps = 7.5, duration = 90, width = 2000, height = 1200, renderer = gifski_renderer("anim_deaths_ro.gif"))































