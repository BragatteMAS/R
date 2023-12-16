# SCRIPT DISSERTACAO Léa Freitas
# Ref:: [leadefreitas (Léa Amaral)](https://github.com/leadefreitas)
# Refactor: BragatteMAS

#  verificar sincronicidade das variantes de COVID-19 e o perfil epidemiológico dos pacientes hospitalizados por SRAG por COVID-19 no Estado do Rio de Janeiro, Brasil, no período de 2020 a 2022.

# To-DO
# Review the code

# chamando as bibliotecas
pacman::p_load(tidyverse, lubridate,vroom, dplyr, DescTools, geobr, ggthemes, ggrepel, grid, sf, sp)

# link para baixar os bancos individualmente 
dados.SES.20 <- vroom("http://sistemas.saude.rj.gov.br/tabnetbd/sivep_gripe/sivep_gripe_2020.csv", locale = vroom::locale(encoding = "latin1"), delim = ";")
dados.SES.21 <- vroom("http://sistemas.saude.rj.gov.br/tabnetbd/sivep_gripe/sivep_gripe_2021.csv", locale = vroom::locale(encoding = "latin1"),delim = ";")
dados.SES.22 <- vroom("http://sistemas.saude.rj.gov.br/tabnetbd/sivep_gripe/sivep_gripe_2022.csv", locale = vroom::locale(encoding = "latin1"),delim = ";")


options(scipen = 10000) # para nao aparecer número científico

# juntando os bancos
dados2 <- merge(dados.SES.20, dados.SES.21,  all = TRUE)  
bancoprimario <- merge(dados2, dados.SES.22, all = TRUE)

# filtrando e selecionando as variaveis de interesse
dados1 <- bancoprimario %>% 
  filter(`Classificação final do caso` == "5 - COVID-19",
         Idade >= 18 ,
         `Houve internação para SRAG`== "1 - Sim",
         `Semana epidemiológica dos primeiros sintomas` <= "202252") %>% 
  select(c("Data dos primeiros sintomas", "Semana epidemiológica da notificação","Semana epidemiológica dos primeiros sintomas", "Idade","Sexo", "Raça/cor",
           "Código do município de residência", "Gestante", 
           "Recebeu vacina COVID-19", "Data vacina COVID-19 1ª dose", "Data vacina COVID-19 2ª dose",
           "Data vacina COVID-19 dose de reforço", "Houve internação para SRAG", "Internado em UTI",
           "Internado em UTI", "Uso de suporte ventilatório"  , 
           "Classificação final do caso" , "Evolução do caso"))

#Categorizando a variavel idade
dados1$FaixaEtária <- cut(dados1$Idade, 
                          breaks = c(17, 29, 39, 49, 59, 69, 79, 89, 199),
                          labels = c('18-29', '30-39', '40-49', '50-59',
                                     '60-69', '70-79', '80-89', '90 ou mais'),
                          right = T)
levels(dados1$FaixaEtária)
Freq(dados1$FaixaEtária)

#Categorizando a variavel vacina

dados1$Vacinas <- "Incompleto"

dados1$Vacinas[which(dados1$`Data vacina COVID-19 1ª dose` >= "01/01/2021" &
                       dados1$`Data vacina COVID-19 2ª dose` >= "01/01/2021")] <-c ("2 Doses")

dados1$Vacinas[which(dados1$`Data vacina COVID-19 dose de reforço` >= "01/01/2021")] <-c ("Reforço")

aux <- unique(dados1$Vacinas)
dados1$Vacinas <- factor(dados1$Vacinas, levels = aux, ordered = T) 


# tbl.regsaude <- read_csv("Data_rl_municip_regsaud.csv") # original data is local
## find source of `regioes de saude  STOP HERE #to-do
## source not have CO_REGSAUD
municipios_22 <- read_municipality(code_muni = "all", year = 2022)
tbl.regsaude <- municipios_22 %>% 
  select(code_muni, name_muni, CO_REGSAUD)

         
dados1 <- dados1 %>% 
  left_join(
    tbl.regsaude %>% 
      mutate(CO_MUNICIP = as.numeric(CO_MUNICIP)), 
    by = c(`Código do município de residência`="CO_MUNICIP")) 

# categorizando as regioes de saude

dados1$Regiaodesaude <- "Incompleto"

dados1$Regiaodesaude[which(dados1$CO_REGSAUD == 33001)] <-c ("Baia da Ilha Grande")
dados1$Regiaodesaude[which(dados1$CO_REGSAUD == 33002)] <-c ("Baixada Litorânea")
dados1$Regiaodesaude[which(dados1$CO_REGSAUD == 33003)] <-c ("Centro-Sul")
dados1$Regiaodesaude[which(dados1$CO_REGSAUD == 33004)] <-c ("Médio Paraíba")
dados1$Regiaodesaude[which(dados1$CO_REGSAUD == 33005)] <-c ("Metropolitana I")
dados1$Regiaodesaude[which(dados1$CO_REGSAUD == 33006)] <-c ("Metropolitana II")
dados1$Regiaodesaude[which(dados1$CO_REGSAUD == 33007)] <-c ("Noroeste")
dados1$Regiaodesaude[which(dados1$CO_REGSAUD == 33008)] <-c ("Norte")
dados1$Regiaodesaude[which(dados1$CO_REGSAUD == 33009)] <-c ("Serrana")
Freq(dados1$Regiaodesaude)

# incluindo o ano na tabela

dados1$Ano <- "Incompleto"

dados1$Ano[which(dados1$`Semana epidemiológica dos primeiros sintomas` <= 202053)] <-c ("2020")
dados1$Ano[which(dados1$`Semana epidemiológica dos primeiros sintomas` >= 202101 & dados1$`Semana epidemiológica dos primeiros sintomas` <=202152 )] <-c ("2021")
dados1$Ano[which(dados1$`Semana epidemiológica dos primeiros sintomas` >= 202201)] <-c ("2022")

# tabela One

library(tableone)
tabelaOne <- CreateTableOne(vars=c("FaixaEtária", "Sexo", "Raça/cor", "Uso de suporte ventilatório", "Internado em UTI","Regiaodesaude","Evolução do caso"), strata = "Vacinas", argsApprox = list(correct = F), data=dados1)
print(tabelaOne, showAllLevels = T)

# criando as tabelas de letalidade

#dados1 %>% 
group_by(#`Raça/cor`, # `Uso de suporte ventilatório`,
  # Regiaodesaude,
  # Epiweek = DATE,
  # FaixaEtária,
  # Vacinas,
  # Sexo,
) %>%
  summarise(
    Hosp = n(),
    Obitos = sum(`Evolução do caso`=="2 - Óbito", na.rm = T), Letalidade = sum(Obitos/Hosp*100, na.rm = T)) %>% View()


# Lendo regionais de saude + mapa já salvos

# Lendo regionais de saude + mapa -----------------------------------------
hr <- read_health_region( year=2013)

RJ.hr <- hr %>% filter(abbrev_state == "RJ")

hr.cod <- as.character(RJ.hr$code_health_region)
hr.name <- sort(as.character(RJ.hr$name_health_region))

# Remove plot axis
no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())


nomes <- unique(RJ.hr$name_health_region)
letras <- LETTERS[1:length(nomes)]
nomes2 <- paste(letras, nomes, sep=" - ")


# Plot all regions in RJ state 
ggplot(data=RJ.hr %>% 
         mutate(name_health_region = factor(name_health_region, levels = nomes,
                                            labels = nomes2) )
) + 
  geom_sf(aes(fill = name_health_region), show.legend = T) +
  theme_minimal(base_size = 14) +
  labs(
    title = "                                Regiões de Saúde do Estado do Rio de Janeiro",
    fill = " ") +
  no_axis

ggplot(data=hr) +
  geom_sf(aes(fill = name_health_region), show.legend = FALSE) + 
  geom_label_repel(aes(label = name_health_region, geometry = geom),
                   stat = "sf_coordinates", size = 5 ) +
  stat_sf_coordinates() +
  theme_minimal(base_size = 14) +
  labs(title = "Regiões de Saúde do Brasil") +
  no_axis

# incluindo as populaçoes de cada regiao de saude
popRJ.hr <- tibble( 
  name_health_region = hr.name,
  pop = c(295944, 855444,  342078,  918097, 10542254, 
          2131058,  349417, 955122, 976775)
)

#  transformando a variavel de primeiros sintomas para data 

dados1 <- dados1 %>%
  #  mutate(DATE = as.Date(`Data dos primeiros sintomas`)) %>%
  # mutate(DATE1 = as.Date(DATE, unit = "week")) 
  mutate(
    Date0 = floor_date(as.Date(`Data dos primeiros sintomas`, format = "%d/%m/%Y", unit ="week" )),
    Date = dmy(`Data dos primeiros sintomas`),
    Date = Date - as.numeric(format.Date(Date,"%w"))
  )


# retirando a regiao de saude categorizada como incompleto
dados1 <- dados1 %>%filter(Regiaodesaude != "Incompleto")

### suavizando os graficos

# incluindo o momento de cada variante no grafico

onda_breaksteste <-  c (ymd("2020-10-04", "2021-01-26", 
                            "2021-06-21",
                            "2021-12-10", "2022-05-01" , "2022-09-07" ))

# Maximos por onda e por regional
onda_breaksteste


dadosRJ.ew.teste <- dados1 %>% 
  mutate(
    Onda = case_when(Date <= onda_breaksteste[1] ~ "B.1.1.33",
                     Date > onda_breaksteste[1] & Date <= onda_breaksteste[2] ~ "Zeta",
                     Date > onda_breaksteste[2] & Date <= onda_breaksteste[3] ~ "Gama",
                     Date > onda_breaksteste[3] & Date <= onda_breaksteste[4] ~ "Delta",
                     Date > onda_breaksteste[4] & Date <= onda_breaksteste[5] ~ "Omicron BA.1",
                     Date > onda_breaksteste[5] & Date <= onda_breaksteste[6] ~ "Omicron BA.2",
                     Date > onda_breaksteste[6] ~ "Omicron BA.5",
                     # Sanity check
                     TRUE ~ "BANANA"),
    Onda = factor(Onda, levels = c("B.1.1.33", "Zeta", "Gama", 
                                   "Delta", "Omicron BA.1", "Omicron BA.2", "Omicron BA.5" ), ordered = T)
  ) 


Freq(dadosRJ.ew.teste$Onda)    

dadosRJ.ew.teste <- dadosRJ.ew.teste %>% 
  group_by(Date, Regiaodesaude, Onda, CO_REGSAUD) %>%
  count() %>% 
  mutate(covid = n)



dadosRJ.ew.teste %>% 
  #filter(name_health_region == "Metropolitana I") %>% 
  group_by(Onda) %>% 
  summarise(
    #  maxRMI = dt_evento_w[which.max(covid)/],
    a = Date [which.max(covid)])


dadosRJ.ew.teste %>% 
  group_by(Onda) %>% 
  mutate(
    maxRMI = Date[which.max(covid)],
  ) %>% ungroup() %>% 
  group_by(Onda, Regiaodesaude) %>% 
  summarise(
    maxRMI = maxRMI[1],
    maxHR = Date[which.max(covid)]
  ) %>% 
  mutate(
    # Tempo após pico da regiao metropolitana I (RMI)
    tempo_pos_RMI = ( - a [Regiaodesaude == "Metropolitana I"]),
    # tempo_pos_RMI.d = cut(tempo_pos_RMI, 
    #                       breaks = c(0,2,4,6,8,20),
    #                       labels = c("0 ou 1",
    #                                  "2 ou 3",
    #                                  "4 ou 5",
    #                                  "6 ou 7",
    #                                  "8 ou mais"),
    #                       right = F)
  ) %>% View()

### modelando

library(INLA)

tbl.inla.teste <- tibble(Date = sort(unique(dadosRJ.ew.teste$Date))) %>% 
  rowid_to_column(var = "Time")


dadosRJ.ew.teste <- dadosRJ.ew.teste %>% 
  left_join(
    popRJ.hr,
    by = c(`Regiaodesaude` = "name_health_region")
  )



RJ.inla.teste <- dadosRJ.ew.teste %>% 
  filter(Date >= "2020-03-05", Date < "2022-12-31") %>% 
  left_join(tbl.inla.teste) %>%
  left_join(tibble(CO_REGSAUD = hr.cod) %>% 
              rowid_to_column(var = "Region")
  )



formula.teste = covid ~ 1 + CO_REGSAUD + f(Time, model = "rw2", 
                                           group = Region, 
                                           control.group = list(model = "iid")
)



output.teste <- inla(formula = formula.teste, family = "poisson", E = pop,
                     data = RJ.inla.teste)



plot(output.teste)

time.re.out <- output.teste$summary.random$Time %>% 
  add_column(CO_REGSAUD = rep(hr.cod, 
                              each = nrow(output.teste$summary.random$Time)/length(hr.cod)) ) %>% 
  left_join(tbl.inla.teste, by = c("ID"="Time")) %>% 
  left_join(RJ.hr %>% 
              sf::st_drop_geometry() %>% 
              select(code_health_region, name_health_region), 
            by = c("CO_REGSAUD"="code_health_region")) 



teste <- time.re.out %>% 
  ggplot(aes(x = Date, y = (`0.5quant`)^2, color = name_health_region)) +
  geom_line() + 
  geom_ribbon(aes(ymin = (`0.025quant`)^2, ymax = (`0.975quant`)^2, 
                  fill = name_health_region), 
              alpha = 0.3, show.legend = F, color = NA) +
  theme_bw(base_size = 16) + 
  labs(
    x = "Semana epidemiológica dos primeiros sintomas",
    y = " Hospitalizações por SRAG-COVID (p/ 100 mil hab.)",
    fill = "Região de saúde",
    color = "Região de saúde")


# incluindo o nome das variantes no grafico

grob <- grobTree(textGrob(c("B.1.1.33", "Zeta", "Gama", 
                            "Delta", "Omicron \n BA.1", 
                            "Omicron \n BA.2", "Omicron \n BA.5" ), 
                          x=c(0.12,.30, .41, .56, .69, .82, .94)-.05,  
                          y=0.9, hjust=0,
                          gp=gpar(col=c("blue", "purple","red", "orange","green","blue", "purple"), fontsize=8, fontface="bold")))



grafico <- teste +
  geom_vline(xintercept = onda_breaksteste, linetype = "dashed")+ 
  annotation_custom(grob) +
  facet_wrap(facets = ~name_health_region, ncol = 3)

grafico

# Sintaxe para exportação de imagens do `ggplot2`
ggsave(
  filename = "suavizadohospartigo.png",
  width = unit(15, "cm"),
  height = unit(10, "cm")
)


grafico <- teste +
  theme_void() +
  theme(panel.background = element_rect(fill = "light blue"))

# hospitalizacoes

tbl.picos <- dadosRJ.ew %>% 
  #filter(name_health_region == "Metropolitana I") %>% 
  group_by(name_health_region, Onda) %>% 
  summarise(
    #  maxRMI = dt_evento_w[which.max(covid)/],
    DT_pico = dt_evento_w[which.max(covid)],
    #DT_pico_2 = paste(epiweek(DT_pico),epiyear(DT_pico), sep = "/")
  ) 


# Casos particulares (periodos com onda e/ou pico nao definidos)

# Baia da Ilha Grande, Delta
tbl.picos$DT_pico[tbl.picos$name_health_region == "Baia da Ilha Grande" &
                    tbl.picos$Onda == "Delta"] <- NA_Date_

# Baixada Litorânea (Delta e B.1.1.33)
tbl.picos$DT_pico[tbl.picos$name_health_region == "Baixada Litorânea" &
                    tbl.picos$Onda == "B.1.1.33"] <- NA_Date_
tbl.picos$DT_pico[tbl.picos$name_health_region == "Baixada Litorânea" &
                    tbl.picos$Onda == "Delta"] <- NA_Date_
# Centro sul
tbl.picos$DT_pico[tbl.picos$name_health_region == "Centro-Sul" &
                    tbl.picos$Onda == "B.1.1.33"] <- NA_Date_

# Noroeste (Zeta e Delta)
tbl.picos$DT_pico[tbl.picos$name_health_region == "Noroeste" &
                    tbl.picos$Onda == "Zeta"] <- NA_Date_
tbl.picos$DT_pico[tbl.picos$name_health_region == "Noroeste" &
                    tbl.picos$Onda == "Delta"] <- NA_Date_

# Norte (Delta)
tbl.picos$DT_pico[tbl.picos$name_health_region == "Norte" &
                    tbl.picos$Onda == "Delta"] <- NA_Date_

# Serrana (B.1.1.33)
tbl.picos$DT_pico[tbl.picos$name_health_region == "Serrana" &
                    tbl.picos$Onda == "B.1.1.33"] <- NA_Date_

# máximo da Medio Paraiba na delta acontece antes do pico 
# substituindo pelo valor mais alto da onda
tbl.picos$DT_pico[tbl.picos$name_health_region == "Médio Paraíba" &
                    tbl.picos$Onda == "Delta"] <- ymd("2021-08-15")



##### Tempo a partir do pico da RMI - hospitalizacoes
tbl.picos <- tbl.picos %>% 
  group_by(Onda) %>% 
  mutate(
    DT_pico_RMI = DT_pico[name_health_region == "Metropolitana I"]
  ) %>% ungroup() %>%
  mutate(
    tempo = DT_pico - DT_pico_RMI
  ) 

# GRAFICOOOOOOOOOOOOOS

nomes <- unique(RJ.hr$name_health_region)
letras <- LETTERS[1:length(nomes)]
nomes2 <- paste(letras, nomes, sep=" - ")

# Todos juntos
ggplot(data = RJ.hr %>% 
         left_join(
           tbl.picos %>% 
             mutate(
               tempo = as.numeric(tempo)/7,
               tempo.d = cut(tempo,
                             breaks = c(-1,0,1,2,3,20),
                             labels = c("-1",
                                        "0",
                                        "1",
                                        "2",
                                        "3 ou mais"),
                             right = F),
               labels = factor(name_health_region, 
                               levels = nomes, 
                               labels = letras)
             )
         )) +
  geom_sf(mapping = aes(fill=tempo.d), 
          size=.25, 
          show.legend = T) +
  # geom_sf(data = RMI.hachurado, size = 1) +
  geom_label(aes(label = labels, geometry = geom),
             stat = "sf_coordinates", size= 2) +
  scale_fill_brewer(palette = "Blues", drop=F, na.value = "grey50") +
  labs(title="Regiões de saúde no RJ" , 
       #subtitle = "Variante predominante: Original B.1.33", 
       size=8, 
       fill = "Semanas após \n pico na Região \n Metropolitana I" ) + 
  theme_minimal(base_size = 10) +
  no_axis + 
  theme(legend.position = c(.0000000001,.5) , plot.title = element_text(hjust = 0.5)) + 
  facet_wrap(~Onda, nrow = 3)



# Cada onda separada
onda_da_vez = "Omicron BA.5"

ggplot(data = RJ.hr %>% 
         left_join(
           tbl.picos %>% 
             filter(Onda == onda_da_vez) %>% 
             mutate(
               tempo = as.numeric(tempo)/7,
               tempo.d = cut(tempo,
                             breaks = c(-1,0,1,2,3,20),
                             labels = c("-1",
                                        "0",
                                        "1",
                                        "2",
                                        "3 ou mais"),
                             right = F),
               labels = factor(Regiaodesaude, 
                               levels = nomes, 
                               labels = letras)
             )
         )) +
  geom_sf(mapping = aes(fill=tempo.d), 
          size=.25, 
          show.legend = T) +
  # geom_sf(data = RMI.hachurado, size = 1) +
  geom_label(aes(label = labels, geometry = geom),
             stat = "sf_coordinates", size = 4) +
  scale_fill_brewer(palette = "Blues", drop=F, na.value = "grey50") +
  labs(title="Regiões de saúde no ERJ", 
       subtitle = paste("Variante predominante:", onda_da_vez), 
       size=8, 
       fill = "Semanas após \n pico na RMI") +
  theme_map(base_size = 16) +
  no_axis + 
  theme(legend.position = c(.05,.5)) 