#Butantan Eficácia aproximações by Bragatte  created 202101141717
packs = c("epiR", "tibble")
lapply(packs, install.packages, character.only = TRUE)
lapply(packs, require, character.only = TRUE)
### Dados
voluntários = 12508
participantes = 9242 #selecionados para estudo dos 12508
Placebo = 4599
Vacinado = 4653
placebo_infectados = 205
vacinado_infectados = 92

score4a6 = 7 #(moderado e grave) 7 + 0 eventos
score3a6 = 38 #(moderado,grave e leve) 31 + 7= 38 eventos
score2a6 = 252 #(moderado,grave,leve e muito leve) 167 + 85= 252 eventos

infectados_vacinados_4a6 = 0
infectados_placebo_4a6 = 7
infectados_vacinados_3a6 = 7
infectados_placebo_3a6 = 31
infectados_vacinados_2a6 = 85
infectados_placebo_2a6 = 167

### Cálculo livros de epidemio sem considerar tempo, apenas razão/proporção entre os grupos
efic = 1 - ((92 / 4653) / (205 / 4599))
efic #=0.5564

efit = 1 - ((92 * (4599 - 205)) / (205 * (4653 - 92)))
efit #= 0.5677

### Cálculo Léo Bastos sem Hazard Ratio
library(tibble)
butantan_4a6 <-
    tibble(
        y = c(infectados_vacinados_4a6, infectados_placebo_4a6),
        n = c(Vacinado, Placebo),
        x = c(1, 0)
    )
butantan_3a6 <-
    tibble(
        y = c(infectados_vacinados_3a6, infectados_placebo_3a6),
        n = c(Vacinado, Placebo),
        x = c(1, 0)
    )
butantan_2a6 <-
    tibble(
        y = c(infectados_vacinados_2a6, infectados_placebo_2a6),
        n = c(Vacinado, Placebo),
        x = c(1, 0)
    )
# Ajustando o modelo
modelo_butantan_4a6 <-
    (glm(cbind(y, n - y) ~ x, family = binomial(link = log), data = butantan_4a6))

modelo_butantan_3a6 <-
    (glm(cbind(y, n - y) ~ x, family = binomial(link = log), data = butantan_3a6))

modelo_butantan_2a6 <-
    (glm(cbind(y, n - y) ~ x, family = binomial(link = log), data = butantan_2a6))

# Estimativas para RR
1 - exp(cbind(coef(modelo_butantan_4a6), confint(modelo_butantan_4a6))[2, c(1, 3, 2)]) #valor zero inadequado
1 - exp(cbind(coef(modelo_butantan_3a6), confint(modelo_butantan_3a6))[2, c(1, 3, 2)])
1 - exp(cbind(coef(modelo_butantan_2a6), confint(modelo_butantan_2a6))[2, c(1, 3, 2)])

### Cálculo Otávio considerando Hazard Ratio e contribuição Rafa Lopes código abertos twitter
#Protocolo 1 - Hazard Ratio => HR (modelo Cox (estratificações como idade e sexo = não temos dados))
## Eficácia Primária ##
taxa_rr_1 <-
    as.table(matrix(
        c(
            infectados_vacinados_2a6,
            infectados_vacinados_2a6 / 11.74,
            infectados_placebo_2a6,
            infectados_placebo_2a6 / 23.64
        ),
        nrow = 2,
        byrow = TRUE
    ))
riscorelativo_rr <-
    epi.2by2(
        dat = rate_rr_1,
        method = "cohort.time",
        conf.level = 0.95,
        units = 100,
        outcome = "as.columns"
    )
ev_riscorelativo <-
    1 -  riscorelativo_rr[["massoc"]][["IRR.strata.wald"]] #ev=eficácia da vacina
ev_riscorelativo

## Eficácia Secundária ##
taxa_rr_2 <-
    as.table(matrix(
        c(
            infectados_vacinados_3a6,
            infectados_vacinados_3a6 / 0.97,
            infectados_placebo_3a6,
            infectados_placebo_3a6 / 4.39
        ),
        nrow = 2,
        byrow = TRUE
    ))
riscorelativo_rr_2 <-
    epi.2by2(
        dat = rate_rr_2,
        method = "cohort.time",
        conf.level = 0.95,
        units = 100,
        outcome = "as.columns"
    )
ev_riscorelativo_2 <-
    1 - riscorelativo_rr_2[["massoc"]][["IRR.strata.wald"]] #ev=eficácia da vacina
ev_riscorelativo_2
