#Butantan teórico
voluntários = 12476
grupo_placebo = (voluntários / 2) #6238
grupo_vacinado = (voluntários / 2) #6238
infectados_voluntários = 218
infectados_vacinados = 58
infectados_placebo = 160
infectados_vacinados_grupo3 = 22 # dos 58 totais = número fictício para explicação
infectados_placebo_grupo3 = 100 # dos 160 totais = número fictício para explicação

library(tibble)
butantan <-
    tibble(
        y = c(infectados_vacinados, infectados_placebo),
        n = c(6238, 6238),
        x = c(1, 0)
    )
butantan_grupo3 <-
    tibble(
        y = c(infectados_vacinados_grupo3, infectados_placebo_grupo3),
        n = c(6238, 6238),
        x = c(1, 0)
    )
# Ajustando o modelo
modelo_butantan <-
    (glm(cbind(y, n - y) ~ x, family = binomial(link = log), data = butantan))
modelo_butanan_grupo3 <-
    (glm(cbind(y, n - y) ~ x, family = binomial(link = log), data = butantan_grupo3))
# Estimativas para RR (Explicação Léo Bastos no link)
1 - exp(cbind(coef(modelo_butantan), confint(modelo_butantan))[2, c(1, 3, 2)])
1 - exp(cbind(coef(modelo_butanan_grupo3), confint(modelo_butanan_grupo3))[2, c(1, 3, 2)])
