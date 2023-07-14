# Projeto: Dengue no ES em 2022
# Script criado em 11-02022, em R 4.2.1
# Marcos V C Vital

# Lendo os dados:
dados <- read.csv ("Dengue_ES_2022.csv", sep=",", dec=".", stringsAsFactors = T, fileEncoding = "ISO-8859-1")

Colatina <- dados [which (dados$MUNICÍPIO == "COLATINA") , 3:19]

Colatina.t <- t (Colatina)

plot (Colatina.t, type="l", xlab="Semana epidemiológica", ylab="Número de casos de Dengue", main = "Colatina", las=1)

##########
# Até aqui é a parte essencial da atividade
# Faça para um município da sua escolha
# Se quiser, faça a sua versão em RMarkdown ou Quarto
# E se quiser, continue a seguir comigo!
#########

# Vamos mudar um pouco o estilo do gráfico:

plot (Colatina.t, type="l", xlab="Semana epidemiológica", ylab="Número de casos de Dengue", main = "Colatina", las=1, lty=2)
points (Colatina.t, pch=21, bg="gray")

# Vamos comparar dois municípios

Guarapari <- dados [which (dados$MUNICÍPIO == "GUARAPARI") , 3:19]

Guarapari.t <- t (Guarapari)

# Gráfico apenas para Guarapari:
plot (Guarapari.t, type="l", xlab="Semana epidemiológica", ylab="Número de casos de Dengue", main = "Guarapari", las=1, lty=2)
points (Guarapari.t, pch=21, bg="gray")

# Vamos calcular os valores de incidiência:

habitantes.guarapari <- dados [which (dados$MUNICÍPIO == "GUARAPARI") , 20]
habitantes.colatina <- dados [which (dados$MUNICÍPIO == "COLATINA") , 20]

Guarapari.t.inc <- (Guarapari.t / habitantes.guarapari) * 100000
Colatina.t.inc <- (Colatina.t / habitantes.colatina) * 100000

# Vamos criar o gráfico:

plot (Colatina.t.inc, type="l", las=1, xlab = "Semana Epidemiológica", ylab = "Incidência de Dengue", lty=2, col = "red4")
points (Colatina.t.inc, pch=21, col="red4", bg="gray")

lines (Guarapari.t.inc, lty=2, col="green4")
points (Guarapari.t.inc, pch=21, col="green4", bg="gray")

legend ("topleft", legend = c ("Colatina", "Guarapari"), pch = 21, col=c("red4", "green4"),lty=2)
