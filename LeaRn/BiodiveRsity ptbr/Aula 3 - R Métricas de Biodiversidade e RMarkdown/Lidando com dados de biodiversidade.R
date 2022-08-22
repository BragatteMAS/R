# Lidando com dados de Biodiversidade
# Prof Marcos Vital, para Maratona de análise de Biodiversidade da Academia do R
# Script criado em 14-12-2021

# Carregando o pacote necessário para a aula (não se esqueça de ter instalado ele antes)
library (vegan)

# Carregando os dados de exemplo de hoje:
data (dune)

# Se quiser, chame a ajuda do R para saber mais:
? dune
# Este sistema de ajuda funciona com TODAS AS FUNÇÕES DO R

###############################3

# Quantas espécies eu tenho em cada uma das 20 parcelas?
specnumber (dune)

# Qual a média de espécies por parcela?
riqueza <- specnumber (dune)
mean (riqueza)  # A riqueza média por parcela
sd (riqueza)    # O desvio padrão desta média

max (riqueza)  # Maior valor de riqueza
min (riqueza)  # Menor valor de riqueza

# Quais parcelas tem mais de 10 espécies?
riqueza [which (riqueza>10) ] 

# Quais parcelas tem menos de 8 espécies?
riqueza [which (riqueza < 8) ]


###################################

# Vamos calcular o total de indivíduos para cada espécie?
total.individuos <- colSums (dune)
total.individuos

# Vamos colocar em ordem decrescente:
total.individuos.ordem <- sort (total.individuos, decreasing = T)
total.individuos.ordem

# Agora vamos colocar isso em um gráfico: o diagrama de rank-abundância:
plot (total.individuos.ordem, xlab="Espécies (em ordem decrescente)", ylab="Total de indivíduos", las=1, type="l", lty=2)
points (total.individuos.ordem, pch=1)


##################################

# Vamos calcular a diversidade de Shannon para cada parcela:
diversidade <- diversity (dune, index="shannon")

# Vamos ver os valores:
diversidade

# Valor médio e desvio padrão:
mean (diversidade)
sd (diversidade)

# E o valor total de diversidade, como seria?
diversity (total.individuos, index="shannon")

#############################

# Vamos calcular uma curva de acúmulo pelo método de rarefação e de quebra calcular uns estimadores de riqueza

# Criando a curva de acúmulo:
acumulo <- specaccum (dune)

# Gráfico rápido: 
plot (acumulo)

# Sempre confira os valores:
acumulo

# Alguns estimadores "clássicos" de riqueza de espécies:
specpool (dune)