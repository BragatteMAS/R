# Nosso primeiro script do R
# Prof Marcos Vital, na Maratona de Análise de Biodiversidade
# Academia do R!
# Criado em 13-12-2021

# O R pode ser usado para operações matemáticas básicas, por exemplo:
10 + 10  # Soma
12 - 13  # Subtração
3 * 15   # Multiplicação
13 / 7   # Divisão

3 ^ 2    # Potência (no exemplo, 3 elevado a 2)

# Calculando uma raiz quadrada:
sqrt (15)

# Agora vamos calcular um log:
log (300, 10)   # No exemplo, log de 300 na base 10

# Todas as funções do R seguem o formato:
# função (argumento, argumento, argumento, ...)

# Criando um objeto no R:
biscoito <- 15
# Criamos um objeto chamado biscoito, e dentro dele armazenamos o número 15

# Uma vez que o objeto existe, ele pode ser "chamado"

biscoito
biscoito * 2
log (biscoito, 10)

###########################################
# Um pequeno exemplo

# Vamos carregar dados de exemplo:
data (iris)   # Carregando os dados de exemplo chamados iris

# Comprimento médio de pétalas das flores:
mean (iris$Petal.Length)

# Histograma da variação de comprimento de pétalas
hist (iris$Petal.Length, xlab = "Comprimento das pétalas", ylab="Frequência", main=" ")

# xlab e ylab determinam nomes dos eixos
# main determina o título acima do gráfico