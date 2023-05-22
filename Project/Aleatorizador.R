# Análise de Dados para a Qualificação da Gestão na Saúde - Módulo 2
# "Alterador" de dados, conjunto de dados de 'câncer de pâncreas'
# Marcos V. C. Vital
# Script criado em R 4.2.1

# Salvei os dados do arquivo do excel no formato txt, basta ler:
dados <- read.table ("dados.txt", h=T)
summary (dados)

# Nesta nova versão, vamos usar os nomes dos alunos:
nomes <- read.csv ("nomes.csv", fileEncoding = "ISO-8859-1")

# A seguir, vamos alterar ao acaso valores de 4 variáveis: sexo, cafe, cigarro e cancerpancreas
# Vou gerar valores de 0 e 1 ao acaso usando a função rbinom
# Para sexo e cafe vai ser um "cara ou coroa", 50% de chance de 0 ou 1
# Para cigaro tem 25% de dar valor 1
# Para cancerpancreas tem 5% de chance de dar valor 1

# Tudo vai em um loop, que cria uma list de dataframes do tamanho da turma (a lista de nomes é o parâmetro)

# Criando a lista vazia para receber os valores:
lista <- list ()

# Rodando o loop. 
for (i in 1:length (nomes$Nomes)) {
  mod.sexo <- rbinom (n = 200, size = 1, prob = 0.5)
  dados [101:300, 2] <- mod.sexo
  mod.cafe <- rbinom (n = 200, size = 1, prob = 0.5)
  dados [101:300, 4] <- mod.cafe
  mod.cigarro <- rbinom (n = 200, size = 1, prob = 0.25)
  dados [101:300, 5] <- mod.cigarro
  mod.pancreas <- rbinom (n = 200, size = 1, prob = 0.05)
  dados [101:300, 6] <- mod.pancreas
  lista [[i]] <- dados
}

# Agora vamos gerar arquivos csv de saída, será um novo loop
# Ele vai gerar n arquivos, sendo n o número de data.frames na lista acima
# Os arquivos serão nomeados usando os nomes completos dos participantes do arquivo lido lá no começo

# Para funcionar, crie uma pasta chamada 'arquivos' dentro do diretório de trabalho atual (ou modifique o nome da pasta dentro do loop a seguir para o desejado).

for (i in 1:length (lista)) {
  write.csv (lista[[i]], file = paste ("arquivos/", nomes[i, 1], ".csv", sep = ""), row.names = F) 
}

# E pronto, temos, com este exemplo, 50 variantes dos dados. Cada uma tem uma aleatorização nas linhas 101 a 150, o que muda pouco os resultados mas faz com que cada resultado exato seja único.
# É possível (e bem fácil) estender estas modificações para mais linhas se desejado

