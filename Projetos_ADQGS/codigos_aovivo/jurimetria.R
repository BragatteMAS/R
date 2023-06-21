## carregar libs
pacman::p_load(vroom)

## chamar dados
# Definir os nomes das colunas
col_names <- c("Número do DUA", "CNPJ", "Órgão", "Área", "Serviço", "Código Receita", "Município", "Valor do Pagamento", "Código do Banco", "Agência", "Data do Pagamento", "Informações Complementares")

# Ler o arquivo usando vroom
data <- vroom("codigos_aovivo/dua_junho.txt", delim = "  ", col_names = col_names, comment = "***")


# Visualizar os dados
print(data)

### read.csv
dua_junho <- read.csv("~/Documents/GitHub/R/Projetos_ADQGS/codigos_aovivo/dua_junho.txt", encoding="UTF-8", header=FALSE, row.names=NULL, comment.char="#", stringsAsFactors=TRUE)

View(dua_junho)

### parametros
#### Definir os nomes das colunas
col_nomes <- c("Número do DUA", "CNPJ", "Órgão", "Área", "Serviço", "Código Receita", "Município", "Valor do Pagamento", "Código do Banco", "Agência" , "Data do Pagamento", "Informações Complementares")

#################################
# Definir os nomes das colunas
col_names <- c("Número do DUA", "CNPJ", "Órgão", "Área", "Serviço", "Código Receita", "Município", "Valor do Pagamento", "Código do Banco", "Agência", "Data do Pagamento", "Informações Complementares")

# Ler o arquivo usando read.table ou read.csv
data <- read.table("codigos_aovivo/dua_junho.txt", sep = ",", quote = "", comment.char = "***", skip = 1, na.strings = "", strip.white = TRUE, col.names = col_names, stringsAsFactors = FALSE, fill = TRUE)

# Remover as colunas vazias adicionais (se necessário)
data <- data[, colSums(is.na(data)) < nrow(data)]

# Visualizar os dados
print(data)


########################







data <- read.fwf("codigos_aovivo/dua_junho.txt", col.names = col_nomes)

df <- readLines('codigos_aovivo/dua_junho.txt') 
df <- gsub("[", "", df, fixed = T)
# change ], for newlines
df <- gsub("],", "\n", df, fixed = T)
df <- gsub("]", "\n", df, fixed = T)
df <- read.table(textConnection(df), sep = ",")
df


# Ler o arquivo usando read.fwf
data <- read.fwf("codigos_aovivo/dua_junho.txt", widths = widths, strip.white = TRUE, stringsAsFactors = FALSE)

# Definir nomes para as colunas
colnames(data) <- c("Campo1", "Campo2", "Campo3", "Campo4", "Campo5", "Campo6", "Campo7", "Campo8", "Campo9", "Campo10", "Campo11", "Campo12")

# Visualizar os dados
print(data)
