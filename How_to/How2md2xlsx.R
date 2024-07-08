# Carregar os pacotes necessários
# install.packages("readr")
# install.packages("openxlsx")
library(readr)
library(openxlsx)

# Definir o caminho do arquivo Markdown e o caminho de saída para o arquivo Excel
markdown_file <- "Users/bragatte/Documents/GitHub/SAVEd_RUE_APS/_tests/db_gal2rnds_itps.md"
output_excel_file <- "Users/bragatte/Downloads/db_gal2rnds_itps.xlsx"

# Verificar se o arquivo Markdown existe
if (!file.exists(markdown_file)) {
  stop(paste("Arquivo não encontrado:", markdown_file))
}

# Ler a tabela do arquivo Markdown
# Assumindo que a tabela Markdown está entre os delimitadores de código de tabela ```markdown e ```
lines <- readLines(markdown_file)
table_lines <- lines[grepl("^\\|", lines)]

# Converter a tabela Markdown para um data frame
table_data <- read.delim(text = paste(table_lines, collapse = "\n"), sep = "|", header = TRUE, stringsAsFactors = FALSE, skip = 1, strip.white = TRUE)

# Remover a primeira coluna que é vazia (resultante do delimitador | no início da linha)
table_data <- table_data[,-1]

# Remover a última coluna que é vazia (resultante do delimitador | no final da linha)
table_data <- table_data[,-ncol(table_data)]

# Escrever o data frame em um arquivo Excel
write.xlsx(table_data, output_excel_file)