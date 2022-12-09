## Script para fazer um gif
## R 4.2.1
## @BragatteMAS


## instalando pacotes necessários - necessário acesso a internet
install.packages("renderthis", dependencies = TRUE)
remotes::install_github('rstudio/chromote')
remotes::install_github('rstudio/pagedown')
remotes::install_github("rstudio/chromote")



## chamar as bibliotecas
pacotes <-
    c("renderthis",
      "pagedown",
      "chromote",
      "pdftools")

lapply(pacotes, require, character.only = TRUE)


## descobrir qual o caminho para o navegador chrome - se usa outros navegadores checar documentação
#browseVignettes("renderthis")

pagedown::find_chrome() ##copiar caminho do terminal


## colar caminho do terminal nas linhas abaixo no lugar do que esta entre "" para definir caminho do chrome
Sys.setenv(PAGEDOWN_CHROME = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")
Sys.setenv(CHROMOTE_CHROME = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")

## alterar para o caminho e arquivo que queres transformar
## gerar pdf
to_pdf(from = "Proposta 05 - MediaMovel Interativa/Proposta 05.qmd")
## gerar gif
to_gif(from = "Proposta 05 - MediaMovel Interativa/Proposta 05.qmd")
