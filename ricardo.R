pacman::p_load(tidyverse)

###
# semestre <-
    vroom("Downloads/controle_semestral_2022.csv",
          delim = ";",
          locale = "ISO-8859-1")
    
LD = LIMITE DE DETECÇÃO
LQ = LIMITE DE QUANTIFICAÇÃO

###
library(readr)
controle_semestral_2022 <-
    read_delim(
        "Downloads/controle_semestral_2022.csv",
        delim = ";",
        escape_double = FALSE,
        locale = locale(date_names = "br",
                        encoding = "ISO-8859-1"),
        trim_ws = TRUE
    )
View(controle_semestral_2022)