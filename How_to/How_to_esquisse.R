packs = c("ggplot", "ploty", "esquisse")
lapply(packs, install.packages, character.only = TRUE)
lapply(packs, require, character.only = TRUE)

library(esquisse)
library(ggplot2)
library(plotly)
esquisse::esquisser()