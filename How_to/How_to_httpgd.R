# How to HTTPGD
# R 4.2.1
# Refactor @BragatteMAS

#Libraries
packs = c("httpgd","devtools", "ggplot2")
lapply(packs, install.packages, character.only = TRUE)
lapply(packs, require, character.only = TRUE)

# HTTPGD viz
## Source: vignettes/a01_how-to-get-started.Rmd
hgd()
## generate url link - copy and paste in the browser
hgd_browse()

### Plot anything
x = seq(0, 3 * pi, by = 0.1)
plot(x, sin(x), type = "l")

### Every plotting library will work.
library(ggplot2)
ggplot(mpg, aes(displ, hwy, colour = class)) +
  geom_point()

### Stop server
dev.off()

# Ref
## [Getting started • httpgd](https://nx10.github.io/httpgd/articles/a01_how-to-get-started.html)