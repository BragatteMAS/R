## Correlation
## R 4.3.0
## Refactor @BragatteMAS
## Update: 2023-11-21

## Libraries
pacman::p_load(ggstatsplot)

## Data
head(mtcars)

## correlation test
cor.test(mtcars$wt, mtcars$mpg)

## plot
ggscatterstats(
  data = mtcars,
  x = wt,
  y = mpg,
  # add = "reg.line",
  # conf.int = TRUE,
  # cor.coef = TRUE,
  # cor.method = "pearson",
  title = "Correlation between wt and mpg",
  # xlab = "wt",
  # ylab = "mpg",
  bf.message = F,
  marginal = F
)
#########################################

pacman::p_load(ggpubr, rstatix, tidyverse)

# Data
the_test <- t_test(mag ~ depth, data = quakes, var.equal = TRUE)

# Plot
ggboxplot(
  quakes,
  x = "depth",
  y = "mag",
  color = "mag",
  add = "jitter"
) +
  labs(subtitle = get_test_label(the_test, detailed = T))

## REF
# [ggpubr: Publication Ready Plots](https://rpkgs.datanovia.com/ggpubr/reference/get_test_label9.html)
# [tiktok smooth learning](https://www.tiktok.com/@smooth.learning.c/video/7263654713416011010)