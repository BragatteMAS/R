# Install and load the 'pacman' package if not already installed
if (!require(pacman)) {
  "install.packagespacman"
  library(pacman)
}
## TODO: CHECK LOOP

# Install and load the tidymodels packages
pacman::p_load(usethis, cli, crayon, rlang,roxygen2,pkgload, recipes, parip, yardstick, workflows, broom)

# Install and load the tidyverse packages
pacman::p_load(readr, ggplot2, dplyr, tidyr, plotly)


## Carregar o dataset mtcars
data(mtcars)

## Agrupar por número de cilindros e calcular as médias de mpg, hp e wt
mtcars_summary <- mtcars %>%
    group_by(cyl) %>%
    summarize(
        mean_mpg = mean(mpg),
        ## Calcular a média de mpg
        mean_hp = mean(hp),
        ## Calcular a média de hp
        mean_wt = mean(wt)      ## Calcular a média de wt
    )

## FIXME: NEW PLOT
p <- ggplot(mtcars_summary, aes(x = factor(cyl))) +
    geom_bar(aes(y = mean_mpg, fill = "mean_mpg"),
             stat = "identity",
             position = "dodge") +
    geom_bar(aes(y = mean_hp, fill = "mean_hp"),
             stat = "identity",
             position = "dodge") +
    geom_bar(aes(y = mean_wt, fill = "mean_wt"),
             stat = "identity",
             position = "dodge") +
    labs(x = "Number of Cylinders", y = "Mean Values", title = "Mean MPG, HP, and WT by Cylinder") +
    scale_fill_manual(
        name = "Metric",
        values = c(
            "mean_mpg" = "blue",
            "mean_hp" = "green",
            "mean_wt" = "red"
        )
    ) +
    theme_minimal()

ggplotly(p)
