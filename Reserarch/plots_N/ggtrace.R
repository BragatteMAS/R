# ggtrace example application
##R 4.2.1
##@BragatteMAS

### install packages and libraries
packs <-
  c("ggplot2", "ggtrace", "tidytable", "palmerpenguins", "httpgd")
#lapply(packs, install.packages, character.only = T)
lapply(packs, require, character.only = T)

### httpgd
hgd() #tolken
hgd_browse() #browser view

## Penguins dataset
penguins_sm <- palmerpenguins::penguins |>
  select.(species, bill_length_mm) |>
  filter.(!is.na(bill_length_mm))
penguins_sm

## Viz
### ggplot standard
penguins_plot <-
  #data
  ggplot(penguins_sm) +
  # mapa / geom
  geom_bar (aes(x = species, fill = species))
penguins_plot

## ggtrace
penguins_sm
### Before Stat
# ggtrace_inspect_args(
#   x = penguins_plot,
#   method = ggplot2:::Layer$compute_statistic)
penguins_plot_t <- 
  geom_label(
    aes(
      x = condition,
      y = stage(start = response_time, after_stat = ymax),
      label = after_stat(ymax)
      ),
    stat = "boxplot",
    data = ~ .x |> filter(condition == "B")
)
