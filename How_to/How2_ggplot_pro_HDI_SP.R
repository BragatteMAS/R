# install_pkgs <- c("ggplot2", "ggthemes", "sf", "ggiraph", "dplyr", "patchwork")
# install.packages(install_pkgs, dependencies = TRUE)

library(ggplot2)
library(ggthemes)
library(sf)
library(ggiraph)
library(dplyr)
library(patchwork)

atlas <- readr::read_rds(
  "https://github.com/viniciusoike/restateinsight/raw/main/static/data/atlas_sp_hdi.rds"
)

pop_hdi <- atlas |>
  st_drop_geometry() |>
  mutate(
    group_hdi = findInterval(HDI, seq(0.65, 0.95, 0.05), left.open = FALSE),
    group_hdi = factor(group_hdi)
  ) |>
  group_by(group_hdi) |>
  summarise(score = sum(pop, na.rm = TRUE)) |>
  ungroup() |>
  mutate(share = score / sum(score) * 100) |>
  na.omit() |>
  mutate(
    y_text = if_else(group_hdi %in% c(0, 7), share + 3, share - 3),
    label = paste0(round(share, 1), "%"),
    data_id = as.character(group_hdi) # Add data_id to pop_hdi
  )

atlas <- atlas |>
  mutate(group_hdi = findInterval(HDI, seq(0.65, 0.95, 0.05), left.open = FALSE))

pmap <- ggplot(atlas) +
  geom_sf_interactive(aes(fill = HDI, data_id = group_hdi, tooltip = paste("HDI:", HDI)), lwd = 0.05, color = "white") +
  scale_fill_fermenter(
    name = "",
    breaks = seq(0.65, 0.95, 0.05),
    direction = 1,
    palette = "YlGnBu"
  ) +
  labs(
    title = "HDI in Sao Paulo, BR (2010)",
    subtitle = "Microregion HDI in Sao Paulo",
    caption = "Source: Atlas Brasil"
  ) +
  theme_map() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

x_labels <- c(
  "0.650 or less", "0.650 to 0.699", "0.700 to 0.749", "0.750 to 0.799",
  "0.800 to 0.849", "0.850 to 0.899", "0.900 to 0.949", "0.950 or more"
)

pcol <- ggplot(pop_hdi, aes(group_hdi, share, fill = group_hdi)) +
  geom_col_interactive(aes(data_id = data_id, tooltip = paste("Share:", label))) +
  geom_hline(yintercept = 0) +
  geom_text_interactive(
    aes(y = y_text, label = label, color = group_hdi, data_id = data_id),
    size = 2
  ) +
  coord_flip() +
  scale_x_discrete(labels = x_labels) +
  scale_fill_brewer(palette = "YlGnBu") +
  scale_color_manual(values = c(rep("black", 5), rep("white", 2), "black")) +
  guides(fill = "none", color = "none") +
  labs(
    title = "",
    x = NULL,
    y = NULL
  ) +
  theme_void() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(size = 8), # Reduced title size
    axis.text.y = element_text(size = 5), # Reduced y-axis text size
    axis.text.x = element_blank(),
    aspect.ratio = 1.5
  )

p_hdi_atlas <- pmap + pcol + plot_layout(widths = c(3, 1))
p_hdi_atlas <- pmap + inset_element(pcol, left = 0.5, bottom = 0, right = 1, top = 0.5)

interactive_plot <- girafe(
  ggobj = p_hdi_atlas,
  options = list(
    opts_hover(css = "fill:orange;"),
    opts_hover_inv(css = "opacity:0.5;"),
    opts_selection(type = "single", only_shiny = FALSE)
  )
)

htmltools::save_html(interactive_plot, "ggiraph-es.html")
