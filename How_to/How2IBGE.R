## load packages
pacman::p_load(geobr,sf, ggplot2, viridis, dplyr)

## load data Brazil
br = read_country()

## load data States
states = read_state()

## check variables
head(br)
head(states)

## read state code
sp = read_state(code_state = "SP")
sudeste = filter(states, name_region == "Sudeste")

## capitals
capitals = read_capitals()

## plot
ggplot() +
  geom_sf(data = br) +
  geom_sf(data = states, fill = NA, color = "black", size = 0.5) +
  geom_sf(data = sp, fill = "red", color = "black", size = 0.5) +
  geom_sf(data = sudeste, fill = "blue", color = "black", size = 0.5) +
  theme_void() +
  labs(title = "Brazilian States",
       subtitle = "Source: IBGE",
       caption = "Data: IBGE") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5))
