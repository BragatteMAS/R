install.packages("plotly")

library(plotly)

fig <- plot_ly(Fe_Zn_Al, x=~Especie, y=~Fe, type='bar', name = "Ferro")
fig <- fig %>% add_trace(y=~Zn, name = "Zinco")
fig <- fig %>% layout(yaxis = list(title="Count"), barmode="group")
fig


fig <- plot_ly(Fe_Zn_Al, x=~Especie, y=~Fe, type='bar', name = "Ferro")
fig <- fig %>% add_trace(y=~Zn, name = "Zinco")
fig <- fig %>% layout(yaxis = list(title="Count"), barmode="stack")
fig