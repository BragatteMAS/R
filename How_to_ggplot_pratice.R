#R Estatística Descritiva 
#<https://www.youtube.com/watch?v=BDu4VF_1Eug&feature=em-lbcastemail>
#Variáveis quantitativas

pacotes <- c('ggplot2', 'dplyr', 'scales', 'tidyverse', 'plotly','gridExtra','kableExtra')

install.packages(pacotes)


lapply(pacotes, require, character.only = TRUE)

view(iris)
dados=iris

#Histogram plot (comportamento da distribuição simétrica ou assimétrica)
grafico_hist = ggplot(dados, aes(x=Sepal.Length)) +
  geom_histogram(color = "black",fill='lightblue', bins = 30) +
  ggtitle('Gráfico histograma do tamanho da Sépala das Plantas') +
  xlab('Tamanho das Sépalas')+
  ylab('Frequência simples')+
  theme()

grafico_hist
ggplotly(grafico_hist)

#Box-plot (identificação de outliers)
grafico_boxplot = ggplot(dados, aes(x=Sepal.Length, y=Petal.Length, fill = Species)) +
  geom_boxplot()+
  ggtitle('Gráfico de Box-Plot do tamanho da Sépala vs tamanho das Pétatas') +
  xlab('Tamanho das Sépalas')+
  ylab('Tamanho das Pépalas')

grafico_boxplot
ggplotly(grafico_boxplot)
  
# Dispersão (Associação linear entre variáveis)
grafico_dispersao_cor = ggplot(dados, aes(x=Sepal.Length, y=Petal.Length, col=Species)) +
  geom_point(size=4)+
  ggtitle('Gráfico de Dispersão tamanho fixos da Sépala vs tamanho das Pétatas') +
  xlab('Tamanho das Sépalas')+
  ylab('Tamanho das Pépalas')+
  facet_wrap(~Species) #separa entre variáveis
  
grafico_dispersao_cor
ggplotly(grafico_dispersao_cor)

# Dispersão tamanhos proporcionais por especie (Associação linear entre variáveis)
grafico_dispersao_especies = ggplot(dados, aes(x=Sepal.Length, y=Petal.Length, col=Species)) +
  geom_point(size=dados$Petal.Length)+ #redirecionar para dataframe com $
  ggtitle('Gráfico de Dispersão por tamanho da Sépala vs tamanho das Pétatas') +
  xlab('Tamanho das Sépalas')+
  ylab('Tamanho das Pépalas')+
  facet_wrap(~Species) #separa entre variáveis

grafico_dispersao_especies
ggplotly(grafico_dispersao_especies)

# Dispersão unido (Associação linear entre variáveis)
grafico_dispersao_tamanho = ggplot(dados, aes(x=Sepal.Length, y=Petal.Length, col=Species)) +
  geom_point(size=dados$Petal.Length)+ #redirecionar para dataframe com $
  ggtitle('Gráfico de Dispersão do tamanho da Sépala vs tamanho das Pétatas') +
  xlab('Tamanho das Sépalas')+
  ylab('Tamanho das Pépalas')#+
  #facet_wrap(~Species) #separa entre variáveis

grafico_dispersao_tamanho
ggplotly(grafico_dispersao_tamanho)

# Grafico de linhas
dados2 = economics
grafico_linhas = ggplot(dados2, aes(x=date, y=unemploy))+
  geom_line(col='red')+ 
  ggtitle('Desemprego nos EUA 1967 a 2015') +
  xlab('Ano')+
  ylab('Desemprego (milhares)')

grafico_linhas
ggplotly(grafico_linhas)

# Agrupar gráficos NÃO dinâmicos
grid.arrange(
  grafico_hist,
  grafico_boxplot,
  grafico_dispersao_cor,
  grafico_dispersao_especies,
  grafico_dispersao_tamanho,
  grafico_linhas,
  nrow=3,ncol=2
  )

# Grafico de linhas horizontal
dados2 = economics
grafico_linhasH = ggplot(dados2, aes(x=date, y=unemploy))+
  geom_line(col='red')+
  geom_hline(yintercept = c(6000,10000)) + #, horizontal=T)
  ggtitle('Desemprego nos EUA 1967 a 2015') +
  xlab('Ano')+
  ylab('Desemprego (milhares)')
  facet_grid()

grafico_linhasH
ggplotly(grafico_linhasH)

#Adicionadas linhas de quartis para limitar 50% dos dados #mutate adiciona varáveis sem alterar originais
dados2 = economics
dados2 = dados2 %>% mutate(estados=case_when(unemploy > mean(unemploy) ~ "estado_acima_a_média",
                                            unemploy <= mean(unemploy) ~ "estado_abaixou_igual_a_média")
                           )

table(dados2$estados)

q1 = quantile(dados2$unemploy,0.25)

q3 = quantile(dados2$unemploy,0.75)

grafico_linhas = ggplot(dados2, aes(x=date, y=unemploy))+
  geom_line(col='red')+
  geom_hline(yintercept = c(q1[[1]],q3[[1]]))+
  ggtitle('Desemprego nos EUA 1967 a 2015') +
  xlab('Ano')+
  ylab('Desemprego (milhares)')+
  facet_grid(~estados)
             
grafico_linhas
ggplotly(grafico_linhas)

#Desvios 1=64% / 2=95% / 3=99,5% para distribuições normais
#Média menos 1 desvio / Segunda opção para gerar linhas
l1 = mean(dados2$unemploy)-2*sd(dados2$unemploy)

#Média mais 1 desvio
l2 = mean(dados2$unemploy)+2*sd(dados2$unemploy)

grafico_linhas = ggplot(dados2, aes(x=date, y=unemploy))+
  geom_line(col='red')+
  geom_hline(yintercept = c(l1,l2))+
  ggtitle('Desemprego nos EUA 1967 a 2015') +
  xlab('Ano')+
  ylab('Desemprego (milhares)')+
  facet_grid(~estados)

grafico_linhas
ggplotly(grafico_linhas)

#Variável categórica por quartis

q1 = quantile(dados2$unemploy,0.25)

q2 = quantile(dados2$unemploy,0.5)

q3 = quantile(dados2$unemploy,0.75)

maximo = max(dados2$unemploy)

dados2$unemploy_classes=
  cut(dados2$unemploy, breaks = c(q1,q2,q3,maximo),
      labels = c("Q1-|Q2", "Q2-|Q3", "Q3-|max"),
      include.lowest = F)

table(dados2$unemploy_classes)

