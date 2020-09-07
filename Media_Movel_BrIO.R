
#install
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("devtools")
install.packages("plotly")
install.packages("ggpubr")
install.packages("esquisse")
--------------------------------------------------------------------------------------------------------------------------------------------------
 
#libraries
library("dplyr")
library("tidyr")
library("ggplot2")
library("devtools")
library("plotly")
library("ggpubr")
library("esquisse")
--------------------------------------------------------------------------------------------------------------------------------------------------

#Dataset
con <- gzcon(url(paste("https://data.brasil.io/dataset/covid19/caso_full.csv.gz", sep=",")))
txt <- readLines(con)
dados <- read.csv(textConnection(txt))

# relevant columns
colnames(dados)[8] <-'confirmed'
colnames(dados)[12] <-'deaths'
colnames(dados)[9] <- 'confirmed_per_100k'

# Adding deaths per 100k '''mutate = adds new variables and preserves existing'''
dados <- dados %>%
  mutate(deaths_per_100k= (100000*deaths)/estimated_population_2019) 

## Brasil filter by "state" and sort by "date"
dados_brasil_total <- dados %>%
  filter(place_type=="state") %>%
  arrange(date)

### Groupby choosen columns with sum
dados_brasil <- group_by(dados_brasil_total,date)%>%
  summarise(confirmed=sum(confirmed),deaths=sum(deaths),
            new_confirmed=sum(new_confirmed), new_deaths=sum(new_deaths),
            confirmed_per_100k=sum(confirmed_per_100k), deaths_per_100k=sum(deaths_per_100k))

### Select Date info
dados_brasil$date <- as.Date(dados_brasil$date)

###Media Móvel 7 dias
dados_brasil <- dados_brasil%>%
  mutate(ma7_confirmed = stats::filter(dados_brasil$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_brasil$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))

#### cases
dados_brasil$ma7_confirmed <- as.numeric(dados_brasil$ma7_confirmed)

## deaths
dados_brasil$ma7_deaths <- as.numeric(dados_brasil$ma7_deaths)

###Média móvel
#Filter ALagoas state
dados_al <- dados %>%
  filter(place_type=="state", state =="AL") 

#### "convolution = moving average, side 1 = coefficients are for past values
dados_al <- dados_al%>%
  mutate(ma7_confirmed = stats::filter(dados_al$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_al$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))

-------------------------------------------------------------------------------------------------------------------------------------------------
#[Ref](https://github.com/BragatteMAS/COVID19/blob/master/COVID_shiny.r)
# rlang::last_error() ## check errors
-------------------------------------------------------------------------------------------------------------------------------------------------
### Plot new cases COVID
ggplot(dados_brasil) +
  aes(x = date, colour = new_deaths, weight = new_confirmed) +
  geom_bar(fill = "#0c4c8a") +
  scale_color_gradient() +
  theme_minimal()+
  legend(x=date, y= new_deaths, legend=c())
### Plot mean average line
ggplot(dados_brasil) +
  aes(x = date, y = ma7_confirmed) +
  geom_line(size = 1.82, colour = "#ef562d") +
  theme_minimal()
-------------------------------------------------------------------------------------------------------------------------------------------------
#esquisser(viewer = "browser")
-------------------------------------------------------------------------------------------------------------------------------------------------
#Cases and Mean average
casos <- ggplot() + 
  geom_bar(data = dados_brasil, aes(x=date, y=new_confirmed), stat = 'identity', fill = "#0c4c8a") +
  geom_line(data = dados_brasil, aes(x=date, y=ma7_confirmed),size = 1.82, colour = "#d67d2a") +
  ggtitle("Pandemia em Alagoas")+
  labs(x="", y="Casos")
  #ggtitle("Novos casos confirmados e média móvel de Alagoas")+
  #labs(x="Tempo de pandemia", y = "Casos confirmados de COVID-19")

#Filter 31 days cases
filter <- dados_brasil %>%
  filter(date >= "2020-08-03" & date <= "2020-09-03")
casos31 <- ggplot() + 
  geom_bar(data = filter, aes(x=date, y=new_confirmed), stat = 'identity', fill = "#0c4c8a") +
  geom_line(data = filter, aes(x=date, y=ma7_confirmed),size = 1.82, colour = "#d67d2a") +
  ggtitle("Últimos 31 dias")+
  labs(x="", y="")
#  ggtitle("Novos casos confirmados e média móvel de Alagoas últimos 31 dias")+
#  labs(x="Tempo de pandemia", y = "Casos confirmados de COVID-19")
-------------------------------------------------------------------------------------------------------------------------------------------------
#Deaths and Mean average
obitos <- ggplot() + 
  geom_bar(data = dados_brasil, aes(x=date, y=new_deaths), stat = 'identity', fill = "#0c4c8a") +
  geom_line(data = dados_brasil, aes(x=date, y=ma7_deaths),size = 1.82, colour = "#FF0000") +
  labs(y="Óbitos") +
  labs(x="")
#  ggtitle("Novos óbitos confirmados e média móvel de Alagoas")+
#  labs(x="Tempo de pandemia", y = "Óbitos confirmados de COVID-19")

#Filter 31 days deaths
filter_obitos <- dados_brasil %>%
  filter(date >= "2020-08-03" & date <= "2020-09-03")
obitos31 <- ggplot() + 
  geom_bar(data = filter_obitos, aes(x=date, y=new_deaths), stat = 'identity', fill = "#0c4c8a") +
  geom_line(data = filter_obitos, aes(x=date, y=ma7_deaths),size = 1.82, colour = "#FF0000") +
  labs(x="",y="")
#  ggtitle("Novos óbitos confirmados e média móvel de Alagoas últimos 31 dias")+
#  labs(x="Tempo de pandemia", y = "Óbitos confirmados de COVID-19")

--------------------------------------------------------------------------------------------------------------------------------------------------
#Grid ggarrange
final <- ggarrange(casos,casos31, obitos,obitos31, nrow = 2, ncol = 2, legend="right", common.legend = TRUE)
final
--------------------------------------------------------------------------------------------------------------------------------------------------