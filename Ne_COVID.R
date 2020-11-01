
#install
install.packages("tidyverse")
  #install.packages("dplyr")
  #install.packages("tidyr")
  #install.packages("ggplot2")#, dependencies = TRUE, repos = "http://cran.us.r-project.org")
install.packages("devtools")
install.packages("plotly")
install.packages("ggpubr")
install.packages("esquisse")
#install.packages("foreign")


#--------------------------------------------------------------------------------------------------------------------------------------------------

#libraries
library("tidyverse")
  #library("dplyr") 
  #library("tidyr")
  #library("ggplot2")
library("devtools")
library("plotly")
library("ggpubr")
library("esquisse")
#--------------------------------------------------------------------------------------------------------------------------------------------------

#Dataset
con <- gzcon(url(paste("https://data.brasil.io/dataset/covid19/caso_full.csv.gz", sep=",")))
txt <- readLines(con)
dados <- read.csv(textConnection(txt)) %>%
  mutate_all(~replace(., is.na(.), 0)) # dados[is.na(dados)] <-  0 #old form BaseR slower

# relevant columns
colnames(dados)[13] <-'deaths'
colnames(dados)[10] <- 'confirmed_per_100k'
colnames(dados)[9] <-'confirmed'


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
            confirmed_per_100k=sum(confirmed_per_100k), deaths_per_100k=sum(deaths_per_100k)) #add , na.rm=T to replace NA with 0

### Select Date info
dados_brasil$date <- as.Date(dados_brasil$date)

##Average mean Br
dados_brasil <- dados_brasil%>%
  mutate(ma7_confirmed = stats::filter(dados_brasil$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_brasil$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))
  dados_brasil[is.na(dados_brasil)] <-  0


#### cases
dados_brasil$ma7_confirmed <- as.numeric(dados_brasil$ma7_confirmed)

## deaths
dados_brasil$ma7_deaths <- as.numeric(dados_brasil$ma7_deaths)

#--------------------------------------------------------------------------------------------------------------------------------------------------
"%>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% "
#Similar to shell - field to complete

#Nordeste
#AL,BA,CE,PB,PI,PE,MA,RN,SE

dados_ne <- dados %>%
  filter(place_type == "state", state=="AL" |state=="BA" |state=="CE"|state=="PB"|state=="PE"|state=="PI"|state=="MA"|state=="RN"|state=="SE") %>%
  arrange(date) 


dados_ne$date <- as.Date(dados_ne$date)


###Média móvel
#AL
dados_al <- dados %>%
  filter(place_type=="state", state =="AL") 

dados_al <- dados_al%>%
  mutate(ma7_confirmed = stats::filter(dados_al$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_al$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))


#ba
dados_ba <- dados %>%
  filter(place_type=="state", state =="BA") 

dados_ba <- dados_ba%>%
  mutate(ma7_confirmed = stats::filter(dados_ba$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_ba$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))


#CE
dados_ce <- dados %>%
  filter(place_type=="state", state =="CE") 

dados_ce <- dados_ce%>%
  mutate(ma7_confirmed = stats::filter(dados_ce$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_ce$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))


#PB
dados_pb <- dados %>%
  filter(place_type=="state", state =="PB") 

dados_pb <- dados_pb%>%
  mutate(ma7_confirmed = stats::filter(dados_pb$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_pb$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))


#PE
dados_pe <- dados %>%
  filter(place_type=="state", state =="PE") 

dados_pe <- dados_pe%>%
  mutate(ma7_confirmed = stats::filter(dados_pe$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_pe$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))


#PI
dados_pi <- dados %>%
  filter(place_type=="state", state =="PI") 

dados_pi <- dados_pi%>%
  mutate(ma7_confirmed = stats::filter(dados_pi$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_pi$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))


#MA
dados_ma <- dados %>%
  filter(place_type=="state", state =="MA") 

dados_ma <- dados_ma%>%
  mutate(ma7_confirmed = stats::filter(dados_ma$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_ma$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))


#RN
dados_rn <- dados %>%
  filter(place_type=="state", state =="RN") 

dados_rn <- dados_rn%>%
  mutate(ma7_confirmed = stats::filter(dados_rn$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_rn$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))


#SE
dados_se <- dados %>%
  filter(place_type=="state", state =="SE") 

dados_se <- dados_se%>%
  mutate(ma7_confirmed = stats::filter(dados_se$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_se$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))


dados_ne_full<- rbind.data.frame(dados_al,dados_ba,dados_ce,dados_pb,dados_pe,dados_pi,dados_ma,dados_rn,dados_se)

dados_ne_full$date <- as.Date(dados_ne_full$date)


#--------------------------------------------------------------------------------------------------------------------------------------------------
##Nordeste

df_ne <- group_by(dados_ne_full,date)%>%
  summarise(confirmed=sum(confirmed), deaths=sum(deaths),
            confirmed_per_100k = sum(confirmed_per_100k),deaths_per_100k = sum(deaths_per_100k),
            new_confirmed=sum(new_confirmed),new_deaths=sum(new_deaths))

df_ne <- df_ne%>%
  mutate(ma7_confirmed = stats::filter(df_ne$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(df_ne$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(state= "Nordeste")

#--------------------------------------------------------------------------------------------------------------------------------------------------
####capitais
#Teresina		
dados_teresina <- dados %>%
  filter(place_type=="city",  city_ibge_code=="2211001") 


dados_teresina <- dados_teresina%>%
  mutate(ma7_confirmed = stats::filter(dados_teresina$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_teresina$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))

#São Luís	
dados_sao_luis <- dados %>%
  filter(place_type=="city",  city_ibge_code=="2111300") 


dados_sao_luis <- dados_sao_luis%>%
  mutate(ma7_confirmed = stats::filter(dados_sao_luis$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_sao_luis$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))

#Salvador		
dados_salvador <- dados %>%
  filter(place_type=="city",  city_ibge_code=="2927408") 


dados_salvador <- dados_salvador%>%
  mutate(ma7_confirmed = stats::filter(dados_salvador$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_salvador$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))

#Maceió		 
dados_maceio <- dados %>%
  filter(place_type=="city",  city_ibge_code=="2704302") 


dados_maceio <- dados_maceio%>%
  mutate(ma7_confirmed = stats::filter(dados_maceio$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_maceio$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))

#Fortaleza		
dados_fortaleza <- dados %>%
  filter(place_type=="city",  city_ibge_code=="2304400") 


dados_fortaleza <- dados_fortaleza%>%
  mutate(ma7_confirmed = stats::filter(dados_fortaleza$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_fortaleza$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))

#Recife		 
dados_recife <- dados %>%
  filter(place_type=="city",  city_ibge_code=="2611606") 


dados_recife <- dados_recife%>%
  mutate(ma7_confirmed = stats::filter(dados_recife$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_recife$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))

#João Pessoa		 
dados_joao_pessoa <- dados %>%
  filter(place_type=="city",  city_ibge_code=="2507507") 


dados_joao_pessoa <- dados_joao_pessoa%>%
  mutate(ma7_confirmed = stats::filter(dados_joao_pessoa$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_joao_pessoa$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))

#Aracaju		 
dados_aracaju <- dados %>%
  filter(place_type=="city",  city_ibge_code=="2800308") 


dados_aracaju <- dados_aracaju%>%
  mutate(ma7_confirmed = stats::filter(dados_aracaju$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_aracaju$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))

#Natal		 
dados_natal <- dados %>%
  filter(place_type=="city",  city_ibge_code=="2408102") 


dados_natal <- dados_natal%>%
  mutate(ma7_confirmed = stats::filter(dados_natal$new_confirmed, filter=rep(1/7, 7), method="convolution", sides=1, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_natal$new_deaths, filter=rep(1/7, 7), method="convolution", sides=1, circular=F))


dados_capitais<- rbind.data.frame(dados_aracaju,dados_fortaleza,dados_joao_pessoa,
                                  dados_maceio,dados_natal,dados_recife,
                                  dados_salvador,dados_sao_luis,
                                  dados_teresina)

dados_capitais$date <- as.Date(dados_capitais$date)






#-------------------------------------------------------------------------------------------------------------------------------------------------
#[Ref](https://github.com/BragatteMAS/COVID19/blob/master/COVID_shiny.r)
# rlang::last_error() ## check errors
#-------------------------------------------------------------------------------------------------------------------------------------------------
### Plot new cases COVID
ggplot(dados_brasil) +
  aes(x = date, colour = new_deaths, weight = new_confirmed) +
  geom_bar(fill = "#0c4c8a") +
  scale_color_gradient() +
  theme_minimal()#+
  #legend(x=date, y= new_deaths, legend=c())
### Plot mean average line
ggplot(dados_brasil) +
  aes(x = date, y = ma7_confirmed) +
  geom_line(size = 1.82, colour = "#ef562d") +
  theme_minimal()
#-------------------------------------------------------------------------------------------------------------------------------------------------
#esquisser(viewer = "browser")
#-------------------------------------------------------------------------------------------------------------------------------------------------
#Cases and Mean average
casos <- ggplot() + 
  geom_bar(data = dados_brasil, aes(x=date, y=new_confirmed), stat = 'identity', fill = "#0c4c8a") +
  geom_line(data = dados_brasil, aes(x=date, y=ma7_confirmed),size = 1.82, colour = "#d67d2a") +
  ggtitle("Pandemia em Alagoas")+
  labs(x="", y="Casos")
#ggtitle("Novos casos confirmados e mÃ©dia mÃ³vel de Alagoas")+
#labs(x="Tempo de pandemia", y = "Casos confirmados de COVID-19")

#Filter 31 days cases
filter <- dados_brasil %>%
  filter(date >= "2020-08-09" & date <= "2020-09-09")
casos31 <- ggplot() + 
  geom_bar(data = filter, aes(x=date, y=new_confirmed), stat = 'identity', fill = "#0c4c8a") +
  geom_line(data = filter, aes(x=date, y=ma7_confirmed),size = 1.82, colour = "#d67d2a") +
  ggtitle("Últimos 31 dias")+
  labs(x="", y="")
#  ggtitle("Novos casos confirmados e mÃ©dia mÃ³vel de Alagoas Ãºltimos 31 dias")+
#  labs(x="Tempo de pandemia", y = "Casos confirmados de COVID-19")
#-------------------------------------------------------------------------------------------------------------------------------------------------
#Deaths and Mean average
obitos <- ggplot() + 
  geom_bar(data = dados_brasil, aes(x=date, y=new_deaths), stat = 'identity', fill = "#0c4c8a") +
  geom_line(data = dados_brasil, aes(x=date, y=ma7_deaths),size = 1.82, colour = "#FF0000") +
  labs(y="Óbitos") +
  labs(x="")
#  ggtitle("Novos Ã³bitos confirmados e mÃ©dia mÃ³vel de Alagoas")+
#  labs(x="Tempo de pandemia", y = "Óbitos confirmados de COVID-19")

#Filter 31 days deaths
filter_obitos <- dados_brasil %>%
  filter(date >= "2020-08-09" & date <= "2020-09-09")
obitos31 <- ggplot() + 
  geom_bar(data = filter_obitos, aes(x=date, y=new_deaths), stat = 'identity', fill = "#0c4c8a") +
  geom_line(data = filter_obitos, aes(x=date, y=ma7_deaths),size = 1.82, colour = "#FF0000") +
  labs(x="",y="")
#  ggtitle("Novos Ã³bitos confirmados e mÃ©dia mÃ³vel de Alagoas Ãºltimos 31 dias")+
#  labs(x="Tempo de pandemia", y = "Óbitos confirmados de COVID-19")

#--------------------------------------------------------------------------------------------------------------------------------------------------
#Grid ggarrange
final <- ggarrange(casos,casos31, obitos,obitos31, nrow = 2, ncol = 2, legend="right", common.legend = TRUE)
final
#--------------------------------------------------------------------------------------------------------------------------------------------------