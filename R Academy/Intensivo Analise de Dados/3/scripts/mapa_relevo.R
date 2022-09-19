meu_mapa <- function(meu_estado){
  
  #Pacotes necessarios
  #raster, ggplot2,sf,dplyr,rgdal
  
  #Carregando os pacotes
  library(raster)  #Para leitura e manipulacao de dados espaciais do tipo raster
  library(ggplot2) #Para visualizacao de dados
  library(dplyr,warn.conflicts=FALSE) #para manipulacao de dados
  
  #Leitura de dados 
  topografia <- raster('dados/topografia.tif')     #Ler os dados de topografia
  estados <- sf::read_sf('dados/poly_estados.shp') #Ler os dados dos limites territoriais dos Estados brasileiros 
  
  estado <- dplyr::filter(estados,abbrv_s==meu_estado) #Seleciona os dados dos limites territoriais para cada estado
  
  # Verifica existem dados para a sigla do Estado informada
  if(nrow(estado)<1) return(paste('A sigla',meu_estado,'nao valida',sep = ' ')) 
  
  #Manipulacao de dados
  top.estado <- topografia %>% 
    crop(estado) %>%  #Recorta os dados para a extensao do Estado 
    mask(estado)      #Recorta os dados para os limites do estado
  
  top.estado[top.estado==99999]=NA #Troca 99999 por NA
  top.estado.sf <- as.data.frame(top.estado,xy=T) %>%  #Converte raster para data.frame
    na.omit() # Remove os NA dos dados 
  
  ##############################################
  ##### Visualizando dos dados            ######
  ##############################################
  
  #Criando paleta de cores
  cores <- c('#bcd2a4','#89d2a4','#28a77e','#90b262',
             '#ddb747','#fecf5b','#da9248','#b75554',
             '#ad7562','#b8a29a','#9f9e98')
  
  p <-  ggplot()+ #Criando a primeira camada do ggplot
    geom_tile(data = top.estado.sf,aes(x,y,fill=topografia))+ #Sobrepondo dos dados de topografia
    geom_sf(data = estado,fill=NA)+ #Sobrepondo os limites territoriais para os Estados selecionado
    scale_fill_gradientn(colours =cores )+ #Ajustando as cores do mapa
    labs(x=NULL,y=NULL,fill='[m]')+ #Sobrepondo os pontos
    theme_light() #Escolhendo o tema do mapa
  
  print(p)
}
