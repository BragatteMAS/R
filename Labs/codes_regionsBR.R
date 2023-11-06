pacman::p_load(geobr, sf)

library(geobr)
library(sf)

# Função para buscar dados de uma cidade pelo nome
get_city_info <- function(city_name, state_abbr = NULL) {
  # Buscar todos os municípios
  munis <- read_municipality(code_muni = 'all', year = 2020)
  
  # Filtrar pelo nome da cidade (e estado, se fornecido)
  if (!is.null(state_abbr)) {
    city <- munis[munis$name_muni == city_name & munis$abbrev_state == state_abbr, ]
  } else {
    city <- munis[munis$name_muni == city_name, ]
  }
  
  # Calcular latitude e longitude do centróide da cidade
  city_centroid <- st_centroid(city)
  
  # Extrair latitude e longitude
  coords <- st_coordinates(city_centroid)
  
  # Extrair códigos do IBGE e informações do estado
  code_muni_ibge <- city$code_muni
  code_state_ibge <- city$code_state
  
  list(latitude = coords[1, 2], longitude = coords[1, 1], 
       code_muni_ibge = code_muni_ibge, code_state_ibge = code_state_ibge)
}

# Exemplo de uso da função
# Substitua 'São Paulo' pelo nome da cidade de interesse e 'SP' pelo estado, se necessário
city_info <- get_city_info('São Paulo', 'SP')
city_info <- get_city_info('Charqueadas', 'RS')

print(city_info)
