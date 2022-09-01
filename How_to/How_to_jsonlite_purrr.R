#Libraries
packs = c("tidyverse","jsonlite", "httr", "purrr")
lapply(packs, install.packages, character.only = TRUE)
lapply(packs, require, character.only = TRUE)

# link to the API output as a JSON file
url_json <- "https://site.web.api.espn.com/apis/fitt/v3/sports/football/nfl/qbr?region=us&lang=en&qbrType=seasons&seasontype=2&isqualified=true&sort=schedAdjQBR%3Adesc&season=2019"

# get the raw json into R
raw_json <- url_json %>% 
  httr::GET() %>% 
  httr::content()

# get names of the QBR categories
category_names <- pluck(raw_json, "categories",1, "labels") %>% unlist()

# Get the QBR stats by each player (row_n = player)
get_qbr_data <- function(row_n) {
  player_stats <- raw_json %>% 
    purrr::pluck("athletes", row_n, "categories", 1, "totals") %>% 
    # assign names from category
    set_names(nm = category_names)
  
  player_nm <- raw_json %>% 
    purrr::pluck("athletes", row_n, "athlete") %>% 
    keep(names(.) %in% c("displayName", "teamName", "teamShortName")) %>% 
    unlist()
  
  headshot <- raw_json %>% 
    purrr::pluck("athletes", row_n, "athlete", "headshot", "href")
  
  #output named list
  c(player_nm, headshot = headshot, player_stats)
}

# create the dataframe and tidy it up
qbr_df <- 1:length(raw_json$athletes) %>% 
  map_dfr(get_qbr_data) %>%
  mutate(across(TQBR:SAC, as.double))

qbr_df %>%
  gt::gt_preview() %>%
  gt::text_transform(gt::cells_body(headshot), gt::web_image)