'''
100daysofcode
"How to easy webscrapping"
create by  @bragatte ^202101191216 
Day 1
[Ref](https://www.r-bloggers.com/2021/01/simple-easy-beginners-web-scraping-in-r-with-ralger/)
'''
packs = c("ralger", "devtools") #ralger (https://github.com/feddelegrand7/ralger)
lapply(packs, install.packages, character.only = TRUE)
lapply(packs, require, character.only = TRUE)

#?scrap
link <- "https://www.imdb.com/search/title/?groups=top_250&sort=user_rating"

my_nodes <- c(
    ".lister-item-header a", # The title
    ".text-muted.unbold", # The year of release
    ".ratings-imdb-rating strong" # The rating)
)

names <- c("title", "year", "rating") # respect the nodes order


tidy_scrap(link = my_link, nodes = my_nodes, colnames = names)