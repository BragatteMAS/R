#[Ref](https://towardsdatascience.com/iterate-your-r-code-efficiently-3c621998eaea)

#   Iterate pratice
"Problem 1: You want to find the standard deviation for each variable in the dataset"
##You could copy-paste the same code for each column:

sd(airquality$Ozone)
#33.27597
sd(airquality$Solar.R)
#91.1523
sd(airquality$Wind)
#3.557713
sd(airquality$Temp)
#9.529969
sd(airquality$Month)
#1.473434
sd(airquality$Day)
#8.707194

head(airquality)

##########################################

##We can write a for()loop
stddev = vector("double", ncol(airquality))
for(i in seq_along(airquality))             
{
  stddev[[i]] = sd(airquality[[i]])          
  
}
stddev
#33.275969 91.152302  3.557713  9.529969  1.473434  8.707194

##########################################

##Or you could just skip the loop and do the trick with just a line of code using sapply() from base R’s apply()family
sapply(airquality, sd)
sapply(airquality, median)
#Ozone      Solar.R      Wind      Temp     Month     Day 
#33.275969 91.152302  3.557713  9.529969  1.473434  8.707194

##########################################

"Problem 2: You want to find the standard deviation and median of each column in your dataset"
stddev =vector("double", ncol(airquality))
median =vector("double", ncol(airquality))
for(i in seq_along(airquality))
{
  stddev[[i]] = sd(airquality[[i]])
  median[[i]] = median(airquality[[i]])
}
stddev
#33.275969 91.152302  3.557713  9.529969  1.473434  8.707194
median
#31.0 207.0   9.7  79.0   7.0  16.0

##########################################

f <- function(x){
  list(sd(x),median(x))
}
sapply(airquality, f)
#Ozone    Solar.R Wind     Temp     Month    Day     
#33.27597 91.1523 3.557713 9.529969 1.473434 8.707194
#31       207     9.7      79       7        16

##########################################

"The Map() functions: 
map() makes a list.
map_lgl() makes a logical vector.
map_int() makes an integer vector.
map_dbl() makes a double vector.
map_chr() makes a character vector."
library("purrr")
map_df(airquality, ~list(med = median(.x), sd = sd(.x)))

##########################################
#GAPMINDER DATASET
library(gapminder)
gapminder = gapminder[complete.cases(gapminder),]


"Problem 3: I want to know which country has the highest GDP Per Capita in each continent and in each year."
 #Using the for() loop approach
list = c("continent", "year")
DF= data.frame()
for( i in list)
{
  df = gapminder %>% group_by_at(i) %>% 
    top_n(1, gdpPercap) %>% 
    mutate(Remark = paste0("Country Max GDP Per capita in the ",i)) %>% 
    data.frame()
  DF = rbind(df,DF)
}
DF

#Using the Apply()approach
do.call(rbind, lapply(list, function(x)
{
  gapminder %>% group_by_at(x) %>% 
    top_n(1, gdpPercap)%>%
    mutate(Remark = paste0("Country with the max GDP Per capita in the ",x)) %>% 
    data.frame
}))

#Using the Purrr::Map() approach
gapminder$year = as.character(gapminder$year)
map_dfr(list, ~gapminder %>% group_by(!!sym(.x)) %>% 
          top_n(1, gdpPercap)%>%
          mutate(Remark = paste0(“Country with the max GDP Per capita in the “,.x)) %>% data.frame()
        
##Extra #Purrr solution:
#Task: Run a piecewise regression for each segment of the data. (Here, continent):
gapminder %>% 
  split(.$Continent) %>% 
  map(~lm(gdpPercap ~ lifeExp, data = .))

#Task: Keep variables basis an arbitrary condition. (Here, if the variable is a factor):
gapminder %>% 
  keep(is.factor) %>% 
  str()

#Task: Check if any variable meets the arbitrary condition(Here, if any variable is a character):
gapminder%>% 
  some(is_character)

#Task: Check if every variable meets the arbitrary condition(Here, if every variable is an integer):
gapminder %>% 
  every(is.integer))

