##. Script para Funky Heat Maps
##  @BragatteMAS
##  R 4.3.0

## read libs
pacman::p_load(vroom, tidyverse, funkyheatmap)

## e.g
data("mtcars")
funkyheatmap::funky_heatmap(mtcars)

data <- mtcars %>%
    rownames_to_column("id") %>%
    arrange(desc(mpg))

column_info <- tribble(
    ~id,     ~group,         ~name,                      ~geom,        ~palette,    ~options,
    "id",    "",             "",                         "text",       NA,          list(hjust = 0, width = 6),
    "mpg",   "overall",      "Miles / gallon",           "bar",        "palette1",  list(width = 4, legend = FALSE),
    "cyl",   "overall",      "Number of cylinders",      "bar",        "palette2",  list(width = 4, legend = FALSE),
    "disp",  "group1",       "Displacement (cu.in.)",    "funkyrect",  "palette1",  lst(),
    "hp",    "group1",       "Gross horsepower",         "funkyrect",  "palette1",  lst(),
    "drat",  "group1",       "Rear axle ratio",          "funkyrect",  "palette1",  lst(),
    "wt",    "group1",       "Weight (1000 lbs)",        "funkyrect",  "palette1",  lst(),
    "qsec",  "group2",       "1/4 mile time",            "circle",     "palette2",  lst(),
    "vs",    "group2",       "Engine",                   "circle",     "palette2",  lst(),
    "am",    "group2",       "Transmission",             "circle",     "palette2",  lst(),
    "gear",  "group2",       "# Forward gears",          "circle",     "palette2",  lst(),
    "carb",  "group2",       "# Carburetors",            "circle",     "palette2",  lst()
)

funky_heatmap(data, column_info = column_info, expand = list(xmax = 4))


## read real data
heatmap <- vroom("")

glimpse(heatmap)

## Ref
[funkyheatmap/funkyheatmap: Visualising data frames as funky heatmaps 📊](https://github.com/funkyheatmap/funkyheatmap)

[Improve your data science STORYTELLING with R - YouTube](https://www.youtube.com/watch?v=9XbxL-Is22k)