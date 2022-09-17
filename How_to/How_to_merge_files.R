# Merge files
# R 4.2.1
# Refactor @BragatteMAS

## installing the required libraries
library(readxl,tidyverse,purrr)

## specifying the path for file
path <- "C:/Users/033270622.ITAUCULTURAL/Documents/GitHub/labdata/MPXV/"

## set the working directory
setwd(path) #dir to check 

## create a list of all files in the working directory with the .csv extension
files <- list.files(pattern="*.xlsx")

## Merge files, adjust types
MPXV_einsten_full <- files %>% map_dfr(read_excel,
                                  col_types = c("text", "numeric", "numeric", 
                                                           "text", "date", "text", "text")) 
# Check duplicates with boolean
duplicated(MPXV_einsten_full)

# Which rows are duplicated
MPXV_einsten_full[duplicated(MPXV_einsten_full),]

# Remove duplicates
MPXV_einsten_full[!duplicated(MPXV_einsten_full),]

# New dataframe without duplicates
MPXV_einsten <-  MPXV_einsten_full[!duplicated(MPXV_einsten_full),]

#check new df
summary(MPXV_einsten)

#save csv
write.csv(MPXV_einsten, file = "MPXV_einsten.csv", row.names = F)

#########################################

## Ref
## [Merge_files_examples_1](https://stackoverflow.com/questions/46305724/merging-of-multiple-excel-files-in-r)
## [Merge_files_examples_2](https://towardsdatascience.com/merge-data-frames-in-python-r-725c0f874147)