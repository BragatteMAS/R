'''
"R2Py"
created by "Bragatte" 20210211
'''
#seeting env py at the first time and restart R
##add this to open file: Sys.setenv(RETICULATE_PYTHON = "/home/bragatte/anaconda3/envs/dsz/bin/python")
'''
usethis::edit_r_profile()
'''
#check py version
reticulate::py_config()
#libraries
packs = c("reticulate", "png")
lapply(packs, install.packages, character.only = TRUE)
lapply(packs, require, character.only = TRUE)

#Load python shell into console of RStudio  >>>python >R
repl_python()
