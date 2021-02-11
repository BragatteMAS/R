'''
"R2Py"
created by "Bragatte" 20210211
'''
#libraries
packs = c("reticulate", "png")
lapply(packs, install.packages, character.only = TRUE)
lapply(packs, require, character.only = TRUE)

usethis::edit_r_profile()

#Load py shell
repl_python()

#check py version
reticulate::py_config()

#alternative load env py - in the term use "which python"
#use_python(" path for python OS", required=TRUE)
use_python("/usr/bin/python3", required=TRUE)
use_python("/home/bragatte/anaconda3/envs/dsz/bin/python", required=TRUE)



