#Ref.: <https://cran.r-project.org/web/packages/dendextend/dendextend.pdf>

#Packages installation
install.packages("pvclust")
install.packages("ggplot2")
install.packages("dendextend")
install.packages("readxl")

#Activate libraries
library("pvclust")
library("ggplot2")
library("dendextend")
library("readxl")

#archive read
library(readxl)
factor8 <- read_excel("factor8.xlsx", col_types = c("skip", 
                                                    "numeric", "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", "numeric", 
                                                    "numeric"))
View(factor8)

#Run pvclust and plot
F <- pvclust(factor8 , method.hclust = "average", method.dist = "correlation",use.cor = "pairwise.complete.obs", nboot = 10, weight = TRUE)
plot(F)
#pvrect(F)

# Convert pvclust(F) into a dendrogram (df) to edite the plot
df <- as.dendrogram(F)

# Editing boarders
##Colors form branches
#a <- df %>% set('rank_branches',value = c("green","red", "blue", "orange", "black", "purple"), k = 6) #more space between branches
#a %>% set('branches_k_color',value = c("green","red", "blue", "orange", "black", "purple"), k = 6) %>% #color branches

## editing clusters
# df %>% rect.dendrogram(6,lty = 4, lwd = 3) #one color
df %>% rect.dendrogram(6,lty = 4, lwd = 3,  which = c(1,2,3,4,5,6), border = c(1,2,3,4,5,6)) #colorfull



