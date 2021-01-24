'''
100daysofcode
"How to TORCH"
create by  @bragatte ^202101222031
Day 3
[Ref](https://www.youtube.com/watch?v=b81QmU9qh6Y)
'''
packs = c("torch")
lapply(packs, install.packages, character.only = TRUE)
lapply(packs, require, character.only = TRUE)

f <- function(x)
    x * (x - 1)
plot(f)

#Aim: Find X who reduce F
#Tensor == matriz (multi)
#gradiente_descend
#x, df/dx
#where X & cost derivate from X
x <-  torch_tensor(0, requires_grad = TRUE)

x$grad
