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
#Torch do Autograd
#x, df/dx
#where X & cost derivate from X
x <- torch_tensor(0, requires_grad = TRUE)

x$grad
#gradient_descend
#x[t+1] = x[t] - a * df/dx[t]

optim <- optim_adam(x, lr=0.01) #improve formulas

fx <- f(x) #calc f(x)
fx$backward() #backpropagation - f direction calc derivative
x$grad

optim$step()
x

points(x, fx, col = "red")

for (pass in 2:100) {
    optim$zero_grad()
    
    fx <- f(x)
    fx$backward()
    optim$step()
    points(x, fx, col = "red")
    Sys.sleep(0.1)
}
