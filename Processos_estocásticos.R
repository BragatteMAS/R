##Ref <http://eduardo.sites.uff.br/> <http://eduardo.sites.uff.br/minicursos/>
u=7.5 ; alpha =2; beta =1
theta=0 ; mu = alpha/beta
lambda = 3; n = 100
c = 3
Wi = NA
Xi = NA;
Wi[1]=rexp(1,lambda) ##dist do primeiro tempo
Xi[1]= rgamma(1,alpha,beta) ##dist da primeira indenização
for (j in 2:n){
  Wi[j]=rexp(1,lambda) ## tempos T1,T2,...Tn
  Xi[j]= rgamma(1,alpha,beta) ## dist das indenizações
}
Ti = cumsum(Wi) ## tempo acumulado
S.t = cumsum(Xi) ## indenizações acumuladas
U.t <- u + c*Ti - S.t ## processo U.t

plot(Ti,S.t,pch=20,xlab=expression(t),t="s",
     ylab=expression(S(t)),main="PP Composto")
plot(Ti, U.t, t="l",lwd=3, main="Processo de Risco",
     xlab=expression(t), ylab=expression(U(t)))

T.i[min(which(U.t<0))]
U.t[min(which(U.t<0))]

nSim <- 10000;
N <- rep(Inf, nSim)
for (k in 1:nSim){
  Wi <-rexp(n, lambda); Ti <- cumsum(Wi)
  Xi <- rgamma(n, alpha, beta) ##severity has meam EX=1
  Si <- cumsum(Xi); Ui <- u + Ti*c - Si
  ruin <- !all(Ui>=0)
  if (ruin) N[k]<- min(which(Ui<0))}
N <- N[N<Inf]; length(N); mean(N); sd(N); max(N)

p.ruina = length(N)/nSim
