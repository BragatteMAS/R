#How_to_ChiSquare - Disfuncao_Eretil created by Bragatte  202102062017
#chamar arquivo
disf_eret <-
    read.csv("~/Downloads/bd_pre_pos.csv",
             sep = ",",
             header = T)
disf_eret_nopac <-
    read.csv("~/Downloads/bd_pre_pos_nopac.csv",
             sep = ",",
             header = T)

View(disf_eret_nopac)

#Criar variável para comparar duas colunas de interesse
x2 = table(disf_eret$Grupo, disf_eret$IIEF_grav_2)
x2

#Qui-quadrado
chisq.test(x2)

#valor p direto do qui-quadraro
chisq.test(x2)$p.value

##segundo método resumo dos dados
summary(x2)

###

library(corrplot)
correlation = disf_eret_nopac.corr()
corrplot(corr, method ='number')
