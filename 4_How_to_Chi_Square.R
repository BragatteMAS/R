#How_to_ChiSquare - Disfuncao_Eretil created by Bragatte  202102062017
#chamar arquivo
disf_eret<- read_delim("Downloads/bd_pre_pos.csv", ";")
disf_eret_nopac<- read_delim("Downloads/bd_pre_pos2.csv", ";")

View(disf_eret)

#Criar variável para comparar duas colunas de interesse
x2 = table(disf_eret$Grupo, disf_eret$IIEF_grav_2)
x2

#Qui-quadrado
chisq.test(x2)

#valor p direto do qui-quadraro
chisq.test(x2)$p.value 

##segundo método resumo dos dados
summary(x2)

