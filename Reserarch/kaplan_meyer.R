library(survival)
data(lung)
km_fit1 <- survfit(Surv(time, status) ~ 1, data = lung)
km_fit2 <- survfit(Surv(time, status) ~ sex, data = lung)

plot(km_fit1, col = "blue", lty = 1, xlab = "Tempo (dias)", ylab = "Probabilidade de sobrevivência", main = "Curva de sobrevivência de Kaplan-Meier para pacientes com câncer de pulmão")
lines(km_fit2, col = "red", lty = 2)

legend("topright", legend = c("Todos os pacientes", "Pacientes por sexo"), col = c("blue", "red"), lty = c(1, 2), cex = 0.8)
