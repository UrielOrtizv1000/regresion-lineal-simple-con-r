# =========================================================
# Archivo: main.R
# =========================================================
# Universidad Autonoma de Aguascalientes
# Centro de Ciencias Basicas
# Departamento de Estadistica
# Inferencia Estadistica
#
# Ejercicio 5
# =========================================================


# setwd("D:/VSC Proyectos/regresion-lineal-simple-con-r/Ejercicio#5")
source("Util.R")

# Datos del problema
X_ingreso = c(20.0, 30.5, 40.0, 55.1, 60.3, 74.9, 88.4, 95.2)
Y_consumo = c(1.8, 3.0, 4.8, 5.0, 6.5, 7.0, 9.0, 9.1)

# Calculos 
ej_regresion = ICbeta1(x = X_ingreso, y = Y_consumo, coefConf = 0.98)

beta0 = ej_regresion$calculos$beta0.hat
beta1 = ej_regresion$calculos$beta1.hat

# ---------------------------------------------------------
cat("\n[ Inciso a ] Diagrama de dispersion\n")
cat("-----------------------------------------------------------\n")
cat(" Grafico generado.\n")

plot(X_ingreso, Y_consumo, 
     main = "Ingreso vs Consumo de Energia",
     xlab = "Ingreso Familiar (Miles de $/ano)",
     ylab = "Consumo (10^8 Btu/ano)",
     pch = 16, col = "blue", cex = 1.2)
abline(a = beta0, b = beta1, col = "red", lwd = 2)
grid()

# ---------------------------------------------------------
cat("\n[ Inciso b ] Estime la ecuacion de regresion lineal\n")
cat("-----------------------------------------------------------\n")
cat(paste(" Respuesta: y.hat =", round(beta0, 4), "+", round(beta1, 4), "x\n"))
cat(" (El calculo de Sxx, Sxy y las betas esta al final).\n")


# ---------------------------------------------------------
cat("\n[ Inciso c ] Consumo para X = 50 (Promedio y Familia)\n")
cat("-----------------------------------------------------------\n")
consumo_50 = beta0 + (beta1 * 50)
cat(" Procedimiento:\n")
cat(paste("    y.hat = beta0 + beta1 * X\n"))
cat(paste("    y.hat =", round(beta0, 4), "+", round(beta1, 4), "* (50)\n"))
cat(paste("    y.hat =", round(consumo_50, 4), "\n\n"))
cat(paste(" Respuesta Promedio: El consumo estimado es de", round(consumo_50, 4), "x 10^8 Btu.\n"))
cat(" Respuesta Individual: La estimacion para UNA sola familia es exactamente el mismo valor.\n")


# ---------------------------------------------------------
cat("\n[ Inciso d ] Aumento en el consumo si el ingreso sube $2,000\n")
cat("-----------------------------------------------------------\n")
cambio_y = beta1 * 2
cat(" Procedimiento:\n")
cat("    1. Como X esta en unidades de $1,000, un aumento de $2,000 significa que delta_X = 2.\n")
cat("    2. El cambio en Y se calcula multiplicando la pendiente por el delta_X:\n")
cat(paste("    delta_Y = beta1 * delta_X\n"))
cat(paste("    delta_Y =", round(beta1, 4), "* 2\n"))
cat(paste("    delta_Y =", round(cambio_y, 4), "\n\n"))
cat(paste(" Respuesta: Se espera que el consumo aumente en", round(cambio_y, 4), "x 10^8 Btu.\n"))


# ---------------------------------------------------------
cat("\n[ Inciso e ] Intervalo de confianza del 98% para beta 1\n")
cat("-----------------------------------------------------------\n")
cat(paste(" Respuesta Final: LIC =", round(ej_regresion$intervalo$LIC, 4), "| LSC =", round(ej_regresion$intervalo$LSC, 4), "\n\n"))

cat(" --- Procedimiento para Incisos B y E ---\n")
for(paso in ej_regresion$procedimiento) {
    cat(paste("   *", paso, "\n"))
}
cat("===========================================================\n\n")