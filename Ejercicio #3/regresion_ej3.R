# Funciones de regresion y ANOVA para el Ejercicio 3

cargar_datos <- function(ruta_archivo) {
  # Cargar datos CSV
  datos <- read.csv(ruta_archivo)
  return(datos)
}

calcular_resultados_ej3 <- function(x, y, alpha = 0.05, valor_prediccion = 3) {
  # Ajuste de regresion lineal simple
  modelo <- lm(y ~ x)
  coefs <- coef(modelo)
  beta_0 <- as.numeric(coefs[1])
  beta_1 <- as.numeric(coefs[2])
  
  # a. Realizar ANOVA
  tabla_anova <- anova(modelo)
  SSR <- tabla_anova$`Sum Sq`[1]
  SSE <- tabla_anova$`Sum Sq`[2]
  SST <- SSR + SSE
  
  F_stat <- tabla_anova$`F value`[1]
  p_value_F <- tabla_anova$`Pr(>F)`[1]
  
  # Conclusion ANOVA
  se_rechaza_h0 <- p_value_F < alpha
  conclusion_anova <- if (se_rechaza_h0) {
    "Rechazar H0 (El modelo es util)"
  } else {
    "No rechazar H0 (No hay evidencia de utilidad)"
  }
  
  # b. Calculo de r y r^2
  r_pearson <- cor(x, y)
  r_cuadrado <- r_pearson^2
  
  # c. Estimacion puntual
  y_pred_val <- beta_0 + beta_1 * valor_prediccion
  
  # Agrupar resultados en lista
  lista_resultados <- list(
    beta_0 = beta_0,
    beta_1 = beta_1,
    SSR = SSR,
    SSE = SSE,
    SST = SST,
    F_stat = F_stat,
    p_value_F = p_value_F,
    conclusion_anova = conclusion_anova,
    r_pearson = r_pearson,
    r_cuadrado = r_cuadrado,
    y_pred_val = y_pred_val
  )
  
  return(lista_resultados)
}

graficar_regresion_ej3 <- function(x, y, beta_0, beta_1, ruta_guardado = "grafica_ejercicio3.png") {
  # Guardar grafica PNG
  png(filename = ruta_guardado, width = 800, height = 600, res = 120)
  
  # Diagrama de dispersion
  plot(x, y, 
       main = "Diagrama de Dispersion y Recta de Regresion",
       xlab = "Numero de Tareas (X)",
       ylab = "Tiempo de Produccion (Y)",
       pch = 19, col = "#2c3e50", cex = 1.2,
       panel.first = grid(lty = "dotted", col = "gray"))
  
  # Trazar recta
  abline(a = beta_0, b = beta_1, col = "#e74c3c", lwd = 3)
  
  # Leyenda
  legend("topleft", 
         legend = c("Datos observados", sprintf("Recta: Y = %.4f + %.4fX", beta_0, beta_1)),
         col = c("#2c3e50", "#e74c3c"), 
         lwd = c(NA, 3), 
         pch = c(19, NA), 
         bty = "o", bg = "white")
  
  dev.off()
}
