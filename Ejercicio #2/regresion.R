# Funciones para el análisis de regresión lineal simple y ANOVA

cargar_datos <- function(ruta_archivo) {
  # Carga el archivo CSV y devuelve un data frame
  datos <- read.csv(ruta_archivo)
  return(datos)
}

calcular_regresion_y_anova <- function(x, y, alpha = 0.05, valor_prediccion = 16) {
  # Realiza el ajuste de regresión lineal simple
  modelo <- lm(y ~ x)
  
  # Extraer coeficientes
  coefs <- coef(modelo)
  beta_0 <- as.numeric(coefs[1])
  beta_1 <- as.numeric(coefs[2])
  
  # Realizar ANOVA
  tabla_anova <- anova(modelo)
  SSR <- tabla_anova$`Sum Sq`[1]
  SSE <- tabla_anova$`Sum Sq`[2]
  SST <- SSR + SSE
  
  F_stat <- tabla_anova$`F value`[1]
  p_value_F <- tabla_anova$`Pr(>F)`[1]
  
  # Conclusión de la prueba de hipótesis del ANOVA
  se_rechaza_h0 <- p_value_F < alpha
  conclusion_anova <- if (se_rechaza_h0) {
    "Rechazar H0 (El modelo es útil)"
  } else {
    "No rechazar H0 (No hay evidencia de utilidad)"
  }
  
  # Estimación puntual para el valor solicitado
  y_pred_val <- beta_0 + beta_1 * valor_prediccion
  
  # Devolver resultados en una lista
  lista_resultados <- list(
    beta_0 = beta_0,
    beta_1 = beta_1,
    SSR = SSR,
    SSE = SSE,
    SST = SST,
    F_stat = F_stat,
    p_value_F = p_value_F,
    conclusion_anova = conclusion_anova,
    y_pred_val = y_pred_val
  )
  
  return(lista_resultados)
}

graficar_regresion <- function(x, y, beta_0, beta_1, ruta_guardado = "grafica_ejercicio2.png") {
  # Guardar la gráfica en formato PNG
  png(filename = ruta_guardado, width = 800, height = 600, res = 120)
  
  # Diagrama de dispersión
  plot(x, y, 
       main = "Diagrama de Dispersión y Recta de Regresión",
       xlab = "Número de vehículos (X)",
       ylab = "Tiempo de congestionamiento en minutos (Y)",
       pch = 19, col = "#2c3e50", cex = 1.2,
       panel.first = grid(lty = "dotted", col = "gray"))
  
  # Trazar la recta de regresión
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
