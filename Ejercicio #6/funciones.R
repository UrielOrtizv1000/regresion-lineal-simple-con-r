# funciones.R

# lógica para calcular la regresión lineal, la prueba de hipótesis, la estimación puntual y el intervalo de predicción

realizar_analisis_calificaciones <- function(datos, alpha, x_nuevo, nivel_confianza) {
  
  # a) Ecuacion de minimos cuadrados
  modelo <- lm(y ~ x, data = datos)
  coeficientes <- coef(modelo)
  ecuacion <- sprintf("y = %.4f + %.4fx", coeficientes[1], coeficientes[2])
  
  # b) Prueba de hipotesis T para la pendiente (H0: beta1 = 0)
  resumen_modelo <- summary(modelo)
  valor_t <- resumen_modelo$coefficients[2, "t value"]
  valor_p <- resumen_modelo$coefficients[2, "Pr(>|t|)"]
  rechazar_h0 <- valor_p < alpha
  
  # c) Estimacion puntual
  datos_pred <- data.frame(x = x_nuevo)
  estimacion <- predict(modelo, newdata = datos_pred)
  
  # d) Intervalo de prediccion
  intervalo <- predict(modelo, newdata = datos_pred, interval = "prediction", level = nivel_confianza)
  
  # Estructurar resultados en una lista
  lista_resultados <- list(
    inciso_a_ecuacion = ecuacion,
    inciso_b_prueba_T = list(
      hipotesis_nula = "La pendiente es igual a 0 (no hay relacion lineal)",
      estadistico_t = valor_t,
      p_valor = valor_p,
      alfa = alpha,
      rechazar_H0 = rechazar_h0
    ),
    inciso_c_estimacion_puntual = unname(estimacion),
    inciso_d_intervalo_prediccion = list(
      limite_inferior = intervalo[1, "lwr"],
      limite_superior = intervalo[1, "upr"],
      nivel_confianza = nivel_confianza,
      interpretacion = sprintf(
        "Con un %d%% de confianza, la calificacion del profesor para un programa evaluado con %.1f por AUTOMARK estara entre %.4f y %.4f.",
        nivel_confianza * 100, x_nuevo, intervalo[1, "lwr"], intervalo[1, "upr"]
      )
    )
  )
  
  return(lista_resultados)
}
