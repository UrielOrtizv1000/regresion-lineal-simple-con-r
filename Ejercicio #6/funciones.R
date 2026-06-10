# funciones.R
# Funciones reutilizables para el Ejercicio 6.
# Analiza la relacion entre calificaciones AUTOMARK (x) y profesor (y).

validar_datos_ejercicio_6 <- function(datos) {
  if (!all(c("x", "y") %in% names(datos))) {
    stop("El archivo de datos debe contener las columnas x e y.", call. = FALSE)
  }

  if (!is.numeric(datos$x) || !is.numeric(datos$y)) {
    stop("Las columnas x e y deben ser numericas.", call. = FALSE)
  }

  if (length(datos$x) != length(datos$y)) {
    stop("x e y deben contener la misma cantidad de observaciones.", call. = FALSE)
  }

  if (length(datos$x) != 33) {
    stop("El Ejercicio 6 debe contener exactamente 33 observaciones.", call. = FALSE)
  }

  if (anyNA(datos)) {
    stop("Los datos no deben contener valores faltantes.", call. = FALSE)
  }

  if (any(!is.finite(datos$x)) || any(!is.finite(datos$y))) {
    stop("Los datos no deben contener valores infinitos.", call. = FALSE)
  }

  if (length(unique(datos$x)) < 2 || var(datos$x) == 0) {
    stop("La variable x no tiene variacion suficiente para ajustar una regresion.",
         call. = FALSE)
  }

  x_esperada <- c(
    12.2, 10.6, 15.1, 16.2, 16.6, 16.6, 17.2, 17.6, 18.2, 16.5, 17.2,
    18.2, 15.1, 17.2, 17.5, 18.6, 18.8, 17.8, 18.0, 18.2, 18.4, 18.6,
    19.0, 19.3, 19.5, 19.7, 18.6, 19.0, 19.2, 19.4, 19.6, 20.1, 19.2
  )
  y_esperada <- c(
    10, 11, 12, 12, 12, 13, 14, 14, 14, 15, 15,
    15, 16, 16, 16, 16, 16, 17, 17, 17, 17, 17,
    17, 17, 17, 17, 18, 18, 18, 18, 18, 18, 19
  )

  if (!isTRUE(all.equal(as.numeric(datos$x), x_esperada,
                        check.attributes = FALSE)) ||
      !isTRUE(all.equal(as.numeric(datos$y), y_esperada,
                        check.attributes = FALSE))) {
    stop("Los datos no coinciden con los valores originales del Ejercicio 6.",
         call. = FALSE)
  }

  data.frame(
    x = as.numeric(datos$x),
    y = as.numeric(datos$y)
  )
}

realizar_analisis_calificaciones <- function(datos, alpha, x_nuevo, nivel_confianza) {
  datos <- validar_datos_ejercicio_6(datos)

  # -------------------------------------------------------------------
  # Ejercicio 6, inciso a)
  # Ajusta la regresion lineal simple y obtiene la ecuacion de minimos
  # cuadrados que relaciona AUTOMARK (x) con la calificacion del profesor (y).
  # -------------------------------------------------------------------
  modelo <- lm(y ~ x, data = datos)
  coeficientes <- coef(modelo)
  ecuacion <- sprintf("y = %.4f + %.4fx", coeficientes[1], coeficientes[2])

  # -------------------------------------------------------------------
  # Ejercicio 6, inciso b)
  # Realiza la prueba t para la pendiente. Evalua H0: beta_1 = 0 contra
  # H1: beta_1 != 0 usando el nivel de significancia indicado.
  # -------------------------------------------------------------------
  resumen_modelo <- summary(modelo)
  valor_t <- resumen_modelo$coefficients[2, "t value"]
  valor_p <- resumen_modelo$coefficients[2, "Pr(>|t|)"]
  rechazar_h0 <- valor_p < alpha

  # -------------------------------------------------------------------
  # Ejercicio 6, inciso c)
  # Calcula la estimacion puntual de la calificacion del profesor cuando
  # la calificacion AUTOMARK toma el valor solicitado.
  # -------------------------------------------------------------------
  datos_prediccion <- data.frame(x = x_nuevo)
  estimacion <- predict(modelo, newdata = datos_prediccion)

  # -------------------------------------------------------------------
  # Ejercicio 6, inciso d)
  # Calcula el intervalo de prediccion para una observacion individual
  # cuando AUTOMARK vale x_nuevo, con el nivel de confianza indicado.
  # -------------------------------------------------------------------
  intervalo <- predict(
    modelo,
    newdata = datos_prediccion,
    interval = "prediction",
    level = nivel_confianza
  )

  list(
    datos = datos,
    modelo = modelo,
    coeficientes = coeficientes,
    parametros = list(
      alpha = alpha,
      x_nuevo = x_nuevo,
      nivel_confianza = nivel_confianza
    ),
    inciso_a_ecuacion = ecuacion,
    inciso_b_prueba_T = list(
      hipotesis_nula = "La pendiente es igual a 0 (no hay relacion lineal)",
      hipotesis_alternativa = "La pendiente es diferente de 0",
      estadistico_t = unname(valor_t),
      p_valor = unname(valor_p),
      alfa = alpha,
      rechazar_H0 = rechazar_h0
    ),
    inciso_c_estimacion_puntual = unname(estimacion),
    inciso_d_intervalo_prediccion = list(
      limite_inferior = unname(intervalo[1, "lwr"]),
      limite_superior = unname(intervalo[1, "upr"]),
      nivel_confianza = nivel_confianza,
      interpretacion = sprintf(
        "Con un %d%% de confianza, la calificacion del profesor para un programa evaluado con %.1f por AUTOMARK estara entre %.4f y %.4f.",
        nivel_confianza * 100,
        x_nuevo,
        intervalo[1, "lwr"],
        intervalo[1, "upr"]
      )
    )
  )
}

imprimir_resultados_legibles <- function(res) {
  cat("--- RESULTADOS DEL ANALISIS ---\n\n")

  cat("a) Ecuacion de minimos cuadrados:\n")
  cat("   ", res$inciso_a_ecuacion, "\n\n")

  cat("b) Prueba de hipotesis T:\n")
  cat("   Hipotesis Nula (H0):", res$inciso_b_prueba_T$hipotesis_nula, "\n")
  cat(sprintf("   Estadistico t: %.4f\n", res$inciso_b_prueba_T$estadistico_t))
  cat(sprintf("   Valor p: %g\n", res$inciso_b_prueba_T$p_valor))
  cat(sprintf("   Nivel de significancia (alfa): %.2f\n", res$inciso_b_prueba_T$alfa))

  if (res$inciso_b_prueba_T$rechazar_H0) {
    cat("   Conclusion: Se rechaza H0. Existe evidencia de una relacion lineal significativa.\n\n")
  } else {
    cat("   Conclusion: No se rechaza H0. No hay evidencia suficiente de relacion lineal.\n\n")
  }

  cat("c) Estimacion puntual:\n")
  cat(sprintf("   Calificacion estimada: %.4f\n\n", res$inciso_c_estimacion_puntual))

  cat("d) Intervalo de prediccion:\n")
  cat(sprintf("   Nivel de confianza: %d%%\n", res$inciso_d_intervalo_prediccion$nivel_confianza * 100))
  cat(sprintf("   Limite inferior: %.4f\n", res$inciso_d_intervalo_prediccion$limite_inferior))
  cat(sprintf("   Limite superior: %.4f\n", res$inciso_d_intervalo_prediccion$limite_superior))
  cat("   Interpretacion:", res$inciso_d_intervalo_prediccion$interpretacion, "\n")
}

generar_grafica_regresion <- function(res) {
  datos <- res$datos
  modelo <- res$modelo
  x_nuevo <- res$parametros$x_nuevo
  estimacion <- res$inciso_c_estimacion_puntual
  intervalo <- res$inciso_d_intervalo_prediccion
  
  plot(datos$x, datos$y, 
       main = "AUTOMARK vs Profesor",
       xlab = "Calificacion AUTOMARK (x)", 
       ylab = "Calificacion Profesor (y)", 
       pch = 16, col = "blue")
  
  abline(modelo, col = "red", lwd = 2)
  
  points(x_nuevo, estimacion, col = "darkgreen", pch = 17, cex = 1.5)
  grid()
}
