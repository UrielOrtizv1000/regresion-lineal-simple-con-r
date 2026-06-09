options(digits = 6)

# ============================================================
# Ejercicio 1: Regresion lineal simple
# ============================================================
# Este programa usa solo R base. Calcula el modelo de regresion
# lineal simple, genera resultados en texto y guarda graficas PNG.

obtener_directorio_script <- function() {
  argumentos <- commandArgs(trailingOnly = FALSE)
  argumento_archivo <- argumentos[grepl("^--file=", argumentos)]

  if (length(argumento_archivo) > 0) {
    ruta_script <- sub("^--file=", "", argumento_archivo[1])
    return(dirname(normalizePath(ruta_script, winslash = "/", mustWork = TRUE)))
  }

  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(dirname(normalizePath(sys.frames()[[1]]$ofile, winslash = "/", mustWork = TRUE)))
  }

  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

formato_numero <- function(valor, digitos = 6) {
  formatC(as.numeric(valor), digits = digitos, format = "f")
}

formato_p <- function(valor) {
  valor <- as.numeric(valor)
  if (is.na(valor)) {
    return("NA")
  }
  if (valor < 0.000001) {
    return("< 0.000001")
  }
  formato_numero(valor)
}

crear_directorio <- function(ruta) {
  if (!dir.exists(ruta)) {
    dir.create(ruta, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(ruta)) {
    stop(sprintf("No se pudo crear el directorio: %s", ruta), call. = FALSE)
  }
}

guardar_grafica <- function(ruta_archivo, funcion_grafica,
                            ancho = 1600, alto = 1200, resolucion = 150) {
  png(filename = ruta_archivo, width = ancho, height = alto, res = resolucion)
  dispositivo <- dev.cur()

  tryCatch(
    {
      funcion_grafica()
    },
    error = function(error) {
      stop(
        sprintf("No se pudo guardar la grafica %s: %s",
                ruta_archivo, conditionMessage(error)),
        call. = FALSE
      )
    },
    finally = {
      dispositivos_abiertos <- dev.list()
      if (!is.null(dispositivos_abiertos) && dispositivo %in% dispositivos_abiertos) {
        dev.off(dispositivo)
      }
    }
  )
}

validar_datos <- function(registros, operaciones_es,
                          registros_originales, operaciones_originales) {
  if (!is.numeric(registros)) {
    stop("La variable registros debe ser un vector numerico.", call. = FALSE)
  }
  if (!is.numeric(operaciones_es)) {
    stop("La variable operaciones_es debe ser un vector numerico.", call. = FALSE)
  }
  if (length(registros) != length(operaciones_es)) {
    stop("Los vectores registros y operaciones_es deben tener la misma longitud.",
         call. = FALSE)
  }
  if (length(registros) != 15) {
    stop("El ejercicio debe contener exactamente 15 observaciones.", call. = FALSE)
  }
  if (anyNA(registros) || anyNA(operaciones_es)) {
    stop("Los datos no deben contener valores NA.", call. = FALSE)
  }
  if (any(!is.finite(registros)) || any(!is.finite(operaciones_es))) {
    stop("Los datos no deben contener valores infinitos.", call. = FALSE)
  }
  if (length(unique(registros)) < 2 || var(registros) == 0) {
    stop("La variable registros no tiene variacion suficiente para la regresion.",
         call. = FALSE)
  }
  if (!identical(registros, registros_originales) ||
      !identical(operaciones_es, operaciones_originales)) {
    stop("Los datos originales fueron alterados.", call. = FALSE)
  }

  TRUE
}

validar_calculo <- function(nombre, calculado, esperado, tolerancia = 0.001) {
  calculado <- as.numeric(calculado)
  esperado <- as.numeric(esperado)
  diferencia_maxima <- max(abs(calculado - esperado))
  coincide <- isTRUE(all.equal(
    unname(calculado),
    unname(esperado),
    tolerance = tolerancia,
    check.attributes = FALSE
  )) || diferencia_maxima <= tolerancia

  if (!coincide) {
    mensaje <- sprintf(
      "ADVERTENCIA: %s difiere de la referencia. Diferencia maxima = %s.",
      nombre,
      formato_numero(diferencia_maxima)
    )
    warning(mensaje, call. = FALSE)
    return(mensaje)
  }

  sprintf("OK: %s coincide con la referencia dentro de la tolerancia %s.",
          nombre, formato_numero(tolerancia, 3))
}

describir_observacion <- function(indice, datos, residuos,
                                  residuos_estudentizados, distancias_cook) {
  sprintf(
    paste(
      "Observacion %d: X = %s, Y = %s, residuo = %s,",
      "residuo estudentizado = %s, distancia de Cook = %s"
    ),
    indice,
    formato_numero(datos$registros[indice], 0),
    formato_numero(datos$operaciones_es[indice], 0),
    formato_numero(residuos[indice]),
    formato_numero(residuos_estudentizados[indice]),
    formato_numero(distancias_cook[indice])
  )
}

ejecutar_ejercicio <- function() {
  directorio_script <- obtener_directorio_script()
  directorio_salidas <- file.path(directorio_script, "salidas")
  directorio_resultados <- file.path(directorio_salidas, "resultados")
  directorio_graficas <- file.path(directorio_salidas, "graficas")

  crear_directorio(directorio_resultados)
  crear_directorio(directorio_graficas)

  ruta_resultados <- file.path(directorio_resultados, "resultados_ejercicio_1.txt")
  conexion <- file(ruta_resultados, open = "wt", encoding = "UTF-8")
  on.exit(close(conexion), add = TRUE)

  escribir <- function(...) {
    lineas <- unlist(list(...), use.names = FALSE)
    for (linea in lineas) {
      cat(linea, "\n")
      cat(linea, "\n", file = conexion)
    }
  }

  seccion <- function(titulo) {
    escribir("")
    escribir(strrep("=", 72))
    escribir(titulo)
    escribir(strrep("=", 72))
  }

  subseccion <- function(titulo) {
    escribir("")
    escribir(strrep("-", 72))
    escribir(titulo)
    escribir(strrep("-", 72))
  }

  # Datos originales del enunciado. X esta en miles de registros.
  registros_originales <- c(
    350, 200, 450, 50, 400,
    150, 350, 300, 150, 500,
    100, 400, 200, 50, 250
  )

  operaciones_originales <- c(
    36, 20, 45, 5, 40,
    18, 38, 32, 21, 54,
    11, 43, 19, 7, 26
  )

  registros <- registros_originales
  operaciones_es <- operaciones_originales

  validar_datos(registros, operaciones_es,
                registros_originales, operaciones_originales)

  datos <- data.frame(
    registros = registros,
    operaciones_es = operaciones_es
  )

  # lm() ajusta el modelo por minimos cuadrados ordinarios.
  modelo <- lm(
    operaciones_es ~ registros,
    data = datos
  )

  resumen_modelo <- summary(modelo)
  coeficientes <- coef(modelo)
  beta_0_hat <- coeficientes["(Intercept)"]
  beta_1_hat <- coeficientes["registros"]
  ecuacion <- sprintf(
    "Y_hat = %s + %s X",
    formato_numero(beta_0_hat),
    formato_numero(beta_1_hat)
  )

  intervalo_90 <- confint(modelo, level = 0.90)
  intervalo_beta_0 <- intervalo_90["(Intercept)", ]
  intervalo_beta_1 <- intervalo_90["registros", ]

  r <- cor(
    datos$registros,
    datos$operaciones_es,
    method = "pearson"
  )
  r_cuadrada <- resumen_modelo$r.squared

  coeficientes_prueba <- resumen_modelo$coefficients
  anova_modelo <- anova(modelo)

  nuevo_dato <- data.frame(registros = 444)
  prediccion_puntual <- as.numeric(predict(
    modelo,
    newdata = nuevo_dato
  ))

  intervalo_media <- predict(
    modelo,
    newdata = nuevo_dato,
    interval = "confidence",
    level = 0.95
  )

  intervalo_prediccion <- predict(
    modelo,
    newdata = nuevo_dato,
    interval = "prediction",
    level = 0.95
  )

  residuos <- residuals(modelo)
  valores_ajustados <- fitted(modelo)
  residuos_estudentizados <- rstudent(modelo)
  distancias_cook <- cooks.distance(modelo)
  prueba_shapiro <- shapiro.test(residuos)

  indice_mayor_residuo <- which.max(abs(residuos))
  indice_mayor_estudentizado <- which.max(abs(residuos_estudentizados))
  indice_mayor_cook <- which.max(distancias_cook)
  limite_cook <- 4 / nrow(datos)
  maximo_estudentizado <- max(abs(residuos_estudentizados))
  maxima_cook <- max(distancias_cook)
  correlacion_abs_residuos_ajustados <- suppressWarnings(cor(abs(residuos), valores_ajustados))

  ruta_dispersion <- file.path(directorio_graficas, "inciso_a_dispersion.png")
  ruta_recta <- file.path(directorio_graficas, "inciso_b_recta_regresion.png")
  ruta_residuos_x <- file.path(directorio_graficas, "inciso_i_residuos_contra_x.png")
  ruta_histograma <- file.path(directorio_graficas, "inciso_i_histograma_residuos.png")
  ruta_qqplot <- file.path(directorio_graficas, "inciso_i_qqplot_residuos.png")
  ruta_secuencial <- file.path(directorio_graficas, "inciso_i_grafica_secuencial.png")
  ruta_diagnostico <- file.path(directorio_graficas, "inciso_i_diagnostico_completo.png")

  guardar_grafica(ruta_dispersion, function() {
    plot(datos$registros, datos$operaciones_es,
         pch = 19, col = "#1F77B4", cex = 1.25,
         main = "Dispersion: registros y operaciones de E/S",
         xlab = "Numero de registros (miles)",
         ylab = "Operaciones de E/S de disco")
    grid(col = "gray80", lty = "dotted")
    points(datos$registros, datos$operaciones_es,
           pch = 19, col = "#1F77B4", cex = 1.25)
  })

  guardar_grafica(ruta_recta, function() {
    plot(datos$registros, datos$operaciones_es,
         pch = 19, col = "#1F77B4", cex = 1.25,
         main = "Recta de regresion lineal simple",
         xlab = "Numero de registros (miles)",
         ylab = "Operaciones de E/S de disco")
    grid(col = "gray80", lty = "dotted")
    points(datos$registros, datos$operaciones_es,
           pch = 19, col = "#1F77B4", cex = 1.25)
    abline(modelo, col = "#D62728", lwd = 2)
    legend("topleft",
           legend = sprintf("Y_hat = %.3f + %.3f X", beta_0_hat, beta_1_hat),
           bty = "n", lwd = 2, col = "#D62728")
  })

  guardar_grafica(ruta_residuos_x, function() {
    plot(datos$registros, residuos,
         pch = 19, col = "#2CA02C", cex = 1.25,
         main = "Residuos contra registros",
         xlab = "Numero de registros (miles)",
         ylab = "Residuo")
    abline(h = 0, col = "#D62728", lwd = 2)
    grid(col = "gray80", lty = "dotted")
    points(datos$registros, residuos,
           pch = 19, col = "#2CA02C", cex = 1.25)
  })

  guardar_grafica(ruta_histograma, function() {
    hist(residuos,
         breaks = "Sturges",
         col = "#9ECAE1",
         border = "white",
         main = "Histograma de residuos",
         xlab = "Residuo",
         ylab = "Frecuencia")
    grid(col = "gray85", lty = "dotted")
  })

  guardar_grafica(ruta_qqplot, function() {
    qqnorm(residuos,
           pch = 19,
           col = "#9467BD",
           main = "Grafica Q-Q de residuos")
    qqline(residuos, col = "#D62728", lwd = 2)
    grid(col = "gray85", lty = "dotted")
  })

  guardar_grafica(ruta_secuencial, function() {
    plot(seq_along(residuos), residuos,
         type = "b", pch = 19, col = "#FF7F0E", lwd = 1.5,
         main = "Grafica secuencial de residuos",
         xlab = "Orden de observacion",
         ylab = "Residuo")
    abline(h = 0, col = "#D62728", lwd = 2)
    grid(col = "gray80", lty = "dotted")
  })

  guardar_grafica(ruta_diagnostico, function() {
    parametros_originales <- par(no.readonly = TRUE)
    on.exit(par(parametros_originales), add = TRUE)
    par(mfrow = c(2, 2))

    plot(datos$registros, residuos,
         pch = 19, col = "#2CA02C",
         main = "Residuos contra registros",
         xlab = "Registros (miles)", ylab = "Residuo")
    abline(h = 0, col = "#D62728", lwd = 2)
    grid(col = "gray80", lty = "dotted")

    hist(residuos, breaks = "Sturges",
         col = "#9ECAE1", border = "white",
         main = "Histograma de residuos",
         xlab = "Residuo", ylab = "Frecuencia")
    grid(col = "gray85", lty = "dotted")

    qqnorm(residuos, pch = 19, col = "#9467BD",
           main = "Grafica Q-Q de residuos")
    qqline(residuos, col = "#D62728", lwd = 2)
    grid(col = "gray85", lty = "dotted")

    plot(seq_along(residuos), residuos,
         type = "b", pch = 19, col = "#FF7F0E", lwd = 1.5,
         main = "Residuos en orden original",
         xlab = "Orden", ylab = "Residuo")
    abline(h = 0, col = "#D62728", lwd = 2)
    grid(col = "gray80", lty = "dotted")
  }, ancho = 1800, alto = 1400, resolucion = 150)

  validaciones <- c(
    validar_calculo("beta_0_hat", beta_0_hat, 1.403153),
    validar_calculo("beta_1_hat", beta_1_hat, 0.101014),
    validar_calculo("r", r, 0.991981),
    validar_calculo("R_cuadrada", r_cuadrada, 0.984026),
    validar_calculo("prediccion puntual X = 444", prediccion_puntual, 46.253153),
    validar_calculo("IC 90% beta_0", intervalo_beta_0, c(-0.464957, 3.271263)),
    validar_calculo("IC 90% beta_1", intervalo_beta_1, c(0.094692, 0.107335)),
    validar_calculo("IC 95% respuesta media",
                    intervalo_media[1, c("lwr", "upr")],
                    c(44.467993, 48.038314)),
    validar_calculo("IP 95% observacion individual",
                    intervalo_prediccion[1, c("lwr", "upr")],
                    c(41.693654, 50.812653))
  )

  seccion("1. Informacion del problema")
  escribir("Se analiza la relacion entre el tamano de un conjunto de datos y el numero de operaciones de E/S de disco.")
  escribir("X = numero de registros contenidos en el conjunto de datos, expresado en miles.")
  escribir("Y = numero de operaciones de entrada/salida de disco, en las unidades originales de la tabla.")
  escribir("Modelo poblacional: Y = beta_0 + beta_1 X + epsilon.")
  escribir("Modelo ajustado con lm(), que estima los coeficientes por minimos cuadrados ordinarios.")

  seccion("2. Datos utilizados")
  escribir(capture.output(print(datos)))
  escribir("Validacion inicial de datos: aprobada.")

  seccion("3. Inciso a)")
  escribir(sprintf("Grafica guardada en: %s",
                   normalizePath(ruta_dispersion, winslash = "/", mustWork = FALSE)))
  escribir("Interpretacion: la nube de puntos muestra un patron visual positivo, fuerte y aproximadamente lineal.")
  escribir("A medida que aumenta el numero de registros, tambien aumenta el numero de operaciones de E/S.")

  seccion("4. Inciso b)")
  escribir(sprintf("beta_0_hat = %s", formato_numero(beta_0_hat)))
  escribir(sprintf("beta_1_hat = %s", formato_numero(beta_1_hat)))
  escribir(sprintf("Ecuacion estimada: %s", ecuacion))
  escribir("Interpretacion de beta_0: representa el numero medio estimado de operaciones de E/S cuando X = 0.")
  escribir("Su interpretacion practica es limitada porque cero registros queda fuera del rango observado.")
  escribir(sprintf(
    "Interpretacion de beta_1: por cada aumento de una unidad en X, es decir, por cada mil registros adicionales, se estiman en promedio %s operaciones de E/S adicionales.",
    formato_numero(beta_1_hat, 4)
  ))
  escribir(sprintf("Grafica guardada en: %s",
                   normalizePath(ruta_recta, winslash = "/", mustWork = FALSE)))

  seccion("5. Inciso c)")
  escribir(sprintf("Estimacion puntual de beta_0: %s", formato_numero(beta_0_hat)))
  escribir(sprintf("IC 90%% para beta_0: [%s, %s]",
                   formato_numero(intervalo_beta_0[1]),
                   formato_numero(intervalo_beta_0[2])))
  contiene_cero_beta_0 <- intervalo_beta_0[1] <= 0 && intervalo_beta_0[2] >= 0
  escribir(sprintf("El intervalo contiene cero: %s",
                   ifelse(contiene_cero_beta_0, "si", "no")))
  if (contiene_cero_beta_0) {
    escribir("Con 90% de confianza no existe evidencia suficiente para concluir que el intercepto poblacional sea diferente de cero.")
  }
  escribir("Ademas, la utilidad practica del intercepto es limitada porque X = 0 esta fuera del rango observado.")

  seccion("6. Inciso d)")
  escribir(sprintf("Estimacion puntual de beta_1: %s", formato_numero(beta_1_hat)))
  escribir(sprintf("IC 90%% para beta_1: [%s, %s]",
                   formato_numero(intervalo_beta_1[1]),
                   formato_numero(intervalo_beta_1[2])))
  contiene_cero_beta_1 <- intervalo_beta_1[1] <= 0 && intervalo_beta_1[2] >= 0
  escribir(sprintf("El intervalo contiene cero: %s",
                   ifelse(contiene_cero_beta_1, "si", "no")))
  if (!contiene_cero_beta_1 && intervalo_beta_1[1] > 0) {
    escribir("Todo el intervalo es positivo, por lo que existe evidencia de una relacion lineal positiva.")
  }
  escribir("Como X esta en miles de registros, el intervalo estima el aumento medio de operaciones de E/S por cada mil registros adicionales.")

  seccion("7. Inciso e)")
  escribir(sprintf("Coeficiente de correlacion de Pearson r = %s", formato_numero(r)))
  escribir("Direccion: positiva.")
  escribir("Intensidad: muy fuerte.")
  escribir(sprintf("Coeficiente de determinacion R^2 = %s", formato_numero(r_cuadrada)))
  escribir(sprintf("R^2 como porcentaje = %s%%", formato_numero(100 * r_cuadrada, 2)))
  escribir("Interpretacion: aproximadamente ese porcentaje de la variacion de Y es explicado por X mediante el modelo lineal.")
  escribir("Los valores de r y R^2 si corresponden con lo observado en la grafica de dispersion, porque la grafica muestra una relacion lineal positiva muy fuerte.")

  seccion("8. Prueba de hipotesis y ANOVA complementarios")
  subseccion("Prueba t para la pendiente")
  escribir("H0: beta_1 = 0")
  escribir("H1: beta_1 != 0")
  escribir(sprintf("Estimacion de la pendiente: %s",
                   formato_numero(coeficientes_prueba["registros", "Estimate"])))
  escribir(sprintf("Error estandar: %s",
                   formato_numero(coeficientes_prueba["registros", "Std. Error"])))
  escribir(sprintf("Estadistico t: %s",
                   formato_numero(coeficientes_prueba["registros", "t value"])))
  escribir(sprintf("Grados de libertad: %s", formato_numero(df.residual(modelo), 0)))
  escribir(sprintf("Valor p: %s",
                   formato_p(coeficientes_prueba["registros", "Pr(>|t|)"])))
  if (coeficientes_prueba["registros", "Pr(>|t|)"] < 0.05) {
    escribir("Con alpha = 0.05 se rechaza H0. La pendiente es estadisticamente diferente de cero.")
  } else {
    escribir("Con alpha = 0.05 no se rechaza H0.")
  }

  subseccion("ANOVA del modelo")
  escribir(capture.output(print(anova_modelo)))
  escribir(sprintf("Suma de cuadrados de regresion: %s",
                   formato_numero(anova_modelo["registros", "Sum Sq"])))
  escribir(sprintf("Suma de cuadrados del error: %s",
                   formato_numero(anova_modelo["Residuals", "Sum Sq"])))
  escribir(sprintf("Grados de libertad de regresion: %s",
                   formato_numero(anova_modelo["registros", "Df"], 0)))
  escribir(sprintf("Grados de libertad del error: %s",
                   formato_numero(anova_modelo["Residuals", "Df"], 0)))
  escribir(sprintf("Cuadrado medio de regresion: %s",
                   formato_numero(anova_modelo["registros", "Mean Sq"])))
  escribir(sprintf("Cuadrado medio del error: %s",
                   formato_numero(anova_modelo["Residuals", "Mean Sq"])))
  escribir(sprintf("Estadistico F: %s",
                   formato_numero(anova_modelo["registros", "F value"])))
  escribir(sprintf("Valor p del ANOVA: %s",
                   formato_p(anova_modelo["registros", "Pr(>F)"])))
  escribir("En regresion lineal simple, la prueba t de la pendiente y la prueba F del ANOVA conducen a la misma conclusion sobre la utilidad del modelo.")

  seccion("9. Inciso f)")
  escribir(sprintf("Valor de X utilizado: %s mil registros",
                   formato_numero(nuevo_dato$registros, 0)))
  escribir(sprintf("Estimacion puntual: %s operaciones de E/S",
                   formato_numero(prediccion_puntual)))
  escribir("Interpretacion: es el numero medio estimado de operaciones de E/S para un archivo de 444 mil registros.")

  seccion("10. Inciso g)")
  escribir(sprintf("Estimacion puntual: %s",
                   formato_numero(intervalo_media[1, "fit"])))
  escribir(sprintf("IC 95%% para la respuesta media: [%s, %s]",
                   formato_numero(intervalo_media[1, "lwr"]),
                   formato_numero(intervalo_media[1, "upr"])))
  escribir("Interpretacion: este intervalo corresponde al numero medio esperado de operaciones de E/S para la poblacion de todos los archivos que tuvieran 444 mil registros.")
  escribir("No es un intervalo para un archivo individual.")

  seccion("11. Inciso h)")
  escribir(sprintf("Estimacion puntual: %s",
                   formato_numero(intervalo_prediccion[1, "fit"])))
  escribir(sprintf("Intervalo de prediccion 95%%: [%s, %s]",
                   formato_numero(intervalo_prediccion[1, "lwr"]),
                   formato_numero(intervalo_prediccion[1, "upr"])))
  escribir("Interpretacion: este intervalo describe el numero observado de operaciones de E/S para un archivo individual de 444 mil registros.")
  escribir("Comparacion: el intervalo de prediccion es mas amplio que el intervalo de confianza para la media porque incorpora la incertidumbre de la media y la variabilidad natural de una observacion individual alrededor de la recta.")

  seccion("12. Inciso i)")
  escribir(sprintf("Grafica de residuos contra X: %s",
                   normalizePath(ruta_residuos_x, winslash = "/", mustWork = FALSE)))
  escribir(sprintf("Histograma de residuos: %s",
                   normalizePath(ruta_histograma, winslash = "/", mustWork = FALSE)))
  escribir(sprintf("Grafica Q-Q de residuos: %s",
                   normalizePath(ruta_qqplot, winslash = "/", mustWork = FALSE)))
  escribir(sprintf("Grafica secuencial: %s",
                   normalizePath(ruta_secuencial, winslash = "/", mustWork = FALSE)))
  escribir(sprintf("Diagnostico completo 2x2: %s",
                   normalizePath(ruta_diagnostico, winslash = "/", mustWork = FALSE)))

  seccion("13. Analisis de los supuestos")
  escribir("Linealidad: la grafica de dispersion y los residuos contra X no muestran una curvatura sistematica clara.")
  escribir(sprintf(
    "Homocedasticidad: la correlacion entre |residuos| y valores ajustados es %s; no se observa una senal grafica clara de varianza no constante.",
    formato_numero(correlacion_abs_residuos_ajustados)
  ))
  escribir(sprintf(
    "Normalidad: el Q-Q plot y el histograma deben leerse como apoyo grafico. Shapiro-Wilk produce W = %s y valor p = %s.",
    formato_numero(prueba_shapiro$statistic),
    formato_p(prueba_shapiro$p.value)
  ))
  if (prueba_shapiro$p.value < 0.05) {
    escribir("Con alpha = 0.05, la prueba complementaria sugiere desviaciones de normalidad; con n pequeno se conserva una interpretacion prudente basada tambien en graficas.")
  } else {
    escribir("Con alpha = 0.05, la prueba complementaria no aporta evidencia suficiente para rechazar normalidad; con n pequeno no sustituye la inspeccion grafica.")
  }
  escribir("Independencia: la grafica secuencial no muestra ciclos o tendencias prolongadas evidentes en el orden de observacion.")
  if (maximo_estudentizado > 2 || maxima_cook > limite_cook) {
    escribir("No se eliminan observaciones automaticamente, pero existe al menos una observacion que merece revision por residuo estudentizado o distancia de Cook.")
  } else {
    escribir("No se observan violaciones graficas claras a los supuestos principales ni senales fuertes de influencia segun las referencias usadas.")
  }

  seccion("14. Observaciones atipicas e influyentes")
  escribir(sprintf("Referencia para residuo estudentizado: |residuo estudentizado| > 2. Maximo observado = %s.",
                   formato_numero(maximo_estudentizado)))
  escribir(sprintf("Referencia para distancia de Cook: 4/n = %s. Maxima observada = %s.",
                   formato_numero(limite_cook), formato_numero(maxima_cook)))
  escribir("Mayor residuo absoluto:")
  escribir(describir_observacion(indice_mayor_residuo, datos, residuos,
                                 residuos_estudentizados, distancias_cook))
  escribir("Mayor residuo estudentizado absoluto:")
  escribir(describir_observacion(indice_mayor_estudentizado, datos, residuos,
                                 residuos_estudentizados, distancias_cook))
  escribir("Mayor distancia de Cook:")
  escribir(describir_observacion(indice_mayor_cook, datos, residuos,
                                 residuos_estudentizados, distancias_cook))
  if (maximo_estudentizado > 2) {
    escribir("Existe al menos un residuo estudentizado absoluto superior a 2, por lo que conviene revisar esa observacion.")
  } else {
    escribir("Ningun residuo estudentizado absoluto supera 2.")
  }
  if (maxima_cook > limite_cook) {
    escribir("Existe al menos una distancia de Cook superior a 4/n, posible senal de influencia que conviene revisar.")
  } else {
    escribir("Ninguna distancia de Cook supera la referencia 4/n.")
  }

  seccion("15. Aclaracion sobre las unidades")
  escribir("La variable X esta expresada en miles de registros.")
  escribir("Los valores de Y se conservaron exactamente en las unidades originales de la tabla.")
  escribir("Aunque algunos incisos mencionan operaciones de E/S en miles, el enunciado no aporta informacion suficiente para transformar Y.")
  escribir("Por ello, no se multiplico ni dividio Y entre 1,000; los resultados se expresan como operaciones de E/S en las unidades originales.")

  seccion("16. Conclusion general")
  escribir(sprintf(
    "El modelo estimado es %s y muestra una relacion lineal positiva muy fuerte entre registros y operaciones de E/S.",
    ecuacion
  ))
  escribir(sprintf(
    "El valor R^2 = %s indica que el modelo explica aproximadamente %s%% de la variacion observada en Y.",
    formato_numero(r_cuadrada),
    formato_numero(100 * r_cuadrada, 2)
  ))
  escribir(sprintf(
    "Para 444 mil registros, la estimacion puntual es %s operaciones de E/S.",
    formato_numero(prediccion_puntual)
  ))
  escribir("Las graficas de residuos no muestran violaciones claras a los supuestos principales, aunque las observaciones senaladas deben revisarse con prudencia.")

  seccion("Validacion numerica de resultados")
  escribir(validaciones)

  seccion("Archivos creados")
  archivos_creados <- c(
    ruta_resultados,
    ruta_dispersion,
    ruta_recta,
    ruta_residuos_x,
    ruta_histograma,
    ruta_qqplot,
    ruta_secuencial,
    ruta_diagnostico
  )
  escribir(normalizePath(archivos_creados, winslash = "/", mustWork = FALSE))
}

tryCatch(
  {
    ejecutar_ejercicio()
  },
  error = function(error) {
    message("ERROR: ", conditionMessage(error))
    quit(status = 1)
  }
)
