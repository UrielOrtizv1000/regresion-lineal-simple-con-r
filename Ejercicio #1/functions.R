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

validar_datos_ejercicio_1 <- function(data) {
  if (!all(c("X", "Y") %in% names(data))) {
    stop("El archivo de datos debe contener las columnas X y Y.", call. = FALSE)
  }

  if (!is.numeric(data$X) || !is.numeric(data$Y)) {
    stop("Las columnas X y Y deben ser numericas.", call. = FALSE)
  }

  if (length(data$X) != length(data$Y)) {
    stop("X y Y deben contener la misma cantidad de observaciones.", call. = FALSE)
  }

  if (length(data$X) != 15) {
    stop("El ejercicio debe contener exactamente 15 observaciones.", call. = FALSE)
  }

  if (anyNA(data)) {
    stop("Los datos no deben contener valores faltantes.", call. = FALSE)
  }

  if (any(!is.finite(data$X)) || any(!is.finite(data$Y))) {
    stop("Los datos no deben contener valores infinitos.", call. = FALSE)
  }

  if (length(unique(data$X)) < 2 || var(data$X) == 0) {
    stop("La variable X no tiene variacion suficiente para la regresion.",
         call. = FALSE)
  }

  x_esperada <- c(
    350, 200, 450, 50, 400,
    150, 350, 300, 150, 500,
    100, 400, 200, 50, 250
  )
  y_esperada <- c(
    36, 20, 45, 5, 40,
    18, 38, 32, 21, 54,
    11, 43, 19, 7, 26
  )

  if (!isTRUE(all.equal(as.numeric(data$X), x_esperada,
                        check.attributes = FALSE)) ||
      !isTRUE(all.equal(as.numeric(data$Y), y_esperada,
                        check.attributes = FALSE))) {
    stop("Los datos no coinciden con los valores originales del ejercicio.",
         call. = FALSE)
  }

  data.frame(
    registros = as.numeric(data$X),
    operaciones_es = as.numeric(data$Y)
  )
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

solve_exercise_1 <- function(data) {
  datos <- validar_datos_ejercicio_1(data)

  # -------------------------------------------------------------------
  # Ejercicio 1, inciso b)
  # Ajusta el modelo de regresion lineal simple, estima beta_0 y beta_1,
  # y construye la ecuacion que se usa en el resto del ejercicio.
  # -------------------------------------------------------------------
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

  # -------------------------------------------------------------------
  # Ejercicio 1, inciso c)
  # Obtiene el intervalo de confianza del 90% para beta_0 y permite
  # revisar si el intercepto poblacional podria ser cero.
  # -------------------------------------------------------------------
  intervalo_90 <- confint(modelo, level = 0.90)
  intervalo_beta_0 <- intervalo_90["(Intercept)", ]

  # -------------------------------------------------------------------
  # Ejercicio 1, inciso d)
  # Obtiene el intervalo de confianza del 90% para beta_1 y permite
  # interpretar el aumento medio por cada mil registros adicionales.
  # -------------------------------------------------------------------
  intervalo_beta_1 <- intervalo_90["registros", ]

  # -------------------------------------------------------------------
  # Ejercicio 1, inciso e)
  # Calcula la correlacion de Pearson y R^2 para medir fuerza de la
  # relacion lineal y proporcion de variacion explicada por el modelo.
  # -------------------------------------------------------------------
  r <- cor(
    datos$registros,
    datos$operaciones_es,
    method = "pearson"
  )
  r_cuadrada <- resumen_modelo$r.squared

  coeficientes_prueba <- resumen_modelo$coefficients
  anova_modelo <- anova(modelo)

  # -------------------------------------------------------------------
  # Ejercicio 1, inciso f)
  # Calcula la estimacion puntual de operaciones de E/S para X = 444,
  # es decir, para un archivo con 444 mil registros.
  # -------------------------------------------------------------------
  nuevo_dato <- data.frame(registros = 444)
  prediccion_puntual <- as.numeric(predict(
    modelo,
    newdata = nuevo_dato
  ))

  # -------------------------------------------------------------------
  # Ejercicio 1, inciso g)
  # Calcula el intervalo de confianza del 95% para la respuesta media
  # cuando X = 444; describe el promedio esperado para esa poblacion.
  # -------------------------------------------------------------------
  intervalo_media <- predict(
    modelo,
    newdata = nuevo_dato,
    interval = "confidence",
    level = 0.95
  )

  # -------------------------------------------------------------------
  # Ejercicio 1, inciso h)
  # Calcula el intervalo de prediccion del 95% para una observacion
  # individual cuando X = 444.
  # -------------------------------------------------------------------
  intervalo_prediccion <- predict(
    modelo,
    newdata = nuevo_dato,
    interval = "prediction",
    level = 0.95
  )

  # -------------------------------------------------------------------
  # Ejercicio 1, inciso i)
  # Obtiene residuos y medidas diagnosticas para evaluar supuestos,
  # detectar observaciones apartadas e identificar posible influencia.
  # -------------------------------------------------------------------
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
  correlacion_abs_residuos_ajustados <- suppressWarnings(
    cor(abs(residuos), valores_ajustados)
  )

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

  # -------------------------------------------------------------------
  # Ejercicio 1, inciso a)
  # Conserva los datos y el nombre de la grafica de dispersion que se
  # genera despues para evaluar visualmente la relacion lineal.
  # -------------------------------------------------------------------
  section_a_result <- list(
    datos = datos,
    grafica = "inciso_a_dispersion.png",
    interpretacion = "Relacion positiva, fuerte y aproximadamente lineal."
  )

  section_b_result <- list(
    modelo = modelo,
    coeficientes = coeficientes,
    beta_0_hat = beta_0_hat,
    beta_1_hat = beta_1_hat,
    ecuacion = ecuacion,
    grafica = "inciso_b_recta_regresion.png"
  )

  section_c_result <- list(
    beta_0_hat = beta_0_hat,
    intervalo_beta_0_90 = intervalo_beta_0,
    contiene_cero = intervalo_beta_0[1] <= 0 && intervalo_beta_0[2] >= 0
  )

  section_d_result <- list(
    beta_1_hat = beta_1_hat,
    intervalo_beta_1_90 = intervalo_beta_1,
    contiene_cero = intervalo_beta_1[1] <= 0 && intervalo_beta_1[2] >= 0
  )

  section_e_result <- list(
    r = r,
    r_cuadrada = r_cuadrada,
    r_cuadrada_porcentaje = 100 * r_cuadrada
  )

  section_f_result <- list(
    nuevo_dato = nuevo_dato,
    prediccion_puntual = prediccion_puntual
  )

  section_g_result <- list(
    nuevo_dato = nuevo_dato,
    intervalo_media_95 = intervalo_media
  )

  section_h_result <- list(
    nuevo_dato = nuevo_dato,
    intervalo_prediccion_95 = intervalo_prediccion
  )

  section_i_result <- list(
    residuos = residuos,
    valores_ajustados = valores_ajustados,
    residuos_estudentizados = residuos_estudentizados,
    distancias_cook = distancias_cook,
    prueba_shapiro = prueba_shapiro,
    indice_mayor_residuo = indice_mayor_residuo,
    indice_mayor_estudentizado = indice_mayor_estudentizado,
    indice_mayor_cook = indice_mayor_cook,
    limite_cook = limite_cook,
    maximo_estudentizado = maximo_estudentizado,
    maxima_cook = maxima_cook,
    correlacion_abs_residuos_ajustados = correlacion_abs_residuos_ajustados,
    graficas = c(
      residuos_x = "inciso_i_residuos_contra_x.png",
      histograma = "inciso_i_histograma_residuos.png",
      qqplot = "inciso_i_qqplot_residuos.png",
      secuencial = "inciso_i_grafica_secuencial.png",
      diagnostico = "inciso_i_diagnostico_completo.png"
    )
  )

  list(
    datos = datos,
    modelo = modelo,
    coeficientes = coeficientes,
    intervalo_beta_0 = intervalo_beta_0,
    intervalo_beta_1 = intervalo_beta_1,
    r = r,
    r_cuadrada = r_cuadrada,
    prediccion_puntual = prediccion_puntual,
    intervalo_media = intervalo_media,
    intervalo_prediccion = intervalo_prediccion,
    residuos = residuos,
    informacion_graficas = section_i_result$graficas,
    section_a = section_a_result,
    section_b = section_b_result,
    section_c = section_c_result,
    section_d = section_d_result,
    section_e = section_e_result,
    section_f = section_f_result,
    section_g = section_g_result,
    section_h = section_h_result,
    section_i = section_i_result,
    analisis_complementario = list(
      coeficientes_prueba = coeficientes_prueba,
      anova_modelo = anova_modelo
    ),
    validaciones = validaciones
  )
}

generar_graficas_ejercicio_1 <- function(lista_resultados, directorio_graficas) {
  crear_directorio(directorio_graficas)

  datos <- lista_resultados$datos
  modelo <- lista_resultados$modelo
  residuos <- lista_resultados$section_i$residuos
  beta_0_hat <- lista_resultados$section_b$beta_0_hat
  beta_1_hat <- lista_resultados$section_b$beta_1_hat

  rutas <- list(
    dispersion = file.path(directorio_graficas, "inciso_a_dispersion.png"),
    recta = file.path(directorio_graficas, "inciso_b_recta_regresion.png"),
    residuos_x = file.path(directorio_graficas, "inciso_i_residuos_contra_x.png"),
    histograma = file.path(directorio_graficas, "inciso_i_histograma_residuos.png"),
    qqplot = file.path(directorio_graficas, "inciso_i_qqplot_residuos.png"),
    secuencial = file.path(directorio_graficas, "inciso_i_grafica_secuencial.png"),
    diagnostico = file.path(directorio_graficas, "inciso_i_diagnostico_completo.png")
  )

  # -------------------------------------------------------------------
  # Ejercicio 1, inciso a)
  # Genera la grafica de dispersion y permite evaluar visualmente si
  # existe una relacion lineal entre X y Y.
  # -------------------------------------------------------------------
  guardar_grafica(rutas$dispersion, function() {
    plot(datos$registros, datos$operaciones_es,
         pch = 19, col = "#1F77B4", cex = 1.25,
         main = "Dispersion: registros y operaciones de E/S",
         xlab = "Numero de registros (miles)",
         ylab = "Operaciones de E/S de disco")
    grid(col = "gray80", lty = "dotted")
    points(datos$registros, datos$operaciones_es,
           pch = 19, col = "#1F77B4", cex = 1.25)
  })

  # -------------------------------------------------------------------
  # Ejercicio 1, inciso b)
  # Genera la grafica de dispersion con la recta de regresion y muestra
  # la ecuacion estimada como leyenda.
  # -------------------------------------------------------------------
  guardar_grafica(rutas$recta, function() {
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

  # -------------------------------------------------------------------
  # Ejercicio 1, inciso i)
  # Genera las graficas de diagnostico de residuos: residuos contra X,
  # histograma, Q-Q plot, grafica secuencial y diagnostico completo.
  # -------------------------------------------------------------------
  guardar_grafica(rutas$residuos_x, function() {
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

  guardar_grafica(rutas$histograma, function() {
    hist(residuos,
         breaks = "Sturges",
         col = "#9ECAE1",
         border = "white",
         main = "Histograma de residuos",
         xlab = "Residuo",
         ylab = "Frecuencia")
    grid(col = "gray85", lty = "dotted")
  })

  guardar_grafica(rutas$qqplot, function() {
    qqnorm(residuos,
           pch = 19,
           col = "#9467BD",
           main = "Grafica Q-Q de residuos")
    qqline(residuos, col = "#D62728", lwd = 2)
    grid(col = "gray85", lty = "dotted")
  })

  guardar_grafica(rutas$secuencial, function() {
    plot(seq_along(residuos), residuos,
         type = "b", pch = 19, col = "#FF7F0E", lwd = 1.5,
         main = "Grafica secuencial de residuos",
         xlab = "Orden de observacion",
         ylab = "Residuo")
    abline(h = 0, col = "#D62728", lwd = 2)
    grid(col = "gray80", lty = "dotted")
  })

  guardar_grafica(rutas$diagnostico, function() {
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

  rutas
}

escribir_resultados_ejercicio_1 <- function(lista_resultados, ruta_resultados,
                                            rutas_graficas) {
  crear_directorio(dirname(ruta_resultados))
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

  datos <- lista_resultados$datos
  modelo <- lista_resultados$modelo
  beta_0_hat <- lista_resultados$section_b$beta_0_hat
  beta_1_hat <- lista_resultados$section_b$beta_1_hat
  ecuacion <- lista_resultados$section_b$ecuacion
  intervalo_beta_0 <- lista_resultados$section_c$intervalo_beta_0_90
  intervalo_beta_1 <- lista_resultados$section_d$intervalo_beta_1_90
  r <- lista_resultados$section_e$r
  r_cuadrada <- lista_resultados$section_e$r_cuadrada
  coeficientes_prueba <- lista_resultados$analisis_complementario$coeficientes_prueba
  anova_modelo <- lista_resultados$analisis_complementario$anova_modelo
  nuevo_dato <- lista_resultados$section_f$nuevo_dato
  prediccion_puntual <- lista_resultados$section_f$prediccion_puntual
  intervalo_media <- lista_resultados$section_g$intervalo_media_95
  intervalo_prediccion <- lista_resultados$section_h$intervalo_prediccion_95
  residuos <- lista_resultados$section_i$residuos
  residuos_estudentizados <- lista_resultados$section_i$residuos_estudentizados
  distancias_cook <- lista_resultados$section_i$distancias_cook
  prueba_shapiro <- lista_resultados$section_i$prueba_shapiro
  indice_mayor_residuo <- lista_resultados$section_i$indice_mayor_residuo
  indice_mayor_estudentizado <- lista_resultados$section_i$indice_mayor_estudentizado
  indice_mayor_cook <- lista_resultados$section_i$indice_mayor_cook
  limite_cook <- lista_resultados$section_i$limite_cook
  maximo_estudentizado <- lista_resultados$section_i$maximo_estudentizado
  maxima_cook <- lista_resultados$section_i$maxima_cook
  correlacion_abs_residuos_ajustados <- lista_resultados$section_i$correlacion_abs_residuos_ajustados

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
                   normalizePath(rutas_graficas$dispersion, winslash = "/", mustWork = FALSE)))
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
                   normalizePath(rutas_graficas$recta, winslash = "/", mustWork = FALSE)))

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
                   normalizePath(rutas_graficas$residuos_x, winslash = "/", mustWork = FALSE)))
  escribir(sprintf("Histograma de residuos: %s",
                   normalizePath(rutas_graficas$histograma, winslash = "/", mustWork = FALSE)))
  escribir(sprintf("Grafica Q-Q de residuos: %s",
                   normalizePath(rutas_graficas$qqplot, winslash = "/", mustWork = FALSE)))
  escribir(sprintf("Grafica secuencial: %s",
                   normalizePath(rutas_graficas$secuencial, winslash = "/", mustWork = FALSE)))
  escribir(sprintf("Diagnostico completo 2x2: %s",
                   normalizePath(rutas_graficas$diagnostico, winslash = "/", mustWork = FALSE)))

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
  escribir(lista_resultados$validaciones)

  seccion("Archivos creados")
  archivos_creados <- c(
    ruta_resultados,
    rutas_graficas$dispersion,
    rutas_graficas$recta,
    rutas_graficas$residuos_x,
    rutas_graficas$histograma,
    rutas_graficas$qqplot,
    rutas_graficas$secuencial,
    rutas_graficas$diagnostico
  )
  escribir(normalizePath(archivos_creados, winslash = "/", mustWork = FALSE))
}
