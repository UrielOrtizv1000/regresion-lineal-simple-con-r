# main.R
# Script principal del Ejercicio 6.
# Carga funciones, lee los datos desde CSV, ejecuta el analisis y muestra
# la lista de resultados solicitada.

options(digits = 6)

obtener_directorio_script <- function() {
  argumentos <- commandArgs(trailingOnly = FALSE)
  argumento_archivo <- argumentos[grepl("^--file=", argumentos)]

  if (length(argumento_archivo) > 0) {
    ruta_script <- sub("^--file=", "", argumento_archivo[1])
    return(dirname(normalizePath(ruta_script, winslash = "/", mustWork = TRUE)))
  }

  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

ejecutar_ejercicio_6 <- function() {
  directorio_script <- obtener_directorio_script()
  ruta_funciones <- file.path(directorio_script, "funciones.R")
  ruta_datos <- file.path(directorio_script, "datos.csv")

  if (!file.exists(ruta_funciones)) {
    stop(sprintf("No se encontro el archivo de funciones: %s", ruta_funciones),
         call. = FALSE)
  }
  if (!file.exists(ruta_datos)) {
    stop(sprintf("No se encontro el archivo de datos: %s", ruta_datos),
         call. = FALSE)
  }

  source(ruta_funciones, encoding = "UTF-8")

  datos <- read.csv(ruta_datos)

  nivel_significancia <- 0.10
  x_evaluar <- 17.5
  confianza_prediccion <- 0.95

  # -------------------------------------------------------------------
  # Ejercicio 6, inciso a)
  # Calcula la ecuacion de minimos cuadrados para explicar la calificacion
  # del profesor a partir de la calificacion AUTOMARK.
  # -------------------------------------------------------------------

  # -------------------------------------------------------------------
  # Ejercicio 6, inciso b)
  # Realiza la prueba de hipotesis t sobre la pendiente del modelo y
  # produce la conclusion con alpha = 0.10.
  # -------------------------------------------------------------------

  # -------------------------------------------------------------------
  # Ejercicio 6, inciso c)
  # Obtiene la estimacion puntual para la calificacion del profesor cuando
  # AUTOMARK es igual a 17.5.
  # -------------------------------------------------------------------

  # -------------------------------------------------------------------
  # Ejercicio 6, inciso d)
  # Calcula el intervalo de prediccion del 95% para una observacion
  # individual cuando AUTOMARK es igual a 17.5.
  # -------------------------------------------------------------------
  lista_resultados <- realizar_analisis_calificaciones(
    datos = datos,
    alpha = nivel_significancia,
    x_nuevo = x_evaluar,
    nivel_confianza = confianza_prediccion
  )

  print(lista_resultados)
  imprimir_resultados_legibles(lista_resultados)

  invisible(lista_resultados)
}

tryCatch(
  {
    ejecutar_ejercicio_6()
  },
  error = function(error) {
    message("ERROR: ", conditionMessage(error))
    quit(status = 1)
  }
)