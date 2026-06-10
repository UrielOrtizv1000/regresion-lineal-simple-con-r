options(digits = 6)

# ============================================================
# Ejercicio 1: Regresion lineal simple
# ============================================================
# Script principal. Carga funciones, lee los datos desde CSV,
# resuelve el ejercicio, imprime la lista de resultados y crea
# los archivos de salida dentro de Ejercicio #1/salidas/.

obtener_directorio_script <- function() {
  argumentos <- commandArgs(trailingOnly = FALSE)
  argumento_archivo <- argumentos[grepl("^--file=", argumentos)]

  if (length(argumento_archivo) > 0) {
    ruta_script <- sub("^--file=", "", argumento_archivo[1])
    return(dirname(normalizePath(ruta_script, winslash = "/", mustWork = TRUE)))
  }

  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

ejecutar_ejercicio <- function() {
  directorio_script <- obtener_directorio_script()
  ruta_funciones <- file.path(directorio_script, "functions.R")
  ruta_datos <- file.path(directorio_script, "data.csv")
  directorio_salidas <- file.path(directorio_script, "salidas")
  directorio_resultados <- file.path(directorio_salidas, "resultados")
  directorio_graficas <- file.path(directorio_salidas, "graficas")
  ruta_resultados <- file.path(directorio_resultados, "resultados_ejercicio_1.txt")

  if (!file.exists(ruta_funciones)) {
    stop(sprintf("No se encontro el archivo de funciones: %s", ruta_funciones),
         call. = FALSE)
  }
  if (!file.exists(ruta_datos)) {
    stop(sprintf("No se encontro el archivo de datos: %s", ruta_datos),
         call. = FALSE)
  }

  source(ruta_funciones, encoding = "UTF-8")

  crear_directorio(directorio_resultados)
  crear_directorio(directorio_graficas)

  datos_crudos <- read.csv(ruta_datos)

  lista_resultados <- solve_exercise_1(datos_crudos)
  print(lista_resultados)

  # -------------------------------------------------------------------
  # Ejercicio 1, inciso a)
  # Genera la grafica de dispersion y permite evaluar visualmente si
  # existe una relacion lineal entre X y Y.
  # -------------------------------------------------------------------

  # -------------------------------------------------------------------
  # Ejercicio 1, inciso b)
  # Usa el modelo ajustado, sus coeficientes y la recta de regresion
  # para mostrar la ecuacion estimada en la grafica.
  # -------------------------------------------------------------------

  # -------------------------------------------------------------------
  # Ejercicio 1, inciso i)
  # Genera las graficas de residuos que sirven para revisar supuestos
  # del modelo y posibles observaciones atipicas o influyentes.
  # -------------------------------------------------------------------
  rutas_graficas <- generar_graficas_ejercicio_1(
    lista_resultados,
    directorio_graficas
  )

  escribir_resultados_ejercicio_1(
    lista_resultados,
    ruta_resultados,
    rutas_graficas
  )

  invisible(lista_resultados)
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
