# main.R

# Cargar el archivo de funciones
source("C:/Users/Dookie/Documents/regresion-lineal-simple-con-r/Ejercicio #6/funciones.R")

# Opcion 1: Cargar datos desde archivo externo (Descomentar para usar)
# Si se tiene un archivo CSV con cabeceras 'x' y 'y'
# datos <- read.csv("datos.csv")

# Opcion 2: Utilizar los datos extraidos de la tabla proporcionada
x <- c(12.2, 10.6, 15.1, 16.2, 16.6, 16.6, 17.2, 17.6, 18.2, 16.5, 17.2,
       18.2, 15.1, 17.2, 17.5, 18.6, 18.8, 17.8, 18.0, 18.2, 18.4, 18.6,
       19.0, 19.3, 19.5, 19.7, 18.6, 19.0, 19.2, 19.4, 19.6, 20.1, 19.2)

y <- c(10, 11, 12, 12, 12, 13, 14, 14, 14, 15, 15,
       15, 16, 16, 16, 16, 16, 17, 17, 17, 17, 17,
       17, 17, 17, 17, 18, 18, 18, 18, 18, 18, 19)

datos <- data.frame(x = x, y = y)

# Parametros del problema
nivel_significancia <- 0.10
x_evaluar <- 17.5
confianza_prediccion <- 0.95

# Llamar a la funcion y almacenar los resultados en una lista
lista_resultados <- realizar_analisis_calificaciones(
  datos = datos,
  alpha = nivel_significancia,
  x_nuevo = x_evaluar,
  nivel_confianza = confianza_prediccion
)

# Mostrar los resultados en consola
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

# Mostrar los resultados formateados en consola
imprimir_resultados_legibles(lista_resultados)

