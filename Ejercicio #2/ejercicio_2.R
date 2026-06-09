# Script principal para resolver el Ejercicio 2 en R

# Cargar las funciones del archivo separado
source("regresion.R")

# Definir las rutas de los archivos
ruta_csv <- "datos_ejercicio2.csv"
ruta_grafica <- "grafica_ejercicio2.png"

# 1. Alimentar datos desde el archivo CSV
datos <- cargar_datos(ruta_csv)
x <- datos$vehiculos
y <- datos$tiempo

# 2. Devolver los resultados de la función en una lista
# Contraste de hipótesis ANOVA con alpha = 0.05 y estimación puntual para X = 16 vehículos
lista_resultados <- calcular_regresion_y_anova(x, y, alpha = 0.05, valor_prediccion = 16)

# 3. Mostrar los resultados en la consola con print(lista_resultados)
cat("lista_resultados =\n")
print(lista_resultados)

cat("\n==================================================\n")
cat("INTERPRETACIÓN DE RESULTADOS - EJERCICIO 2 (en R)\n")
cat("==================================================\n")
cat(sprintf("a. Diagrama de dispersión generado y guardado en: %s\n", normalizePath(ruta_grafica, mustWork = FALSE)))

cat("\nb. Análisis de Varianza (ANOVA) con alpha = 0.05:\n")
cat(sprintf("   - Suma de Cuadrados de Regresión (SSR): %.6f\n", lista_resultados$SSR))
cat(sprintf("   - Suma de Cuadrados de Error (SSE): %.6f\n", lista_resultados$SSE))
cat(sprintf("   - Suma de Cuadrados Totales (SST): %.6f\n", lista_resultados$SST))
cat(sprintf("   - Estadístico F observado: %.4f\n", lista_resultados$F_stat))
cat(sprintf("   - p-valor asociado: %.6g\n", lista_resultados$p_value_F))
cat(sprintf("   - Conclusión: %s\n", lista_resultados$conclusion_anova))

cat("\nc. Línea de Mínimos Cuadrados:\n")
cat(sprintf("   - Ecuación: Y^ = %.6f + %.6f * X\n", lista_resultados$beta_0, lista_resultados$beta_1))
cat(sprintf("   - beta_0 (intercepto) estimado: %.6f\n", lista_resultados$beta_0))
cat(sprintf("   - beta_1 (pendiente) estimada: %.6f\n", lista_resultados$beta_1))

cat("\nd. Recta de regresión trazada en el gráfico.\n")

cat("\ne. Interpretación de los coeficientes:\n")
cat(sprintf("   - beta_0 (%.6f): Tiempo esperado de congestionamiento (en minutos) si no hay vehículos (X = 0).\n", lista_resultados$beta_0))
cat(sprintf("   - beta_1 (%.6f): Por cada vehículo adicional, el tiempo de congestionamiento aumenta en %.6f minutos (%.2f segundos).\n", 
            lista_resultados$beta_1, lista_resultados$beta_1, lista_resultados$beta_1 * 60))

cat(sprintf("\nf. Estimación puntual para X = 16 vehículos:\n"))
cat(sprintf("   - Tiempo estimado de congestionamiento: %.6f minutos.\n", lista_resultados$y_pred_val))
cat("==================================================\n")

# 4. Generar y guardar la gráfica
graficar_regresion(x, y, lista_resultados$beta_0, lista_resultados$beta_1, ruta_grafica)
