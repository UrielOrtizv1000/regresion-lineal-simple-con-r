# Script principal del Ejercicio 3 en R

# Cargar funciones
source("regresion_ej3.R")

# Definir rutas
ruta_csv <- "datos_ejercicio3.csv"
ruta_grafica <- "grafica_ejercicio3.png"

# 1. Alimentar datos
datos <- cargar_datos(ruta_csv)
x <- datos$tareas
y <- datos$tiempo

# 2. Obtener resultados
lista_resultados <- calcular_resultados_ej3(x, y, alpha = 0.05, valor_prediccion = 3)

# 3. Mostrar resultados
cat("lista_resultados =\n")
print(lista_resultados)

cat("\n==================================================\n")
cat("INTERPRETACION DE RESULTADOS - EJERCICIO 3 (en R)\n")
cat("==================================================\n")

cat("\na. Analisis de Varianza (ANOVA) con alpha = 0.05:\n")
cat(sprintf("   - Suma de Cuadrados de Regresion (SSR): %.6f\n", lista_resultados$SSR))
cat(sprintf("   - Suma de Cuadrados de Error (SSE): %.6f\n", lista_resultados$SSE))
cat(sprintf("   - Suma de Cuadrados Totales (SST): %.6f\n", lista_resultados$SST))
cat(sprintf("   - Estadistico F observado: %.4f\n", lista_resultados$F_stat))
cat(sprintf("   - p-valor asociado: %.6g\n", lista_resultados$p_value_F))
cat(sprintf("   - Conclusion: %s. \n     Hay evidencia para afirmar que el modelo es util.\n", lista_resultados$conclusion_anova))

cat("\nb. Coeficiente de correlacion (r) y determinacion (r^2):\n")
cat(sprintf("   - r = %.4f: Correlacion lineal positiva muy fuerte.\n", lista_resultados$r_pearson))
cat(sprintf("   - r^2 = %.4f: El %.2f%% de variabilidad es explicada por el numero de tareas.\n", lista_resultados$r_cuadrado, lista_resultados$r_cuadrado * 100))

cat(sprintf("\nc. Estimacion puntual para X = 3 tareas:\n"))
cat(sprintf("   - Ecuacion de la recta: Y^ = %.4f + %.4f * X\n", lista_resultados$beta_0, lista_resultados$beta_1))
cat(sprintf("   - Tiempo estimado (Y^): %.4f unidades estandarizadas.\n", lista_resultados$y_pred_val))
cat("==================================================\n")

# 4. Generar grafica
graficar_regresion_ej3(x, y, lista_resultados$beta_0, lista_resultados$beta_1, ruta_grafica)
cat(sprintf("\n[v] Grafica guardada en: %s\n", ruta_grafica))
