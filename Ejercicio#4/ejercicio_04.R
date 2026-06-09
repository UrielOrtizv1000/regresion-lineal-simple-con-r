# main.R
source("regresion.R")

# Paso 1: Leer datos desde CSV
# El archivo debe tener dos columnas: X y Y
datos = read.csv("datos.csv")

X = datos$X
Y = datos$Y

# Paso 2: Calcular medidas básicas
cat("Media Y:", calcular_media(Y), "\n")
cat("Varianza Y:", calcular_varianza(Y), "\n")
cat("Desviación estándar Y:", calcular_sd(Y), "\n")
cat("Correlación X-Y:", calcular_correlacion(X, Y), "\n")

# Paso 3: Ajustar regresión lineal
modelo = calcular_regresion(X, Y)
print(summary(modelo))

# Paso 4: Graficar dispersión + recta
graficar_regresion(X, Y, modelo)

# Paso 5: Coeficientes
beta0 = coef(modelo)[1]
beta1 = coef(modelo)[2]
cat("β0 =", beta0, " β1 =", beta1, "\n")

# Paso 6: Predicción puntual
prediccion = predecir_valor(modelo, 888)
cat("Atenuación estimada para 888 MHz:", prediccion, "dBL\n")
