# =========================================================
# Archivo: Util.R
# =========================================================
# Universidad Autonoma de Aguascalientes
# Centro de Ciencias Basicas
# Departamento de Estadistica
# Inferencia Estadistica
#
# Utileria para el ejercicio 5
# =========================================================

# =========================================================
# validarProbabilidad
# Verifica que el coeficiente de confianza sea valido
# =========================================================
validarProbabilidad = function(valor, nombreParametro) {
    if(valor <= 0 || valor >= 1) {
        stop(paste(nombreParametro, "Debe estar entre 0 y 1."))
    }
}

# =========================================================
# ICbeta1
# Intervalo de confianza para la pendiente de regresion 
# =========================================================
ICbeta1 = function(x, y, coefConf=0.95) {
    
    # Validaciones basicas
    if(length(x) != length(y)) {
        stop("Los vectores x e y deben tener la misma longitud.")
    }
    
    n = length(x)
    if(n <= 2) {
        stop("Se necesitan mas de 2 pares de datos para la regresion.")
    }
    
    validarProbabilidad(coefConf, "coefConf")

    # Inicializacion de variables
    alfa = 1 - coefConf
    alfaMedios = alfa / 2
    gl = n - 2

    # Calculos de medias
    x.barra = mean(x)
    y.barra = mean(y)

    # Sumas de cuadrados
    Sxx = sum((x - x.barra)^2)
    Sxy = sum((x - x.barra) * (y - y.barra))
    Syy = sum((y - y.barra)^2)

    # Estimadores de minimos cuadrados
    beta1.hat = Sxy / Sxx
    beta0.hat = y.barra - (beta1.hat * x.barra)

    # Suma de cuadrados de error (SSE) y varianza residual (s^2)
    SSE = Syy - (beta1.hat * Sxy)
    s2 = SSE / gl
    s = sqrt(s2)

    # Error estandar de la pendiente
    ee_beta1 = s / sqrt(Sxx)
    
    # Estadistico critico (t Student)
    estadistico = qt(alfaMedios, df = gl, lower.tail = FALSE)
    error = estadistico * ee_beta1

    # Limites del intervalo
    lic = beta1.hat - error
    lsc = beta1.hat + error

    # -----------------------------------------------------
    # Procedimiento mecanico
    # -----------------------------------------------------
    pasos = c(
        paste("Tamaño de muestra n =", n),
        paste("Grados de libertad (n-2) =", gl),
        paste("alfa = 1 -", coefConf, "=", round(alfa, 6)),
        paste("Sxx =", round(Sxx, 6)),
        paste("Sxy =", round(Sxy, 6)),
        paste("Inciso b -> Ecuacion estimada: y =", round(beta0.hat, 4), "+", round(beta1.hat, 4), "x"),
        paste("  Interseccion (beta0 hat) =", round(beta0.hat, 6)),
        paste("  Pendiente (beta1 hat) =", round(beta1.hat, 6)),
        paste("Suma de cuadrados del error (SSE) =", round(SSE, 6)),
        paste("Varianza residual (s^2) = SSE / gl =", round(s2, 6)),
        paste("Error estandar de beta1 = s / sqrt(Sxx) =", round(ee_beta1, 6)),
        paste("Distribucion utilizada: t Student"),
        paste("Estadistico critico t =", round(estadistico, 6)),
        paste("Error maximo =", round(estadistico, 6), "*", round(ee_beta1, 6), "=", round(error, 6)),
        paste("LIC =", round(beta1.hat, 6), "-", round(error, 6), "=", round(lic, 6)),
        paste("LSC =", round(beta1.hat, 6), "+", round(error, 6), "=", round(lsc, 6))
    )

    # Construccion del objeto de respuesta
    resultado = list(
        metodo = "IC para pendiente de regresion (beta 1)",
        parametros = list(n = n, coefConf = coefConf),
        calculos = list(
            Sxx = Sxx, 
            Sxy = Sxy, 
            beta0.hat = beta0.hat, 
            beta1.hat = beta1.hat, 
            s = s, 
            ee_beta1 = ee_beta1, 
            estadistico = estadistico, 
            error = error
        ),
        intervalo = list(LIC = lic, LSC = lsc),
        procedimiento = pasos
    )

    return(resultado)
}