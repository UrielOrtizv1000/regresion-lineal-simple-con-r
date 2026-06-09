calcular_media = function(datos) {
  return(mean(datos, na.rm = TRUE))
}

calcular_varianza = function(datos) {
  return(var(datos, na.rm = TRUE))
}

calcular_sd = function(datos) {
  return(sd(datos, na.rm = TRUE))
}

calcular_correlacion = function(x, y) {
  return(cor(x, y))
}

calcular_regresion = function(x, y) {
  modelo = lm(y ~ x)
  return(modelo)
}

graficar_regresion = function(x, y, modelo) {
  plot(x, y,
       main = "Dispersión de datos",
       xlab = "Variable independiente (x)",
       ylab = "Variable dependiente (y)",
       pch = 19, col = "blue")
  abline(modelo, col = "red", lwd = 2)
}

predecir_valor = function(modelo, nuevo_x) {
  nuevo = data.frame(x = nuevo_x)
  return(predict(modelo, nuevo))
}