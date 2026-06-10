# Analisis de Calificaciones: AUTOMARK vs. Profesor

Este ejercicio analiza la relacion entre la calificacion generada por AUTOMARK (`x`) y la calificacion asignada por el profesor (`y`) para tareas de FORTRAN77.

## Archivos

- `main.R`: script principal. Carga funciones, lee `datos.csv`, ejecuta el analisis y muestra `print(lista_resultados)`.
- `funciones.R`: contiene las funciones reutilizables para validar datos, ajustar el modelo, resolver los incisos e imprimir resultados.
- `datos.csv`: contiene los 33 pares de datos originales en las columnas `x` y `y`.

## Como ejecutar

Desde la raiz del repositorio:

```powershell
Rscript "Ejercicio #6/main.R"
```

Desde la carpeta del ejercicio:

```powershell
cd "Ejercicio #6"
Rscript main.R
```

## Incisos

- Inciso a): obtiene la ecuacion de minimos cuadrados.
- Inciso b): realiza la prueba t para la pendiente con `alpha = 0.10`.
- Inciso c): estima la calificacion del profesor cuando `x = 17.5`.
- Inciso d): calcula el intervalo de prediccion del 95% para una observacion individual cuando `x = 17.5`.

## Resultados principales esperados

- Ecuacion: `y = -0.7995 + 0.9320x`.
- Estadistico t: aproximadamente `8.2954`.
- Valor p: aproximadamente `2.271832e-09`.
- Estimacion puntual para `x = 17.5`: aproximadamente `15.5113`.
- Intervalo de prediccion 95%: aproximadamente `[12.7988, 18.2239]`.