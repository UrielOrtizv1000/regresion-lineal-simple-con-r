```python
readme_content = """# Análisis de Calificaciones: AUTOMARK vs. Profesor

Este proyecto contiene un programa en R desarrollado para evaluar la efectividad del sistema automatizado de calificación AUTOMARK en comparación con las calificaciones asignadas por un profesor para tareas de FORTRAN77.

## Estructura del Proyecto

El programa está dividido en los siguientes archivos para cumplir con los requerimientos de modularidad:

1. **`funciones.R`**: Archivo que contiene la lógica matemática y estadística. Define la función `realizar_analisis_calificaciones`, la cual calcula la regresión lineal por mínimos cuadrados, realiza la prueba de hipótesis T, genera la estimación puntual y calcula el intervalo de predicción. Retorna todos los resultados estructurados en una lista de R.
2. **`main.R`**: Archivo principal de ejecución. Carga las funciones, define los datos (ya sea internamente o mediante un archivo externo), invoca el análisis y muestra los resultados formateados en la consola.
3. **`datos.csv`** *(Opcional)*: Archivo externo para alimentar los datos del modelo de forma dinámica.

## Requisitos Previos

Tener instalado R (versión 4.0 o superior recomendada). No se requieren paquetes externos adicionales, ya que el análisis utiliza funciones nativas de R (`lm`, `predict`, `summary`).

## Instrucciones de Uso

### Opción 1: Uso con datos integrados (Predeterminado)

Por defecto, `main.R` ya incluye los 33 pares de datos extraídos de la tabla del ejercicio. Para ejecutar el análisis:

1. Coloque los archivos `main.R` y `funciones.R` en el mismo directorio de trabajo.
2. Abra su consola de R o IDE (como RStudio) y establezca el directorio de trabajo donde están los archivos.
3. Ejecute el archivo principal con el siguiente comando:

```R
source("main.R")

```

### Opción 2: Uso con archivo externo (Alimentación dinámica)

Si desea utilizar un archivo externo para cambiar los datos del análisis:

1. Cree un archivo llamado `datos.csv` en el mismo directorio.
2. Asegúrese de que el archivo tenga el siguiente formato de columnas (usando `x` para AUTOMARK y `y` para el Profesor):

```csv
x,y
12.2,10
10.6,11
15.1,12
16.2,12

```

3. Abra el archivo `main.R` y modifique las líneas de carga de datos para activar la lectura externa desmarcando el comentario:

```R
# Descomente esta línea y comente el bloque de vectores x e y inferiores
datos <- read.csv("datos.csv")

```

4. Ejecute el programa usando `source("main.R")`.

## Parámetros Modificables

En el archivo `main.R` puede ajustar las variables del problema según sus necesidades:

* `nivel_significancia`: Cambia el valor de alfa ($\\\alpha$) para la prueba de hipótesis T (por defecto `0.10`).
* `x_evaluar`: El valor de la calificación AUTOMARK para la estimación puntual e intervalo de predicción (por defecto `17.5`).
* `confianza_prediccion`: El nivel de confianza para el intervalo de predicción (por defecto `0.95`).

## Descripción de los Resultados en Consola

Al ejecutar el programa, se imprimirá un reporte limpio con la siguiente estructura:

* **a) Ecuación de mínimos cuadrados:** Muestra la recta de regresión lineal obtenida.
* **b) Prueba de hipótesis T:** Detalla el estadístico t, el valor p, y concluye si se rechaza o no la hipótesis nula ($H_0: \\\beta_1 = 0$) basándose en el nivel de significancia seleccionado.
* **c) Estimación puntual:** Muestra el valor esperado de la calificación del profesor para la nota de AUTOMARK definida.
* **d) Intervalo de predicción:** Muestra los límites inferior y superior junto con una interpretación formal del intervalo bajo el nivel de confianza establecido.
