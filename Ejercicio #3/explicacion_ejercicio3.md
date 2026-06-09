# Explicacion del Programa: Ejercicio 3 (Regresion Lineal Simple)

Este documento explica como esta estructurado el codigo del Ejercicio 3, como ejecutarlo, y que hace cada una de sus funciones.

## 1. Estructura de Archivos

El programa esta dividido en los siguientes archivos clave en la carpeta `Ejercicio #3`:

- **`datos_ejercicio3.csv`**: Contiene la base de datos (Tareas en `X` y Tiempo en `Y`). Se utiliza para cargar los datos.
- **`regresion_ej3.R`**: Guarda las funciones matematicas. Separa la logica de los calculos del script principal.
- **`ejercicio_3.R`**: Script principal. Lee el archivo CSV, llama a las funciones de `regresion_ej3.R` e imprime resultados.
- **`respuestas_ejercicio3.txt`**: Archivo que muestra la salida esperada en consola.

---

## 2. Funciones Principales

La logica esta en `regresion_ej3.R` con dos funciones:

### `cargar_datos(ruta_archivo)`
* **Objetivo:** Leer datos desde una fuente externa (CSV) y convertirlos a un data frame nativo de R.

### `calcular_resultados_ej3(x, y, alpha = 0.05, valor_prediccion = 3)`
* **Objetivo:** Calcular metricas estadisticas y empaquetarlas.
* **Que hace:** 
  1. **Ajuste:** Usa `lm(y ~ x)` para crear el modelo de regresion.
  2. **ANOVA:** Usa `anova(modelo)` para extraer sumas de cuadrados, F y p-valor. Decide si rechazar H0.
  3. **Correlacion:** Calcula `cor(x, y)` (r) y r cuadrado.
  4. **Prediccion:** Estima el tiempo sustituyendo X = 3 en la recta.
* **Salida:** Agrupa los resultados crudos en un objeto `list()` y lo devuelve.

---

## 3. Como ejecutar el programa?

### Opcion 1: Desde RStudio (Recomendado)
1. Abre **`ejercicio_3.R`** en RStudio.
2. Configura tu directorio de trabajo en esta carpeta (`Session > Set Working Directory > To Source File Location`).
3. Ejecuta todo el codigo (boton `Run`). Veras los resultados y se guardara la grafica automaticamente.

### Opcion 2: Desde la Terminal
Si tienes R en tus variables de entorno:
1. Abre la consola (CMD o PowerShell).
2. Navega a la carpeta: `cd "C:\Ruta\Hacia\Ejercicio #3"`
3. Ejecuta: `Rscript ejercicio_3.R`
