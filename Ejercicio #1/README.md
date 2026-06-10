# Ejercicio 1: Regresion lineal simple

## Objetivo

Resolver el Ejercicio 1 mediante un programa funcional en R base que ajuste un modelo de regresion lineal simple, genere resultados numericos, interprete los incisos a) a i) y produzca las graficas solicitadas.

## Descripcion del problema

Se estudia la eficiencia de un paquete de software de consulta para acceder y mantener conjuntos de datos a gran escala. La eficiencia se mide mediante el numero de operaciones de entrada/salida de disco requeridas.

- `X = registros`: numero de registros del conjunto de datos, expresado en miles.
- `Y = operaciones_es`: numero de operaciones de entrada/salida de disco.

## Datos utilizados

```csv
X,Y
350,36
200,20
450,45
50,5
400,40
150,18
350,38
300,32
150,21
500,54
100,11
400,43
200,19
50,7
250,26
```

## Requisitos

- Tener R instalado.
- Ejecutar el programa con `Rscript`.
- No se requieren paquetes externos; todo el analisis utiliza R base.

## Como ejecutar

Desde la raiz del repositorio:

```powershell
Rscript "Ejercicio #1/ejercicio_1_regresion_lineal_simple.R"
```

Desde la carpeta del ejercicio:

```powershell
cd "Ejercicio #1"
Rscript ejercicio_1_regresion_lineal_simple.R
```

El script detecta la ubicacion del archivo ejecutado y crea las salidas dentro de `Ejercicio #1/salidas/`, independientemente del directorio desde donde se lance.

## Estructura generada

```text
Ejercicio #1/
|-- data.csv
|-- ejercicio_1_regresion_lineal_simple.R
|-- functions.R
|-- README.md
`-- salidas/
    |-- resultados/
    |   `-- resultados_ejercicio_1.txt
    `-- graficas/
        |-- inciso_a_dispersion.png
        |-- inciso_b_recta_regresion.png
        |-- inciso_i_residuos_contra_x.png
        |-- inciso_i_histograma_residuos.png
        |-- inciso_i_qqplot_residuos.png
        |-- inciso_i_grafica_secuencial.png
        `-- inciso_i_diagnostico_completo.png
```

## Contenido del analisis

- Inciso a): grafica de dispersion e interpretacion visual.
- Inciso b): estimacion puntual de `beta_0` y `beta_1`, ecuacion ajustada, interpretacion y recta de regresion.
- Inciso c): intervalo de confianza del 90% para `beta_0`.
- Inciso d): intervalo de confianza del 90% para `beta_1`.
- Inciso e): coeficiente de correlacion de Pearson `r` y coeficiente de determinacion `R^2`.
- Analisis complementario: prueba de hipotesis para la pendiente y ANOVA del modelo.
- Inciso f): estimacion puntual para un archivo de 444 mil registros.
- Inciso g): intervalo de confianza del 95% para la respuesta media con 444 mil registros.
- Inciso h): intervalo de prediccion del 95% para una observacion individual con 444 mil registros.
- Inciso i): graficas de residuos y analisis de supuestos.

## Interpretacion de los coeficientes

El modelo poblacional es:

```text
Y = beta_0 + beta_1 X + epsilon
```

El intercepto `beta_0` representa el numero medio estimado de operaciones de E/S cuando `X = 0`. Su interpretacion practica es limitada porque cero registros queda fuera del rango observado.

La pendiente `beta_1` representa el cambio medio esperado en las operaciones de E/S por cada aumento de una unidad en `X`. Como `X` esta expresada en miles, una unidad adicional equivale a mil registros adicionales.

## Correlacion y determinacion

El coeficiente `r` mide direccion e intensidad de la asociacion lineal entre `X` y `Y`.

El coeficiente `R^2` mide la proporcion de variacion observada en `Y` que es explicada por el modelo lineal usando `X`.

## Intervalo de confianza y prediccion

El intervalo de confianza para la media estima el numero medio esperado de operaciones de E/S para todos los archivos con 444 mil registros.

El intervalo de prediccion individual estima el rango probable para un archivo individual con 444 mil registros. Este intervalo es mas amplio porque incluye tanto la incertidumbre de la respuesta media como la variabilidad natural de una observacion individual.

## Supuestos del modelo

El programa revisa graficamente:

- Linealidad.
- Varianza constante.
- Normalidad aproximada de los residuos.
- Independencia en el orden de observacion.
- Posibles observaciones atipicas o influyentes.

Tambien incluye la prueba de Shapiro-Wilk como apoyo complementario. La decision principal sobre residuos se apoya en la inspeccion grafica, especialmente porque la muestra es pequena.

## Aclaracion sobre las unidades

La variable `X` esta expresada en miles de registros. Los valores de `Y` se conservan en las unidades originales de la tabla.

Aunque algunos incisos mencionan operaciones de E/S en miles, el enunciado no proporciona informacion suficiente para transformar `Y`. Por lo tanto, el programa no multiplica ni divide `Y` entre 1,000.

## Nota complementaria

La prueba de hipotesis y el ANOVA se presentan como analisis complementarios porque el proyecto general los menciona, aunque no aparecen como un inciso independiente del Ejercicio 1.
