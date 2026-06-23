# Determinantes del salario y de la informalidad laboral en Argentina

**Trabajo Final — Pre-procesamiento de datos**
Licenciatura en Ciencia de Datos · Universidad Austral

---

## Resumen

Este trabajo analiza los **determinantes del salario** y de la **informalidad
laboral** en el mercado de trabajo argentino, utilizando microdatos reales de la
**Encuesta Permanente de Hogares (EPH)** del INDEC. Se integran las técnicas de
pre-procesamiento, análisis exploratorio y modelado (regresión lineal y
logística) vistas a lo largo de la asignatura.

## Objetivos

1. **Regresión lineal:** modelar el (log) salario horario de las personas
   asalariadas en función del capital humano (educación, experiencia) y otras
   características (sexo, horas trabajadas, condición de informalidad, sector,
   región). Marco de referencia: la **ecuación de Mincer**.
2. **Regresión logística:** explicar y predecir la probabilidad de que una
   persona ocupada tenga un **empleo informal** (no registrado) en función de
   las mismas covariables, evaluando la capacidad predictiva del modelo.

> La condición de **informalidad** cumple un doble rol: es la **variable
> respuesta** del modelo logístico y, a la vez, una **variable predictora**
> del modelo lineal de salarios.

## Fuente de datos

- **Encuesta Permanente de Hogares (EPH)** — INDEC, base de individuos.
- Descargada con el paquete de R [`eph`](https://holatam.github.io/eph/).
- Trimestre utilizado: *(a definir — el más reciente disponible).*

## Estructura del repositorio

```
Trabajo Final/
├── README.md            # este archivo
├── informe.qmd          # informe principal (Quarto → HTML)
├── R/
│   └── 01_descarga_datos.R   # descarga y armado de la base con el paquete eph
└── data/
    ├── raw/             # microdatos crudos (no versionados)
    └── processed/       # base procesada para el análisis
```

## Reproducir

Requiere R (>= 4.5) y los paquetes `eph`, `tidyverse`, entre otros.

```r
# 1. Descargar y armar la base
source("R/01_descarga_datos.R")

# 2. Renderizar el informe
quarto::quarto_render("informe.qmd")
```

## Autor

Lautaro Sanfilippo
