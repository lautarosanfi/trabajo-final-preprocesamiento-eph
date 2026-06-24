# Determinantes del salario y de la informalidad laboral en Argentina

Trabajo Final de la materia **Taller de Pre-procesamiento de Datos**.

Análisis de los determinantes del salario horario y de la probabilidad de
informalidad laboral sobre microdatos de la **Encuesta Permanente de Hogares (EPH)**
del INDEC (4.º trimestre de 2025).

## Contenido

- **[`informe_final.qmd`](informe_final.qmd)** — informe completo en Quarto. Es
  **autónomo**: descarga la EPH con el paquete `eph`, hace todo el
  pre-procesamiento (limpieza, imputación), el análisis exploratorio y el modelado
  dentro del mismo documento. Se renderiza de punta a punta.
- **[`informe_final.html`](informe_final.html)** — informe ya renderizado.

## Modelos

- **Regresión lineal** del logaritmo del salario horario: retornos a la educación,
  perfil de edad, brecha de género, penalización por informalidad, prima por tamaño
  del establecimiento y diferencias regionales.
- **Regresión logística** de la informalidad: determinantes y capacidad predictiva
  (ROC/AUC) validada por validación cruzada.

## Reproducir

Requiere **R** y **Quarto**. La descarga de los datos necesita conexión a internet.

```bash
quarto render informe_final.qmd
```

Paquetes utilizados: `tidyverse`, `eph`, `glmnet`, `VIM`, `missForest`, `mice`,
`nnet`, `pROC`, `car`, `lmtest`, `sandwich`, `knitr`, `kableExtra`, `scales`.
