# =============================================================================
# 01_descarga_datos.R
# Descarga de microdatos de la EPH (INDEC) mediante el paquete `eph`
# Trabajo Final - Pre-procesamiento de datos
# =============================================================================

# --- Paquetes -----------------------------------------------------------------
paquetes <- c("eph", "tidyverse", "here")
faltan <- paquetes[!sapply(paquetes, requireNamespace, quietly = TRUE)]
if (length(faltan) > 0) install.packages(faltan)

library(eph)
library(tidyverse)

# --- Parámetros ---------------------------------------------------------------
# OJO: verificar el trimestre más reciente disponible al momento de ejecutar.
ANIO      <- 2025
TRIMESTRE <- 3

# --- Descarga de la base de individuos ---------------------------------------
individual <- get_microdata(
  year   = ANIO,
  period = TRIMESTRE,
  type   = "individual"
)

# --- Guardado del crudo -------------------------------------------------------
dir.create("data/raw", showWarnings = FALSE, recursive = TRUE)
saveRDS(
  individual,
  file = sprintf("data/raw/eph_individual_%d_T%d.rds", ANIO, TRIMESTRE)
)

message("Base descargada: ", nrow(individual), " filas, ",
        ncol(individual), " columnas.")
