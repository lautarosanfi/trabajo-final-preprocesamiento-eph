# =============================================================================
# 03_imputacion.R
# Imputación de tamaño del establecimiento (predictora, 622 NA = 5.7%).
# Los faltantes son TODOS no-domésticos -> problema de 3 niveles
# (Chico/Mediano/Grande). Se comparan 3 métodos (U6):
#   - KNN (k=5)        : método principal
#   - missForest       : control (random forest)
#   - MICE (m=5)       : imputación múltiple
# Donantes: incluyen la respuesta (log_salario) -> recomendado en MI para
# preservar la relación predictora-respuesta.
# =============================================================================

suppressMessages({library(dplyr); library(VIM); library(missForest); library(mice)})

base <- readRDS("data/processed/eph_procesada.rds")

donantes <- c("log_salario", "informal", "sexo", "edad",
              "nivel_ed", "region", "sector", "horas")

# --- Separar régimen doméstico (tamaño conocido) de no-domésticos ------------
domestico <- base %>% filter(tamano_estab == "Casa particular")
nodom     <- base %>% filter(is.na(tamano_estab) | tamano_estab != "Casa particular")
nodom$tamano3 <- droplevels(factor(nodom$tamano_estab,
                                   levels = c("Chico (hasta 5)", "Mediano (6 a 40)",
                                              "Grande (mas de 40)")))
idx_na <- which(is.na(nodom$tamano3))
cat("No-domésticos:", nrow(nodom), "| a imputar:", length(idx_na), "\n")

df <- nodom[, c("tamano3", donantes)]

# --- 1. KNN (k = 5) -----------------------------------------------------------
set.seed(2026)
knn_out <- kNN(df, variable = "tamano3", dist_var = donantes, k = 5, imp_var = FALSE)
imp_knn <- knn_out$tamano3[idx_na]

# --- 2. missForest ------------------------------------------------------------
set.seed(2026)
mf_out  <- missForest(as.data.frame(df))$ximp
imp_mf  <- mf_out$tamano3[idx_na]

# --- 3. MICE (m = 5, polytomous) ---------------------------------------------
mice_out <- mice(df, m = 5, method = "polyreg", printFlag = FALSE, seed = 2026)
# distribución agregada de los valores imputados a lo largo de las m=5 réplicas
long <- complete(mice_out, "long")
imp_mice_long <- long$tamano3[long$.id %in% idx_na]
# versión "single" (1ra réplica) para la base final si se eligiera MICE
imp_mice1 <- complete(mice_out, 1)$tamano3[idx_na]

# --- Comparación de distribuciones de los valores imputados ------------------
obs <- prop.table(table(droplevels(na.omit(nodom$tamano3))))   # observados (no-dom)
tab <- rbind(
  Observada_no_dom = round(100 * as.numeric(obs), 1),
  KNN              = round(100 * as.numeric(prop.table(table(imp_knn))), 1),
  missForest       = round(100 * as.numeric(prop.table(table(imp_mf))), 1),
  MICE_m5          = round(100 * as.numeric(prop.table(table(imp_mice_long))), 1)
)
colnames(tab) <- levels(nodom$tamano3)
cat("\n== Distribución (%) de tamaño: observada vs imputada por método ==\n")
print(tab)
saveRDS(as.data.frame(tab), "data/processed/diag_imputacion.rds")

cat("\n== Coincidencia KNN vs missForest en los 622 casos ==\n")
cat(round(100 * mean(imp_knn == imp_mf), 1), "% de acuerdo\n")

# --- Base final: imputación KNN (principal) ----------------------------------
nodom$tamano_estab <- as.character(nodom$tamano_estab)
nodom$tamano_estab[idx_na] <- as.character(imp_knn)
nodom$tamano3 <- NULL
final <- bind_rows(nodom, domestico) %>%
  mutate(tamano_estab = factor(tamano_estab,
           levels = c("Chico (hasta 5)", "Mediano (6 a 40)",
                      "Grande (mas de 40)", "Casa particular")))

saveRDS(final, "data/processed/eph_imputada.rds")
cat("\nBase imputada guardada. NAs restantes:", sum(is.na(final$tamano_estab)), "\n")
cat("Distribución final de tamaño_estab:\n"); print(table(final$tamano_estab))
