# =============================================================================
# 02_procesamiento.R
# Armado de la base analítica a partir del microdato crudo de la EPH
# Población objetivo: asalariados ocupados de 18 a 65 años con ingreso > 0
# =============================================================================

suppressMessages({library(dplyr); library(forcats)})

cruda <- readRDS("data/raw/eph_individual_2025_T4.rds")

base <- cruda %>%
  # --- Filtros de población objetivo ----------------------------------------
  filter(
    ESTADO == 1,            # ocupados
    CAT_OCUP == 3,          # asalariados
    CH06 >= 18, CH06 <= 65, # edad en edad de trabajar
    P21 > 0,                # ingreso positivo (descarta -9 = Ns/Nr y 0)
    !is.na(PP3E_TOT), PP3E_TOT > 0, PP3E_TOT != 999, # horas válidas
    PP07H %in% c(1, 2)      # con dato de registro (formal/informal)
  ) %>%
  # --- Variables derivadas y recodificación ---------------------------------
  transmute(
    # respuesta lineal: salario horario (P21 mensual / horas mensuales)
    salario_horario = P21 / (PP3E_TOT * 4.33),
    log_salario     = log(salario_horario),
    # respuesta logística (Informal = "éxito" por ser 2º nivel)
    informal = factor(if_else(PP07H == 2, "Informal", "Formal"),
                      levels = c("Formal", "Informal")),
    # predictoras
    edad  = CH06,
    horas = PP3E_TOT,
    sexo  = factor(if_else(CH04 == 1, "Varon", "Mujer"),
                   levels = c("Varon", "Mujer")),
    nivel_ed = factor(case_when(
      NIVEL_ED %in% c(7, 1) ~ "Primario incompleto o menos",
      NIVEL_ED == 2 ~ "Primario completo",
      NIVEL_ED == 3 ~ "Secundario incompleto",
      NIVEL_ED == 4 ~ "Secundario completo",
      NIVEL_ED == 5 ~ "Superior incompleto",
      NIVEL_ED == 6 ~ "Superior completo"
    ), levels = c("Primario incompleto o menos", "Primario completo",
                  "Secundario incompleto", "Secundario completo",
                  "Superior incompleto", "Superior completo")),
    region = factor(case_when(
      REGION == 1  ~ "GBA",
      REGION == 40 ~ "NOA",
      REGION == 41 ~ "NEA",
      REGION == 42 ~ "Cuyo",
      REGION == 43 ~ "Pampeana",
      REGION == 44 ~ "Patagonia"
    ))
  )

# --- Guardado -----------------------------------------------------------------
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(base, "data/processed/eph_procesada.rds")

# --- Diagnóstico --------------------------------------------------------------
cat("Filas base analítica:", nrow(base), "\n")
cat("NAs por variable:\n"); print(colSums(is.na(base)))
cat("\nInformalidad:\n"); print(prop.table(table(base$informal)))
cat("\nNivel educativo:\n"); print(table(base$nivel_ed))
cat("\nRegión:\n"); print(table(base$region))
cat("\nSalario horario (resumen):\n"); print(summary(base$salario_horario))
