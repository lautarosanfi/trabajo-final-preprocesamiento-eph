# =============================================================================
# 02_procesamiento.R
# Construcción de la base analítica a partir del microdato crudo de la EPH.
#
# Población objetivo: ASALARIADOS OCUPADOS de 18 a 65 años.
# Respuesta lineal: salario horario (requiere ingreso > 0 y horas válidas).
# Respuesta logística: informalidad (registro).
#
# Tratamiento de códigos especiales (se distinguen por su SIGNIFICADO):
#   P21  == -9  -> Ns/Nr ingreso        -> NA  -> CCA (no se imputa la respuesta)
#   P21  ==  0  -> sin ingreso en período-> EXCLUIR (otra población, no es faltante)
#   PP3E_TOT 999/0/NA -> Ns/Nr horas    -> alimenta la respuesta -> CCA
#   PP04C == 99 & PP04C99 == 9 -> Ns/Nr tamaño -> NA -> SE IMPUTA (es predictora)
#   PP04C ==  0 -> servicio doméstico (no aplica establecimiento) -> "Casa particular"
# =============================================================================

suppressMessages({library(dplyr); library(eph)})

cruda <- readRDS("data/raw/eph_individual_2025_T4.rds")

# --- 1. Población objetivo y registro del flujo de casos ----------------------
pob <- cruda %>% filter(ESTADO == 1, CAT_OCUP == 3, CH06 >= 18, CH06 <= 65)
n0 <- nrow(pob)

n_ingreso_nr   <- sum(pob$P21 == -9, na.rm = TRUE)   # no respuesta de ingreso
n_ingreso_cero <- sum(pob$P21 == 0,  na.rm = TRUE)   # sin ingreso en el período
n_horas_inval  <- sum(pob$PP3E_TOT %in% c(0, 999) | is.na(pob$PP3E_TOT))

# Sample analítica: ingreso positivo declarado + horas válidas (CCA sobre la respuesta)
ana <- pob %>% filter(P21 > 0, !PP3E_TOT %in% c(0, 999), !is.na(PP3E_TOT))

# --- 2. Construcción de variables --------------------------------------------
ana <- organize_caes(ana)   # agrega caes_eph_label (sector, 12 ramas)

base <- ana %>% transmute(
  # --- respuesta lineal ---
  salario_horario = P21 / (PP3E_TOT * 4.33),     # P21 mensual / horas mensuales
  log_salario     = log(salario_horario),
  # --- respuesta logística (Informal = "éxito", 2º nivel) ---
  informal = factor(if_else(PP07H == 2, "Informal", "Formal"),
                    levels = c("Formal", "Informal")),
  # --- predictoras ---
  edad  = CH06,
  horas = PP3E_TOT,
  sexo  = factor(if_else(CH04 == 1, "Varon", "Mujer"), levels = c("Varon", "Mujer")),
  nivel_ed = factor(case_when(
    NIVEL_ED %in% c(7, 1) ~ "Primario incompleto o menos",
    NIVEL_ED == 2 ~ "Primario completo",
    NIVEL_ED == 3 ~ "Secundario incompleto",
    NIVEL_ED == 4 ~ "Secundario completo",
    NIVEL_ED == 5 ~ "Superior incompleto",
    NIVEL_ED == 6 ~ "Superior completo"),
    levels = c("Primario incompleto o menos", "Primario completo",
               "Secundario incompleto", "Secundario completo",
               "Superior incompleto", "Superior completo")),
  region = factor(case_when(
    REGION == 1 ~ "GBA", REGION == 40 ~ "NOA", REGION == 41 ~ "NEA",
    REGION == 42 ~ "Cuyo", REGION == 43 ~ "Pampeana", REGION == 44 ~ "Patagonia"),
    levels = c("GBA", "Pampeana", "NOA", "NEA", "Cuyo", "Patagonia")),
  sector = caes_eph_label,
  # --- tamaño del establecimiento (con NA explícito a imputar) ---
  tamano_estab = factor(case_when(
    PP04C == 0                       ~ "Casa particular",
    PP04C %in% 1:5                   ~ "Chico (hasta 5)",
    PP04C %in% 6:8                   ~ "Mediano (6 a 40)",
    PP04C %in% 9:12                  ~ "Grande (mas de 40)",
    PP04C == 99 & PP04C99 == 1       ~ "Chico (hasta 5)",
    PP04C == 99 & PP04C99 == 2       ~ "Mediano (6 a 40)",
    PP04C == 99 & PP04C99 == 3       ~ "Grande (mas de 40)",
    TRUE                             ~ NA_character_),   # PP04C99 == 9 (Ns/Nr real)
    levels = c("Chico (hasta 5)", "Mediano (6 a 40)", "Grande (mas de 40)",
               "Casa particular"))
)

# --- 3. Guardado --------------------------------------------------------------
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(base, "data/processed/eph_procesada.rds")

# --- 4. Auditoría del flujo de casos y faltantes ------------------------------
flujo <- tibble::tibble(
  Etapa = c("Asalariados ocupados 18-65",
            "(-) Ingreso Ns/Nr (P21 = -9) -> NA, CCA",
            "(-) Sin ingreso (P21 = 0) -> excluidos",
            "(-) Horas invalidas (999/0/NA) -> CCA",
            "Base analitica final"),
  n = c(n0, -n_ingreso_nr, -n_ingreso_cero, -n_horas_inval, nrow(base))
)
saveRDS(flujo, "data/processed/diag_flujo.rds")

cat("================ FLUJO DE CASOS ================\n")
cat("Asalariados ocupados 18-65 (inicio):", n0, "\n")
cat("  (-) Ingreso Ns/Nr (P21=-9), NA->CCA :", n_ingreso_nr, "\n")
cat("  (-) Sin ingreso (P21=0), excluidos  :", n_ingreso_cero, "\n")
cat("  (-) Horas inválidas (999/0/NA)      :", n_horas_inval, "\n")
cat("Base analítica final                  :", nrow(base), "\n")
cat("================================================\n\n")

cat("Faltantes por variable en la base final:\n")
print(colSums(is.na(base)))
cat("\nÚnica variable con NA = tamaño del establecimiento (a imputar en 03).\n\n")

cat("tamaño_estab:\n"); print(table(base$tamano_estab, useNA = "ifany"))
cat("\ninformalidad:\n"); print(prop.table(table(base$informal)))
