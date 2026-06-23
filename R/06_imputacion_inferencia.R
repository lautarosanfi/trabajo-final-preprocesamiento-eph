# =============================================================================
# 06_imputacion_inferencia.R
# ¿Cómo afecta el MÉTODO de imputación a la INFERENCIA (coeficientes + IC)?
# Se ajusta el modelo de salarios bajo CCA, KNN, missForest y MICE, y se
# comparan los coeficientes de la variable imputada (tamaño_estab), con foco
# en el ERROR ESTÁNDAR / ANCHO DEL IC. La imputación simple trata los valores
# imputados como observados (subestima la varianza); MICE la corrige (Rubin).
# =============================================================================

suppressMessages({library(dplyr); library(VIM); library(missForest); library(mice)})

base <- readRDS("data/processed/eph_procesada.rds")
form <- log_salario ~ nivel_ed + edad + I(edad^2) + sexo + informal +
          tamano_estab + region
donantes <- c("log_salario","informal","sexo","edad","nivel_ed","region","sector")
niv3 <- c("Chico (hasta 5)","Mediano (6 a 40)","Grande (mas de 40)")

dom <- base %>% filter(tamano_estab == "Casa particular")
nd  <- base %>% filter(is.na(tamano_estab) | tamano_estab != "Casa particular")
nd$tamano3 <- droplevels(factor(nd$tamano_estab, levels = niv3))
df  <- nd[, c("tamano3", donantes)]

rehacer <- function(t3){
  nd2 <- nd; nd2$tamano_estab <- factor(as.character(t3), levels = c(niv3,"Casa particular"))
  bind_rows(nd2 %>% select(-tamano3), dom)
}
extraer <- function(m, metodo, n){
  s <- summary(m)$coefficients; ci <- confint.default(m)
  f <- grep("tamano_estab", rownames(s))
  data.frame(metodo, termino = gsub("tamano_estab","",rownames(s)[f]),
             est = s[f,1], se = s[f,2], ic_low = ci[f,1], ic_high = ci[f,2],
             ancho = ci[f,2]-ci[f,1], row.names = NULL)
}

# CCA
r_cca <- extraer(lm(form, base), "CCA", NA)
# KNN simple
set.seed(2026); knn <- kNN(df, variable="tamano3", dist_var=donantes, k=5, imp_var=FALSE)
r_knn <- extraer(lm(form, rehacer(knn$tamano3)), "KNN (simple)", NA)
# missForest simple
set.seed(2026); mf <- missForest(as.data.frame(df))$ximp
r_mf <- extraer(lm(form, rehacer(mf$tamano3)), "missForest (simple)", NA)
# MICE multiple (m=20) + reglas de Rubin
mice_nd <- mice(df, m=20, method="polyreg", printFlag=FALSE, seed=2026)
fits <- lapply(1:20, function(i) lm(form, rehacer(complete(mice_nd,i)$tamano3)))
M <- 20; coefs <- sapply(fits, coef); vars <- sapply(fits, function(f) diag(vcov(f)))
Qbar <- rowMeans(coefs); Ubar <- rowMeans(vars); B <- apply(coefs,1,var)
se <- sqrt(Ubar + (1+1/M)*B)                       # varianza total de Rubin
f <- grep("tamano_estab", names(Qbar))
r_mice <- data.frame(metodo="MICE (multiple)", termino=gsub("tamano_estab","",names(Qbar)[f]),
  est=Qbar[f], se=se[f], ic_low=Qbar[f]-1.96*se[f], ic_high=Qbar[f]+1.96*se[f],
  ancho=2*1.96*se[f], row.names=NULL)

comp <- bind_rows(r_cca, r_knn, r_mf, r_mice) %>%
  mutate(across(c(est,se,ic_low,ic_high,ancho), \(x) round(x,4))) %>%
  arrange(termino, metodo)
print(comp, row.names=FALSE)

cat("\n== SE relativo a KNN (=100) por término ==\n")
comp %>% group_by(termino) %>%
  mutate(se_rel = round(100*se/se[metodo=="KNN (simple)"],1)) %>%
  select(termino, metodo, se, ancho, se_rel) %>% as.data.frame() %>% print()

saveRDS(comp, "data/processed/diag_imput_inferencia.rds")
