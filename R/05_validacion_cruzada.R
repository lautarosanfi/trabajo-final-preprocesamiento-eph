# =============================================================================
# 05_validacion_cruzada.R
# Evaluación de la capacidad predictiva del modelo logístico de informalidad.
# - Métricas in-sample (umbral 0,5 y óptimo de Youden) + ROC/AUC
# - Validación cruzada estratificada 10-fold, con IMPUTACIÓN DENTRO DE CADA FOLD
#   (donantes sin 'informal', para no filtrar el resultado a predecir)
# =============================================================================

suppressMessages({library(dplyr); library(nnet); library(pROC)})

form_g <- informal ~ nivel_ed + edad + I(edad^2) + tamano_estab + region

# ----- Métricas y matriz de confusión (Informal = positivo) ------------------
metricas <- function(obs, prob, corte) {
  pred <- factor(ifelse(prob >= corte, "Informal", "Formal"),
                 levels = c("Formal", "Informal"))
  VP <- sum(pred=="Informal" & obs=="Informal"); FP <- sum(pred=="Informal" & obs=="Formal")
  FN <- sum(pred=="Formal"   & obs=="Informal"); VN <- sum(pred=="Formal"   & obs=="Formal")
  n <- VP+FP+FN+VN; po <- (VP+VN)/n
  pe <- ((VP+FN)/n)*((VP+FP)/n) + ((FP+VN)/n)*((FN+VN)/n)
  sens <- VP/(VP+FN); spec <- VN/(VN+FP); ppv <- VP/(VP+FP); npv <- VN/(VN+FN)
  c(Exactitud=po, Sensibilidad=sens, Especificidad=spec, VPP=ppv, VPN=npv,
    F1=2*ppv*sens/(ppv+sens), Kappa=(po-pe)/(1-pe))
}

# ===== 1) IN-SAMPLE ==========================================================
d <- readRDS("data/processed/eph_imputada.rds")
g_final <- glm(form_g, family = binomial("logit"), data = d)
prob_is <- predict(g_final, type = "response")
roc_is  <- roc(d$informal, prob_is, levels=c("Formal","Informal"), direction="<", quiet=TRUE)
corte   <- as.numeric(coords(roc_is, "best", best.method="youden", ret="threshold")[1])
cat("AUC in-sample:", round(as.numeric(auc(roc_is)),4),
    " IC:", paste(round(as.numeric(ci.auc(roc_is)),4)[c(1,3)], collapse="-"), "\n")
cat("Corte óptimo (Youden):", round(corte,4), "\n\n")
cat("== Métricas in-sample (corte 0,5) ==\n");      print(round(metricas(d$informal, prob_is, 0.5),3))
cat("== Métricas in-sample (corte óptimo) ==\n");   print(round(metricas(d$informal, prob_is, corte),3))

# ===== 2) VALIDACIÓN CRUZADA estratificada con imputación en cada fold ========
base <- readRDS("data/processed/eph_procesada.rds")
donantes <- c("log_salario","sexo","edad","nivel_ed","region","sector")  # SIN informal
niv3 <- c("Chico (hasta 5)","Mediano (6 a 40)","Grande (mas de 40)")
es_dom <- !is.na(base$tamano_estab) & base$tamano_estab=="Casa particular"

imputar <- function(tr, te){            # imputa tamano_estab en tr y te usando solo tr
  tr_fit <- tr %>% filter(!es_dom_tr, !is.na(tamano_estab))
  mod <- multinom(reformulate(donantes, "t3"),
                  data = transform(tr_fit, t3 = droplevels(factor(tr_fit$tamano_estab, levels=niv3))),
                  trace = FALSE)
  fill <- function(df){
    na_nd <- !df$es_dom & is.na(df$tamano_estab)
    if (any(na_nd)) df$tamano_estab[na_nd] <- as.character(predict(mod, df[na_nd,]))
    factor(df$tamano_estab, levels = c(niv3, "Casa particular"))
  }
  list(tr=fill(tr), te=fill(te))
}

set.seed(2026)
k <- 10
# folds estratificados por 'informal'
fold <- integer(nrow(base))
for (lv in levels(base$informal)) {
  idx <- which(base$informal==lv)
  fold[idx] <- sample(rep(1:k, length.out=length(idx)))
}
base$es_dom <- es_dom
oof <- rep(NA_real_, nrow(base))
for (i in 1:k) {
  tr <- base[fold!=i, ]; te <- base[fold==i, ]
  tr$es_dom_tr <- tr$es_dom
  imp <- imputar(tr, te)
  tr$tamano_estab <- imp$tr; te$tamano_estab <- imp$te
  g <- glm(form_g, family = binomial("logit"), data = tr)
  oof[fold==i] <- predict(g, te, type="response")
}
roc_cv <- roc(base$informal, oof, levels=c("Formal","Informal"), direction="<", quiet=TRUE)
cat("\n== VALIDACIÓN CRUZADA (10-fold, imputación intra-fold) ==\n")
cat("AUC CV:", round(as.numeric(auc(roc_cv)),4),
    " IC:", paste(round(as.numeric(ci.auc(roc_cv)),4)[c(1,3)], collapse="-"), "\n")
corte_cv <- as.numeric(coords(roc_cv, "best", best.method="youden", ret="threshold")[1])
cat("== Métricas CV (corte óptimo) ==\n"); print(round(metricas(base$informal, oof, corte_cv),3))

saveRDS(data.frame(informal=base$informal, prob=oof), "data/processed/cv_oof.rds")
saveRDS(data.frame(informal=d$informal, prob=prob_is), "data/processed/insample_prob.rds")
