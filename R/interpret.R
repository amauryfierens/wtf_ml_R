# Lecture automatique des résultats, en phrases simples, pour l'encadré « Ce qu'il faut retenir ».

strength_label <- function(value, metric) {
  if (metric == "r2") {
    if (value < 0.02) "aucune capacité prédictive" else if (value < 0.10) "une capacité prédictive faible"
    else if (value < 0.30) "une capacité prédictive modérée" else "une capacité prédictive forte"
  } else {
    if (value < 0.55) "aucune capacité prédictive" else if (value < 0.65) "une capacité prédictive faible"
    else if (value < 0.75) "une capacité prédictive modérée" else "une capacité prédictive forte"
  }
}

fmt <- function(x) formatC(x, format = "f", digits = 2, decimal.mark = ",")

# Renvoie une liste de phrases (HTML) sur une cible x un jeu de prédicteurs
interpret_target <- function(res, target, fs) {
  metric <- main_metric(res); col <- paste0(metric, "_mean")
  sc <- res$scores[res$scores$target == target & res$scores$features == fs, ]
  if (!nrow(sc)) return(character(0))
  base <- sc[sc$model == "baseline", col]
  cand <- sc[sc$model != "baseline", ]
  best <- cand[which.max(cand[[col]]), ]
  out <- character(0)

  out <- c(out, sprintf(
    "Pour <b>%s</b>, le meilleur modèle (<b>%s</b>) atteint %s = <b>%s</b> sur des répondants qu'il n'a jamais vus, soit %s%s.",
    label_of(target), MODEL_LABELS()[[best$model]], METRIC_NAME[[metric]], fmt(best[[col]]),
    strength_label(best[[col]], metric),
    if (metric == "r2") sprintf(" (environ %d %% des différences entre répondants sont expliquées)", max(0, round(100 * best[[col]]))) else ""))

  # Écart-type entre plis : précision de l'estimation
  sdv <- best[[paste0(metric, "_sd")]]
  if (!is.na(sdv) && sdv > 0.1)
    out <- c(out, sprintf("Attention : ce score varie beaucoup d'un pli à l'autre (écart-type %s) ; il est peu précis, souvent à cause d'un petit échantillon.", fmt(sdv)))

  # Linéaire vs non linéaire
  lin <- cand[cand$model %in% c("ols", "elasticnet"), ]
  nonlin <- cand[cand$model %in% c("random_forest", "grad_boosting"), ]
  if (nrow(lin) && nrow(nonlin)) {
    d <- max(nonlin[[col]]) - max(lin[[col]])
    out <- c(out, if (d > 0.03)
      sprintf("Les modèles à base d'arbres font mieux que la régression (+%s) : il y a probablement des effets non linéaires ou des interactions.", fmt(d))
    else "La régression fait aussi bien que les modèles plus complexes : les liens semblent surtout <b>linéaires et additifs</b>, une régression classique suffit donc pour les interpréter.")
  }

  # Variables les plus utiles
  if (!is.null(res$importance)) {
    imp <- res$importance[res$importance$target == target & res$importance$features == fs, ]
    imp <- utils::head(imp[order(-imp$importance), ], 3)
    imp <- imp[imp$importance > 0, ]
    if (nrow(imp)) {
      sgn <- if (!is.null(res$coefs)) {
        cf <- res$coefs[res$coefs$target == target & res$coefs$features == fs, ]
        vapply(imp$variable, function(v) { k <- cf$coef_std[cf$variable == v]; if (length(k) == 1) sign(k) else 0 }, 0)
      } else rep(0, nrow(imp))
      desc <- sprintf("%s%s", label_of(imp$variable),
                      c("-1" = " (plus elle est élevée, plus la cible est basse)", "0" = "",
                        "1" = " (plus elle est élevée, plus la cible est élevée)")[as.character(sgn)])
      out <- c(out, paste0("Variables les plus utiles : ", paste(sprintf("<b>%s</b>", desc), collapse = " ; "), "."))
    }
  }
  out
}

interpret_results <- function(res) {
  metric <- main_metric(res); col <- paste0(metric, "_mean")
  sc <- res$scores
  out <- character(0)
  if (isTRUE(res$spec$shuffle))
    out <- c(out, "<b>Contrôle « cible mélangée »</b> : les scores devraient être proches du hasard. S'ils le sont, l'outil ne « triche » pas.")

  # Cible principale : la première choisie, avec le jeu de prédicteurs le plus performant
  target <- sc$target[1]
  fs_best <- names(which.max(tapply(sc[sc$target == target & sc$model != "baseline", col],
                                    sc$features[sc$target == target & sc$model != "baseline"], max)))
  out <- c(out, interpret_target(res, target, fs_best))

  # Effet des variables proximales
  if (all(c("scores", "scores+proximal") %in% sc$features)) {
    a <- max(sc[sc$target == target & sc$features == "scores" & sc$model != "baseline", col])
    b <- max(sc[sc$target == target & sc$features == "scores+proximal" & sc$model != "baseline", col])
    out <- c(out, sprintf(
      "Ajouter les variables proximales (sacrifice, comportements hypothétiques) fait passer le %s de %s à %s. Elles mesurent presque la même chose que la cible : ce gain n'est pas une découverte.",
      METRIC_NAME[[metric]], fmt(a), fmt(b)))
  }

  # Autres cibles
  others <- setdiff(unique(sc$target), target)
  if (length(others)) {
    best_by <- vapply(others, function(t) max(sc[sc$target == t & sc$model != "baseline", col]), 0)
    o <- order(-best_by)
    out <- c(out, sprintf("Autres cibles, de la mieux à la moins bien prédite : %s.",
                          paste(sprintf("%s (%s)", label_of(others[o]), fmt(best_by[o])), collapse = " ; ")))
  }
  out
}
