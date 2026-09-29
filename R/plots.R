# Figures (ggplot2)

INK <- "#1f1f1e"; MUTED <- "#6b6a66"; BLUE <- "#2a78d6"; RED <- "#d8453b"; GRID <- "#e6e5e1"
MODEL_LABELS <- function() vapply(MODELS, `[[`, "", "label")

theme_wtf <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.title.position = "plot", text = ggplot2::element_text(colour = INK),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_line(colour = GRID, linewidth = 0.4))
}

METRIC_NAME <- c(r2 = "R²", mae = "MAE", rmse = "RMSE", auc = "AUC", bal_acc = "Exactitude équilibrée", brier = "Brier")

main_metric <- function(res) if (res$spec$binary) "auc" else "r2"

# Heatmap : performance par cible x modèle, pour un jeu de prédicteurs
plot_heatmap <- function(res, fs) {
  metric <- main_metric(res); col <- paste0(metric, "_mean")
  d <- res$scores[res$scores$features == fs, ]
  if (!nrow(d)) return(NULL)
  d$cible <- factor(label_of(d$target), levels = rev(unique(label_of(d$target))))
  d$modele <- factor(MODEL_LABELS()[d$model], levels = MODEL_LABELS()[unique(d$model)])
  lo <- if (metric == "auc") 0.5 else 0
  top <- max(if (metric == "auc") 0.9 else 0.5, d[[col]], na.rm = TRUE)
  ggplot2::ggplot(d, ggplot2::aes(modele, cible, fill = pmax(.data[[col]], lo))) +
    ggplot2::geom_tile(colour = "white", linewidth = 1) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", .data[[col]]),
                                    colour = ifelse(.data[[col]] > lo + 0.6 * (top - lo), "white", INK)), size = 3.8) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_fill_gradientn(colours = c("#f4f8fd", "#86b6ef", "#256abf", "#0d366b"), limits = c(lo, top),
                                  name = METRIC_NAME[[metric]]) +
    ggplot2::scale_x_discrete(labels = function(x) gsub(" \\(", "\n(", x)) +
    ggplot2::labs(title = sprintf("%s en validation croisée imbriquée", METRIC_NAME[[metric]]),
                  subtitle = FEATURE_SET_LABELS[[fs]], x = NULL, y = NULL) +
    theme_wtf() + ggplot2::theme(panel.grid = ggplot2::element_blank())
}

# Comparaison des modèles pour une cible : moyenne +- écart-type sur les plis externes
plot_models <- function(res, target) {
  metric <- main_metric(res)
  d <- res$folds[res$folds$target == target, ]
  if (!nrow(d)) return(NULL)
  d$modele <- factor(MODEL_LABELS()[d$model], levels = rev(MODEL_LABELS()[unique(d$model)]))
  d$jeu <- FEATURE_SET_LABELS[d$features]
  ggplot2::ggplot(d, ggplot2::aes(.data[[metric]], modele)) +
    ggplot2::geom_vline(xintercept = if (metric == "auc") 0.5 else 0, colour = MUTED, linetype = 2) +
    ggplot2::geom_point(colour = BLUE, alpha = 0.35, size = 2.2, position = ggplot2::position_jitter(height = 0.12, seed = 1)) +
    ggplot2::stat_summary(fun = mean, geom = "point", size = 4, colour = INK, shape = 18) +
    ggplot2::facet_wrap(~jeu, ncol = 1) +
    ggplot2::labs(title = sprintf("%s : %s par pli externe", label_of(target), METRIC_NAME[[metric]]),
                  subtitle = "points = plis de test externes ; losange = moyenne ; tirets = niveau du hasard",
                  x = METRIC_NAME[[metric]], y = NULL) +
    theme_wtf()
}

# Importance par permutation, colorée par le sens de l'effet (coefficient ElasticNet)
plot_importance <- function(res, target, fs, top = 15) {
  if (is.null(res$importance)) return(NULL)
  d <- res$importance[res$importance$target == target & res$importance$features == fs, ]
  if (!nrow(d)) return(NULL)
  d <- utils::head(d[order(-d$importance), ], top)
  sgn <- rep(0, nrow(d))
  if (!is.null(res$coefs)) {
    cf <- res$coefs[res$coefs$target == target & res$coefs$features == fs, ]
    sgn <- vapply(d$variable, function(v) { k <- cf$coef_std[cf$variable == v]; if (length(k) == 1) sign(k) else 0 }, 0)
  }
  d$sens <- factor(c("-1" = "négatif", "0" = "catégorielle / nul", "1" = "positif")[as.character(sgn)],
                   levels = c("positif", "négatif", "catégorielle / nul"))
  d$var_label <- factor(label_of(d$variable), levels = rev(unique(label_of(d$variable))))
  ggplot2::ggplot(d, ggplot2::aes(importance, var_label, fill = sens)) +
    ggplot2::geom_col(width = 0.62) +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = importance - sd, xmax = importance + sd), orientation = "y",
                           width = 0, colour = MUTED) +
    ggplot2::geom_vline(xintercept = 0, colour = MUTED, linewidth = 0.4) +
    ggplot2::scale_fill_manual(values = c("positif" = BLUE, "négatif" = RED, "catégorielle / nul" = "#a9a8a3"),
                               name = "Sens de l'effet", drop = FALSE) +
    ggplot2::labs(title = sprintf("%s : variables les plus utiles", label_of(target)),
                  subtitle = sprintf("Baisse de performance quand on brouille la variable (%s, plis de test)\n%s",
                                     MODEL_LABELS()[[res$spec$importance_model]], FEATURE_SET_LABELS[[fs]]),
                  x = sprintf("Perte de %s", METRIC_NAME[[main_metric(res)]]), y = NULL) +
    theme_wtf() + ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(), legend.position = "bottom")
}
