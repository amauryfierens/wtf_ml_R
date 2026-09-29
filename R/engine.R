# Moteur de modélisation : validation croisée imbriquée (nested CV).
#
#   Boucle EXTERNE (k plis x répétitions) : estime la performance hors échantillon.
#     Boucle INTERNE (k plis dans l'échantillon d'entraînement externe uniquement) :
#       choisit les hyperparamètres de chaque modèle.
#     Le modèle réglé est réentraîné sur tout l'entraînement externe, puis évalué UNE fois
#     sur le pli de test externe, qu'il n'a jamais vu (ni pour l'apprentissage, ni pour le réglage,
#     ni pour le prétraitement).
#
# Le prétraitement (imputation, standardisation, indicatrices) est appris séparément dans chaque
# pli, interne comme externe : aucune information du pli de validation ne fuit vers l'entraînement.

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---------------------------------------------------------------------------
# Prétraitement
# ---------------------------------------------------------------------------
prep_fit <- function(d, cols) {
  cat_cols <- intersect(cols, CATEGORICAL)
  num_cols <- setdiff(cols, cat_cols)
  med <- vapply(d[num_cols], function(x) { m <- stats::median(x, na.rm = TRUE); if (is.na(m)) 0 else m }, 0)
  imputed <- Map(function(x, m) replace(x, is.na(x), m), d[num_cols], med)
  sds <- vapply(imputed, stats::sd, 0)
  list(num = num_cols, cat = cat_cols, med = med,
       mu = vapply(imputed, mean, 0), sd = ifelse(is.na(sds) | sds == 0, 1, sds),
       miss = num_cols[vapply(d[num_cols], anyNA, TRUE)],
       levels = lapply(d[cat_cols], function(x) sort(unique(as.character(stats::na.omit(x))))),
       mode = lapply(d[cat_cols], function(x) { t <- table(x); if (length(t)) names(which.max(t)) else NA }))
}

prep_apply <- function(p, d) {
  n <- nrow(d)
  X <- matrix(vapply(p$num, function(v) (replace(d[[v]], is.na(d[[v]]), p$med[[v]]) - p$mu[[v]]) / p$sd[[v]],
                     numeric(n)), nrow = n, dimnames = list(NULL, p$num))
  if (length(p$miss))
    X <- cbind(X, matrix(vapply(p$miss, function(v) as.numeric(is.na(d[[v]])), numeric(n)), nrow = n,
                         dimnames = list(NULL, paste0("manquant_", p$miss))))
  for (v in p$cat) {
    x <- as.character(d[[v]])
    x[is.na(x)] <- p$mode[[v]]
    if (length(p$levels[[v]]))
      X <- cbind(X, matrix(vapply(p$levels[[v]], function(l) as.numeric(x == l), numeric(n)), nrow = n,
                           dimnames = list(NULL, paste0(v, "_", p$levels[[v]]))))
  }
  X
}

# ---------------------------------------------------------------------------
# Métriques et fonctions de perte
# ---------------------------------------------------------------------------
r2 <- function(y, p) 1 - sum((y - p)^2) / sum((y - mean(y))^2)
auc <- function(y, p) {
  r <- rank(p); n1 <- sum(y == 1); n0 <- sum(y == 0)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
logloss <- function(y, p) { p <- pmin(pmax(p, 1e-6), 1 - 1e-6); -mean(y * log(p) + (1 - y) * log(1 - p)) }
loss <- function(y, p, binary) if (binary) logloss(y, p) else mean((y - p)^2)

metrics <- function(y, p, binary) {
  if (binary) c(auc = auc(y, p), bal_acc = mean(c(mean(p[y == 1] >= 0.5), mean(p[y == 0] < 0.5))),
                brier = mean((y - p)^2))
  else c(r2 = r2(y, p), mae = mean(abs(y - p)), rmse = sqrt(mean((y - p)^2)))
}
METRIC_INFO <- list(
  r2 = "R² hors échantillon (part de variance expliquée ; 0 = pas mieux que la moyenne)",
  mae = "Erreur absolue moyenne (en points de l'échelle 1-7)",
  rmse = "Racine de l'erreur quadratique moyenne",
  auc = "AUC (0,5 = hasard, 1 = parfait)",
  bal_acc = "Exactitude équilibrée (moyenne sensibilité / spécificité)",
  brier = "Score de Brier (erreur des probabilités, plus bas = mieux)"
)

make_folds <- function(y, k, repeats, binary) {
  unlist(lapply(seq_len(repeats), function(r) {
    f <- integer(length(y))
    if (binary) for (cl in unique(y)) f[y == cl] <- sample(rep(seq_len(k), length.out = sum(y == cl)))
    else f <- sample(rep(seq_len(k), length.out = length(y)))
    lapply(seq_len(k), function(i) which(f == i))
  }), recursive = FALSE)
}

# ---------------------------------------------------------------------------
# Modèles : chacun a une fonction de réglage (sur les plis internes), d'ajustement et de prédiction.
# `inner` = liste de plis internes déjà prétraités : list(Xtr, ytr, Xva, yva).
# ---------------------------------------------------------------------------
params_to_string <- function(p) {
  p <- p[!names(p) %in% c("lambda_seq")]
  paste(sprintf("%s=%s", names(p), vapply(p, function(v) format(signif(v, 3)), "")), collapse = ", ")
}

sample_candidates <- function(space, n_iter, default) {
  # Recherche aléatoire (Bergstra & Bengio, 2012) : le premier candidat est toujours le réglage par défaut
  c(list(default), lapply(seq_len(max(0, n_iter - 1)), function(i) lapply(space, function(f) f())))
}

MODELS <- list(
  baseline = list(
    label = "Référence (moyenne)",
    tune = function(inner, Xo, yo, binary, n_iter) list(),
    fit = function(X, y, binary, p) mean(y),
    predict = function(obj, X, binary) rep(obj, nrow(X))
  ),

  ols = list(
    label = "Régression linéaire (OLS)",
    regression_only = TRUE,
    tune = function(inner, Xo, yo, binary, n_iter) list(),
    fit = function(X, y, binary, p) { b <- qr.coef(qr(cbind(1, X)), y); replace(b, is.na(b), 0) },
    predict = function(obj, X, binary) drop(cbind(1, X) %*% obj)
  ),

  elasticnet = list(
    label = "Régression régularisée (ElasticNet / logistique)",
    # alpha : 0 = ridge, 1 = lasso ; lambda : force de la régularisation (chemin complet évalué)
    tune = function(inner, Xo, yo, binary, n_iter) {
      fam <- if (binary) "binomial" else "gaussian"
      best <- list(loss = Inf)
      for (a in c(0, 0.25, 0.5, 0.75, 1)) {
        lam <- glmnet::glmnet(Xo, yo, alpha = a, family = fam, nlambda = 60)$lambda
        L <- sapply(inner, function(f) {
          m <- glmnet::glmnet(f$Xtr, f$ytr, alpha = a, family = fam, lambda = lam)
          P <- stats::predict(m, f$Xva, s = lam, type = "response")
          apply(P, 2, function(p) loss(f$yva, p, binary))
        })
        l <- rowMeans(matrix(L, nrow = length(lam)))
        if (min(l) < best$loss) best <- list(loss = min(l), alpha = a, lambda = lam[which.min(l)], lambda_seq = lam)
      }
      best[c("alpha", "lambda", "lambda_seq")]
    },
    fit = function(X, y, binary, p) {
      m <- glmnet::glmnet(X, y, alpha = p$alpha, family = if (binary) "binomial" else "gaussian",
                          lambda = p$lambda_seq)
      list(model = m, lambda = p$lambda)
    },
    predict = function(obj, X, binary) drop(stats::predict(obj$model, X, s = obj$lambda, type = "response"))
  ),

  random_forest = list(
    label = "Forêt aléatoire (ranger)",
    tune = function(inner, Xo, yo, binary, n_iter) {
      p_feat <- ncol(Xo)
      space <- list(mtry_frac = function() sample(c(0.1, 0.2, 0.33, 0.5, 0.7), 1),
                    min.node.size = function() sample(c(1, 3, 5, 10, 20, 40), 1),
                    sample.fraction = function() sample(c(0.5, 0.632, 0.8), 1))
      default <- list(mtry_frac = if (binary) sqrt(p_feat) / p_feat else 0.33, min.node.size = 5, sample.fraction = 0.632)
      cands <- sample_candidates(space, n_iter, default)
      L <- vapply(cands, function(p) mean(vapply(inner, function(f) {
        m <- MODELS$random_forest$fit(f$Xtr, f$ytr, binary, p, num.trees = 250)
        loss(f$yva, MODELS$random_forest$predict(m, f$Xva, binary), binary)
      }, 0)), 0)
      cands[[which.min(L)]]
    },
    fit = function(X, y, binary, p, num.trees = 500) {
      ranger::ranger(x = X, y = if (binary) factor(y, levels = 0:1) else y, num.trees = num.trees,
                     mtry = max(1, round(p$mtry_frac * ncol(X))), min.node.size = p$min.node.size,
                     sample.fraction = p$sample.fraction, replace = FALSE,
                     probability = binary, num.threads = 1)
    },
    predict = function(obj, X, binary) {
      p <- stats::predict(obj, X)$predictions
      if (binary) p[, "1"] else p
    }
  ),

  grad_boosting = list(
    label = "Gradient boosting (xgboost)",
    # Nombre d'arbres choisi par arrêt précoce sur les plis internes, le reste par recherche aléatoire
    tune = function(inner, Xo, yo, binary, n_iter) {
      space <- list(max_depth = function() sample(c(1, 2, 3, 4, 6), 1),
                    learning_rate = function() exp(stats::runif(1, log(0.01), log(0.3))),
                    subsample = function() stats::runif(1, 0.5, 1),
                    colsample_bytree = function() stats::runif(1, 0.4, 1),
                    min_child_weight = function() sample(c(1, 3, 5, 10), 1),
                    lambda = function() exp(stats::runif(1, log(0.1), log(10))))
      default <- list(max_depth = 3, learning_rate = 0.05, subsample = 0.8, colsample_bytree = 0.8,
                      min_child_weight = 1, lambda = 1)
      cands <- sample_candidates(space, n_iter, default)
      res <- lapply(cands, function(p) {
        curves <- lapply(inner, function(f) {
          m <- xgboost::xgb.train(
            params = c(p, objective = if (binary) "binary:logistic" else "reg:squarederror",
                       eval_metric = if (binary) "logloss" else "rmse", nthread = 1),
            data = xgboost::xgb.DMatrix(f$Xtr, label = f$ytr), nrounds = 1500,
            evals = list(val = xgboost::xgb.DMatrix(f$Xva, label = f$yva)),
            early_stopping_rounds = 50, verbose = 0)
          attributes(m)$evaluation_log[[2]]
        })
        len <- max(lengths(curves))
        mean_curve <- rowMeans(sapply(curves, function(cv) c(cv, rep(cv[length(cv)], len - length(cv)))))
        list(loss = min(mean_curve), nrounds = which.min(mean_curve))
      })
      k <- which.min(vapply(res, `[[`, 0, "loss"))
      c(cands[[k]], nrounds = res[[k]]$nrounds)
    },
    fit = function(X, y, binary, p) {
      xgboost::xgb.train(
        params = c(p[names(p) != "nrounds"], objective = if (binary) "binary:logistic" else "reg:squarederror",
                   nthread = 1),
        data = xgboost::xgb.DMatrix(X, label = y), nrounds = p$nrounds, verbose = 0)
    },
    predict = function(obj, X, binary) stats::predict(obj, xgboost::xgb.DMatrix(X))
  )
)

models_for <- function(binary) names(MODELS)[!(binary & vapply(MODELS, function(m) isTRUE(m$regression_only), TRUE))]

# ---------------------------------------------------------------------------
# Un pli externe : réglage interne, ajustement, évaluation, importance
# ---------------------------------------------------------------------------
prep_inner <- function(d, y, cols, k, binary) {
  lapply(make_folds(y, k, 1, binary), function(va) {
    p <- prep_fit(d[-va, , drop = FALSE], cols)
    list(Xtr = prep_apply(p, d[-va, , drop = FALSE]), ytr = y[-va],
         Xva = prep_apply(p, d[va, , drop = FALSE]), yva = y[va])
  })
}

permutation_importance <- function(model, obj, p, d_te, y_te, cols, binary, n_rep) {
  score <- function(dd) {
    pr <- MODELS[[model]]$predict(obj, prep_apply(p, dd), binary)
    if (binary) auc(y_te, pr) else r2(y_te, pr)
  }
  ref <- score(d_te)
  vapply(cols, function(v) mean(replicate(n_rep, {
    dp <- d_te; dp[[v]] <- sample(dp[[v]]); ref - score(dp)
  })), 0)
}

run_outer_fold <- function(te, d, y, cols, spec, compute_importance, seed) {
  set.seed(seed)
  d_tr <- d[-te, , drop = FALSE]; y_tr <- y[-te]
  inner <- prep_inner(d_tr, y_tr, cols, spec$inner_k, spec$binary)
  p <- prep_fit(d_tr, cols)
  Xo <- prep_apply(p, d_tr); Xte <- prep_apply(p, d[te, , drop = FALSE])
  out <- list(metrics = list(), params = list(), importance = NULL, errors = list())
  for (m in spec$models) {
    res <- tryCatch({
      prm <- MODELS[[m]]$tune(inner, Xo, y_tr, spec$binary, spec$n_iter)
      obj <- MODELS[[m]]$fit(Xo, y_tr, spec$binary, prm)
      pred <- MODELS[[m]]$predict(obj, Xte, spec$binary)
      imp <- if (compute_importance && m == spec$importance_model)
        permutation_importance(m, obj, p, d[te, , drop = FALSE], y[te], cols, spec$binary, spec$perm_repeats)
      list(metrics = metrics(y[te], pred, spec$binary), params = params_to_string(prm), importance = imp)
    }, error = function(e) list(error = conditionMessage(e)))
    if (!is.null(res$error)) { out$errors[[m]] <- res$error; next }
    out$metrics[[m]] <- res$metrics
    out$params[[m]] <- res$params
    if (!is.null(res$importance)) out$importance <- res$importance
  }
  out
}

# Fonction envoyée aux processus de calcul : ne capture que ce dont elle a besoin
make_fold_fun <- function(folds, d, y, cols, spec) {
  function(i) run_outer_fold(folds[[i]], d, y, cols, spec, compute_importance = i <= spec$outer_k,
                             seed = spec$seed + i)
}

# ---------------------------------------------------------------------------
# Une cible x un jeu de prédicteurs
# ---------------------------------------------------------------------------
evaluate_one <- function(df, target, fs_name, cols, spec) {
  cols <- setdiff(intersect(cols, names(df)), target)
  cols <- cols[vapply(df[cols], function(x) sum(!is.na(x)) > 0, TRUE)]
  d <- df[!is.na(df[[target]]), c(cols, target), drop = FALSE]
  y <- if (spec$binary) as.numeric(d[[target]] >= spec$threshold) else d[[target]]
  if (spec$binary && (length(unique(y)) < 2 || min(table(y)) < 2 * spec$outer_k)) {
    return(list(skipped = sprintf("%s : trop peu de répondants dans une des deux classes (seuil %s)", target, spec$threshold)))
  }
  set.seed(spec$seed)
  if (spec$shuffle) y <- sample(y)   # contrôle : lien réel détruit, performance attendue ~ hasard
  folds <- make_folds(y, spec$outer_k, spec$repeats, spec$binary)

  papply <- spec$.backend %||% lapply
  spec$.backend <- NULL
  fold_res <- papply(seq_along(folds), make_fold_fun(folds, d, y, cols, spec))
  bad <- vapply(fold_res, function(r) inherits(r, "try-error"), TRUE)
  if (any(bad)) stop(as.character(fold_res[[which(bad)[1]]]))

  fold_scores <- do.call(rbind, lapply(seq_along(fold_res), function(i) {
    r <- fold_res[[i]]
    if (!length(r$metrics)) return(NULL)
    do.call(rbind, lapply(names(r$metrics), function(m)
      data.frame(fold = i, repeat_id = (i - 1) %/% spec$outer_k + 1, model = m, t(r$metrics[[m]]),
                 params = r$params[[m]])))
  }))
  mets <- setdiff(names(fold_scores), c("fold", "repeat_id", "model", "params"))
  scores <- do.call(rbind, lapply(unique(fold_scores$model), function(m) {
    s <- fold_scores[fold_scores$model == m, mets, drop = FALSE]
    data.frame(model = m, n = length(y), n_folds = nrow(s),
               t(setNames(c(colMeans(s), apply(s, 2, stats::sd)), c(paste0(mets, "_mean"), paste0(mets, "_sd")))))
  }))
  imps <- Filter(Negate(is.null), lapply(fold_res, `[[`, "importance"))
  importance <- if (length(imps)) {
    I <- do.call(cbind, imps)
    data.frame(variable = rownames(I), importance = rowMeans(I), sd = apply(I, 1, stats::sd), row.names = NULL)
  }
  errors <- unique(unlist(lapply(fold_res, function(r) sprintf("%s : %s", names(r$errors), unlist(r$errors)))))

  # Modèle ElasticNet final, réglé par validation croisée sur toutes les données : sens des effets
  coefs <- NULL
  if ("elasticnet" %in% spec$models) {
    set.seed(spec$seed)
    p <- prep_fit(d, cols); X <- prep_apply(p, d)
    prm <- MODELS$elasticnet$tune(prep_inner(d, y, cols, spec$inner_k, spec$binary), X, y, spec$binary, 0)
    b <- stats::coef(MODELS$elasticnet$fit(X, y, spec$binary, prm)$model, s = prm$lambda)
    coefs <- data.frame(variable = rownames(b)[-1], coef_std = as.numeric(b)[-1],
                        alpha = prm$alpha, lambda = prm$lambda)
  }

  tag <- function(x) if (!is.null(x) && nrow(x)) cbind(target = target, features = fs_name, x)
  list(scores = tag(scores), folds = tag(fold_scores), importance = tag(importance), coefs = tag(coefs),
       errors = errors)
}

# ---------------------------------------------------------------------------
# Lancement complet
# ---------------------------------------------------------------------------
default_spec <- function() list(
  task = "task2", targets = "wtf_score", feature_sets = "scores",
  models = c("baseline", "ols", "elasticnet", "random_forest", "grad_boosting"),
  binary = FALSE, threshold = 5,
  outer_k = 5, repeats = 3, inner_k = 5, n_iter = 10,
  importance_model = "random_forest", perm_repeats = 5,
  shuffle = FALSE, seed = 42,
  cores = max(1, parallel::detectCores() - 1)
)

# Calcul parallèle sur les plis externes : fork sous macOS / Linux, grappe de processus sous Windows
make_backend <- function(cores) {
  if (cores <= 1) return(list(apply = lapply, stop = function() NULL))
  if (.Platform$OS.type != "windows")
    return(list(apply = function(X, FUN) parallel::mclapply(X, FUN, mc.cores = cores, mc.preschedule = FALSE),
                stop = function() NULL))
  cl <- parallel::makeCluster(cores)
  parallel::clusterCall(cl, setwd, getwd())
  parallel::clusterEvalQ(cl, for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f, encoding = "UTF-8"))
  list(apply = function(X, FUN) parallel::parLapply(cl, X, FUN), stop = function() parallel::stopCluster(cl))
}

INTENSITY <- list(
  rapide = list(repeats = 1, inner_k = 3, n_iter = 4),
  standard = list(repeats = 3, inner_k = 5, n_iter = 10),
  approfondi = list(repeats = 5, inner_k = 5, n_iter = 25)
)

run_analysis <- function(df, spec, progress = function(value, detail) NULL) {
  spec$models <- intersect(spec$models, models_for(spec$binary))
  if (!spec$importance_model %in% spec$models) spec$importance_model <- setdiff(spec$models, "baseline")[1]
  fsets <- TASKS[[spec$task]]$feature_sets[spec$feature_sets]
  jobs <- expand.grid(target = intersect(spec$targets, names(df)), fs = names(fsets), stringsAsFactors = FALSE)
  if (!nrow(jobs)) stop("Aucune cible sélectionnée n'est disponible dans les données.")
  backend <- make_backend(spec$cores)
  on.exit(backend$stop(), add = TRUE)
  spec$.backend <- backend$apply
  res <- vector("list", nrow(jobs)); skipped <- character(0)
  for (j in seq_len(nrow(jobs))) {
    progress((j - 1) / nrow(jobs), sprintf("%s  |  %s  (%d/%d)", label_of(jobs$target[j]),
                                           FEATURE_SET_LABELS[[jobs$fs[j]]], j, nrow(jobs)))
    r <- evaluate_one(df, jobs$target[j], jobs$fs[j], fsets[[jobs$fs[j]]], spec)
    if (!is.null(r$skipped)) skipped <- c(skipped, r$skipped) else res[[j]] <- r
  }
  progress(1, "Terminé")
  res <- Filter(Negate(is.null), res)
  bind <- function(k) { x <- do.call(rbind, lapply(res, `[[`, k)); if (!is.null(x)) rownames(x) <- NULL; x }
  spec$.backend <- NULL
  list(spec = spec, scores = bind("scores"), folds = bind("folds"), importance = bind("importance"),
       coefs = bind("coefs"), skipped = skipped, errors = unique(unlist(lapply(res, `[[`, "errors"))),
       n = nrow(df), time = Sys.time())
}

# Écrit les résultats sur le disque (CSV + réglages utilisés) et renvoie le dossier
save_results <- function(res, out_dir) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  rnd <- function(x) { if (is.null(x)) return(x); x[] <- lapply(x, function(c) if (is.numeric(c)) round(c, 4) else c); x }
  for (k in c("scores", "folds", "importance", "coefs"))
    if (!is.null(res[[k]])) utils::write.csv(rnd(res[[k]]), file.path(out_dir, paste0(k, ".csv")), row.names = FALSE)
  jsonlite::write_json(res$spec, file.path(out_dir, "options.json"), auto_unbox = TRUE, pretty = TRUE)
  out_dir
}
