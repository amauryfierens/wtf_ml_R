# Version ligne de commande (sans interface), utile pour relancer une analyse complète à l'identique.
# La correspondance des colonnes et les réglages viennent d'une configuration enregistrée par l'interface
# (dossier configurations/). Sans --config, la correspondance automatique est utilisée.
#
#   Rscript run_models.R --data data/simulated_qualtrics.sav --task task2 --intensity standard
#   Rscript run_models.R --data mes_donnees.sav --config configurations/mes_donnees.json --task task1 --all
#
# Options :
#   --task task1|task2          question de recherche (défaut task2)
#   --targets a,b,c             cibles (défaut : la première de la tâche) ; --all = toutes les cibles et tous les jeux
#   --features a,b              jeux de prédicteurs (défaut : le premier)
#   --intensity rapide|standard|approfondi
#   --binary                    prêt·e (>= --threshold, défaut 5) oui / non
#   --shuffle                   contrôle : cibles mélangées
#   --cores n

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f, encoding = "UTF-8")
suppressPackageStartupMessages(library(ggplot2))

args <- commandArgs(trailingOnly = TRUE)
opt <- function(name, default = NULL) {
  i <- match(paste0("--", name), args)
  if (is.na(i)) default else if (i == length(args) || startsWith(args[i + 1], "--")) TRUE else args[i + 1]
}
split <- function(x) trimws(strsplit(x, ",")[[1]])

data_path <- opt("data", "data/simulated_qualtrics.sav")
raw <- read_raw(data_path)
cfg <- if (!is.null(opt("config"))) load_config(opt("config")) else
  list(mapping = auto_map(column_info(raw))$mapping, settings = default_settings())

df0 <- to_canonical(raw, cfg$mapping, cfg$settings)
ck <- check_mapping(df0, cfg$mapping)
if (any(ck$status == "problème")) { cat("Problèmes de codage :\n"); print(ck[ck$status == "problème", c("var", "column", "message")]) }
df <- prepare_data(df0, cfg$settings)

spec <- default_spec()
spec$task <- opt("task", "task2")
tk <- TASKS[[spec$task]]
all <- isTRUE(opt("all", FALSE))
spec$targets <- if (all) tk$targets else split(opt("targets", tk$targets[1]))
spec$feature_sets <- if (all) names(tk$feature_sets) else split(opt("features", names(tk$feature_sets)[1]))
spec <- utils::modifyList(spec, INTENSITY[[opt("intensity", "standard")]])
spec$binary <- isTRUE(opt("binary", FALSE))
spec$threshold <- as.numeric(opt("threshold", 5))
spec$shuffle <- isTRUE(opt("shuffle", FALSE))
spec$cores <- as.integer(opt("cores", spec$cores))

res <- run_analysis(df, spec, progress = function(v, d) cat(sprintf("[%3.0f%%] %s\n", 100 * v, d)))
out <- save_results(res, file.path("resultats", format(Sys.time(), "%Y-%m-%d_%Hh%Mm%S")))
m <- paste0(main_metric(res), "_mean")
print(res$scores[, c("target", "features", "model", m)], digits = 3)
for (fs in unique(res$scores$features)) {
  ggsave(file.path(out, sprintf("vue_ensemble_%s.png", fs)), plot_heatmap(res, fs),
         width = 10, height = 1.5 + 0.4 * length(unique(res$scores$target)), dpi = 150, bg = "white")
  for (t in unique(res$scores$target)) {
    p <- plot_importance(res, t, fs)
    if (!is.null(p)) ggsave(file.path(out, sprintf("importance_%s_%s.png", t, fs)), p, width = 8, height = 6.5, dpi = 150, bg = "white")
  }
}
if (length(res$errors)) cat("Erreurs de modèle :", res$errors, sep = "\n")
cat("Résultats enregistrés dans", out, "\n")
