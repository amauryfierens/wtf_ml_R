# Lance l'interface graphique dans le navigateur.
#
#   Terminal :  Rscript lancer_interface.R
#   RStudio  :  ouvrir ce fichier et cliquer sur « Source »
#
# Les packages manquants sont installés automatiquement au premier lancement.

dossier <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) {
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if (length(f)) dirname(normalizePath(f)) else getwd()
})
setwd(dossier)

packages <- c("shiny", "bslib", "DT", "ggplot2", "glmnet", "ranger", "xgboost", "haven", "jsonlite")
manquants <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(manquants)) {
  message("Installation des packages manquants : ", paste(manquants, collapse = ", "))
  install.packages(manquants, repos = "https://cloud.r-project.org")
}

message("Ouverture de l'interface dans le navigateur… (fermer cette fenêtre ou Ctrl+C pour arrêter)")
shiny::runApp(dossier, launch.browser = TRUE)
