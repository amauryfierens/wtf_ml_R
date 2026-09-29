# Interface graphique (Shiny) : parcours guidé en 4 étapes
#   Accueil -> 1. Importer -> 2. Vérifier -> 3. Analyser -> 4. Résultats
# avec une démo guidée sur données simulées.
#
# Lancement :  Rscript lancer_interface.R      (ou, dans RStudio : ouvrir lancer_interface.R puis « Source »)

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(ggplot2)
})
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f, encoding = "UTF-8")
options(shiny.maxRequestSize = 500 * 1024^2)
`%||%` <- function(a, b) if (is.null(a)) b else a
options(DT.options = list(language = list(
  search = "Rechercher :", lengthMenu = "Afficher _MENU_ lignes", info = "Lignes _START_ à _END_ sur _TOTAL_",
  infoEmpty = "Aucune ligne", infoFiltered = "(filtré sur _MAX_)", zeroRecords = "Aucun résultat",
  emptyTable = "Aucune donnée", paginate = list(previous = "Précédent", `next` = "Suivant"))))
CONFIG_DIR <- "configurations"
RESULTS_DIR <- "resultats"
DEMO_FILE <- "data/export_qualtrics_probable.sav"
dir.create(CONFIG_DIR, showWarnings = FALSE)

NONE <- "(aucune colonne)"
BLOCKS <- unique(CODEBOOK$block)
ICON <- c(ok = "✅", attention = "⚠️", "problème" = "❌", absente = "➖")
STEPS <- c(home = "Accueil", import = "Importer", check = "Vérifier", setup = "Analyser", results = "Résultats")

help_text <- function(...) tags$p(class = "text-muted small mb-2", ...)
input_id <- function(v) paste0("map__", v)
info <- function(text) tooltip(tags$span(icon("circle-question"), class = "text-primary ms-1", style = "cursor:help"), text)
num <- function(n) tags$span(class = "num", n)

nav_row <- function(step, prev = TRUE, nxt = TRUE, next_label = "Suivant", prev_label = "Précédent") {
  div(class = "nav-row",
      if (prev) actionButton(paste0("prev_", step), prev_label, icon = icon("arrow-left"), class = "btn-outline-secondary") else div(),
      if (nxt) actionButton(paste0("next_", step), tagList(next_label, icon("arrow-right")), class = "btn-primary") else div())
}

CSS <- "
body { background: #f5f7fa; }
.topbar { background: #0d366b; color: #fff; display: flex; justify-content: space-between; align-items: center;
          padding: 10px 24px; flex-wrap: wrap; gap: 8px; }
.topbar .brand { font-size: 1.15rem; font-weight: 600; letter-spacing: .2px; }
.stepper { display: flex; justify-content: center; align-items: center; padding: 12px 8px; background: #fff;
           border-bottom: 1px solid #e3e7ee; flex-wrap: wrap; gap: 4px; }
.step { display: flex; align-items: center; color: #6b7280; text-decoration: none; padding: 6px 10px; border-radius: 8px; }
.step:hover { background: #f1f5fb; color: #0d366b; }
.step .dot { width: 30px; height: 30px; border-radius: 50%; border: 2px solid #cbd5e1; display: inline-flex;
             align-items: center; justify-content: center; margin-right: 8px; font-weight: 600; background: #fff; }
.step.current { color: #0d366b; font-weight: 600; }
.step.current .dot { background: #256abf; border-color: #256abf; color: #fff; }
.step.done .dot { background: #e8f1fc; border-color: #256abf; color: #256abf; }
.step.locked { opacity: .4; pointer-events: none; }
.step-sep { width: 36px; height: 2px; background: #cbd5e1; }
.main { padding-top: 18px; }
.demo-banner { background: #fff8e6; border: 1px solid #f5c451; border-left: 6px solid #f0a500; border-radius: 10px;
               padding: 12px 16px; margin-bottom: 16px; display: flex; gap: 14px; align-items: flex-start; }
.demo-banner .demo-icon { font-size: 1.6rem; line-height: 1; }
.hero { padding: 28px 0 18px; }
.hero h1 { font-weight: 700; color: #0d366b; }
.step-card .card-body { text-align: left; }
.step-card .big { font-size: 1.9rem; color: #256abf; }
.num { display: inline-flex; width: 26px; height: 26px; border-radius: 50%; background: #256abf; color: #fff;
       align-items: center; justify-content: center; margin-right: 8px; font-size: .9rem; font-weight: 600; }
.nav-row { display: flex; justify-content: space-between; margin: 22px 0 40px; }
.takeaway { border-left: 6px solid #256abf; }
.takeaway li { margin-bottom: 6px; }
.map-row { border-bottom: 1px solid #eee; padding: 4px 0; align-items: center; }
.map-row .form-group { margin-bottom: 0; }
.checklist .item { display: flex; gap: 10px; padding: 8px 0; border-bottom: 1px solid #eef1f5; }
.checklist .item:last-child { border-bottom: none; }
.checklist .ic { width: 22px; flex: none; }
.choice-rich .radio label, .choice-rich .checkbox label { padding: 6px 0; }
.choice-desc { display: block; color: #6b7280; font-size: .85rem; }
.sticky { position: sticky; top: 12px; }
"

# =============================================================================
# UI
# =============================================================================
home_ui <- tagList(
  div(class = "hero",
    h1("Prédire la volonté de se battre"),
    p(class = "lead", style = "max-width: 820px",
      "Cet outil compare plusieurs modèles de machine learning pour mesurer dans quelle mesure les réponses au ",
      "questionnaire « Guerre & Paix » permettent de prédire le ", tags$b("Will to Fight"), " et les ",
      tags$b("intentions de comportement"), ", et quelles questions comptent le plus."),
    div(class = "d-flex gap-2 flex-wrap mt-3",
      actionButton("home_demo", "Découvrir avec la démo guidée (2 min)", icon = icon("play"), class = "btn-primary btn-lg"),
      actionButton("home_start", "Commencer avec mes données", icon = icon("file-import"), class = "btn-outline-primary btn-lg"))),
  layout_column_wrap(width = "220px", fill = FALSE,
    card(class = "step-card", card_body(div(class = "big", icon("file-import")), h5("1. Importer"),
      p("Chargez l'export SPSS (.sav) de Qualtrics. Les questions sont reconnues automatiquement."))),
    card(class = "step-card", card_body(div(class = "big", icon("list-check")), h5("2. Vérifier"),
      p("L'outil contrôle les codages et ne vous montre que ce qui demande votre attention."))),
    card(class = "step-card", card_body(div(class = "big", icon("sliders")), h5("3. Analyser"),
      p("Choisissez quoi prédire et avec quelles informations, puis lancez le calcul."))),
    card(class = "step-card", card_body(div(class = "big", icon("chart-simple")), h5("4. Lire les résultats"),
      p("Un résumé en français, puis les graphiques et tableaux détaillés.")))),
  card(class = "mt-3",
    card_header("Pour aller plus loin"),
    accordion(open = FALSE,
      accordion_panel("Comment l'outil évite-t-il de se tromper ?",
        p("Un modèle peut « apprendre par cœur » ses données et paraître excellent sans l'être. Pour l'éviter, ",
          "la performance est toujours mesurée sur des répondants que le modèle n'a ", tags$b("jamais vus"), " :"),
        tags$ol(
          tags$li("Les répondants sont répartis en 5 groupes (« plis »). Chaque groupe sert une fois de test ; le modèle apprend sur les 4 autres."),
          tags$li("Les réglages de chaque modèle (ses « hyperparamètres ») sont choisis par une seconde validation croisée, à l'intérieur des 4 groupes d'apprentissage uniquement."),
          tags$li("Même le traitement des données manquantes est appris sans regarder le groupe de test."),
          tags$li("Un modèle de « référence », qui prédit toujours la moyenne, sert d'étalon : un bon modèle doit faire nettement mieux."))),
      accordion_panel("Que signifient R² et AUC ?",
        p(tags$b("R²"), " (score continu, de 1 à 7) : part des différences entre répondants que le modèle explique. ",
          "0 = pas mieux que deviner la moyenne ; 0,3 = 30 % expliqué, ce qui est déjà élevé en psychologie sociale."),
        p(tags$b("AUC"), " (mode prêt·e oui / non) : probabilité que le modèle classe plus haut un répondant « prêt » qu'un répondant « pas prêt ». ",
          "0,5 = hasard, 0,7 = correct, 0,8 et plus = bon.")),
      accordion_panel("Quels modèles sont comparés ?",
        tags$ul(
          tags$li(tags$b("Régression linéaire"), " : la régression classique."),
          tags$li(tags$b("Régression régularisée (ElasticNet)"), " : une régression qui réduit automatiquement le poids des variables peu utiles ; robuste avec beaucoup de questions."),
          tags$li(tags$b("Forêt aléatoire"), " et ", tags$b("gradient boosting"), " : des ensembles d'arbres de décision, capables de repérer des effets non linéaires et des interactions."))))
  )
)

import_ui <- tagList(
  h3("1. Importer les données"),
  layout_columns(col_widths = c(5, 7),
    card(card_header("Choisir le fichier"),
      fileInput("file", NULL, accept = c(".sav", ".csv"), width = "100%", buttonLabel = "Parcourir…",
                placeholder = "Fichier .sav (SPSS) ou .csv"),
      help_text("Export SPSS de Qualtrics, brut ou nettoyé. Les questions sont reconnues grâce aux libellés des variables.",
                "Les données restent sur cet ordinateur."),
      actionLink("import_demo", "Pas de fichier sous la main ? Charger les données de démonstration", icon = icon("flask"))),
    uiOutput("import_summary")),
  nav_row("import", next_label = "Suivant : vérifier les données")
)

check_ui <- tagList(
  h3("2. Vérifier les données"),
  p(class = "text-muted", "Les sections marquées ⚠️ ou ❌ demandent votre attention ; les autres peuvent être laissées telles quelles."),
  layout_columns(col_widths = c(8, 4),
    accordion(id = "check_acc", multiple = TRUE, open = FALSE,
      accordion_panel(value = "map", title = uiOutput("t_map", inline = TRUE), icon = icon("link"),
        help_text("Pour chaque question du questionnaire, la colonne de votre fichier qui lui correspond.",
                  "Les questions incertaines ou introuvables sont regroupées en premier. Une question absente de votre fichier est simplement ignorée."),
        uiOutput("mapping_ui")),
      accordion_panel(value = "values", title = uiOutput("t_values", inline = TRUE), icon = icon("ruler"),
        help_text("Vérifie que chaque variable a des valeurs possibles (ex. 1 à 7 pour les échelles d'accord)."),
        uiOutput("values_problems"),
        accordion(open = FALSE, accordion_panel("Voir toutes les variables", DT::DTOutput("check_table")))),
      accordion_panel(value = "coding", title = uiOutput("t_coding", inline = TRUE), icon = icon("arrows-rotate"),
        tags$label(class = "form-label fw-semibold", "Items formulés à l'envers : faut-il les inverser ?",
                   info("Ex. « Il n'y a pas de justification concevable à la guerre » va à l'encontre de l'échelle « attitude favorable à la guerre » : dans les données brutes, il faut l'inverser (1 devient 7) avant de faire la moyenne. S'il a déjà été inversé dans SPSS, il ne faut pas le refaire.")),
        help_text("Cochée = l'outil inverse l'item. La recommandation vient de la corrélation de chaque item avec les autres items de son échelle : négative = pas encore inversé."),
        uiOutput("reversal_ui"),
        radioButtons("reverse_status", "Échelle de statut subjectif",
                     c("1 = en haut (codage du questionnaire) : inverser" = "yes", "Déjà recodée (10 = en haut)" = "no")),
        checkboxInput("fix_lr_1_11", "Gauche-droite exporté en 1-11 par Qualtrics : ramener automatiquement à 0-10", TRUE),
        layout_columns(col_widths = c(6, 6),
          textInput("missing_codes", tagList("Codes de valeurs manquantes", info("Valeurs utilisées dans SPSS pour « pas de réponse », séparées par des virgules.")), "-99, -9, 99, 999"),
          numericInput("survey_year", tagList("Année de passation", info("Sert à transformer l'année d'éruption imaginée en « nombre d'années avant le conflit ».")), 2026, 2000, 2100)),
        layout_columns(col_widths = c(6, 6),
          checkboxInput("drop_attention", "Exclure les répondants qui ratent l'item d'attention", TRUE),
          numericInput("attention_expected", "Bonne réponse à l'item d'attention", 3, 1, 7))),
      accordion_panel(value = "missing", title = uiOutput("t_missing", inline = TRUE), icon = icon("circle-half-stroke"),
        sliderInput("max_missing", "Exclure un répondant au-delà de ce % de questions sans réponse", 10, 100, 50, step = 5, post = " %", width = "100%"),
        help_text("Écarte surtout les abandons en cours de questionnaire."),
        sliderInput("min_scale_items", "Calculer un score d'échelle si au moins ce % de ses items est répondu", 0, 100, 50, step = 10, post = " %", width = "100%"),
        help_text("Les manques restants sont complétés par la médiane (avec un indicateur « était manquant »), à l'intérieur de chaque pli de validation, sans fuite d'information."),
        uiOutput("missing_summary")),
      accordion_panel(value = "conflict", title = "Conflit imaginé : classement des réponses (facultatif)", icon = icon("tags"),
        uiOutput("conflict_source_ui"),
        conditionalPanel("input.conflict_source != 'coded'",
          help_text("La réponse libre « quel conflit » est rangée dans la première catégorie dont un mot-clé apparaît (sans accents, en minuscules)."),
          layout_columns(col_widths = c(5, 7), uiOutput("keywords_ui"), DT::DTOutput("conflict_table"))),
        conditionalPanel("input.conflict_source == 'coded'",
          sliderInput("min_category", "Regrouper dans « Autres » les catégories de moins de", 5, 200, 30, step = 5, post = " répondants"),
          help_text("Les catégories trop rares apportent du bruit : elles sont regroupées."),
          DT::DTOutput("coded_table"))),
      accordion_panel(value = "alpha", title = "Fiabilité des échelles (information)", icon = icon("circle-info"),
        help_text("Alpha de Cronbach : cohérence des items d'une même échelle (> 0,7 = bonne)."),
        tableOutput("alpha_table")),
      accordion_panel(value = "config", title = "Enregistrer / réutiliser ces réglages (facultatif)", icon = icon("floppy-disk"),
        help_text("Les réglages sont enregistrés automatiquement pour ce fichier et rechargés à la prochaine importation.",
                  "Vous pouvez aussi les exporter pour un collègue ou pour la version en ligne de commande."),
        downloadButton("save_cfg", "Exporter la configuration", class = "btn-sm btn-outline-primary"),
        fileInput("load_cfg", NULL, accept = ".json", buttonLabel = "Importer…", placeholder = "configuration .json"))
    ),
    div(class = "sticky", uiOutput("check_summary"))),
  nav_row("check", next_label = "Suivant : choisir l'analyse")
)

intensity_choices <- list(
  names = list(
    tagList(tags$b("Rapide"), tags$span(class = "choice-desc", "Pour essayer : moins de réglages testés, estimation moins stable.")),
    tagList(tags$b("Standard"), tags$span(class = "choice-desc", "Pour explorer : bon compromis précision / temps.")),
    tagList(tags$b("Approfondi"), tags$span(class = "choice-desc", "Pour les résultats à publier : plus long.")),
    tagList(tags$b("Personnalisé"), tags$span(class = "choice-desc", "Choisir soi-même le nombre de plis et de réglages testés."))),
  values = c("rapide", "standard", "approfondi", "custom"))

setup_ui <- tagList(
  h3("3. Choisir l'analyse"),
  layout_columns(col_widths = c(8, 4),
    div(
      card(card_header(num(1), "Que voulez-vous prédire ?"),
        div(class = "choice-rich", radioButtons("task", NULL, width = "100%",
          choiceNames = list(
            tagList(tags$b("Les comportements si le conflit imaginé éclate"),
                    tags$span(class = "choice-desc", "Tâche 1 : protection civile, effort de guerre, se battre, s'engager dans l'armée…")),
            tagList(tags$b("Le Will to Fight et les intentions de comportement"),
                    tags$span(class = "choice-desc", "Tâche 2 : prêt à se battre pour la Belgique / l'UE / contre la Russie, et actions concrètes."))),
          choiceValues = names(TASKS))),
        uiOutput("targets_ui")),
      card(card_header(num(2), "À partir de quelles informations ?"), uiOutput("fsets_ui")),
      card(card_header(num(3), "Précision du calcul"),
        div(class = "choice-rich", radioButtons("intensity", NULL, choiceNames = intensity_choices$names,
                                                 choiceValues = intensity_choices$values, selected = "standard", width = "100%")),
        conditionalPanel("input.intensity == 'custom'",
          layout_columns(col_widths = c(3, 3, 3, 3),
            numericInput("outer_k", "Plis d'évaluation", 5, 3, 10),
            numericInput("repeats", "Répétitions", 3, 1, 20),
            numericInput("inner_k", "Plis de réglage", 5, 2, 10),
            numericInput("n_iter", "Réglages testés par modèle", 10, 1, 100)))),
      accordion(open = FALSE, class = "mb-3",
        accordion_panel("Options avancées (facultatif)", icon = icon("gear"),
          checkboxGroupInput("models", "Modèles comparés", setNames(names(MODELS)[-1], vapply(MODELS, `[[`, "", "label")[-1]),
                             selected = names(MODELS)[-1], inline = TRUE),
          help_text("Le modèle de référence (moyenne) est toujours inclus."),
          radioButtons("mode", tagList("Type de prédiction", info("Continu : prédire le score de 1 à 7 (R²). Oui/non : prédire si la personne est « prête » (score au-dessus d'un seuil), évalué par l'AUC.")),
                       c("Score continu (1 à 7)" = "continuous", "Prêt·e oui / non" = "binary"), inline = TRUE),
          conditionalPanel("input.mode == 'binary'", sliderInput("threshold", "« Prêt·e » à partir de", 2, 7, 5, step = 0.5)),
          selectInput("importance_model", "Modèle utilisé pour mesurer l'importance des variables",
                      setNames(names(MODELS)[-1], vapply(MODELS, `[[`, "", "label")[-1]), "random_forest"),
          checkboxInput("shuffle", tagList("Test de contrôle : mélanger la variable à prédire",
                        info("Détruit tout lien réel : la performance doit tomber au niveau du hasard. Sert à vérifier que l'outil ne « triche » pas.")), FALSE),
          layout_columns(col_widths = c(6, 6),
            numericInput("seed", tagList("Graine aléatoire", info("Même graine = mêmes résultats.")), 42),
            numericInput("cores", "Cœurs de calcul", default_spec()$cores, 1, parallel::detectCores()))))),
    div(class = "sticky",
      card(card_header(icon("clipboard-check"), "Récapitulatif"),
        uiOutput("setup_summary"),
        actionButton("run", "Lancer l'analyse", icon = icon("play"), class = "btn-primary btn-lg w-100 mt-2"),
        uiOutput("estimate")))),
  nav_row("setup", nxt = FALSE)
)

results_ui <- tagList(
  div(class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
      h3("4. Résultats"), uiOutput("results_meta")),
  layout_columns(col_widths = c(6, 6), fill = FALSE,
    uiOutput("target_pick_ui"), uiOutput("fs_pick_ui")),
  card(class = "takeaway", card_header(icon("lightbulb"), "Ce qu'il faut retenir"), uiOutput("takeaway")),
  navset_card_underline(
    nav_panel("Variables importantes",
      help_text("De combien la performance baisse quand on brouille les réponses à une question.",
                "Bleu = plus la variable est élevée, plus la cible est élevée ; rouge = l'inverse."),
      plotOutput("importance_plot", height = "520px")),
    nav_panel("Comparaison des modèles",
      help_text("Performance de chaque modèle pour chaque cible (jeu de prédicteurs choisi ci-dessus)."),
      plotOutput("heatmap", height = "auto")),
    nav_panel("Stabilité",
      help_text("Chaque point est un pli de test. Des points très dispersés = estimation peu précise."),
      plotOutput("models_plot", height = "auto")),
    nav_panel("Tableaux détaillés",
      navset_pill(
        nav_panel("Scores", DT::DTOutput("scores_table")),
        nav_panel("Hyperparamètres choisis",
          help_text("Réglages retenus dans chaque pli. S'ils varient beaucoup, le modèle est instable (fréquent avec un petit échantillon)."),
          DT::DTOutput("params_table")),
        nav_panel("Coefficients (ElasticNet)",
          help_text("Coefficients standardisés du modèle ElasticNet final. 0 = variable écartée par la régularisation."),
          DT::DTOutput("coef_table")))),
    nav_panel("Télécharger",
      help_text("Tous les résultats sont aussi enregistrés automatiquement dans le dossier indiqué en haut à droite."),
      div(class = "d-flex flex-wrap gap-2",
        downloadButton("dl_scores", "Scores (CSV)"), downloadButton("dl_folds", "Détail par pli (CSV)"),
        downloadButton("dl_imp", "Importances (CSV)"), downloadButton("dl_coefs", "Coefficients (CSV)"),
        downloadButton("dl_heatmap", "Figure : comparaison (PNG)"), downloadButton("dl_importance", "Figure : importance (PNG)")))
  ),
  nav_row("results", prev_label = "Modifier l'analyse", nxt = FALSE)
)

ui <- page_fluid(
  theme = bs_theme(version = 5, primary = "#256abf", "font-size-base" = "0.95rem"),
  padding = 0,
  tags$head(tags$style(HTML(CSS)),
            tags$script(HTML("$(document).on('shiny:connected', function() { Shiny.addCustomMessageHandler('scrollTop', function(x) { window.scrollTo({top: 0, behavior: 'smooth'}); }); });"))),
  div(class = "topbar",
    div(class = "brand", icon("shield-halved"), " Will to Fight · exploration ML"),
    div(class = "d-flex gap-2",
      actionButton("demo_start", "Démo guidée", icon = icon("wand-magic-sparkles"), class = "btn-sm btn-warning"),
      actionButton("help", "Aide", icon = icon("circle-question"), class = "btn-sm btn-outline-light"))),
  uiOutput("stepper"),
  div(class = "container-xl main",
    uiOutput("demo_banner"),
    navset_hidden(id = "steps",
      nav_panel_hidden("home", home_ui),
      nav_panel_hidden("import", import_ui),
      nav_panel_hidden("check", check_ui),
      nav_panel_hidden("setup", setup_ui),
      nav_panel_hidden("results", results_ui)))
)

# =============================================================================
# Serveur
# =============================================================================
server <- function(input, output, session) {
  rv <- reactiveValues(step = "home", demo = FALSE, raw = NULL, name = NULL, info = NULL, auto = NULL,
                       flagged = character(0), keywords = DEFAULT_KEYWORDS, res = NULL, out_dir = NULL,
                       cfg_loaded = NULL, log = NULL, rev_cfg = NULL)

  # ------------------------------------------------------------- navigation
  unlocked <- function(step) switch(step, home = , import = TRUE, check = , setup = !is.null(rv$raw), results = !is.null(rv$res))
  go <- function(step) {
    if (!unlocked(step)) {
      msg <- if (step == "results") "Lancez d'abord une analyse (étape 3)." else "Importez d'abord un fichier (étape 1)."
      return(showNotification(msg, type = "warning"))
    }
    rv$step <- step
    nav_select("steps", step)
    session$sendCustomMessage("scrollTop", TRUE)
  }
  order_steps <- names(STEPS)
  for (s in order_steps) local({
    st <- s
    idx <- match(st, order_steps)
    if (idx > 1) observeEvent(input[[paste0("prev_", st)]], go(order_steps[idx - 1]))
    if (idx < length(order_steps)) observeEvent(input[[paste0("next_", st)]], go(order_steps[idx + 1]))
  })
  observeEvent(input$home_start, go("import"))
  observeEvent(input$goto, go(input$goto))

  output$stepper <- renderUI({
    cur <- match(rv$step, order_steps)
    items <- lapply(seq_along(STEPS), function(i) {
      id <- order_steps[i]
      cls <- paste("step", if (i == cur) "current" else if (i < cur) "done" else "", if (!unlocked(id)) "locked" else "")
      dot <- if (i == 1) icon("house") else if (i < cur) icon("check") else i - 1
      tagList(if (i > 1) div(class = "step-sep"),
              tags$a(href = "#", class = cls, tagList(tags$span(class = "dot", dot), STEPS[[i]]),
                     onclick = sprintf("Shiny.setInputValue('goto', '%s', {priority: 'event'}); return false;", id)))
    })
    div(class = "stepper", items)
  })

  observeEvent(input$help, showModal(modalDialog(
    title = "Aide", size = "xl", easyClose = TRUE, footer = modalButton("Fermer"),
    shiny::markdown(readLines("README.md", encoding = "UTF-8")))))

  # ------------------------------------------------------------- démo guidée
  start_demo <- function() {
    rv$demo <- TRUE
    if (!file.exists(DEMO_FILE)) {
      showNotification("Génération des données de démonstration…")
      e <- new.env(); sys.source("simulate_data.R", envir = e)
      dir.create("data", showWarnings = FALSE)
      e$to_qualtrics_sav(e$make_realistic(e$simulate(400)), DEMO_FILE)
    }
    load_file(DEMO_FILE, basename(DEMO_FILE))
    updateRadioButtons(session, "task", selected = "task2")
    updateRadioButtons(session, "intensity", selected = "rapide")
    go("import")
  }
  observeEvent(input$demo_start, start_demo())
  observeEvent(input$home_demo, start_demo())
  observeEvent(input$demo_quit, rv$demo <- FALSE)

  DEMO_TEXT <- list(
    home = "Bienvenue dans la démo ! Elle vous fait parcourir les 4 étapes avec un faux jeu de données. Suivez les bulles jaunes.",
    import = HTML("Un <b>faux export Qualtrics</b> de 400 répondants vient d'être chargé, avec les défauts d'un vrai export (abandons, codes -99, âges en texte…). L'outil a reconnu les questions tout seul grâce aux libellés SPSS. <br>→ Cliquez sur <b>« Suivant »</b> en bas de la page."),
    check = HTML("Chaque section a un statut. À droite, la liste de contrôle résume tout. Ici, l'outil a détecté que les items formulés à l'envers n'étaient pas encore inversés et que l'échelle gauche-droite était codée 1-11 : il corrige lui-même. Ouvrez <b>« Données incomplètes »</b> pour voir combien de répondants sont écartés.<br>→ Puis <b>« Suivant »</b>."),
    setup = HTML("Les choix sont pré-remplis : prédire le <b>Will to Fight</b> à partir des scores des échelles, <b>avec et sans</b> les variables très proches de la cible, en mode <b>rapide</b> (moins d'une minute).<br>→ Cliquez sur <b>« Lancer l'analyse »</b> à droite."),
    results = HTML("Commencez par l'encadré <b>« Ce qu'il faut retenir »</b>. Changez la cible ou les prédicteurs avec les menus juste au-dessus, puis parcourez les onglets. <br><i>Rappel : données simulées, résultats sans valeur scientifique.</i> Pour analyser vos données, quittez la démo et importez votre fichier à l'étape 1.")
  )
  output$demo_banner <- renderUI({
    if (!rv$demo) return(NULL)
    div(class = "demo-banner",
        div(class = "demo-icon", icon("wand-magic-sparkles")),
        div(class = "flex-grow-1", tags$b("Démo guidée · "), DEMO_TEXT[[rv$step]]),
        actionLink("demo_quit", "Quitter la démo", class = "text-nowrap"))
  })

  # ------------------------------------------------------------- import
  load_file <- function(path, name) {
    raw <- tryCatch(read_raw(path, name), error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
    if (is.null(raw)) return()
    rv$raw <- raw; rv$name <- name; rv$info <- column_info(raw); rv$res <- NULL
    rv$auto <- auto_map(rv$info); rv$cfg_loaded <- NULL; rv$rev_cfg <- NULL
    cfg_path <- file.path(CONFIG_DIR, paste0(tools::file_path_sans_ext(name), ".json"))
    if (file.exists(cfg_path) && !rv$demo) {
      apply_config(load_config(cfg_path))
      showNotification("Réglages précédents de ce fichier rechargés.", type = "message")
    }
    m <- rv$cfg_loaded %||% rv$auto$mapping
    rv$flagged <- CODEBOOK$var[is.na(m) | (is.null(rv$cfg_loaded) & rv$auto$score < 0.6)]
  }
  observeEvent(input$file, { rv$demo <- FALSE; load_file(input$file$datapath, input$file$name) })
  observeEvent(input$import_demo, start_demo())

  apply_config <- function(cfg) {
    rv$cfg_loaded <- cfg$mapping
    s <- cfg$settings
    updateTextInput(session, "missing_codes", value = paste(s$missing_codes, collapse = ", "))
    updateNumericInput(session, "survey_year", value = s$survey_year)
    updateCheckboxInput(session, "drop_attention", value = s$drop_attention)
    updateNumericInput(session, "attention_expected", value = s$attention_expected)
    rv$rev_cfg <- if (is.logical(s$reverse_items) && length(s$reverse_items) > 1) s$reverse_items else NULL
    if (!is.null(s$min_category)) updateSliderInput(session, "min_category", value = s$min_category)
    updateRadioButtons(session, "reverse_status", selected = if (isTRUE(s$reverse_status)) "yes" else "no")
    updateCheckboxInput(session, "fix_lr_1_11", value = isTRUE(s$fix_lr_1_11))
    updateSliderInput(session, "max_missing", value = 100 * s$max_missing)
    updateSliderInput(session, "min_scale_items", value = 100 * s$min_scale_items)
    rv$keywords <- s$keywords
  }
  observeEvent(input$load_cfg, {
    cfg <- tryCatch(load_config(input$load_cfg$datapath), error = function(e) NULL)
    if (is.null(cfg)) return(showNotification("Fichier de configuration illisible.", type = "error"))
    apply_config(cfg)
    showNotification("Configuration importée.", type = "message")
  })

  output$import_summary <- renderUI({
    if (is.null(rv$raw)) return(card(card_body(class = "text-muted text-center py-5",
      icon("file-circle-question", class = "fa-2x mb-2"), p("Aucun fichier chargé pour l'instant."))))
    m <- rv$cfg_loaded %||% rv$auto$mapping
    n_found <- sum(!is.na(m))
    card(card_header(icon("circle-check", class = "text-success"), " Fichier chargé"),
      layout_column_wrap(width = "160px", fill = FALSE,
        value_box("Fichier", tags$span(style = "font-size:1rem; word-break:break-all", rv$name), theme = "primary"),
        value_box("Répondants", nrow(rv$raw), sprintf("%d colonnes", ncol(rv$raw)), theme = "secondary"),
        value_box("Questions reconnues", sprintf("%d / %d", n_found, nrow(CODEBOOK)),
                  sprintf("%d à vérifier à l'étape 2", length(rv$flagged)),
                  theme = if (length(rv$flagged)) "warning" else "success")),
      accordion(open = FALSE, accordion_panel("Aperçu du fichier", DT::DTOutput("raw_table"))))
  })
  output$raw_table <- DT::renderDT({
    req(rv$raw)
    d <- as.data.frame(lapply(rv$raw, function(x) if (inherits(x, "haven_labelled")) as.vector(x) else x))
    DT::datatable(utils::head(d, 200), options = list(scrollX = TRUE, pageLength = 5), rownames = FALSE)
  })

  # ------------------------------------------------------------- correspondance
  mapping_row <- function(v, choices, initial) {
    sel <- initial[[v]]; if (is.na(sel) || !sel %in% rv$info$column) sel <- NONE
    badge <- if (sel == NONE) ICON[["absente"]] else if (v %in% rv$flagged) ICON[["attention"]] else ICON[["ok"]]
    cb <- CODEBOOK[CODEBOOK$var == v, ]
    div(class = "row map-row",
        div(class = "col-md-5", tags$b(cb$short), tags$br(), tags$span(class = "text-muted small", cb$block)),
        div(class = "col-md-6", selectInput(input_id(v), NULL, choices, sel, width = "100%")),
        div(class = "col-md-1", badge))
  }
  output$mapping_ui <- renderUI({
    req(rv$info)
    choices <- c(setNames(NONE, NONE), setNames(rv$info$column, column_choice_label(rv$info)))
    initial <- rv$cfg_loaded %||% rv$auto$mapping
    flagged <- rv$flagged
    first <- if (length(flagged)) div(class = "mb-3",
      h6(sprintf("À vérifier (%d)", length(flagged))),
      help_text("Correspondance incertaine ou introuvable : choisissez la bonne colonne, ou laissez « aucune colonne » si la question n'est pas dans votre fichier."),
      lapply(flagged, mapping_row, choices = choices, initial = initial))
    else div(class = "alert alert-success py-2", "Toutes les questions ont été reconnues avec certitude.")
    panels <- lapply(BLOCKS, function(b) {
      vars <- setdiff(CODEBOOK$var[CODEBOOK$block == b], flagged)
      if (!length(vars)) return(NULL)
      accordion_panel(sprintf("%s (%d)", b, length(vars)), lapply(vars, mapping_row, choices = choices, initial = initial))
    })
    tagList(first, h6("Questions reconnues automatiquement"),
            do.call(accordion, c(list(open = FALSE, multiple = TRUE), Filter(Negate(is.null), panels))))
  })

  mapping <- reactive({
    req(rv$info)
    vals <- lapply(CODEBOOK$var, function(v) input[[input_id(v)]])
    m <- if (all(vapply(vals, is.null, TRUE))) rv$cfg_loaded %||% rv$auto$mapping
         else setNames(vapply(vals, function(x) if (is.null(x) || x == NONE) NA_character_ else x, ""), CODEBOOK$var)
    attr(m, "dup") <- unique(m[!is.na(m)][duplicated(m[!is.na(m)])])
    m
  })

  settings <- reactive({
    codes <- suppressWarnings(as.numeric(trimws(strsplit(input$missing_codes %||% "", "[,;]")[[1]])))
    list(missing_codes = codes[!is.na(codes)], survey_year = input$survey_year %||% 2026,
         drop_attention = isTRUE(input$drop_attention %||% TRUE), attention_expected = input$attention_expected %||% 3,
         reverse_items = reverse_choice(), reverse_status = (input$reverse_status %||% "yes") == "yes",
         conflict_source = input$conflict_source %||% "auto", min_category = input$min_category %||% 30,
         fix_lr_1_11 = isTRUE(input$fix_lr_1_11 %||% TRUE), max_missing = (input$max_missing %||% 50) / 100,
         min_scale_items = (input$min_scale_items %||% 50) / 100, keywords = rv$keywords)
  })

  # Lecture des colonnes : ne dépend que des réglages de lecture (pas des choix d'inversion, qui en dépendent)
  read_settings <- reactive({
    codes <- suppressWarnings(as.numeric(trimws(strsplit(input$missing_codes %||% "", "[,;]")[[1]])))
    list(missing_codes = codes[!is.na(codes)], survey_year = input$survey_year %||% 2026,
         fix_lr_1_11 = isTRUE(input$fix_lr_1_11 %||% TRUE))
  })
  canon <- reactive({ req(rv$raw); to_canonical(rv$raw, mapping(), read_settings()) })
  checks <- reactive(check_mapping(canon(), mapping()))
  prepared <- reactive(prepare_data(canon(), settings(), verbose = FALSE))
  reversal <- reactive(tryCatch(detect_reversal(canon()), error = function(e) NULL))
  # Choix item par item (cases à cocher) ; "auto" tant que les cases ne sont pas affichées
  reverse_choice <- reactive({
    r <- reversal()
    if (is.null(r) || !nrow(r)) return("auto")
    vals <- lapply(r$item, function(v) input[[paste0("rev__", v)]])
    if (all(vapply(vals, is.null, TRUE))) return(rv$rev_cfg %||% "auto")
    setNames(vapply(seq_along(vals), function(i) vals[[i]] %||% r$reverse[i], TRUE), r$item)
  })
  dir_warnings <- reactive(tryCatch(direction_checks(prepared()), error = function(e) character(0)))

  observe({
    req(rv$name, rv$info, !rv$demo)
    save_config(file.path(CONFIG_DIR, paste0(tools::file_path_sans_ext(rv$name), ".json")), mapping(), settings())
  })
  output$save_cfg <- downloadHandler(
    filename = function() paste0("configuration_", tools::file_path_sans_ext(rv$name %||% "donnees"), ".json"),
    content = function(file) save_config(file, mapping(), settings()))

  # ------------------------------------------------------------- statuts de vérification
  status <- reactive({
    req(rv$raw)
    m <- mapping(); ck <- checks()
    unsure <- sum(vapply(rv$flagged, function(v) !is.na(m[[v]]) && identical(m[[v]], rv$auto$mapping[[v]]) && rv$auto$score[[v]] < 0.6, TRUE))
    absent <- sum(is.na(m))
    rev <- reversal(); ch <- reverse_choice()
    rev_ok <- is.null(rev) || !nrow(rev) || identical(ch, "auto") ||
      all(unname(ch[rev$item]) == rev$reverse, na.rm = TRUE)
    n_rev <- if (identical(ch, "auto")) sum(rev$reverse %||% FALSE) else sum(ch)
    list(unsure = unsure, absent = absent, dup = attr(m, "dup"),
         problems = sum(ck$status == "problème"), warnings = sum(ck$status == "attention"),
         dir = dir_warnings(), rev = rev, n_rev = n_rev, rev_ok = rev_ok, df = prepared())
  })
  sicon <- function(ok, warn = FALSE) if (ok) ICON[["ok"]] else if (warn) ICON[["attention"]] else ICON[["problème"]]

  output$t_map <- renderUI({
    s <- status()
    ok <- !length(s$dup) && s$unsure == 0
    tagList(sicon(ok, warn = !length(s$dup)), sprintf(" Correspondance des questions : %d/%d associées%s%s",
      nrow(CODEBOOK) - s$absent, nrow(CODEBOOK), if (s$unsure) sprintf(", %d à vérifier", s$unsure) else "",
      if (length(s$dup)) ", colonne utilisée deux fois !" else ""))
  })
  output$t_values <- renderUI({
    s <- status()
    n_warn <- s$warnings + length(s$dir)
    tagList(sicon(s$problems == 0 && n_warn == 0, warn = s$problems == 0),
            if (s$problems) sprintf(" Valeurs et cohérence : %d variable(s) hors échelle", s$problems)
            else if (n_warn) sprintf(" Valeurs et cohérence : %d point(s) à vérifier", n_warn)
            else " Valeurs et cohérence : rien à signaler")
  })
  output$t_coding <- renderUI({
    s <- status()
    tagList(sicon(s$rev_ok, warn = TRUE), if (s$rev_ok) sprintf(" Codage : %d item(s) inversé(s) par l'outil, conforme au diagnostic", s$n_rev)
            else " Codage : vos choix d'inversion diffèrent du diagnostic")
  })
  output$t_missing <- renderUI({
    s <- status(); log <- attr(s$df, "log")
    tagList(ICON[["ok"]], sprintf(" Données incomplètes : %d répondants retenus sur %d", nrow(s$df), nrow(rv$raw)))
  })

  output$check_summary <- renderUI({
    s <- status()
    item <- function(ic, title, detail) div(class = "item", div(class = "ic", ic), div(tags$b(title), tags$br(), tags$span(class = "small text-muted", detail)))
    ready <- !length(s$dup) && s$problems == 0
    card(card_header(icon("list-check"), "Liste de contrôle"),
      div(class = "checklist",
        item(sicon(!length(s$dup) && s$unsure == 0, !length(s$dup)), "Questions associées",
             if (length(s$dup)) sprintf("Colonne utilisée deux fois : %s", paste(s$dup, collapse = ", "))
             else sprintf("%d/%d%s", nrow(CODEBOOK) - s$absent, nrow(CODEBOOK), if (s$unsure) sprintf(" (%d à vérifier)", s$unsure) else "")),
        item(sicon(s$problems == 0, TRUE), "Valeurs dans les échelles",
             if (s$problems) sprintf("%d variable(s) à corriger", s$problems)
             else if (s$warnings) sprintf("%d variable(s) à vérifier", s$warnings) else "aucun problème"),
        item(sicon(!length(s$dir), TRUE), "Cohérence des échelles",
             if (length(s$dir)) sprintf("%d alerte(s) : voir « Valeurs et cohérence »", length(s$dir)) else "sens des échelles cohérent"),
        item(sicon(s$rev_ok, TRUE), "Items formulés à l'envers",
             if (is.null(s$rev) || !nrow(s$rev)) "pas de diagnostic possible"
             else sprintf("%d inversé(s) par l'outil sur %d%s", s$n_rev, nrow(s$rev), if (s$rev_ok) "" else " (différent du diagnostic)")),
        item(ICON[["ok"]], sprintf("%d répondants analysables", nrow(s$df)),
             HTML(paste(utils::head(attr(s$df, "log"), -1), collapse = "<br>")))),
      if (ready) div(class = "alert alert-success py-2 mb-0 mt-2", icon("circle-check"), " Prêt pour l'analyse.")
      else div(class = "alert alert-danger py-2 mb-0 mt-2", "Corrigez les points marqués ❌ avant de continuer."))
  })

  # Ouvre automatiquement les sections qui demandent de l'attention
  observeEvent(rv$info, {
    session$onFlushed(function() {
      s <- isolate(status())
      open <- c(if (s$unsure || s$absent || length(s$dup)) "map", if (s$problems || s$warnings || length(s$dir)) "values",
                if (!s$rev_ok) "coding")
      accordion_panel_close("check_acc", TRUE)
      if (length(open)) accordion_panel_open("check_acc", open)
    }, once = TRUE)
  })

  output$values_problems <- renderUI({
    ck <- checks(); pb <- ck[ck$status %in% c("problème", "attention"), ]
    dw <- dir_warnings()
    tagList(
      if (!nrow(pb)) div(class = "alert alert-success py-2", "Toutes les variables associées ont des valeurs dans l'échelle attendue.")
      else tags$ul(lapply(seq_len(nrow(pb)), function(i)
        tags$li(ICON[[pb$status[i]]], tags$b(pb$question[i]), sprintf(" (colonne %s) : %s", pb$column[i], pb$message[i])))),
      if (length(dw)) div(class = "alert alert-warning py-2",
        tags$b("Cohérence des échelles"), tags$ul(class = "mb-1", lapply(dw, tags$li)),
        tags$span(class = "small", "L'analyse reste possible, mais les scores concernés risquent d'être mal interprétés : à vérifier dans le fichier SPSS.")))
  })
  output$check_table <- DT::renderDT({
    ck <- checks()
    ck$status <- paste(ICON[ck$status], ck$status)
    DT::datatable(ck[, c("status", "block", "question", "column", "n_valid", "min", "max", "message")],
                  colnames = c("Statut", "Bloc", "Question", "Colonne", "N valides", "Min", "Max", "Remarque"),
                  rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$reversal_ui <- renderUI({
    r <- reversal()
    if (is.null(r) || !nrow(r)) return(help_text("Aucun item concerné n'est associé à une colonne."))
    m <- mapping()
    rows <- lapply(seq_len(nrow(r)), function(i) {
      v <- r$item[i]
      cur <- isolate(input[[paste0("rev__", v)]]) %||% (if (!is.null(rv$rev_cfg) && v %in% names(rv$rev_cfg)) rv$rev_cfg[[v]] else r$reverse[i])
      div(class = "row map-row",
        div(class = "col-md-7", checkboxInput(paste0("rev__", v), tagList(tags$b(label_of(v)), tags$br(),
            tags$span(class = "small text-muted", sprintf("colonne %s · échelle « %s »", m[[v]], label_of(r$scale[i])))), cur)),
        div(class = "col-md-5 small", sprintf("r = %s → ", fmt(r$r[i])),
            if (r$reverse[i]) tags$b("pas encore inversé : à inverser") else tags$b("déjà dans le bon sens")))
    })
    tagList(rows, actionLink("apply_rev", "Appliquer toutes les recommandations", icon = icon("wand-magic-sparkles")))
  })
  observeEvent(input$apply_rev, {
    r <- reversal()
    for (i in seq_len(nrow(r))) updateCheckboxInput(session, paste0("rev__", r$item[i]), value = r$reverse[i])
  })

  output$missing_summary <- renderUI({
    df <- status()$df
    div(class = "alert alert-light border py-2 small", HTML(paste(attr(df, "log"), collapse = "<br>")))
  })

  output$alpha_table <- renderTable(reliability_table(prepared()), na = "")

  # Mots-clés du conflit imaginé
  output$keywords_ui <- renderUI({
    kw <- rv$keywords
    tagList(
      lapply(names(kw), function(k) textAreaInput(paste0("kw__", k), k, paste(kw[[k]], collapse = ", "), rows = 2, width = "100%")),
      actionButton("kw_apply", "Appliquer", class = "btn-sm btn-primary"),
      actionButton("kw_reset", "Valeurs par défaut", class = "btn-sm btn-outline-secondary"))
  })
  observeEvent(input$kw_apply, {
    rv$keywords <- setNames(lapply(names(rv$keywords), function(k) trimws(strsplit(input[[paste0("kw__", k)]], ",")[[1]])), names(rv$keywords))
  })
  observeEvent(input$kw_reset, rv$keywords <- DEFAULT_KEYWORDS)
  output$conflict_source_ui <- renderUI({
    has_coded <- "conflict_coded" %in% names(canon())
    has_text <- "conflict_text" %in% names(canon())
    if (!has_coded && !has_text) return(help_text("Aucune colonne associée à la question « conflit imaginé »."))
    choices <- c(if (has_coded) c("Codage manuel présent dans le fichier (recommandé)" = "coded"),
                 if (has_text) c("Mots-clés sur la réponse libre" = "keywords"))
    radioButtons("conflict_source", "Catégories utilisées pour l'analyse", choices,
                 selected = isolate(input$conflict_source) %||% choices[[1]])
  })
  output$coded_table <- DT::renderDT({
    df <- prepared()
    validate(need("conflict_cat" %in% names(df), "Pas de catégorie de conflit."))
    t <- as.data.frame(sort(table(df$conflict_cat), decreasing = TRUE), stringsAsFactors = FALSE)
    names(t) <- c("Catégorie", "Répondants")
    t$`%` <- round(100 * t$Répondants / sum(t$Répondants), 1)
    DT::datatable(t, rownames = FALSE, options = list(pageLength = 10, dom = "tp"))
  })
  output$conflict_table <- DT::renderDT({
    df <- canon()
    validate(need("conflict_text" %in% names(df), "La question « conflit imaginé » n'est associée à aucune colonne."))
    t <- as.data.frame(table(reponse = trimws(df$conflict_text)), stringsAsFactors = FALSE)
    t <- t[t$reponse != "", ]
    t$categorie <- code_conflict(t$reponse, rv$keywords)
    t <- t[order(t$categorie, -t$Freq), c("categorie", "reponse", "Freq")]
    DT::datatable(t, colnames = c("Catégorie", "Réponse", "Nombre"), rownames = FALSE, filter = "top",
                  options = list(pageLength = 10))
  })

  # ------------------------------------------------------------- choix de l'analyse
  output$targets_ui <- renderUI({
    tk <- TASKS[[input$task %||% "task1"]]
    sel <- if (rv$demo && input$task == "task2") c("wtf_score", "wtf_be") else tk$targets[1]
    tagList(
      tags$label(class = "form-label fw-semibold mt-2", "Variable(s) à prédire"),
      layout_columns(col_widths = c(6, 6),
        checkboxGroupInput("targets", NULL, setNames(tk$targets, label_of(tk$targets)), selected = sel),
        div(help_text("Le premier choix est le score composite (moyenne des items). Cochez plusieurs variables pour les comparer ; chaque variable ajoutée allonge le calcul."),
            actionLink("targets_all", "Tout cocher"), " · ", actionLink("targets_none", "Tout décocher"))))
  })
  observeEvent(input$targets_all, updateCheckboxGroupInput(session, "targets", selected = TASKS[[input$task]]$targets))
  observeEvent(input$targets_none, updateCheckboxGroupInput(session, "targets", selected = character(0)))

  FS_DESC <- c(
    "futur_seul" = "Uniquement les réponses du bloc « Futur » : menace perçue, probabilités du conflit, type de conflit, horizon.",
    "futur+demo+identite" = "Le bloc « Futur » + âge, genre, orientation politique… + identification aux Belges et aux Européens.",
    "scores" = "Un score par échelle (confiance, menace russe, attitudes guerre/paix, narratifs…) + démographie. Recommandé.",
    "scores+proximal" = "Comme ci-dessus + disposition au sacrifice et comportements hypothétiques, très proches du Will to Fight. Pour comparaison.",
    "items" = "Chaque question séparément (plus de détail, mais plus de variables pour peu de répondants).")
  output$fsets_ui <- renderUI({
    fs <- names(TASKS[[input$task %||% "task1"]]$feature_sets)
    sel <- if (rv$demo && input$task == "task2") c("scores", "scores+proximal") else fs[1]
    div(class = "choice-rich", checkboxGroupInput("fsets", NULL, width = "100%", selected = sel, choiceValues = fs,
      choiceNames = lapply(fs, function(f) tagList(tags$b(FEATURE_SET_LABELS[[f]]), tags$span(class = "choice-desc", FS_DESC[[f]])))))
  })

  spec <- reactive({
    s <- default_spec()
    s[c("task", "targets", "feature_sets", "models", "binary", "threshold", "importance_model", "shuffle", "seed", "cores")] <-
      list(input$task %||% "task1", input$targets, input$fsets, union("baseline", input$models), identical(input$mode, "binary"),
           input$threshold %||% 5, input$importance_model %||% "random_forest", isTRUE(input$shuffle), input$seed %||% 42,
           input$cores %||% s$cores)
    if (identical(input$intensity, "custom")) {
      s[c("outer_k", "repeats", "inner_k", "n_iter")] <- list(input$outer_k, input$repeats, input$inner_k, input$n_iter)
    } else s <- utils::modifyList(s, INTENSITY[[input$intensity %||% "standard"]])
    s
  })

  output$setup_summary <- renderUI({
    s <- spec()
    li <- function(k, v) tags$li(tags$span(class = "text-muted", k), " ", v)
    tagList(
      tags$ul(class = "list-unstyled mb-2",
        li("Prédire :", if (length(s$targets)) paste(label_of(s$targets), collapse = " ; ") else tags$span(class = "text-danger", "rien de coché")),
        li("À partir de :", if (length(s$feature_sets)) paste(FEATURE_SET_LABELS[s$feature_sets], collapse = " ; ") else tags$span(class = "text-danger", "rien de coché")),
        li("Modèles :", sprintf("%d + référence", length(s$models) - 1)),
        li("Validation :", sprintf("%d plis × %d, réglage sur %d plis internes, %d réglages testés", s$outer_k, s$repeats, s$inner_k, s$n_iter)),
        li("Répondants :", if (!is.null(rv$raw)) nrow(prepared()) else "aucun fichier"),
        if (s$binary) li("Mode :", sprintf("prêt·e oui/non (seuil %s)", s$threshold)),
        if (s$shuffle) li("", tags$span(class = "text-warning", "Test de contrôle : cible mélangée")))
    )
  })

  output$estimate <- renderUI({
    s <- spec()
    n_jobs <- length(s$targets) * length(s$feature_sets)
    fits <- n_jobs * s$outer_k * s$repeats * (1 + s$inner_k * (2 + 2 * s$n_iter))
    n <- if (!is.null(rv$raw)) nrow(prepared()) else 400
    # ~0,25 s par ajustement à 400 répondants, croissance sous-linéaire avec la taille (mesuré)
    sec <- 0.25 * (max(n, 400) / 400)^0.7 * fits / max(1, min(s$cores, s$outer_k * s$repeats))
    help_text(class = "mt-2 mb-0", icon("clock"), sprintf(" Durée estimée : %s", if (sec < 90) sprintf("~%d s", max(5, round(sec))) else sprintf("~%d min", round(sec / 60))))
  })

  observeEvent(input$run, {
    if (is.null(rv$raw)) return(showNotification("Importez d'abord des données (étape 1).", type = "error"))
    s <- spec()
    if (!length(s$targets) || !length(s$feature_sets))
      return(showNotification("Cochez au moins une variable à prédire et un jeu d'informations.", type = "error"))
    if (length(attr(mapping(), "dup")))
      return(showNotification("Une même colonne est associée à plusieurs questions : corrigez l'étape 2.", type = "error"))
    df <- prepared()
    t0 <- Sys.time()
    res <- withProgress(message = "Analyse en cours", value = 0, {
      tryCatch(run_analysis(df, s, progress = function(v, d) setProgress(v, detail = d)),
               error = function(e) { showNotification(paste("Erreur :", conditionMessage(e)), type = "error", duration = NULL); NULL })
    })
    if (is.null(res)) return()
    rv$res <- res
    rv$out_dir <- save_results(res, file.path(RESULTS_DIR, format(Sys.time(), "%Y-%m-%d_%Hh%Mm%S")))
    rv$log <- sprintf("Terminé en %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins")))
    if (length(res$skipped)) showNotification(paste("Ignoré :", paste(res$skipped, collapse = " ; ")), type = "warning", duration = 15)
    if (length(res$errors)) showNotification(paste("Erreur de modèle :", paste(res$errors, collapse = " ; ")), type = "warning", duration = 15)
    go("results")
  })

  # ------------------------------------------------------------- résultats
  res <- reactive({ req(rv$res); rv$res })

  output$results_meta <- renderUI({
    r <- res(); s <- r$spec
    tags$div(class = "text-muted small text-end",
      sprintf("%d répondants · %s · validation %d plis × %d · %s", r$n, if (s$binary) sprintf("prêt·e ≥ %s (AUC)", s$threshold) else "score continu (R²)",
              s$outer_k, s$repeats, rv$log), tags$br(), "Enregistré dans ", tags$code(rv$out_dir))
  })
  output$target_pick_ui <- renderUI({
    t <- unique(res()$scores$target)
    selectInput("target_pick", "Variable prédite", setNames(t, label_of(t)), width = "100%")
  })
  output$fs_pick_ui <- renderUI({
    fs <- unique(res()$scores$features)
    selectInput("fs_pick", "Informations utilisées", setNames(fs, FEATURE_SET_LABELS[fs]), width = "100%")
  })

  output$takeaway <- renderUI({
    req(input$target_pick, input$fs_pick)
    r <- res()
    lines <- c(if (isTRUE(r$spec$shuffle)) "<b>Test de contrôle « cible mélangée »</b> : les scores ci-dessous doivent être proches du hasard.",
               interpret_target(r, input$target_pick, input$fs_pick))
    sc <- r$scores; metric <- main_metric(r); col <- paste0(metric, "_mean")
    if (all(c("scores", "scores+proximal") %in% sc$features)) {
      a <- max(sc[sc$target == input$target_pick & sc$features == "scores" & sc$model != "baseline", col])
      b <- max(sc[sc$target == input$target_pick & sc$features == "scores+proximal" & sc$model != "baseline", col])
      lines <- c(lines, sprintf("Ajouter les variables proximales fait passer le %s de %s à %s : elles mesurent presque la même chose que la cible, ce gain n'est donc pas une découverte.",
                                METRIC_NAME[[metric]], fmt(a), fmt(b)))
    }
    others <- setdiff(unique(sc$target), input$target_pick)
    if (length(others)) {
      best_by <- vapply(others, function(t) max(sc[sc$target == t & sc$features == input$fs_pick & sc$model != "baseline", col]), 0)
      o <- order(-best_by)
      lines <- c(lines, sprintf("Autres variables prédites (même jeu d'informations) : %s.",
                                paste(sprintf("%s : %s", label_of(others[o]), fmt(best_by[o])), collapse = " ; ")))
    }
    tags$ul(class = "mb-0", lapply(lines, function(l) tags$li(HTML(l))))
  })

  heatmap_plot <- reactive({ req(input$fs_pick); plot_heatmap(res(), input$fs_pick) })
  output$heatmap <- renderPlot(heatmap_plot(), height = function() 140 + 38 * length(unique(res()$scores$target)), res = 96)
  importance_plot <- reactive({ req(input$target_pick, input$fs_pick); plot_importance(res(), input$target_pick, input$fs_pick) })
  output$importance_plot <- renderPlot({
    p <- importance_plot()
    validate(need(!is.null(p), "Pas d'importance disponible pour ce choix."))
    p
  }, res = 96)
  output$models_plot <- renderPlot({ req(input$target_pick); plot_models(res(), input$target_pick) },
    height = function() 130 + 170 * length(unique(res()$scores$features)), res = 96)

  fmt_table <- function(d) {
    numc <- vapply(d, is.numeric, TRUE)
    DT::formatRound(DT::datatable(d, rownames = FALSE, filter = "top", options = list(pageLength = 15, scrollX = TRUE)),
                    names(d)[numc & !names(d) %in% c("n", "n_folds", "fold", "repeat_id")], 3)
  }
  output$scores_table <- DT::renderDT({
    d <- res()$scores
    d$target <- label_of(d$target); d$model <- MODEL_LABELS()[d$model]; d$features <- FEATURE_SET_LABELS[d$features]
    fmt_table(d)
  })
  output$params_table <- DT::renderDT({
    d <- res()$folds[, c("target", "features", "model", "fold", "params")]
    d <- d[d$params != "", ]
    d$target <- label_of(d$target); d$model <- MODEL_LABELS()[d$model]
    DT::datatable(d, rownames = FALSE, filter = "top", options = list(pageLength = 15, scrollX = TRUE))
  })
  output$coef_table <- DT::renderDT({
    d <- res()$coefs
    validate(need(!is.null(d), "L'ElasticNet n'a pas été lancé."))
    d$target <- label_of(d$target); d$question <- label_of(sub("^manquant_", "", d$variable))
    fmt_table(d[order(d$target, d$features, -abs(d$coef_std)), c("target", "features", "question", "variable", "coef_std", "alpha", "lambda")])
  })

  csv_dl <- function(key) downloadHandler(filename = function() paste0(key, ".csv"),
                                          content = function(f) utils::write.csv(res()[[key]], f, row.names = FALSE))
  output$dl_scores <- csv_dl("scores"); output$dl_folds <- csv_dl("folds")
  output$dl_imp <- csv_dl("importance"); output$dl_coefs <- csv_dl("coefs")
  output$dl_heatmap <- downloadHandler("comparaison_modeles.png", function(f)
    ggsave(f, heatmap_plot(), width = 10, height = 1.5 + 0.4 * length(unique(res()$scores$target)), dpi = 150, bg = "white"))
  output$dl_importance <- downloadHandler(function() paste0("importance_", input$target_pick, ".png"), function(f)
    ggsave(f, importance_plot(), width = 8, height = 6.5, dpi = 150, bg = "white"))
}

shinyApp(ui, server)
