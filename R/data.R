`%||%` <- function(a, b) if (is.null(a)) b else a

# Import des données, correspondance automatique des colonnes, contrôles, nettoyage et variables construites.

default_settings <- function() {
  list(
    missing_codes = c(-99, -9, 99, 999),
    survey_year = 2026,
    drop_attention = TRUE,
    attention_expected = 3,
    reverse_items = "auto",   # "auto" (diagnostic par corrélation), TRUE / FALSE pour tous, ou c(item = TRUE/FALSE)
    reverse_status = TRUE,    # statut subjectif : 1 = en haut dans le questionnaire -> inversé pour que 10 = en haut
    fix_lr_1_11 = TRUE,       # gauche-droite exporté par Qualtrics en 1-11 au lieu de 0-10 -> ramené à 0-10
    max_missing = 0.5,        # répondants exclus au-delà de cette part de questions sans réponse
    min_scale_items = 0.5,    # score d'échelle calculé seulement si au moins cette part des items est répondue
    conflict_source = "auto", # "coded" (codage manuel du fichier), "keywords" (mots-clés), "auto" = codé si présent
    min_category = 30,        # catégories de conflit plus rares regroupées dans « Autres »
    keywords = DEFAULT_KEYWORDS
  )
}

# ---------------------------------------------------------------------------
# Lecture
# ---------------------------------------------------------------------------
read_raw <- function(path, name = basename(path)) {
  ext <- tolower(tools::file_ext(name))
  if (ext == "sav") return(haven::read_sav(path))
  if (ext %in% c("csv", "txt")) {
    first <- readLines(path, n = 1, warn = FALSE)
    sep <- if (lengths(regmatches(first, gregexpr(";", first))) > lengths(regmatches(first, gregexpr(",", first)))) ";" else ","
    return(utils::read.csv(path, sep = sep, stringsAsFactors = FALSE, check.names = FALSE,
                           dec = if (sep == ";") "," else ".", encoding = "UTF-8"))
  }
  stop("Format non reconnu : utilisez un fichier .sav (SPSS) ou .csv")
}

# Tableau descriptif des colonnes du fichier : nom, libellé SPSS, type, exemple de valeurs
column_info <- function(raw) {
  lab <- vapply(raw, function(x) { l <- attr(x, "label"); if (is.null(l)) "" else as.character(l) }, "")
  data.frame(
    column = names(raw), label = lab,
    kind = vapply(raw, function(x) if (is.numeric(x)) "numérique" else "texte", ""),
    n_missing = vapply(raw, function(x) sum(is.na(x) | (is.character(x) & trimws(x) == "")), 0L),
    values = vapply(raw, function(x) {
      u <- unique(stats::na.omit(as.vector(x)))
      paste(utils::head(sort(u), 8), collapse = ", ")
    }, ""),
    stringsAsFactors = FALSE, row.names = NULL
  )
}

# ---------------------------------------------------------------------------
# Correspondance automatique : noms identiques, sinon similarité entre le texte de la question
# (codebook) et le libellé SPSS de la colonne (TF-IDF sur les mots, sans accents ni mots vides)
# ---------------------------------------------------------------------------
STOPWORDS <- c("les", "des", "une", "est", "que", "qui", "vous", "votre", "vos", "dans", "pour", "par", "sur",
               "avec", "aux", "leur", "leurs", "elle", "ils", "pas", "plus", "cela", "ce", "cette", "ses", "son",
               "sont", "etre", "the", "and", "quel", "quelle", "point", "mesure", "suivantes", "affirmations")

tokens <- function(x) {
  x <- tolower(iconv(x, to = "ASCII//TRANSLIT"))
  w <- unlist(strsplit(x, "[^a-z0-9]+"))
  unique(w[nchar(w) >= 3 & !w %in% STOPWORDS])
}

auto_map <- function(info, threshold = 0.3) {
  cb_tok <- lapply(paste(CODEBOOK$stem, CODEBOOK$item, EN_LABELS[CODEBOOK$var]), tokens)
  col_tok <- lapply(paste(info$label, if (all(info$label == "")) info$column else ""), tokens)
  vocab <- unique(unlist(cb_tok))
  idf <- log(1 + length(cb_tok) / (1 + table(factor(unlist(cb_tok), levels = vocab))))
  w <- function(t) sum(idf[intersect(t, vocab)])
  S <- sapply(seq_along(col_tok), function(j) sapply(seq_along(cb_tok), function(i) {
    shared <- intersect(cb_tok[[i]], col_tok[[j]])
    if (!length(shared)) return(0)
    w(shared) / sqrt(w(cb_tok[[i]]) * max(w(col_tok[[j]]), 1e-9))
  }))
  S <- matrix(S, nrow = nrow(CODEBOOK))
  # Nom de colonne identique au nom canonique : correspondance certaine
  same <- outer(CODEBOOK$var, info$column, function(a, b) tolower(a) == tolower(b))
  S[same] <- 2
  # Noms connus du fichier SPSS de Loïc (ALIASES) : correspondance certaine
  alias <- outer(CODEBOOK$var, info$column, function(a, b) tolower(ALIASES[a]) == tolower(b))
  S[!is.na(alias) & alias] <- 3   # prioritaire sur un nom identique : APWS_5 du fichier n'est pas l'item 5 du questionnaire
  # Attribution gloutonne une-à-une, en commençant par les paires les plus sûres
  mapping <- setNames(rep(NA_character_, nrow(CODEBOOK)), CODEBOOK$var)
  score <- setNames(rep(0, nrow(CODEBOOK)), CODEBOOK$var)
  ord <- order(S, decreasing = TRUE)
  used_r <- rep(FALSE, nrow(S)); used_c <- rep(FALSE, ncol(S))
  for (k in ord) {
    s <- S[k]
    if (s < threshold) break
    i <- (k - 1) %% nrow(S) + 1; j <- (k - 1) %/% nrow(S) + 1
    if (used_r[i] || used_c[j]) next
    mapping[i] <- info$column[j]; score[i] <- s
    used_r[i] <- TRUE; used_c[j] <- TRUE
  }
  # Libellés identiques au sein d'une même grille (ex. tronqués par SPSS) : impossible de les distinguer
  # par le texte, on les attribue dans l'ordre du questionnaire (un export Qualtrics suit cet ordre).
  lab_of <- setNames(info$label, info$column)
  for (g in unique(paste(CODEBOOK$block, CODEBOOK$stem))) {
    vars <- CODEBOOK$var[paste(CODEBOOK$block, CODEBOOK$stem) == g]
    cols <- mapping[vars]
    for (l in unique(lab_of[stats::na.omit(cols)])) {
      same <- vars[!is.na(cols) & lab_of[cols] == l]
      if (length(same) > 1 && l != "") {
        mapping[same] <- sort_by_position(mapping[same], info$column)
        score[same] <- pmin(score[same], 0.5)   # signalé comme « à vérifier » dans l'interface
      }
    }
  }
  list(mapping = mapping, score = pmin(score, 1))
}

sort_by_position <- function(cols, all_cols) cols[order(match(cols, all_cols))]

# Libellé lisible d'une colonne pour les menus déroulants
column_choice_label <- function(info) {
  l <- ifelse(nchar(info$label) > 90, paste0(substr(info$label, 1, 87), "..."), info$label)
  ifelse(l == "", info$column, paste0(info$column, " : ", l))
}

# ---------------------------------------------------------------------------
# Application de la correspondance + contrôles
# ---------------------------------------------------------------------------
to_canonical <- function(raw, mapping, settings) {
  mapping <- mapping[!is.na(mapping) & mapping != ""]
  df <- data.frame(row.names = seq_len(nrow(raw)))
  for (v in names(mapping)) {
    x <- raw[[mapping[[v]]]]
    type <- CODEBOOK$type[CODEBOOK$var == v]
    if (type == "text") {
      x <- as.character(if (inherits(x, "haven_labelled")) haven::zap_labels(x) else x)
    } else if (type == "coded") {
      # Catégories codées : on garde le libellé de la catégorie (ex. 1 -> "Russia")
      x <- if (inherits(x, "haven_labelled")) as.character(haven::as_factor(x, levels = "labels")) else as.character(x)
    } else {
      if (inherits(x, "haven_labelled")) x <- as.vector(haven::zap_labels(x))
      if (is.character(x)) {
        xs <- toupper(trimws(x))
        # Cercles codés en lettres A-E
        x <- if (type == "ios5" && all(xs[!is.na(xs) & xs != ""] %in% LETTERS[1:5])) match(xs, LETTERS[1:5])
             else if (type == "year") parse_year(xs, settings$survey_year)
             else parse_number(xs)
      }
      x <- as.numeric(x)
      x[x %in% settings$missing_codes] <- NA
      if (type == "lr010" && isTRUE(settings$fix_lr_1_11) && any(!is.na(x)) &&
          min(x, na.rm = TRUE) >= 1 && max(x, na.rm = TRUE) == 11) x <- x - 1
    }
    df[[v]] <- x
  }
  df
}

# Texte libre -> nombre : "23", "23 ans", "23,5" ; sinon NA
parse_number <- function(x) {
  m <- regmatches(x, regexpr("-?[0-9]+([.,][0-9]+)?", x))
  out <- rep(NA_real_, length(x))
  out[grepl("-?[0-9]", x)] <- as.numeric(sub(",", ".", m))
  out
}

# Année d'éruption en texte libre : "2030", "2030-2035" (milieu), "dans 10 ans" ; sinon NA
parse_year <- function(x, survey_year) {
  x <- tolower(iconv(x, to = "ASCII//TRANSLIT"))
  out <- rep(NA_real_, length(x))
  yrs <- regmatches(x, gregexpr("(19|20|21|22|23|24|25)[0-9]{2}", x))
  has <- lengths(yrs) > 0
  out[has] <- vapply(yrs[has], function(y) mean(as.numeric(y)), 0)
  rel <- !has & grepl("dans [0-9]+ an", x)
  out[rel] <- survey_year + as.numeric(sub(".*dans ([0-9]+) an.*", "\\1", x[rel]))
  out
}

check_mapping <- function(df, mapping) {
  rows <- lapply(CODEBOOK$var, function(v) {
    type <- CODEBOOK$type[CODEBOOK$var == v]
    col <- mapping[[v]]
    if (is.na(col) || col == "" || !v %in% names(df))
      return(data.frame(var = v, column = "", n_valid = NA, min = NA, max = NA,
                        status = "absente", message = "Variable non trouvée dans le fichier"))
    x <- df[[v]]
    nv <- sum(!is.na(x) & (!is.character(x) | trimws(x) != ""))
    mn <- if (is.numeric(x) && nv) min(x, na.rm = TRUE) else NA
    mx <- if (is.numeric(x) && nv) max(x, na.rm = TRUE) else NA
    status <- "ok"; msg <- ""
    rg <- TYPE_RANGE[[type]]
    if (nv == 0) { status <- "problème"; msg <- "Aucune valeur valide" }
    else if (!is.null(rg) && (mn < rg[1] || mx > rg[2])) {
      status <- "problème"
      msg <- sprintf("Valeurs attendues entre %g et %g, trouvées entre %g et %g", rg[1], rg[2], mn, mx)
      if (type == "lr010" && mn >= 1 && mx <= 11) msg <- paste(msg, "(codage 1-11 ? soustraire 1 dans SPSS)")
    } else if (type == "likert7" && v %in% unlist(SCALES) && length(unique(stats::na.omit(x))) <= 4 && (mn >= 4 || mx <= 4)) {
      status <- "attention"
      msg <- sprintf("Seulement les valeurs %g à %g : inversion incomplète dans SPSS ? (ex. 1→7, 2→6, 3→5 recodés, mais pas 5→3, 6→2, 7→1)", mn, mx)
    } else if (nv < 0.5 * length(x)) { status <- "attention"; msg <- "Plus de la moitié de valeurs manquantes" }
    data.frame(var = v, column = col, n_valid = nv, min = mn, max = mx, status = status, message = msg)
  })
  out <- do.call(rbind, rows)
  cbind(block = CODEBOOK$block, question = CODEBOOK$short, out)
}

# Les items à inverser vont-ils dans le mauvais sens par rapport au reste de leur échelle ?
# Corrélation négative avec les autres items = données brutes (à inverser) ; positive = déjà inversés.
detect_reversal <- function(df) {
  rows <- lapply(REVERSED, function(v) {
    scale <- names(SCALES)[vapply(SCALES, function(s) v %in% s, TRUE)][1]
    others <- setdiff(intersect(SCALES[[scale]], names(df)), REVERSED)
    if (!v %in% names(df) || !length(others)) return(NULL)
    r <- suppressWarnings(stats::cor(df[[v]], rowMeans(df[others], na.rm = TRUE), use = "pairwise.complete.obs"))
    data.frame(item = v, scale = scale, r = round(r, 2), reverse = !is.na(r) && r < 0)
  })
  do.call(rbind, rows)
}

# Items à inverser selon le réglage : "auto" = diagnostic, TRUE/FALSE = tous, vecteur nommé = item par item
items_to_reverse <- function(df, setting) {
  present <- intersect(REVERSED, names(df))
  if (identical(setting, "auto")) {
    d <- detect_reversal(df)
    return(if (is.null(d)) present else intersect(present, d$item[d$reverse]))
  }
  if (length(setting) == 1 && is.logical(setting)) return(if (isTRUE(setting)) present else character(0))
  setting <- unlist(setting)
  intersect(present, names(setting)[as.logical(setting)])
}

# Contrôles de cohérence sur les données préparées : renvoie des messages d'alerte
direction_checks <- function(df) {
  out <- character(0)
  if (all(c("war_attitude", "peace_attitude") %in% names(df))) {
    r <- suppressWarnings(stats::cor(df$war_attitude, df$peace_attitude, use = "pairwise.complete.obs"))
    if (!is.na(r) && r > 0.1)
      out <- c(out, sprintf("Les scores « attitude favorable à la guerre » et « à la paix » vont dans le même sens (r = %.2f), alors qu'ils devraient s'opposer. Vérifiez le sens de codage des items APWS « war ».", r))
  }
  if (all(c("war_attitude", "wtf_score") %in% names(df))) {
    r <- suppressWarnings(stats::cor(df$war_attitude, df$wtf_score, use = "pairwise.complete.obs"))
    if (!is.na(r) && r < -0.1)
      out <- c(out, sprintf("L'« attitude favorable à la guerre » est liée négativement au Will to Fight (r = %.2f) : les items APWS « war » semblent codés dans le sens « anti-guerre ».", r))
  }
  for (s in names(SCALES)[lengths(SCALES) > 2]) {
    items <- intersect(SCALES[[s]], names(df))
    if (length(items) < 3) next
    rr <- vapply(items, function(i) suppressWarnings(stats::cor(df[[i]], rowMeans(df[setdiff(items, i)], na.rm = TRUE),
                                                                   use = "pairwise.complete.obs")), 0)
    bad <- names(rr)[!is.na(rr) & rr < 0]
    if (length(bad)) out <- c(out, sprintf("Échelle « %s » : %s va à l'encontre des autres items (à inverser ?).",
                                         label_of(s), paste(label_of(bad), collapse = ", ")))
  }
  out
}

# Regroupe les catégories rares
lump <- function(x, min_n, other = "Autres") {
  t <- table(x)
  x[!is.na(x) & x %in% names(t)[t < min_n]] <- other
  x
}

# ---------------------------------------------------------------------------
# Nettoyage + variables construites
# ---------------------------------------------------------------------------
strip_accents <- function(x) tolower(iconv(x, to = "ASCII//TRANSLIT"))

code_conflict <- function(text, keywords = DEFAULT_KEYWORDS) {
  t <- strip_accents(ifelse(is.na(text), "", text))
  out <- rep("other", length(t))
  for (cat in rev(names(keywords))) {   # rev : la première catégorie de la liste gagne
    kw <- strip_accents(keywords[[cat]])
    kw <- kw[nchar(trimws(kw)) > 0]
    if (!length(kw)) next
    hit <- Reduce(`|`, lapply(kw, function(k) grepl(trimws(k), t, fixed = TRUE)))
    out[hit] <- cat
  }
  out[trimws(t) %in% c("", "?", "nan", "na") | grepl("sais pas|aucun|idee|^non$", t)] <- "none"
  out
}

prepare_data <- function(df, settings, verbose = TRUE) {
  log <- character(0)
  # Répondants trop incomplets (abandons en cours de questionnaire, refus de consentement)
  qvars <- intersect(CODEBOOK$var[!CODEBOOK$type %in% c("text", "coded")], names(df))
  if (length(qvars) && !is.null(settings$max_missing)) {
    miss <- rowMeans(is.na(df[qvars]))
    keep <- miss <= settings$max_missing
    log <- c(log, sprintf("Réponses trop incomplètes (> %d %% de questions sans réponse) : %d répondant(s) exclu(s).",
                          round(100 * settings$max_missing), sum(!keep)))
    df <- df[keep, , drop = FALSE]
  }
  if (settings$drop_attention && "attn_check" %in% names(df)) {
    ok <- !is.na(df$attn_check) & df$attn_check == settings$attention_expected
    log <- c(log, sprintf("Item d'attention : %d répondant(s) exclu(s) sur %d.", sum(!ok), nrow(df)))
    df <- df[ok, , drop = FALSE]
  }
  rev <- items_to_reverse(df, settings$reverse_items)
  for (v in rev) df[[v]] <- 8 - df[[v]]
  if (length(rev)) log <- c(log, sprintf("Items inversés : %s.", paste(rev, collapse = ", ")))
  if (settings$reverse_status && "subj_status" %in% names(df)) df$subj_status <- 11 - df$subj_status

  src <- settings$conflict_source %||% "auto"
  if ("conflict_coded" %in% names(df) && src != "keywords") {
    df$conflict_cat <- lump(ifelse(is.na(df$conflict_coded), "Sans réponse", df$conflict_coded), settings$min_category %||% 30)
  } else if ("conflict_text" %in% names(df)) {
    df$conflict_cat <- code_conflict(df$conflict_text, settings$keywords)
  }
  if ("conflict_year" %in% names(df)) {
    h <- df$conflict_year - settings$survey_year
    h[!is.na(h) & (h < 0 | h > 500)] <- NA
    df$conflict_horizon_log <- log1p(h)
  }
  for (s in names(SCALES)) {
    items <- intersect(SCALES[[s]], names(df))
    if (length(items)) {
      m <- rowMeans(df[items], na.rm = TRUE)
      enough <- rowMeans(!is.na(df[items])) >= (settings$min_scale_items %||% 0)
      df[[s]] <- replace(m, is.nan(m) | !enough, NA)
    }
  }
  if (all(c("prob_be_assertive", "prob_be_indulgent") %in% names(df)))
    df$deterrence_belief_be <- df$prob_be_indulgent - df$prob_be_assertive
  if (all(c("prob_eu_assertive", "prob_eu_indulgent") %in% names(df)))
    df$deterrence_belief_eu <- df$prob_eu_indulgent - df$prob_eu_assertive
  rownames(df) <- NULL
  log <- c(log, sprintf("%d répondants retenus pour l'analyse.", nrow(df)))
  if (verbose) cat(log, sep = "\n")
  attr(df, "log") <- log
  df
}

cronbach_alpha <- function(x) {
  x <- stats::na.omit(x)
  k <- ncol(x)
  if (k < 2 || nrow(x) < 3) return(NA)
  k / (k - 1) * (1 - sum(apply(x, 2, stats::var)) / stats::var(rowSums(x)))
}

reliability_table <- function(df) {
  s <- names(SCALES)[lengths(SCALES) > 1]
  s <- s[vapply(s, function(v) sum(SCALES[[v]] %in% names(df)) > 1, TRUE)]
  a <- vapply(s, function(v) cronbach_alpha(df[intersect(SCALES[[v]], names(df))]), 0)
  data.frame(echelle = unname(label_of(s)), variable = s,
             n_items = vapply(s, function(v) sum(SCALES[[v]] %in% names(df)), 0L),
             alpha = round(a, 2),
             lecture = cut(a, c(-Inf, .6, .7, .8, Inf), c("faible", "acceptable", "bonne", "très bonne")),
             row.names = NULL)
}

# ---------------------------------------------------------------------------
# Sauvegarde / chargement d'une configuration (correspondance + réglages)
# ---------------------------------------------------------------------------
save_config <- function(path, mapping, settings) {
  jsonlite::write_json(list(mapping = as.list(mapping[!is.na(mapping)]), settings = settings),
                       path, auto_unbox = TRUE, pretty = TRUE)
}

load_config <- function(path) {
  cfg <- jsonlite::read_json(path, simplifyVector = TRUE)
  s <- utils::modifyList(default_settings(), cfg$settings)
  s$keywords <- lapply(s$keywords, as.character)
  if (is.list(s$reverse_items)) s$reverse_items <- unlist(s$reverse_items)
  m <- setNames(rep(NA_character_, nrow(CODEBOOK)), CODEBOOK$var)
  m[names(cfg$mapping)] <- unlist(cfg$mapping)
  list(mapping = m, settings = s)
}
