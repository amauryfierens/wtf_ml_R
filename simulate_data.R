# Génère un faux jeu de données avec la même structure que le questionnaire,
# pour développer et tester le pipeline sans toucher aux données réelles.
#
# Les réponses sont tirées de quelques facteurs latents (militarisme, pacifisme,
# identification, confiance, adhésion aux narratifs pro-russes, menace perçue),
# de sorte que les modèles aient un vrai signal à retrouver.
#
#   Rscript simulate_data.R            # -> 400 répondants
#   Rscript simulate_data.R 1000       # -> 1000 répondants
#
# Produit deux fichiers :
#   data/simulated.csv                 données propres, noms canoniques
#   data/export_qualtrics_probable.sav imitation d'un export brut Qualtrics -> SPSS : colonnes techniques,
#                                      consentement, noms Qxx_k, libellés de questions, champs « Autre (précisez) »,
#                                      âge et année en texte libre, abandons en cours de route, codes -99,
#                                      échelle 0-10 codée 1-11, items non inversés.

source("R/codebook.R")
SURVEY_YEAR <- 2026
FUTURE_THREAT <- c("fut_threat_be", "fut_threat_self", "fut_threat_eu")
FUTURE_PROB <- grep("^prob_", CODEBOOK$var, value = TRUE)
TRUST <- vars_of("Confiance")
THREAT_RUSSIA <- vars_of("Menace russe")
APWS <- paste0("apws_", 1:10)
APWS_WAR <- SCALES$war_attitude
APWS_REVERSED <- c("apws_5", "apws_9", "apws_6")
APPEASEMENT_REVERSED <- c("app_eu_intransig", "app_be_intransig")

likert <- function(latent, k = 7, noise = 1) {
  x <- latent + rnorm(length(latent), 0, noise)
  as.integer(pmin(pmax(round((k + 1) / 2 + 1.3 * x), 1), k))
}

simulate <- function(n = 400, seed = 0) {
  set.seed(seed)
  z <- function() rnorm(n)

  gender <- sample(1:3, n, TRUE, c(.45, .52, .03))
  male <- as.numeric(gender == 1)
  left_right <- as.integer(pmin(pmax(round(5 + 2 * z()), 0), 10))
  right <- (left_right - 5) / 2

  militarism <- 0.4 * male + 0.3 * right + z()
  pacifism <- -0.4 * militarism + 0.8 * z()
  ident <- 0.2 * right + z()
  trust <- 0.5 * ident + z()
  pro_russia <- -0.4 * trust + 0.2 * right + z()
  threat <- -0.5 * pro_russia + 0.3 * trust + z()

  d <- data.frame(
    gender = gender,
    age = round(pmin(pmax(rgamma(n, 4, 1 / 9) + 18, 18), 85)),
    nationality_be = sample(c(1, 0), n, TRUE, c(.9, .1)),
    migration_bg = sample(1:2, n, TRUE, c(.3, .7)),
    mother_tongue = sample(1:5, n, TRUE, c(.75, .08, .02, .1, .05)),
    education = sample(1:6, n, TRUE, c(.01, .05, .2, .25, .47, .02)),
    left_right = left_right,
    subj_status = as.integer(pmin(pmax(round(4.5 + 1.6 * z()), 1), 10)),
    family_military = sample(1:3, n, TRUE, c(.12, .83, .05)),
    family_victim = sample(1:3, n, TRUE, c(.15, .75, .10)),
    id_eu = likert(0.8 * ident - 0.2 * right),
    id_be = likert(ident),
    fusion_be = likert(ident, k = 5),
    fusion_eu = likert(0.7 * ident, k = 5)
  )

  labels <- c("Russie", "guerre avec la Russie", "Poutine envahit les pays baltes", "3e guerre mondiale",
              "conflit OTAN", "Chine / Taïwan", "Iran", "guerre civile", "USA Groenland", "je ne sais pas", "")
  d$conflict_text <- sample(labels, n, TRUE, c(.3, .12, .05, .12, .06, .08, .05, .05, .04, .08, .05))
  horizon <- pmin(pmax(rlnorm(n, log(8), 0.8) - 0.8 * threat, 0), 80)
  d$conflict_year <- round(SURVEY_YEAR + horizon)

  for (i in seq_along(FUTURE_THREAT)) d[[FUTURE_THREAT[i]]] <- likert(c(1, .8, 1)[i] * threat)
  for (v in FUTURE_PROB) {
    w <- if (grepl("indulgent", v)) -0.3 else 0.3
    d[[v]] <- likert(0.5 * threat + w * militarism)
  }

  readiness <- 0.7 * militarism + 0.4 * ident + 0.3 * threat - 0.3 * pro_russia + 0.6 * z()
  helping <- 0.5 * readiness - 0.3 * male + 0.8 * z()
  d$hyp_civil_protection <- likert(helping + 0.5)
  d$hyp_war_effort <- likert(0.7 * readiness + 0.3 * helping)
  d$hyp_fight <- likert(readiness, noise = 0.7)
  d$hyp_army_noncombat <- likert(0.6 * readiness + 0.3 * helping - 0.3)
  d$hyp_army_combat <- likert(readiness - 0.6, noise = 0.7)

  for (v in TRUST) d[[v]] <- likert(trust)
  for (v in THREAT_RUSSIA) d[[v]] <- likert(threat + 0.5, noise = 0.8)
  for (v in APWS) {
    lat <- if (v %in% APWS_WAR) militarism else pacifism + 1
    d[[v]] <- likert(if (v %in% APWS_REVERSED) -lat else lat)
  }

  d$sacr_eu <- likert(0.6 * readiness + 0.4 * ident - 1.2)
  d$sacr_be <- likert(0.8 * readiness + 0.4 * ident - 1.0)
  d$info_intl <- likert(0.3 * male + 0.5 * z())
  d$info_ukraine <- likert(0.3 * male + 0.5 * z())
  for (v in NARRATIVES) d[[v]] <- likert(pro_russia - 0.8)
  d$attn_check <- ifelse(runif(n) < 0.93, 3L, sample(1:7, n, TRUE))

  appease <- pacifism - 0.4 * militarism
  for (v in APPEASEMENT) d[[v]] <- likert(if (v %in% APPEASEMENT_REVERSED) -appease else appease)

  wtf <- 0.9 * readiness + 0.2 * trust + 0.5 * z()
  d$wtf_be <- likert(wtf, noise = 0.6)
  d$wtf_russia <- likert(wtf + 0.4 * threat - 0.3 * pro_russia, noise = 0.6)
  d$wtf_eu <- likert(wtf - 0.2, noise = 0.6)

  support_ukr <- 0.5 * threat - 0.6 * pro_russia + 0.2 * trust + 0.6 * z()
  d$now_fund_ukraine <- likert(support_ukr)
  d$now_social_cuts <- likert(0.5 * militarism + 0.3 * right - 0.8)
  d$now_legion <- likert(0.6 * readiness + 0.3 * support_ukr - 2.0)
  d$now_reserve <- likert(0.7 * readiness - 1.3)
  d$now_humanitarian <- likert(0.5 * support_ukr + 0.3 * helping - 0.8)
  d$now_refugees <- likert(0.6 * support_ukr + 0.3 * pacifism - 0.5)

  d$war_army_combat <- likert(wtf - 0.5, noise = 0.7)
  d$war_civil_protection <- likert(helping + 0.4)
  d$war_leave_family <- likert(0.7 * wtf - 0.6)
  d$war_army_noncombat <- likert(0.6 * wtf + 0.3 * helping - 0.2)
  d$war_resistance <- likert(0.8 * wtf - 0.3)
  d$war_skills <- likert(0.6 * wtf + 0.4 * helping + 0.3)
  d$feedback_text <- ""

  # quelques trous, comme dans la vraie vie
  for (v in c("age", "left_right", "conflict_year", "subj_status")) d[[v]][runif(n) < 0.03] <- NA
  d
}

# ---------------------------------------------------------------------------
# Réalisme : distributions plausibles pour un échantillon belge francophone
# ---------------------------------------------------------------------------
# Moyennes visées (échelle brute, avant inversion), ordres de grandeur inspirés des enquêtes
# européennes récentes : Will to Fight bas, adhésion forte à la paix, narratifs pro-russes minoritaires.
REALISTIC_MEANS <- c(
  id_eu = 4.6, id_be = 5.0, fusion_be = 2.8, fusion_eu = 2.3,
  fut_threat_be = 4.4, fut_threat_self = 5.0, fut_threat_eu = 4.2,
  prob_be_statusquo = 3.9, prob_be_assertive = 4.3, prob_be_indulgent = 3.5,
  prob_eu_statusquo = 4.0, prob_eu_indulgent = 3.6, prob_eu_assertive = 4.4,
  hyp_civil_protection = 4.9, hyp_war_effort = 4.2, hyp_fight = 3.1, hyp_army_noncombat = 3.0, hyp_army_combat = 2.2,
  trust_gov_be = 3.3, trust_gov_eu = 3.5, trust_belgians = 4.3, trust_europeans = 4.4,
  ru_sec_eu = 5.4, ru_val_be = 4.5, ru_sec_be = 5.0, ru_val_eu = 4.8,
  apws_1 = 5.8, apws_2 = 3.3, apws_3 = 3.9, apws_4 = 2.3, apws_5 = 3.9,
  apws_6 = 2.1, apws_7 = 5.3, apws_8 = 5.1, apws_9 = 4.9, apws_10 = 6.4,
  sacr_eu = 1.9, sacr_be = 2.4, info_intl = 3.8, info_ukraine = 4.3,
  narr_1 = 2.2, narr_2 = 2.3, narr_3 = 3.2, narr_4 = 1.8, narr_5 = 3.5, narr_6 = 2.0,
  narr_7 = 3.0, narr_8 = 2.4, narr_9 = 2.6, narr_10 = 2.9, narr_11 = 1.9, narr_12 = 2.8,
  app_be_avoid = 4.4, app_eu_intransig = 3.3, app_eu_avoid = 4.1, app_be_intransig = 3.0,
  wtf_be = 3.2, wtf_russia = 2.8, wtf_eu = 2.7,
  now_fund_ukraine = 3.3, now_social_cuts = 2.5, now_legion = 1.4, now_reserve = 1.9,
  now_humanitarian = 2.2, now_refugees = 2.7,
  war_army_combat = 2.4, war_civil_protection = 4.6, war_leave_family = 2.9, war_army_noncombat = 2.9,
  war_resistance = 2.8, war_skills = 4.2
)

# Recale un item sur une distribution cible (normale discrétisée de moyenne ~m sur 1..k) en conservant
# l'ordre des répondants : les corrélations entre items sont préservées.
reshape_item <- function(x, m, k = 7, sd = 1.5) {
  ok <- !is.na(x)
  mu <- stats::uniroot(function(u) {
    p <- diff(stats::pnorm(c(-Inf, seq(1.5, k - 0.5), Inf), u, sd)); sum(p * seq_len(k)) - m
  }, c(-5, k + 5))$root
  p <- diff(stats::pnorm(c(-Inf, seq(1.5, k - 0.5), Inf), mu, sd))
  r <- rank(x[ok] + stats::runif(sum(ok), 0, 0.01), ties.method = "first") / sum(ok)
  x[ok] <- findInterval(r, cumsum(p), left.open = TRUE) + 1L
  pmin(x, k)
}

CONFLICT_ANSWERS <- c(
  "La Russie" = 60, "Russie" = 45, "russie" = 25, "Guerre avec la Russie" = 20, "Russie / OTAN" = 10,
  "Une invasion russe des pays baltes" = 8, "La Russie qui attaque un pays de l'OTAN" = 8, "poutine" = 5,
  "Conflit Russie-Ukraine qui s'étend" = 10, "Troisième guerre mondiale" = 15, "3ème guerre mondiale" = 10,
  "une guerre nucléaire" = 4, "Chine" = 12, "Chine - Taïwan" = 8, "Iran" = 5, "Israël / Iran" = 4,
  "Moyen-Orient" = 4, "Etats-Unis" = 4, "Les USA (Trump, Groenland)" = 3, "guerre civile" = 4,
  "Terrorisme" = 4, "Corée du Nord" = 3, "Serbie / Kosovo" = 2, "cyberguerre" = 3, "Biélorussie" = 2,
  "je ne sais pas" = 18, "aucune idée" = 8, "?" = 3, "Aucun" = 4
)

messy_year <- function(y) {
  s <- as.character(round(y))
  u <- stats::runif(length(y))
  s[u < 0.05] <- sprintf("dans %d ans", pmax(1, round(y[u < 0.05] - SURVEY_YEAR)))
  s[u >= 0.05 & u < 0.09] <- sprintf("%d-%d", round(y[u >= 0.05 & u < 0.09]), round(y[u >= 0.05 & u < 0.09]) + 5)
  s[u >= 0.09 & u < 0.12] <- sample(c("jamais", "je ne sais pas", "?", "bientôt"), sum(u >= 0.09 & u < 0.12), TRUE)
  s[is.na(y)] <- ""
  s
}

make_realistic <- function(d, seed = 1) {
  set.seed(seed)
  n <- nrow(d)
  for (v in intersect(names(REALISTIC_MEANS), names(d)))
    d[[v]] <- reshape_item(d[[v]], REALISTIC_MEANS[[v]], k = if (v %in% c("fusion_be", "fusion_eu")) 5 else 7)
  # Échantillon typique d'une étude en ligne universitaire : plus de femmes, beaucoup d'étudiants
  d$gender <- sample(1:3, n, TRUE, c(.38, .60, .02))
  d$age <- round(ifelse(stats::runif(n) < 0.5, stats::runif(n, 18, 26), stats::rgamma(n, 6, 1 / 7)))
  d$age <- pmin(pmax(d$age, 18), 84)
  d$conflict_text <- sample(names(CONFLICT_ANSWERS), n, TRUE, CONFLICT_ANSWERS)
  d$feedback_text <- ifelse(stats::runif(n) < 0.12, sample(c(
    "Questionnaire intéressant mais un peu angoissant.", "Merci, ça fait réfléchir.",
    "Certaines questions étaient difficiles à comprendre.", "Je n'avais jamais vraiment pensé à tout ça.",
    "Un peu long.", "Bonne chance pour votre recherche !"), n, TRUE), "")
  d
}

# ---------------------------------------------------------------------------
# Export façon Qualtrics -> SPSS : colonnes techniques, consentement, noms Qxx / Qxx_k, champs
# « Autre (précisez) », âge et année en texte libre, réponses incomplètes, codes 1-11 pour le 0-10.
# ---------------------------------------------------------------------------
to_qualtrics_sav <- function(d, path, seed = 2) {
  set.seed(seed)
  n <- nrow(d)
  cb <- CODEBOOK[CODEBOOK$var %in% names(d), ]
  grp <- paste(cb$block, cb$stem)
  q <- cumsum(!duplicated(grp) | cb$stem == "")
  qnum <- (q + 2) + cumsum(c(0, diff(q) > 0 & stats::runif(length(q) - 1) < 0.25) * sample(1:6, length(q), TRUE))
  multi <- ave(q, q, FUN = length) > 1
  sub <- ave(q, q, FUN = seq_along)
  qname <- ifelse(multi, sprintf("Q%d_%d", qnum, sub), sprintf("Q%d", qnum))
  qname[cb$var == "conflict_text"] <- "Q86_1"   # QID86 : identifiant réel, visible dans le texte redirigé du questionnaire
  qname[cb$var == "conflict_year"] <- "Q87"

  start <- as.POSIXct("2026-03-02 08:00", tz = "Europe/Brussels") + sort(stats::runif(n, 0, 60 * 86400))
  dur <- round(stats::rlnorm(n, log(14 * 60), 0.45))
  progress <- ifelse(stats::runif(n) < 0.08, sample(c(4, 12, 25, 41, 58, 73, 89), n, TRUE), 100)
  consent <- ifelse(stats::runif(n) < 0.015, 2, 1)
  progress[consent == 2] <- 2
  out <- data.frame(
    StartDate = start, EndDate = start + dur,
    Status = haven::labelled(rep(0, n), c("IP Address" = 0, "Survey Preview" = 1), "Response Type"),
    Progress = haven::labelled(progress, label = "Progress"),
    Duration__in_seconds_ = haven::labelled(dur, label = "Duration (in seconds)"),
    Finished = haven::labelled(as.numeric(progress == 100), c("False" = 0, "True" = 1), "Finished"),
    RecordedDate = start + dur + 5,
    ResponseId = haven::labelled(sprintf("R_%s", replicate(n, paste(sample(c(letters, LETTERS, 0:9), 15, TRUE), collapse = ""))),
                                 label = "Response ID"),
    DistributionChannel = haven::labelled(rep("anonymous", n), label = "Distribution Channel"),
    UserLanguage = haven::labelled(rep("FR", n), label = "User Language"),
    Q1 = haven::labelled(rep(1, n), c("Je déclare avoir été informé·e de cette recherche et donne mon consentement" = 1,
                                      "J'aimerais en savoir plus sur les conditions de participation" = 2),
                         "Cher participant, chère participante, Merci par avance pour votre participation"),
    Q2 = haven::labelled(consent, c("Je déclare avoir été informé·e de cette recherche et donne mon consentement libre et éclairé" = 1,
                                    "Je veux quitter cette étude" = 2), "Consentement éclairé Je déclare :"),
    check.names = FALSE
  )

  likert_lab <- c("Pas du tout d'accord" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5, "6" = 6, "Tout à fait d'accord" = 7)
  value_labels <- list(
    gender = c(Masculin = 1, "Féminin" = 2, Autre = 3),
    nationality_be = c(Belge = 1, "Autre (précisez)" = 2),
    migration_bg = c("Oui : moi ou (au moins) l'un de mes parents sommes nés à l'étranger" = 1,
                     "Non : moi et mes parents sommes nés en Belgique" = 2),
    mother_tongue = c("Français" = 1, "Néerlandais" = 2, Allemand = 3, "Multilinguisme (précisez)" = 4, "Autre (précisez)" = 5),
    education = c("Enseignement primaire" = 1, "Enseignement secondaire, incomplet" = 2,
                  "Enseignement secondaire, entièrement achevé" = 3, "Enseignement supérieur non-universitaire" = 4,
                  "Enseignement universitaire" = 5, "Autre (précisez)" = 6),
    left_right = setNames(1:11, c("Complètement à gauche 0", 1:4, "Centre 5", 6:9, "Complètement à droite 10")),
    subj_status = setNames(1:10, c("En haut 1", 2:9, "En bas 10")),
    family_military = c(Oui = 1, Non = 2, "Je ne sais pas" = 3),
    family_victim = c(Oui = 1, Non = 2, "Je ne sais pas" = 3),
    fusion_be = c(A = 1, B = 2, C = 3, D = 4, E = 5), fusion_eu = c(A = 1, B = 2, C = 3, D = 4, E = 5)
  )
  d$nationality_be <- ifelse(d$nationality_be == 1, 1, 2)
  d$left_right <- d$left_right + 1                          # Qualtrics code les choix 0..10 en 1..11
  d$age <- ifelse(stats::runif(n) < 0.03, paste(d$age, "ans"), as.character(d$age))
  d$conflict_year <- messy_year(d$conflict_year)

  text_extra <- list(
    nationality_be = list(code = 2, sub = "2_TEXT", values = c("Française", "Italienne", "Marocaine", "Française et belge", "Espagnole", "Roumaine", "Congolaise")),
    mother_tongue = list(code = 4, sub = "4_TEXT", values = c("Français et néerlandais", "français/arabe", "Français - Italien", "FR/EN")),
    education = list(code = 6, sub = "6_TEXT", values = c("Bachelier en cours", "Master en cours", "Formation professionnelle"))
  )
  for (i in seq_len(nrow(cb))) {
    v <- cb$var[i]
    lab <- if (cb$stem[i] != "") paste(cb$stem[i], "-", cb$item[i]) else cb$item[i]
    x <- d[[v]]
    if (v %in% names(text_extra)) lab <- paste(lab, "- Selected Choice")
    out[[qname[i]]] <- if (is.character(x)) haven::labelled(x, label = lab)
                       else haven::labelled(as.numeric(x), labels = value_labels[[v]] %||% if (cb$type[i] == "likert7") likert_lab, label = lab)
    if (v %in% names(text_extra)) {
      te <- text_extra[[v]]
      txt <- ifelse(x == te$code, sample(te$values, n, TRUE), "")
      out[[paste0(qname[i], "_", te$sub)]] <- haven::labelled(txt, label = paste(cb$item[i], "- Autre (précisez) - Text"))
    }
  }
  out$Q_ressenti <- haven::labelled(d$feedback_text, label = "(Question facultative) Comment vous sentez-vous après avoir répondu à ce questionnaire ?")

  # Abandons en cours de route : tout ce qui suit le point d'arrêt est vide
  qcols <- names(out)[grep("^Q", names(out))][-(1:2)]
  for (r in which(progress < 100)) {
    keep <- floor(length(qcols) * max(0, progress[r] - 2) / 100)
    for (c in qcols[setdiff(seq_along(qcols), seq_len(keep))]) {
      out[[c]][r] <- if (is.character(out[[c]])) "" else NA
    }
  }
  # Quelques codes manquants SPSS explicites
  out[[qname[cb$var == "subj_status"]]][sample(which(progress == 100), 4)] <- -99
  haven::write_sav(out, path)
  invisible(out)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  n <- if (length(args)) as.integer(args[1]) else 400
  dir.create("data", showWarnings = FALSE)
  d <- make_realistic(simulate(n))
  write.csv(d, "data/simulated.csv", row.names = FALSE)
  to_qualtrics_sav(d, "data/export_qualtrics_probable.sav")
  cat(n, "répondants simulés -> data/simulated.csv (propre) et data/export_qualtrics_probable.sav (façon export Qualtrics)\n")
}
