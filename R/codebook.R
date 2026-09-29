# Codebook du questionnaire "Study 1 - survey FR" (guerre & paix, UCLouvain / U. Lorraine).
#
# Une ligne par variable, dans l'ordre du questionnaire. Les textes (`stem` + `item`) reprennent ceux
# du questionnaire : ils servent à reconnaître automatiquement les colonnes d'un fichier SPSS / Qualtrics
# grâce aux libellés de variables, et à afficher des descriptions lisibles dans l'interface.
#
# type : cat (catégorielle), num, year, text, likert7 (1-7), ios5 (cercles A-E -> 1-5),
#        lr010 (0-10), status110 (1-10)

.cb <- list()
.q <- function(block, var, type, item, stem = "", short = item) {
  .cb[[length(.cb) + 1]] <<- data.frame(var = var, block = block, type = type, stem = stem,
                                        item = item, short = short)
}

# --- Démographie -----------------------------------------------------------
B <- "Démographie"
.q(B, "gender", "cat", "Quel est votre genre ?", short = "Genre (1 M, 2 F, 3 autre)")
.q(B, "age", "num", "Quel est votre âge ?", short = "Âge")
.q(B, "nationality_be", "cat", "Quelle est votre nationalité ? Belge", short = "Nationalité")
.q(B, "migration_bg", "cat", "Êtes-vous, ou au moins l'un de vos parents, né à l'étranger ?",
   short = "Moi ou un parent né à l'étranger")
.q(B, "mother_tongue", "cat", "Quelle est votre langue maternelle ?", short = "Langue maternelle")
.q(B, "education", "cat", "Quel est votre niveau d'étude (diplôme) le plus élevé ?", short = "Niveau d'étude")
.q(B, "left_right", "lr010", "En politique, les termes gauche et droite sont souvent utilisés. Situer vos opinions",
   short = "Positionnement gauche (0) - droite (10)")
.q(B, "subj_status", "status110",
   "Notre société est composée de différents groupes plus ou moins bien lotis. Échelle sommet (les mieux lotis) bas",
   short = "Statut subjectif (échelle sociale)")
.q(B, "family_military", "cat", "Est-ce qu'un ou plusieurs membres de votre famille proche sont militaires ?",
   short = "Famille proche militaire")
.q(B, "family_victim", "cat",
   "Est-ce que vous ou l'un des membres de votre famille proche a été victime de violence liée à une guerre ou un conflit armé ?",
   short = "Famille victime de violence de guerre")

# --- Identification --------------------------------------------------------
B <- "Identification"
S <- "Sentiment d'identification à différents groupes"
.q(B, "id_eu", "likert7", "Je m'identifie fortement aux Européens.", S)
.q(B, "id_be", "likert7", "Je m'identifie fortement aux Belges.", S)
S <- "Laquelle des configurations de cercles représente le mieux votre relation avec"
.q(B, "fusion_be", "ios5", "Moi et les Belges", S, "Fusion (cercles) avec les Belges")
.q(B, "fusion_eu", "ios5", "Moi et les Européens", S, "Fusion (cercles) avec les Européens")

# --- Futur -----------------------------------------------------------------
B <- "Futur"
.q(B, "conflict_text", "text",
   "Si vous devez penser à un conflit armé qui impliquera la Belgique dans le futur, quel est le premier qui vous vient à l'esprit ? Conflit futur",
   short = "Conflit imaginé (texte libre)")
.q(B, "conflict_coded", "coded", "Conflit futur codé (catégories attribuées manuellement)",
   short = "Conflit imaginé (codage manuel)")
.q(B, "conflict_year", "year", "Si vous deviez estimer quand ce conflit surviendrait, quand cela serait-il ? année",
   short = "Année d'éruption imaginée")
S <- "S'il devait survenir, à quel point les conséquences de ce conflit menaceraient"
.q(B, "fut_threat_be", "likert7", "L'existence de la Belgique dans son ensemble", S,
   "Conflit menacerait l'existence de la Belgique")
.q(B, "fut_threat_self", "likert7", "Votre existence et celle de vos proches", S,
   "Conflit menacerait mon existence et celle de mes proches")
.q(B, "fut_threat_eu", "likert7", "L'existence de l'Union européenne dans son ensemble", S,
   "Conflit menacerait l'existence de l'UE")
S <- "À quel point la survenue de ce conflit est-elle probable"
.q(B, "prob_be_statusquo", "likert7", "Si la Belgique ne change rien à ce qu'elle fait actuellement", S,
   "Probabilité si la Belgique ne change rien")
.q(B, "prob_be_assertive", "likert7",
   "Si la Belgique devient plus assertive au niveau international (augmentation des budgets de défense, dissuasion, sanctions)", S,
   "Probabilité si la Belgique devient plus assertive")
.q(B, "prob_be_indulgent", "likert7",
   "Si la Belgique devient plus indulgente au niveau international (négociations, diminution des sanctions, diminution des dépenses de défense)", S,
   "Probabilité si la Belgique devient plus indulgente")
.q(B, "prob_eu_statusquo", "likert7", "Si l'Union européenne ne change rien à ce qu'elle fait actuellement", S,
   "Probabilité si l'UE ne change rien")
.q(B, "prob_eu_indulgent", "likert7",
   "Si l'Union européenne devient plus indulgente au niveau international (négociations, diminution des sanctions, diminution des dépenses de défense)", S,
   "Probabilité si l'UE devient plus indulgente")
.q(B, "prob_eu_assertive", "likert7",
   "Si l'Union européenne devient plus assertive au niveau international (augmentation des budgets de défense, dissuasion, sanctions)", S,
   "Probabilité si l'UE devient plus assertive")

# --- Comportements hypothétiques (cibles tâche 1) ---------------------------
B <- "Comportements si le conflit éclate"
S <- "Imaginons maintenant que ce conflit s'est déclenché et implique la Belgique. Prêt à effectuer"
.q(B, "hyp_civil_protection", "likert7",
   "Rejoindre la protection civile afin de venir en aide aux populations affectées par le conflit (secourisme, lutte contre les incendies, distribution de nourriture)", S,
   "Rejoindre la protection civile")
.q(B, "hyp_war_effort", "likert7", "Participer à l'effort de guerre à la hauteur de vos compétences.", S,
   "Participer à l'effort de guerre")
.q(B, "hyp_fight", "likert7", "Vous battre pour votre pays.", S, "Se battre pour son pays")
.q(B, "hyp_army_noncombat", "likert7",
   "Vous engagez dans l'armée pour y être formé et rejoindre une unité non-combattante (soutien, logistique, bureautique).", S,
   "Armée, unité non combattante")
.q(B, "hyp_army_combat", "likert7", "Vous engagez dans l'armée pour y être formé et rejoindre une unité combattante.", S,
   "Armée, unité combattante")

# --- Confiance ---------------------------------------------------------------
B <- "Confiance"
S <- "Dans quelle mesure accordez-vous votre confiance"
.q(B, "trust_gov_be", "likert7", "aux personnes gouvernant la Belgique", S, "Confiance : gouvernants belges")
.q(B, "trust_gov_eu", "likert7", "aux personnes gouvernant l'Union européenne", S, "Confiance : gouvernants de l'UE")
.q(B, "trust_belgians", "likert7", "aux Belges dans leur ensemble", S, "Confiance : les Belges")
.q(B, "trust_europeans", "likert7", "aux Européens dans leur ensemble", S, "Confiance : les Européens")

# --- Menace russe -------------------------------------------------------------
B <- "Menace russe"
S <- "À quel point la Russie représente-t-elle une menace pour"
.q(B, "ru_sec_eu", "likert7", "La sécurité de l'Union européenne", S, "Menace russe : sécurité de l'UE")
.q(B, "ru_val_be", "likert7", "Les valeurs de la Belgique", S, "Menace russe : valeurs de la Belgique")
.q(B, "ru_sec_be", "likert7", "La sécurité de la Belgique", S, "Menace russe : sécurité de la Belgique")
.q(B, "ru_val_eu", "likert7", "Les valeurs de l'Union européenne", S, "Menace russe : valeurs de l'UE")

# --- APWS : attitudes envers la paix et la guerre ------------------------------
B <- "Attitudes paix / guerre (APWS)"
S <- ""
.q(B, "apws_1", "likert7", "La paix fait ressortir les meilleures qualités d'une société.", S)
.q(B, "apws_2", "likert7", "Bien que la guerre soit terrible, elle a une certaine utilité.", S)
.q(B, "apws_3", "likert7", "Sous certaines conditions, la guerre est nécessaire pour maintenir la justice.", S)
.q(B, "apws_4", "likert7", "La guerre est parfois la meilleure manière de résoudre un conflit.", S)
.q(B, "apws_5", "likert7", "Il n'y a pas de justification concevable à la guerre.", S)
.q(B, "apws_6", "likert7", "En général, je ne suis pas trop concerné par la paix dans le monde.", S)
.q(B, "apws_7", "likert7", "La première priorité de notre pays devrait être la paix dans le monde.", S)
.q(B, "apws_8", "likert7", "Nous devons consacrer toute notre énergie à assurer la paix dans le monde entier.", S)
.q(B, "apws_9", "likert7", "La guerre est une lutte futile résultant en l'autodestruction.", S)
.q(B, "apws_10", "likert7", "Je crois que la paix est extrêmement importante.", S)

# --- Sacrifice, information --------------------------------------------------
B <- "Sacrifice & information"
.q(B, "sacr_eu", "likert7", "Je donnerais ma vie si cela sauvait l'Union européenne.")
.q(B, "sacr_be", "likert7", "Je donnerais ma vie si cela sauvait la Belgique.")
.q(B, "info_intl", "likert7",
   "Avez-vous l'impression d'être bien informé sur les questions de politique internationale, de sécurité et de défense ?",
   short = "Bien informé : politique internationale, sécurité, défense")
.q(B, "info_ukraine", "likert7", "Avez-vous l'impression d'être bien informé sur la guerre en Ukraine ?",
   short = "Bien informé : guerre en Ukraine")

# --- Narratifs pro-russes ----------------------------------------------------
B <- "Narratifs sur la guerre en Ukraine"
S <- "Affirmations concernant le conflit en Ukraine"
.q(B, "narr_1", "likert7", "Les pays occidentaux ont envoyé des armes en Ukraine pour provoquer le conflit.", S)
.q(B, "narr_2", "likert7", "Le gouvernement ukrainien est une marionnette des pays occidentaux.", S)
.q(B, "narr_3", "likert7",
   "Imposer des sanctions à la Russie ne servira à rien, car elles ont déjà été appliquées et n'ont rien fait.", S)
.q(B, "narr_4", "likert7", "Beaucoup d'Ukrainiens sont des néo-nazis.", S)
.q(B, "attn_check", "likert7", "Après avoir lu cet énoncé, cochez la case 3.", S, "Item d'attention (« cochez la case 3 »)")
.q(B, "narr_5", "likert7", "L'expansion de l'OTAN dans les anciens pays du bloc de l'Est a mené à ce conflit.", S)
.q(B, "narr_6", "likert7", "C'est l'Ukraine qui a provoqué le conflit en rompant la trêve avec les séparatistes.", S)
.q(B, "narr_7", "likert7", "Imposer des sanctions à la Russie fera plus de mal à l'Occident qu'à la Russie.", S)
.q(B, "narr_8", "likert7", "L'Ukraine est un instrument anti-russe entre les mains de l'Occident.", S)
.q(B, "narr_9", "likert7", "La Russie essaie juste de protéger ses frontières de l'OTAN.", S)
.q(B, "narr_10", "likert7",
   "Imposer des sanctions à la Russie ne fera qu'entraver le dialogue et les solutions diplomatiques.", S)
.q(B, "narr_11", "likert7",
   "Les actions de la Russie visent à protéger les vies des Russes ethniques en Ukraine d'un génocide.", S)
.q(B, "narr_12", "likert7", "Le manque de volonté réelle de l'Occident à dialoguer a conduit à ce conflit.", S)

# --- Appeasement -------------------------------------------------------------
B <- "Appeasement"
S <- "Dans ses rapports avec le monde"
.q(B, "app_be_avoid", "likert7",
   "la Belgique devrait tout faire pour éviter d'entrer en guerre, quitte à faire des concessions majeures.", S,
   "Belgique : éviter la guerre quitte à faire des concessions")
.q(B, "app_eu_intransig", "likert7",
   "l'Union Européenne devrait être intransigeante, quitte à provoquer des tensions majeures.", S,
   "UE : être intransigeante quitte à provoquer des tensions")
.q(B, "app_eu_avoid", "likert7",
   "l'Union européenne devrait tout faire pour éviter d'entrer en guerre, quitte à faire des concessions majeures.", S,
   "UE : éviter la guerre quitte à faire des concessions")
.q(B, "app_be_intransig", "likert7",
   "la Belgique devrait être intransigeante, quitte à provoquer des tensions majeures.", S,
   "Belgique : être intransigeante quitte à provoquer des tensions")

# --- Will to Fight (cibles tâche 2) -------------------------------------------
B <- "Will to Fight"
S <- "Bien sûr, nous espérons tous qu'il n'y aura pas de nouvelle guerre, mais si cela devait arriver, seriez-vous prêt à vous battre"
.q(B, "wtf_be", "likert7", "pour la Belgique ?", S, "Prêt à se battre pour la Belgique")
.q(B, "wtf_russia", "likert7", "contre la Russie ?", S, "Prêt à se battre contre la Russie")
.q(B, "wtf_eu", "likert7", "pour l'Union Européenne ?", S, "Prêt à se battre pour l'UE")

# --- Intentions de comportement (cibles tâche 2) -------------------------------
B <- "Intentions : situation actuelle"
S <- "Dans la situation actuelle, à quel point vous sentez-vous prêt à mettre en oeuvre les actions suivantes"
.q(B, "now_fund_ukraine", "likert7", "Financer des équipements (exemple drones, matériel de déminage) à destination de l'Ukraine.", S,
   "Financer des équipements pour l'Ukraine")
.q(B, "now_social_cuts", "likert7",
   "Consentir à une diminution des dépenses sociales, pour financer une augmentation des budgets de défense.", S,
   "Accepter moins de dépenses sociales pour la défense")
.q(B, "now_legion", "likert7",
   "Vous engager dans des unités de volontaires (par exemple, la Légion internationale ukrainienne) combattant contre les Russes ?", S,
   "S'engager dans la Légion internationale ukrainienne")
.q(B, "now_reserve", "likert7",
   "Vous engager dans la réserve des forces armées de votre pays, même si cela implique de renoncer à des jours de congé.", S,
   "S'engager dans la réserve")
.q(B, "now_humanitarian", "likert7", "Effectuer des missions de volontariat humanitaire en Ukraine.", S,
   "Volontariat humanitaire en Ukraine")
.q(B, "now_refugees", "likert7", "Accueillir des réfugiés Ukrainiens chez vous.", S, "Accueillir des réfugiés ukrainiens")

B <- "Intentions : si guerre en Belgique"
S <- "Si une nouvelle guerre devait survenir et directement impliquer la Belgique, à quel point vous sentez-vous prêt"
.q(B, "war_army_combat", "likert7",
   "Vous engager dans les forces armées belges, afin d'y recevoir une formation militaire et de combattre.", S,
   "Forces armées belges, pour combattre")
.q(B, "war_civil_protection", "likert7",
   "Vous engager dans la protection civile (par exemple, secouristes, aide à la population civile).", S,
   "Protection civile")
.q(B, "war_leave_family", "likert7", "Quitter votre famille, vos amis et votre ville selon les besoins de la Belgique.", S,
   "Quitter famille, amis et ville")
.q(B, "war_army_noncombat", "likert7",
   "Vous engager dans les forces armées belges, pour y exercer des fonctions non-combattantes (logistique, bureautique, construction, etc.)", S,
   "Forces armées belges, fonctions non combattantes")
.q(B, "war_resistance", "likert7",
   "Le cas échéant, participer à la résistance armée en territoire occupé (sabotage, renseignement, guérilla).", S,
   "Résistance armée en territoire occupé")
.q(B, "war_skills", "likert7", "Mettre à disposition votre temps, et vos compétences pour la défense de la Belgique.", S,
   "Mettre temps et compétences au service de la défense")

CODEBOOK <- do.call(rbind, .cb)
rm(.cb, .q, B, S)

# ---------------------------------------------------------------------------
# Noms et libellés du fichier SPSS nettoyé de Loïc (Dataset .sav) : reconnus avec certitude.
# Les APWS y sont numérotés par sous-échelle (peace 1-5, war 1-5) et non dans l'ordre du questionnaire ;
# l'attribution item par item ci-dessous est donc approximative, mais l'appartenance à l'échelle est exacte.
# ---------------------------------------------------------------------------
ALIASES <- c(
  gender = "Gender", age = "Age", nationality_be = "Nationality", migration_bg = "Migration",
  mother_tongue = "Language", education = "Diploma", left_right = "Ideology", subj_status = "SSE",
  family_military = "Family_army", family_victim = "Family_war",
  id_eu = "ID_EU", id_be = "ID_BE", fusion_be = "Fusion_BE", fusion_eu = "Fusion_EU",
  conflict_text = "Future_war", conflict_coded = "Future_confllict_coded", conflict_year = "Future_date",
  fut_threat_be = "Threat_BE", fut_threat_self = "Threat_me", fut_threat_eu = "Threat_EU",
  prob_be_statusquo = "Proba_BE_neutral", prob_be_assertive = "Proba_BE_assertive", prob_be_indulgent = "Proba_BE_indugling",
  prob_eu_statusquo = "Proba_EU_neutral", prob_eu_indulgent = "Proba_EU_indulging", prob_eu_assertive = "Proba_EU_assertive",
  hyp_fight = "Future_WTF_1", hyp_war_effort = "Future_WTF_2", hyp_army_combat = "Future_WTF_3",
  hyp_army_noncombat = "Future_WTF_4", hyp_civil_protection = "Future_WTF_5",
  trust_gov_be = "Trust_gov_BE", trust_gov_eu = "Trust_gove_EU", trust_belgians = "Trust_civi_BE", trust_europeans = "Trust_civi_EU",
  ru_sec_eu = "Russia_EU_security", ru_val_be = "Russia_BE_values", ru_sec_be = "Russia_BE_security", ru_val_eu = "Russia_EU_values",
  apws_1 = "APWS_1", apws_7 = "APWS_2", apws_8 = "APWS_3", apws_10 = "APWS_4", apws_6 = "APWS_5",
  apws_2 = "APWS_6", apws_5 = "APWS_7", apws_3 = "APWS_8", apws_9 = "APWS_9", apws_4 = "APWS_10",
  sacr_be = "Sacrifice_BE", sacr_eu = "Sacrifice_EU", info_intl = "Info_general", info_ukraine = "Info_Ukraine",
  setNames(paste0("Russia_nar_", 1:12), paste0("narr_", 1:12)),
  attn_check = "Attention_check_1",
  app_be_avoid = "Dove_BE", app_eu_avoid = "Dove_EU", app_be_intransig = "Hawk_BE", app_eu_intransig = "Hawk_EU",
  wtf_be = "WTF_origi_BE", wtf_eu = "WTF_origi_EU", wtf_russia = "WTF_vs_Russia",
  now_social_cuts = "Military_spending", now_reserve = "Military_reserve", now_refugees = "Ukraine_refugee",
  now_fund_ukraine = "Ukraine_weapons", now_humanitarian = "Ukraine_humanitarian", now_legion = "Ukraine_fight",
  war_army_combat = "War_training_fight", war_army_noncombat = "War_non_fight", war_civil_protection = "War_civilprotection",
  war_skills = "War_personal_effort", war_leave_family = "War_leave_family", war_resistance = "War_resistance"
)

# Libellés courts en anglais (style du fichier de Loïc), ajoutés au texte de reconnaissance automatique
# pour retrouver les colonnes même si leurs noms changent un peu.
EN_LABELS <- c(
  gender = "Gender", age = "Age", nationality_be = "Nationality", migration_bg = "Migration background",
  mother_tongue = "Language mother tongue", education = "Diploma education", left_right = "Political ideology left right",
  subj_status = "Socio economic status subjective", family_military = "Family member in the military",
  family_victim = "Family victim of violence war", id_eu = "Identification EU", id_be = "Identification Belgium",
  fusion_be = "Fusion Belgians", fusion_eu = "Fusion Europeans", conflict_text = "Future conflict involving Belgium",
  conflict_coded = "Future conflict coded", conflict_year = "Time probable eruption of the conflict date",
  fut_threat_be = "Threat for Belgium", fut_threat_self = "Threat for me", fut_threat_eu = "Threat for the EU",
  prob_be_statusquo = "Probability conflict Belgium neutral", prob_be_assertive = "Probability conflict Belgium assertive",
  prob_be_indulgent = "Probability conflict Belgium indulging", prob_eu_statusquo = "Probability conflict EU neutral",
  prob_eu_indulgent = "Probability conflict EU indulging", prob_eu_assertive = "Probability conflict EU assertive",
  hyp_fight = "Conflict future WTF classic", hyp_war_effort = "Conflict future WTF competences",
  hyp_army_combat = "Conflict future WTF training fight", hyp_army_noncombat = "Conflict future WTF non fight army",
  hyp_civil_protection = "Conflict future WTF civil protection",
  trust_gov_be = "Trust Belgium politicians", trust_gov_eu = "Trust EU politicians",
  trust_belgians = "Trust Belgians", trust_europeans = "Trust Europeans",
  ru_sec_eu = "Russia threat to EU security", ru_val_be = "Russia threat to Belgium values",
  ru_sec_be = "Russia threat to Belgium security", ru_val_eu = "Russia threat to EU values",
  sacr_eu = "Sacrifice for EU", sacr_be = "Sacrifice for Belgium",
  info_intl = "Information security international", info_ukraine = "Information Ukrainian war",
  attn_check = "Attention check",
  app_be_avoid = "Dove BE", app_eu_avoid = "Dove EU", app_be_intransig = "Hawk BE", app_eu_intransig = "Hawk EU",
  wtf_be = "WTF for Belgium", wtf_eu = "WTF for EU", wtf_russia = "WTF vs Russia",
  now_social_cuts = "Less welfare more military spendings", now_reserve = "Army reserve", now_refugees = "Ukrainian refugees",
  now_fund_ukraine = "Finance weapons for Ukraine", now_humanitarian = "Voluntary missions in Ukraine",
  now_legion = "Joining voluntary fighters in Ukraine",
  war_army_combat = "Belgium war join army to fight", war_army_noncombat = "Belgium war join army to non-fight",
  war_civil_protection = "Belgium war civil protection", war_skills = "Belgium war time and competences",
  war_leave_family = "Belgium war leave family and friends", war_resistance = "Belgium war armed resistance"
)

vars_of <- function(block) CODEBOOK$var[CODEBOOK$block == block]
TYPE_RANGE <- list(likert7 = c(1, 7), ios5 = c(1, 5), lr010 = c(0, 10), status110 = c(1, 10),
                   num = c(14, 100), year = c(1900, 2600))

# ---------------------------------------------------------------------------
# Échelles et items inversés
# ---------------------------------------------------------------------------
HYPOTHETICAL <- vars_of("Comportements si le conflit éclate")
WTF <- vars_of("Will to Fight")
BEH_NOW <- vars_of("Intentions : situation actuelle")
BEH_WAR <- vars_of("Intentions : si guerre en Belgique")
NARRATIVES <- paste0("narr_", 1:12)
APPEASEMENT <- vars_of("Appeasement")

SCALES <- list(
  id_score = c("id_eu", "id_be"),
  fusion_score = c("fusion_be", "fusion_eu"),
  fut_threat_score = c("fut_threat_be", "fut_threat_self", "fut_threat_eu"),
  trust_gov = c("trust_gov_be", "trust_gov_eu"),
  trust_people = c("trust_belgians", "trust_europeans"),
  threat_russia = vars_of("Menace russe"),
  war_attitude = c("apws_2", "apws_3", "apws_4", "apws_5", "apws_9"),
  peace_attitude = c("apws_1", "apws_6", "apws_7", "apws_8", "apws_10"),
  sacrifice = c("sacr_eu", "sacr_be"),
  subj_info = c("info_intl", "info_ukraine"),
  pro_russia_narr = NARRATIVES,
  appeasement = APPEASEMENT,
  hyp_score = HYPOTHETICAL,
  wtf_score = WTF,
  beh_now_score = BEH_NOW,
  beh_war_score = BEH_WAR
)
# Items formulés à l'envers de leur échelle (inversés 1<->7 si les données sont brutes)
REVERSED <- c("apws_5", "apws_9", "apws_6", "app_eu_intransig", "app_be_intransig")

SCALE_LABELS <- c(
  id_score = "Identification (BE + UE)", fusion_score = "Fusion identitaire (cercles)",
  fut_threat_score = "Menace perçue du conflit futur", trust_gov = "Confiance dans les gouvernants",
  trust_people = "Confiance dans les Belges / Européens", threat_russia = "Menace russe perçue",
  war_attitude = "Attitude favorable à la guerre (APWS)", peace_attitude = "Attitude favorable à la paix (APWS)",
  sacrifice = "Disposition au sacrifice", subj_info = "Sentiment d'être informé",
  pro_russia_narr = "Adhésion aux narratifs pro-russes", appeasement = "Appeasement (éviter la guerre à tout prix)",
  hyp_score = "Comportements hypothétiques (moyenne)", wtf_score = "Will to Fight (score composite)",
  beh_now_score = "Intentions actuelles (moyenne)", beh_war_score = "Intentions si guerre (moyenne)",
  conflict_cat = "Conflit imaginé (catégorie)", conflict_horizon_log = "Horizon du conflit (log années)",
  deterrence_belief_be = "Croyance en la dissuasion (BE)", deterrence_belief_eu = "Croyance en la dissuasion (UE)"
)

label_of <- function(v) {
  out <- ifelse(v %in% names(SCALE_LABELS), SCALE_LABELS[v], CODEBOOK$short[match(v, CODEBOOK$var)])
  ifelse(is.na(out), v, out)
}

CATEGORICAL <- c(CODEBOOK$var[CODEBOOK$type == "cat"], "conflict_cat")

# ---------------------------------------------------------------------------
# Codage du conflit imaginé par mots-clés (sans accents). Ordre = priorité.
# Modifiable dans l'interface.
# ---------------------------------------------------------------------------
DEFAULT_KEYWORDS <- list(
  russia = c("russ", "poutine", "putin", "ukrain", "kremlin", "baltique", "moldav"),
  ww3_nato = c("mondiale", "ww3", "otan", "nato", "nucleaire"),
  china = c("chin", "taiwan", "pekin"),
  usa = c("usa", "etats-unis", "etats unis", "amerique", "trump", "groenland"),
  middle_east = c("iran", "israel", "gaza", "moyen-orient", "palestin", "syrie"),
  internal = c("civile", "flandre", "separat", "emeute", "terror", "islam")
)

# ---------------------------------------------------------------------------
# Tâches : cibles et jeux de prédicteurs
# ---------------------------------------------------------------------------
DEMOGRAPHICS <- vars_of("Démographie")
IDENTIFICATION <- vars_of("Identification")
FUTURE_FEATURES <- c("fut_threat_be", "fut_threat_self", "fut_threat_eu",
                     grep("^prob_", CODEBOOK$var, value = TRUE),
                     "conflict_cat", "conflict_horizon_log", "deterrence_belief_be", "deterrence_belief_eu")
T2_SCALE_FEATURES <- c(DEMOGRAPHICS, "id_score", "fusion_score", "fut_threat_score", "conflict_cat",
                       "conflict_horizon_log", "deterrence_belief_be", "deterrence_belief_eu",
                       "trust_gov", "trust_people", "threat_russia", "war_attitude", "peace_attitude",
                       "subj_info", "pro_russia_narr", "appeasement")
PROXIMAL <- c("hyp_score", "sacrifice")   # quasi identiques à la VD : seulement dans "+proximal"
T2_ITEM_FEATURES <- c(DEMOGRAPHICS, IDENTIFICATION, FUTURE_FEATURES, vars_of("Confiance"), vars_of("Menace russe"),
                      paste0("apws_", 1:10), "info_intl", "info_ukraine", NARRATIVES, APPEASEMENT)

TASKS <- list(
  task1 = list(
    label = "Tâche 1 : comportements si le conflit imaginé éclate",
    targets = c("hyp_score", HYPOTHETICAL),
    feature_sets = list(
      "futur_seul" = FUTURE_FEATURES,
      "futur+demo+identite" = c(DEMOGRAPHICS, IDENTIFICATION, FUTURE_FEATURES)
    )
  ),
  task2 = list(
    label = "Tâche 2 : Will to Fight et intentions de comportement",
    targets = c("wtf_score", WTF, "beh_now_score", "beh_war_score", BEH_NOW, BEH_WAR),
    feature_sets = list(
      "scores" = T2_SCALE_FEATURES,
      "scores+proximal" = c(T2_SCALE_FEATURES, PROXIMAL),
      "items" = T2_ITEM_FEATURES
    )
  )
)
FEATURE_SET_LABELS <- c(
  "futur_seul" = "Bloc « Futur » seul",
  "futur+demo+identite" = "Futur + démographie + identification",
  "scores" = "Scores par échelle (sans variables proximales)",
  "scores+proximal" = "Scores + variables proximales (sacrifice, comportements hypothétiques)",
  "items" = "Items bruts (un prédicteur par question)"
)
