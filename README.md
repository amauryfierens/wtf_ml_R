# Will to Fight : exploration par machine learning

Outil pour prédire, à partir du questionnaire « Guerre & Paix » :

- **Tâche 1** : les 5 comportements du bloc « *Imaginons maintenant que ce conflit s'est déclenché…* »,
  à partir du bloc « Futur » (et, en option, de la démographie et de l'identification) ;
- **Tâche 2** : le **Will to Fight** et les **intentions de comportement**, à partir du reste du questionnaire.

> **Première utilisation ?** Suivez le guide pas à pas **[INSTALLATION.md](INSTALLATION.md)** :
> installer R et RStudio, puis lancer l'outil, sans connaissance préalable.

## Lancer l'interface

**RStudio** : double-cliquer sur `wtf_ml_R.Rproj`, ouvrir `lancer_interface.R`, puis cliquer sur **Source**.

**Terminal** (dans ce dossier) :

```
Rscript lancer_interface.R
```

L'interface s'ouvre dans le navigateur. Au premier lancement, les packages manquants sont installés
automatiquement (quelques minutes). Pour arrêter : fermer la console R ou taper Ctrl+C.

## Utilisation

L'interface est un parcours guidé : **Accueil → 1. Importer → 2. Vérifier → 3. Analyser → 4. Résultats**.
La barre du haut indique l'étape en cours ; les boutons « Suivant » / « Précédent » en bas de page font avancer.
Pour une première prise en main, cliquez sur **Démo guidée** (en haut à droite) : elle parcourt les 4 étapes
sur un faux jeu de données, avec une bulle d'explication à chaque étape.

1. **Importer** : choisir le fichier **.sav** (export SPSS de Qualtrics, brut ou nettoyé) ou **.csv**.
   Les questions sont reconnues automatiquement grâce aux libellés SPSS.
2. **Vérifier** : une liste de contrôle résume l'état des données. Seules les sections marquées ⚠️ ou ❌
   demandent une action ; elles s'ouvrent automatiquement.
   - *Correspondance des questions* : les questions incertaines ou introuvables sont listées en premier.
   - *Valeurs* : variables hors de l'échelle attendue (ex. autre chose que 1 à 7).
   - *Codage* : items formulés à l'envers (un diagnostic dit s'ils sont bruts ou déjà inversés), statut subjectif,
     échelle gauche-droite exportée en 1-11, codes de valeurs manquantes, item d'attention.
   - *Données incomplètes* : seuils d'exclusion des répondants et de calcul des scores.
   - *Conflit imaginé*, *fiabilité des échelles*, *enregistrer les réglages* : facultatif.
   Les réglages sont enregistrés automatiquement (dossier `configurations/`) et rechargés au prochain import
   du même fichier.
3. **Analyser** : choisir ce qu'on veut prédire, à partir de quelles informations et avec quelle précision,
   puis **Lancer l'analyse**. Un récapitulatif et une durée estimée s'affichent à droite. Les options avancées
   (modèles, mode oui/non, test de contrôle) sont facultatives.

   | Précision | Validation externe | Réglage interne | Usage |
   |---|---|---|---|
   | Rapide | 5 plis × 1 | 3 plis, 4 réglages testés par modèle | essais |
   | Standard | 5 plis × 3 | 5 plis, 10 réglages testés | exploration |
   | Approfondi | 5 plis × 5 | 5 plis, 25 réglages testés | résultats à publier |

4. **Résultats** : l'encadré **« Ce qu'il faut retenir »** résume en français la variable choisie (qualité de
   la prédiction, linéaire ou non, variables les plus utiles et sens de leur effet). Les onglets donnent les
   détails : variables importantes, comparaison des modèles, stabilité pli par pli, tableaux, téléchargements.
   Tout est aussi enregistré dans `resultats/<date_heure>/`.

## Le fichier SPSS de Loïc

Les noms de variables du fichier nettoyé (`ID_BE`, `Future_WTF_1`, `WTF_origi_BE`, `Dove_BE`…) sont connus de l'outil
(table `ALIASES` dans `R/codebook.R`) : les 85 variables sont reconnues automatiquement. Particularités prises en compte :

- **Conflit imaginé** : le codage manuel (`Future_confllict_coded`, 31 catégories) est utilisé de préférence aux
  mots-clés ; les catégories de moins de 30 répondants (réglable) sont regroupées dans « Autres ».
- **Items inversés** : décision **item par item**, d'après la corrélation de chaque item avec le reste de son échelle.
  Dans le fichier, les APWS sont déjà inversés mais pas les items Hawk (appeasement « intransigeance ») : l'outil
  n'inverse que ces derniers.
- **Idéologie** codée 1-11 : ramenée à 0-10.
- **APWS** : le fichier les numérote par sous-échelle (peace 1-5, war 1-5), pas dans l'ordre du questionnaire.
  L'appartenance à l'échelle est exacte, mais le texte affiché pour chaque item est une supposition :
  à confirmer avec l'ordre réel des items.
- **Contrôles de cohérence** : l'outil signale une échelle dont les valeurs sont « compressées » (par ex. 4 à 7 seulement,
  signe d'un recodage incomplet), ou des échelles guerre et paix qui vont dans le même sens. Le jeu de données généré
  déclenche ces deux alertes. Si c'est aussi le cas avec les vraies données, il faut vérifier le recodage dans SPSS.
- **Taille** : avec environ 5 500 répondants, compter environ 1 min par combinaison cible × prédicteurs en mode Rapide,
  environ 10 min en Standard et environ 40 min en Approfondi sur un ordinateur à 12 cœurs. Plus lent avec moins de cœurs.

## Méthode

**Modèles comparés** : référence (prédit la moyenne), régression linéaire, régression régularisée ElasticNet
(logistique en mode oui/non), forêt aléatoire (`ranger`), gradient boosting (`xgboost`).

**Validation croisée imbriquée** (*nested cross-validation*) :

1. **Boucle externe** : les répondants sont répartis en 5 plis ; chaque pli sert une fois de test. C'est elle
   qui donne la performance affichée, toujours mesurée sur des répondants que le modèle n'a jamais vus.
   Elle est répétée avec des découpages différents pour stabiliser l'estimation.
2. **Boucle interne** : dans chaque échantillon d'entraînement externe, une seconde validation croisée choisit
   les hyperparamètres de chaque modèle. Le pli de test externe n'intervient jamais dans ce choix.
3. **Prétraitement sans fuite** : imputation des valeurs manquantes (médiane + indicateur de manquant),
   standardisation et codage des variables catégorielles sont appris sur les seules données d'entraînement
   de chaque pli, interne comme externe.

**Hyperparamètres réglés** :

| Modèle | Réglages | Méthode de recherche |
|---|---|---|
| ElasticNet | mélange ridge / lasso (α ∈ {0 ; 0,25 ; 0,5 ; 0,75 ; 1}), force de pénalité λ | grille sur α × chemin complet de λ (60 valeurs) |
| Forêt aléatoire | part de variables testées par nœud, taille minimale des feuilles, fraction d'échantillon | recherche aléatoire |
| Gradient boosting | profondeur, taux d'apprentissage, sous-échantillonnage, régularisation, **nombre d'arbres** | recherche aléatoire + arrêt précoce pour le nombre d'arbres |

La recherche aléatoire teste N combinaisons tirées au hasard (Bergstra & Bengio, 2012) ; le réglage par défaut
fait toujours partie des candidats. Les réglages retenus dans chaque pli sont consultables : s'ils varient
beaucoup d'un pli à l'autre, le modèle est instable.

**Données incomplètes** :

- Les répondants dont plus de 50 % des questions sont sans réponse (abandons, refus de consentement) sont
  exclus (seuil réglable).
- Un score d'échelle n'est calculé que si au moins 50 % de ses items sont répondus (réglable).
- Un répondant sans réponse à la variable prédite est écarté pour cette variable uniquement.
- Les manques restants dans les prédicteurs sont remplacés par la médiane, avec un indicateur « était manquant »
  (le modèle peut ainsi apprendre si le fait de ne pas répondre est informatif). Cette imputation est apprise
  dans chaque pli d'entraînement : pas de fuite. Elle reste simple : si beaucoup de données manquent, comparer
  les résultats en variant le seuil d'exclusion (analyse de sensibilité).

**Interprétation** : l'importance par permutation mesure la baisse de performance sur les plis de test
externes quand on mélange les réponses à une question. Le sens de l'effet vient des coefficients d'un
ElasticNet final, réglé par validation croisée sur toutes les données.

**Contrôle** : l'option « mélanger la cible » (Options avancées) détruit tout lien réel ; la performance doit
alors tomber au niveau du hasard (R² ≈ 0, AUC ≈ 0,5). Si ce n'est pas le cas, il y a un problème.

## Points à garder en tête

- **Données simulées** : générées avec des liens volontairement injectés. Les bons scores obtenus dessus
  montrent seulement que l'outil retrouve un signal connu. Sur les vraies données, un R² hors échantillon
  de 0,2 à 0,4 est déjà un bon résultat en psychologie sociale.
- **Petit échantillon** : avec quelques centaines de répondants, la régression régularisée fait souvent aussi
  bien que les forêts ou le boosting. C'est un résultat en soi : les relations sont alors surtout linéaires
  et additives.
- **Comparer beaucoup de modèles ou de cibles** sur les mêmes données rend le meilleur score un peu optimiste :
  choisir les comparaisons à rapporter avant de regarder les résultats, ou le signaler.
- **Variables proximales (tâche 2)** : « se battre pour son pays » (bloc hypothétique) et « je donnerais ma
  vie pour la Belgique » mesurent presque la même chose que le WtF. Le jeu « Scores » les exclut ; « Scores +
  proximales » les inclut. La différence montre ce qu'elles ajoutent.
- **Ordre du questionnaire** : le bloc hypothétique vient avant la confiance, la menace russe, etc.
  « Prédire » veut donc dire ici « associer », pas « causer ».

## Organisation des fichiers

| Fichier | Contenu |
|---|---|
| `lancer_interface.R` | lance l'interface (installe les packages manquants) |
| `app.R` | interface Shiny |
| `R/codebook.R` | questions du questionnaire, échelles, items inversés, tâches et jeux de prédicteurs |
| `R/data.R` | import, correspondance automatique, contrôles, construction des scores |
| `R/engine.R` | modèles, validation croisée imbriquée, réglage des hyperparamètres |
| `R/plots.R` | figures |
| `R/interpret.R` | résumé automatique des résultats en français |
| `run_models.R` | même analyse en ligne de commande, sans interface |
| `simulate_data.R` | génère les données simulées : `data/simulated.csv` (propre) et `data/export_qualtrics_probable.sav` (imitation d'un export brut Qualtrics → SPSS, utilisé par le mode démo) |

Ligne de commande, par exemple pour tout relancer à l'identique avec une configuration enregistrée :

```
Rscript run_models.R --data mes_donnees.sav --config configurations/mes_donnees.json --task task2 --all --intensity approfondi
```
