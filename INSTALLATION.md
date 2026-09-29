# Installation pas à pas

Ce guide part de zéro : aucune connaissance en programmation n'est nécessaire.
Comptez environ **20 minutes** la première fois, dont la plupart à attendre des téléchargements.
Ensuite, lancer l'outil prend 2 clics.

Vous allez installer deux logiciels gratuits :

- **R** : le moteur qui fait les calculs. Vous ne l'ouvrirez jamais directement.
- **RStudio** : la fenêtre dans laquelle on travaille avec R.

---

## Étape 1 : installer R

1. Allez sur **https://cloud.r-project.org**.
2. Cliquez sur le lien qui correspond à votre ordinateur :
   - **Windows** : « Download R for Windows », puis « **base** », puis « **Download R-4.x.x for Windows** »
     (le numéro exact n'a pas d'importance, prenez la version proposée).
   - **Mac** : « Download R for macOS », puis choisissez le fichier `.pkg` qui correspond à votre processeur :
     - Mac récent (puce **Apple M1, M2, M3…**) : le fichier qui contient **arm64** ;
     - Mac plus ancien (processeur **Intel**) : le fichier qui contient **x86_64**.

     Pour savoir lequel vous avez : menu  (en haut à gauche) → « À propos de ce Mac » → ligne « Puce » ou « Processeur ».
3. Ouvrez le fichier téléchargé et suivez l'installation en gardant **toutes les options par défaut**
   (cliquez sur « Suivant » / « Continuer » jusqu'au bout).

Il faut **R 4.3 ou plus récent**. Si R est déjà installé sur votre ordinateur depuis longtemps, réinstallez-le.

## Étape 2 : installer RStudio

1. Allez sur **https://posit.co/download/rstudio-desktop/**.
2. Descendez jusqu'à « **2: Install RStudio** » et cliquez sur le gros bouton de téléchargement
   (le site détecte votre système automatiquement).
3. Installez-le avec les options par défaut.
   - **Mac** : faites glisser l'icône RStudio dans le dossier « Applications ».

## Étape 3 : récupérer le dossier de l'outil

Vous avez reçu le dossier **`wtf_ml_R`** (par exemple dans un fichier `.zip`).

1. Si c'est un `.zip`, **décompressez-le** : clic droit → « Extraire tout… » (Windows) ou double-clic (Mac).
   Ne travaillez pas directement dans le `.zip`.
2. Placez le dossier `wtf_ml_R` à un endroit simple, par exemple dans **Documents**.

Le dossier doit contenir, entre autres : `lancer_interface.R`, `app.R`, `wtf_ml_R.Rproj`, et un dossier `R`.

## Étape 4 : ouvrir le projet dans RStudio

1. Dans le dossier `wtf_ml_R`, **double-cliquez sur `wtf_ml_R.Rproj`** (l'icône est un cube bleu marqué « R »).
   RStudio s'ouvre directement dans le bon dossier.
   - Si Windows demande avec quel programme ouvrir le fichier, choisissez **RStudio**.
2. En bas à droite de RStudio, l'onglet **Files** affiche le contenu du dossier.
   Cliquez sur **`lancer_interface.R`** : le fichier s'ouvre en haut à gauche.

## Étape 5 : lancer l'outil

1. En haut à droite du fichier `lancer_interface.R`, cliquez sur le bouton **Source**
   (icône avec une petite flèche bleue).
2. **La première fois seulement**, R télécharge les compléments dont l'outil a besoin (« packages »).
   Cela prend **5 à 15 minutes**. Des lignes de texte (parfois en rouge) défilent en bas à gauche, dans la
   **Console** : c'est normal, ce ne sont pas des erreurs.
   - Si une question apparaît dans la Console, voir « Questions possibles » ci-dessous.
3. Votre **navigateur internet s'ouvre** sur la page de l'outil. C'est prêt !
   Commencez par le bouton **Démo guidée** pour une visite de 2 minutes.

Les fois suivantes : double-clic sur `wtf_ml_R.Rproj`, ouvrir `lancer_interface.R`, **Source**.
L'outil s'ouvre en quelques secondes.

### Questions possibles pendant la première installation

Répondez en tapant la réponse dans la **Console** (en bas à gauche), puis Entrée.

| Question affichée | Réponse |
|---|---|
| « Would you like to use a personal library instead? » | tapez **yes** |
| « Would you like to create a personal library … to install packages into? » | tapez **yes** |
| « Do you want to install from sources the packages which need compilation? » | tapez **no** |
| Une fenêtre demande de choisir un « CRAN mirror » (serveur de téléchargement) | choisissez **0-Cloud** ou **Belgium** |
| RStudio propose de mettre à jour des packages | cliquez sur **Oui** ou ignorez, les deux conviennent |

## Étape 6 : arrêter l'outil

- Dans RStudio, cliquez sur le **panneau rouge « Stop »** en haut de la Console, ou appuyez sur **Échap** dans la Console.
- Fermer l'onglet du navigateur ne suffit pas : l'outil continue de tourner tant que R n'est pas arrêté.

---

## Utiliser vos propres données

Pas besoin de copier vos données dans le dossier de l'outil : à l'étape « 1. Importer » de l'interface,
cliquez sur **Parcourir…** et choisissez votre fichier **.sav** (SPSS) ou **.csv**, où qu'il se trouve.

Les données **restent sur votre ordinateur** : rien n'est envoyé sur internet. L'outil ne se connecte à internet
que pour installer les packages la première fois.

Les résultats sont enregistrés dans le dossier `wtf_ml_R/resultats/`, un sous-dossier par analyse (date et heure).

## En cas de problème

| Problème | Solution |
|---|---|
| Rien ne se passe après **Source**, ou message « cannot open file 'app.R' » | RStudio n'est pas dans le bon dossier. Fermez RStudio et rouvrez-le en double-cliquant sur **`wtf_ml_R.Rproj`** (étape 4). |
| « there is no package called … » | L'installation des packages a été interrompue. Relancez **Source** : elle reprend là où elle s'était arrêtée. |
| Erreur pendant l'installation d'un package | Vérifiez la connexion internet, fermez RStudio, rouvrez-le et relancez **Source**. Si l'erreur persiste, copiez le message rouge de la Console et envoyez-le. |
| Le navigateur ne s'ouvre pas | Regardez dans la Console une ligne « Listening on http://127.0.0.1:XXXX » et copiez cette adresse dans votre navigateur. |
| Message « Warning » en rouge | Un avertissement n'est pas une erreur : si l'outil s'ouvre, tout va bien. |
| Le calcul est très long | Choisissez le mode **Rapide** à l'étape « 3. Analyser », ou moins de variables à prédire. La durée estimée s'affiche avant le lancement. Évitez de mettre l'ordinateur en veille pendant un calcul. |
| « R version … is required » ou l'interface s'affiche mal | R est trop ancien : réinstallez R (étape 1) puis relancez. |
| L'antivirus bloque quelque chose (Windows) | Autorisez R / RStudio : l'outil ouvre une page web locale sur votre propre ordinateur, rien d'autre. |

## Pour les utilisateurs avancés

- Sans RStudio, depuis un terminal ouvert dans le dossier : `Rscript lancer_interface.R`.
- Analyse sans interface, par exemple pour relancer à l'identique : voir `run_models.R` et la fin du `README.md`.
- **Linux** : installer R depuis les dépôts de la distribution. Les packages y sont compilés, ce qui demande quelques
  bibliothèques système (par exemple sous Ubuntu : `sudo apt install build-essential libcurl4-openssl-dev libssl-dev libxml2-dev`).
