### Présentation de l'outil
Cet outil Shiny a pour but de vous aider dans l'exploration de vos bases de données. Celui-ci se compose de plusieurs onglets, décrivant différents aspects de votre base de données.

Attention : Ce package souffre pour l'instant de très faible fuite de mémoire. Celles-ci sont corrigées à chaque redémarrage de l'ordinateur.
Il est déconseillé d'utiliser ce package sur des serveurs ou des ordinateurs peu ou jamais redémarrés.

### Utilisation
2 choix :
- cloner le repo dans votre dossier de travail. 
- Télécharger le fichier shinyapp.R dans votre dossier de travail. 

Une fois les fichiers shinyapp.R et script_async.R sont ajouté à votre dossier de travail, les commandes sont : 
```r
source("script_async.R")
source("shinyapp.R") # Chargement des fichiers

rs <- init_board(<votre_base>, <chemin_vers_votre_dossier_de_travail>)

upload_dash(<votre_nouvelle_base>, <chemin_vers_votre_dossier_de_travail>)

close_board(rs, <chemin_vers_votre_dossier_de_travail>) # Fermeture du dashboard et suppression des fichiers temporaires
```

### Explications
Ce package lance un novueau processus R, et exécute au sein de ce nouveau processus un dashboard shiny.

### Mot de la fin
Enjoy !

-----------------------------------------------------------------------------------
## Project Following

Pour la suite : 
- Editer le contenu du bandeau du haut du dashboard
- couleur petit bandeau box : bleu si infos, orange si saisie : https://rstudio.github.io/shinydashboard/structure.html#bookmarking-and-restoring-selected-tabs
- ajouter tabBOx pour ML : https://rstudio.github.io/shinydashboard/structure.html#bookmarking-and-restoring-selected-tabs
- valueBox données manquantes : https://rstudio.github.io/shinydashboard/structure.html#bookmarking-and-restoring-selected-tabs

29-11-2022 : 
- Modification des icones des onglets
- Correction de quelques erreurs d'orthographe
- Modification du correlation plot

16-11-2022 : 
- Première version opérationnelle du dashboard
- tag v1

08-11-2022 : 
- Preuve de concept fonctionnel de lancement asynchrone, avec interactive database via pin_reactive
- Problème de prise en chrge de base réactive dans switch

07-11-2022 : 
- Début des tests de lancement du dashboard dans un nouveau processus

27-10-2022 : 
- Ajout du README.md
- Ajout de tableone

26-10-2022 : 
- Passage du projet en shiny
- Description jusqu'à l'ACP

25-10-2022 : Création du projet




