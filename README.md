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
