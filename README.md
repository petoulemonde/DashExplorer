### Présentation de l'outil
Cet outil Shiny a pour but de vous aider dans l'exploration de vos bases de données. Ceclui-ci se compose de plusieurs onglets, décrivant différents aspects de votre base de données.

### Utilisation
2 choix :
- cloner le repo dans votre dossier de travail. Les commandes depuis votre script sont :
```r
source("dataexplorer_Rproject/shinyapp.R") # Chargement de la fonction

data_discovery() # Découvrir l'outil sur des bases existantes
data_discovery(<votre_base>) # Utiliser l'outil avec votre base de donnée
```

- Télécharger le fichier shinyapp.R dans votre dossier de travail. Les commandes à lancer depuis votre script sont :
```r
source("shinyapp.R")# Chargement de la fonction

data_discovery() # Découvrir l'outil sur des bases existantes
data_discovery(<votre_base>) # Utiliser l'outil avec votre base de donnée
```

### Mot de la fin
Enjoy !
