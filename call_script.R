source("shinyapp.R")

data("DNase")
head(DNase)

data_discovery(DNase) # APpel en passant sa propre base de données
# data_discovery() # APpel sans passe de bases de données
