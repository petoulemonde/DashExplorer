# ---- Pour obsidian
librarian::shelf("job", 
                 "callr", 
                 "processx",
                 "shiny",
                 quiet = TRUE)

job(base_job = {
  for (i in 1:100) {
    data(mtcars)
    moy = mean(mtcars$gear)
    
    print(paste("Tour ", i, sep = ""))
  }
  
  # export(value = "all")
  export(value = c(mtcars, moy))# Renvoie de l'ensemble des éléments du job
  #c(mtcars, moy) : choisir quoi renvoyer
})

print("Console is still available, and job run.")
# It's possible to follow nw job in onglet Background Jobs

# ---------- dataexplorer functions files -----------

# ---- Install packages
packages <- c("librarian")

installed_packages <- packages %in% row.names(installed.packages())
if( any( installed_packages == FALSE ) ) {
  install.packages(packages[!installed_packages])
}
rm(packages, installed_packages)

librarian::shelf("callr",
                 "pins",
                 quiet= TRUE)

# ---- upload function
init_board <- function(base, chemin = getwd() ) {
  
  chemin = paste0(getwd(), "/codes/dataexplorer_R")
  board_dataexplorer <- board_folder( paste(chemin, "/board_dataexplorer/", sep = "") )
  pin_write(board_dataexplorer, 
            x = base,
            # x = mtcars,
            name = "database",
            title = "Database for dataexplorer dashboard")
  
  rs <- callr::r_session$new()
  rs$poll_process(0)
  rs
  rs$call(function(chemin) { 
    # appUrl <- "http://0.0.0.0:1212";
    print("Let's go app\n");
    print(paste0(chemin, "\n\n"));
    
    install.packages("shiny", repo = "https://cran.rstudio.com/", dep = TRUE);
    library(shiny, quiet = TRUE);
    install.packages("pins", repo = "https://cran.rstudio.com/", dep = TRUE);
    library(pins, quiet = TRUE);
    
    print("\n\n Install Packages OK \n \n");
    # runExample("01_hello", # FOnctionnel
    # launch.browser = function(appUrl) {
    #             url <- paste0(appUrl, "/?param=", chemin )
    #             browseURL(url) } ) ;
    
    runApp( paste0(chemin, "/POC_url_shiny.R"),
            launch.browser = function(appUrl) {
              url <- paste0(appUrl, "/?param=", chemin )
              browseURL(url)
              # invisible(.Call("rs_shinyviewer", url, chemin, "browser", NULL, PACKAGE = "(embedding)"))
              # invisible(utils::browseURL(url)) # alternative
            } )
  } , 
  args = list(chemin = paste0(getwd(), "/codes/dataexplorer_R") ) ) 
  return(rs)
}

close_board <- function(process, chemin = getwd() ) {
  process$kill()
  
  chemin = paste0(getwd(), "/codes/dataexplorer_R")
  unlink(paste(chemin, "/board_dataexplorer/", sep = ""), recursive = TRUE, force = TRUE)
}

upload_board <- function(base) {
  chemin = paste0(getwd(), "/codes/dataexplorer_R")
  board_dataexplorer <- board_folder( paste(chemin, "/board_dataexplorer/", sep = "") )
  
  pin_write(board_dataexplorer, 
            x = base,
            # x = mtcars,
            name = "database",
            title = "Database for dataexplorer dashboard")
}

