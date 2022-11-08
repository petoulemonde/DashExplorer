packages <- c("librarian")
installed_packages <- packages %in% row.names(installed.packages())
if( any( installed_packages == FALSE ) ) {
  install.packages(packages[!installed_packages])
}
rm(packages, installed_packages)

librarian::shelf("callr",
                 "pins",
                 quiet= TRUE)

# ---- Functions
init_board <- function(base, chemin = getwd() ) {
  
  # Vérifier si dossier existe : dir.exists(normalizePath(R.home()))
  
  board_dataexplorer <- board_folder( paste(chemin, "/board_dataexplorer", sep = "") )
  pin_write(board_dataexplorer, 
            x = iris,
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
    library(shiny);
    install.packages("shinydashboard", repo = "https://cran.rstudio.com/", dep = TRUE);
    library(shinydashboard);
    install.packages("tidyverse", repo = "https://cran.rstudio.com/", dep = TRUE);
    library(tidyverse);
    install.packages("visdat", repo = "https://cran.rstudio.com/", dep = TRUE);
    library(visdat);
    install.packages("plotly", repo = "https://cran.rstudio.com/", dep = TRUE);
    library(plotly);
    install.packages("VIM", repo = "https://cran.rstudio.com/", dep = TRUE);
    library(VIM);
    install.packages("corrplot", repo = "https://cran.rstudio.com/", dep = TRUE);
    library(corrplot);
    install.packages("DT", repo = "https://cran.rstudio.com/", dep = TRUE);
    library(DT);
    install.packages("GGally", repo = "https://cran.rstudio.com/", dep = TRUE);
    library(GGally);
    install.packages("VIM", repo = "https://cran.rstudio.com/", dep = TRUE);
    library(VIM);
    install.packages("FactoMineR", repo = "https://cran.rstudio.com/", dep = TRUE);
    library(FactoMineR);
    install.packages("gtsummary", repo = "https://cran.rstudio.com/", dep = TRUE);
    library(gtsummary);
    install.packages("pins", repo = "https://cran.rstudio.com/", dep = TRUE);
    library(pins);
    
    print("\n\n Install Packages OK \n \n");
    # runExample("01_hello", # Fonctionnel
    # launch.browser = function(appUrl) {
    #             url <- paste0(appUrl, "/?param=", chemin )
    #             browseURL(url) } ) ;
    
    runApp( paste0(chemin, "/shinyapp.R"),
    # runApp( paste0(chemin, "/POC_url_shiny.R"),
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

close_board <- function(process, chemin) {
  print("Beginning of closing request")
  process$kill()
  print("Shiny process killed")
  unlink(paste0(chemin, "/board_dataexplorer/"), recursive = TRUE, force = TRUE)
  print("Temporary database deleted")
}

update_board <- function(base, chemin) {
  board_dataexplorer <- board_folder( paste(chemin, "/board_dataexplorer", sep = "") )
  
  pin_write(board_dataexplorer, 
            x = base,
            # x = mtcars,
            name = "database",
            title = "Database for dataexplorer dashboard")
}

acces <- "C:/Users/Pierre-Etienne_local/Documents/codes/dataexplorer_R"
data(iris)

rs <- init_board(iris, acces)
close_board(rs, acces)
