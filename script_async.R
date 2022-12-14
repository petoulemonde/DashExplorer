# ---- Pour obsidian
librarian::shelf("job", 
                 "tidyverse",
                 "callr", 
                 "processx",
                 "shiny",
                 quiet = TRUE)


# It's possible to follow nw job in onglet Background Jobs

# autre : Sys.getenv()
# R.home()

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
                 quiet= FALSE)

# ---- upload function
init_board <- function(base, chemin = getwd() ) {
  
  print("Board creation ...")
  
  # chemin = paste0(getwd(), "/codes/dataexplorer_R")
  board_dataexplorer <- board_folder( paste(chemin, "/board_dataexplorer/", sep = "") )
  pin_write(board_dataexplorer, 
            x = base,
            # x = mtcars,
            name = "database",
            title = "Database for dataexplorer dashboard")
  
  print("Base writing")
  
  rs <- callr::r_session$new()
  rs$poll_process(0)
  rs
  rs$call(function(chemin) { 
    # appUrl <- "http://0.0.0.0:1212";
    print("Let's go app !");
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
    
    # runApp( paste0(chemin, "/POC_url_shiny.R"),
    runApp( paste0(chemin, "/shinyapp.R"),       
            launch.browser = function(appUrl) {
              url <- paste0(appUrl, "/?param=", chemin )
              browseURL(url)
              # invisible(.Call("rs_shinyviewer", url, chemin, "browser", NULL, PACKAGE = "(embedding)"))
              # invisible(utils::browseURL(url)) # alternative
            } )
  } , 
  # args = list(chemin = paste0(getwd(), "/codes/dataexplorer_R") ) )
  args = list(chemin = chemin) )
  
  print("Subprocess called")
  print("End of creating subprocess")
  
  return(rs)
}

close_board <- function(process, chemin = getwd() ) {
  print("Close boarding initialization ... \n")
  process$kill()
  print("Process killed")
  
  # chemin = paste0(getwd(), "/codes/dataexplorer_R")
  unlink(paste(chemin, "/board_dataexplorer/", sep = ""), recursive = TRUE, force = TRUE)
  print("Floder deleted")
  print("End of closing process")
}

upload_dash <- function(base, chemin) {
  # chemin = paste0(getwd(), "/codes/dataexplorer_R")
  board_dataexplorer <- board_folder( paste(chemin, "/board_dataexplorer", sep = "") )
  
  pin_write(board_dataexplorer, 
            x = base,
            # x = mtcars,
            name = "database")
  # title = "Database for dataexplorer dashboard")
  print("Upload OK !")
}

data("USArrests")
chemin = getwd()

rs <- init_board(USArrests, chemin)
# upload_dash(BOD, chemin)
# rs$read()
# close_board(rs, chemin)
