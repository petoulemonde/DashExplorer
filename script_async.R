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

# ---- Essais

library(callr)

rs <- callr::r_session$new()
rs$run(function() runif(10))

rs$call(function() rnorm(1000))
rs
ls(rs)
rs$read()

rs <- callr::r_session$new()
rs$poll_process(2)
rs
rs$call(function() { install.packages("shiny"); library(shiny); runExample("01_hello") } )
rs$read()

rs$kill()
rs

# ---- Essais
call_shiny <- function(base = data(mtcars)) {
  rs <- callr::r_session$new()
  rs$poll_process(0)
  rs
  rs$call(function() { install.packages("shiny"); 
    library(shiny); 
    # runExample("01_hello") 
    runApp(paste(getwd(), "/codes/test/app.R", sep = "")) } )
  rs$read()
  
  rs$kill()
  rs
}
