
# data_discovery <- function(database = swiss) {

# Liens : 
# http://rstudio.github.io/shinydashboard/structure.html#body
  
  if(!"librarian" %in% rownames(installed.packages())) {
    install.packages("librarian", repos = "https://cran.rstudio.com/", dep = TRUE)
  }
  librarian::shelf(shiny, shinydashboard, tidyverse, visdat, plotly, naniar, corrplot, quiet = TRUE)
  
  shinyApp(
    
    ui = dashboardPage(
      dashboardHeader(),
      
      dashboardSidebar(
        sidebarMenu(
          menuItem("Général", tabName = "dashboard", icon = icon("dashboard")),
          menuItem("Variables numériques", tabName = "tab1", icon = icon("th")),
          menuItem("Variables catégorielles", tabName = "tab2", icon = icon("th"))
        )
      ),
      
      dashboardBody(
        tabItems(
          tabItem(tabName = "dashboard",
                  fluidRow( # ---- Choix des variables
                    h2("Choix de la base et des varaibles"),
                    box(
                      selectInput("database", 
                                    "Jeu de données : ",
                                    choices = c("Ma base", "Jeu mtcars", "Jeu iris", "Jeu swiss")),
                      uiOutput("choix_var")
                      ),
                    
                    box(
                      tableOutput("liste_var")
                    ) 
                  ),
                  fluidRow( # ---- Données descriptives générales
                    h2("Données générales"),
                    box (
                      tableOutput("table_descriptive")
                    ), 
                    box (
                      tableOutput("tab_missing_data")
                    )
                  ),
                  
                  fluidRow( # ---- Données manquantes
                    h2("Etude des données manquantes"),
                    textOutput("text_NA"),
                    plotlyOutput("plot_NA_visdat"),
                    plotlyOutput("plot_NA_naniar")
                  )
          ),
          
          # --- Second tab content : variables nuémriques
          tabItem(tabName = "tab1",
                  h2("Etude des valeurs manquantes"),
                  textOutput("text_NA"),
                  plotlyOutput("plot_NA_visdat_num"),
                  plotlyOutput("plot_NA_naniar_num"),
                  
                  h2("Description univariées"),
                  
                  h3("Description"),
                  plotOutput("plot_univariate_num"),
                  
                  h3("Graphiques des quantiles (variables numériques seulement)"),
                  plotOutput("plot_qq_num"),
                  
                  h2("Etude des corrélations"),
                  fluidRow(
                    h2("Etude des corrélations"),
                    box ( tableOutput("tab_correlation") ),
                    plotOutput("plot_correlation")
                    
                  )
          ), 
          
          # 3e tab content : variables catégorielles
          tabItem(tabName = "tab2",h2("Etude des valeurs manquantes"),
                  textOutput("text_NA"),
                  plotlyOutput("plot_NA_visdat_cat"),
                  plotlyOutput("plot_NA_naniar_cat"),
                  
                
                  h2("Description univariées"),
                  plotOutput("plot_univariate_cat")
                  
            )
        )
      )
    ), 
    
    server = function(input, output) {
      # --- Reactive des bases
      base_ref <- reactive({
        aux <- switch(input$database, 
                      # "Ma base" = database,
                      "Ma base" = Orange,
                      "Jeu mtcars" = mtcars, 
                      "Jeu iris" = iris,
                      "Jeu swiss" = swiss)
        return(aux) 
      })
      
      base <- reactive({
        # base <- base_ref()[ ,colnames(base_ref()) %in% input$choix_var]
        base <- base_ref() %>%
          select(input$choix_var)
        
        return(base)
      })
      
      # ---- Les outputs généraux
      output$choix_var <- renderUI({ checkboxGroupInput("choix_var",
                                          "Choix des variables",
                                          choices = names(base_ref()),
                                          selected = names(base_ref())
                                          )
                                  })
      output$liste_var <- renderTable({ data.frame(variables = names(base() ),
                                                   type = apply(base(), 2, class)) })
      
      output$table_descriptive <- renderTable({
        data.frame(
          Name = c("Nombre de lignes", 
                   "Nombre de colonnes", 
                   "Nombre de colonnes discrètes", 
                   "Nombre de colonnes continues", 
                   "Memory allocation"),
          Value = c(nrow(base()), 
                    ncol(base()), 
                    length(select_if(base(),is.numeric)), 
                    length(select(base(), -names(select_if(base(), is.numeric) ) ) ), 
                    paste(as.character(object.size(base())), "kb")
                    ),
          Percent = c("/", "/", 
                      paste(
                        as.character(
                          round(length(select_if(base(),is.numeric)) / length(names(base())) * 100, 2), 
                          "%", 
                          sep = "")), 
                      paste(
                        as.character(
                          round(length(select(base(), -names(select_if(base(), is.numeric) ) ) ) / length(names(base() ) ) * 100, 2 ), 
                            "%", sep = "" ) ),
                      "/")
                  )
      })
      
      output$tab_missing_data <- renderTable({ data.frame( variables = names(base()), 
                                                           n_NA = apply(base(), 2, function(.x) {sum(is.na(.x) )} ), 
                                                           percent_NA = apply(base(), 2, function(.x) {
                                                             paste(round(sum(is.na(.x) ) / nrow(base()) * 100, 2) , "%") 
                                                             } ))
        
                                              })
      
      output$text_NA <- renderText({ 
        if (sum(is.na(base())) == 0) { 
          print("Aucune valeur manquante")
        } })
      
      output$plot_NA_visdat <- renderPlotly({ 
          if (sum(is.na(base())) > 0 )  { 
            vis_dat(base())
          } 
      }) 
      output$plot_NA_naniar <- renderPlotly({ 
        if ( sum(is.na(base() ) ) > 0 & sum( any( apply( base(), 2, function(.x) sum(is.na(.x) > 1) ) ) ) ) {
          gg_miss_upset(base())
        }
      })
      
      output$plot_univariate_num <- renderPlot({
        base() %>% 
          select_if(is.numeric) %>% 
          gather() %>%
          ggplot(aes(x=value)) +
            geom_histogram() +
          facet_wrap(~key, scales = "free")
      })
      
      output$plot_qq_num <- renderPlot({
        base() %>% 
          select_if(is.numeric) %>% 
          gather() %>%
          ggplot(aes(sample=value)) +
          stat_qq() +
          stat_qq_line() + 
          facet_wrap(~key, scales = "free")
      })
      
      output$plot_univariate_cat <- renderPlot({
        select(base(), -names(select_if(base(), is.numeric) ) ) %>% 
          gather() %>%
          ggplot(aes(x=value)) +
          geom_bar() +
          facet_wrap(~key, scales = "free")
      })
      
      output$tab_correlation <- renderTable({
        cor( select_if(base(), is.numeric) )
      })
      
      output$plot_correlation <- renderPlot({
        corrplot(cor(select_if(base(), is.numeric)))
      })
      
      # ---- Les tests
      # output$test_output <- renderText({ print(database) })
      # output$test_table <- renderTable({ database })
      
    }
  )
# }