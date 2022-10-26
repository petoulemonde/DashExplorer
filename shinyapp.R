
# data_discovery <- function(database = swiss) {

# Liens : 
# http://rstudio.github.io/shinydashboard/structure.html#body
  
  if(!"librarian" %in% rownames(installed.packages())) {
    install.packages("librarian", repos = "https://cran.rstudio.com/", dep = TRUE)
  }
  librarian::shelf(shiny, 
                   shinydashboard, 
                   tidyverse, 
                   visdat, 
                   plotly, 
                   VIM, 
                   corrplot, 
                   funModelling,
                   DT,
                   GGally,
                   quiet = TRUE)
  
  shinyApp(
    
    ui = dashboardPage(
      dashboardHeader(),
      
      dashboardSidebar(
        sidebarMenu(
          menuItem("Général", tabName = "dashboard", icon = icon("dashboard")),
          menuItem("Variables numériques", tabName = "tab1", icon = icon("th")),
          menuItem("Variables catégorielles", tabName = "tab2", icon = icon("th")),
          menuItem("ACP", tabName = "tab3", icon = icon("th")),
          menuItem("Clusteurisation", tabName = "tab4", icon = icon("th"))
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
                    plotOutput("plot_NA_naniar")
                  ),
                  
                  h2("Résumé"),
                  plotOutput("plot_ggpairs")
          ),
          
          # --- Second tab content : variables nuémriques
          tabItem(tabName = "tab1",
                  h2("Description univariées"),
                  
                  h3("Description"),
                  plotOutput("plot_univariate_num"),
                  
                  h3("Graphiques des quantiles (variables numériques seulement)"),
                  plotOutput("plot_qq_num"),
                  
                  h2("Etude des corrélations"),
                  tableOutput("tab_correlation"),
                  plotOutput("plot_correlation")
                  
                  
          ), 
          
          # 3e tab content : variables catégorielles
          tabItem(tabName = "tab2",
                  
                  h2("Description univariées"),
                  plotOutput("plot_univariate_cat")
                  
            ), #,
          
          # 4e tab content : ACP
          tabItem(tabName = "tab3",
                  fluidRow(
                    h2("Description univariées"),
                    
                    box(
                      h3("Analyse en composante principale"),
                      plotOutput("acp_var"),
                      h3("Tableau des pourcentages d'inertie"),
                      tableOutput("acp_tab_var"),
                      h3("Histrogramme des % d'inertie"),
                      plotOutput("acp_plot_tab_var")
                    ),
                    
                    box(
                      h3("Analyse en composante principale (individus)"),
                      plotOutput("acp_ind"),
                      h3("Coordonnées des individus"),
                      dataTableOutput("acp_tab_ind")
                    )
                  )
                ),
          
          # 5e tab content : hclust
          # https://www.rdocumentation.org/packages/FactoMineR/versions/2.6/topics/plot.HCPC
          # Voir pour bouton de choix du nombre de clusters
          tabItem(tabName = "tab4",
                  fluidRow(
                      h2("Clusteurisation"),
                      # Plot
                      # plot(HCPC(iris_num, 3), choice = "factor.map")
                      # plot(HCPC(iris_num, 3), choice = "map")
                      # plot(HCPC(iris_num, 3), choice = "3D.map")
                    )
                  )
          # new tab : 
            )
          )
    ), 
    # ------------------------------------------------------------------------------------------------------------------------------------
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
      output$plot_NA_naniar <- renderPlot({ 
        if ( sum(is.na(base() ) ) > 0 ) {
          aggr(base(),
                 col=c('navyblue','red'),
                 numbers=TRUE,
                 sortVars=TRUE,
                 labels=names(base()),
                 cex.axis=.7, gap=3,
                 ylab=c("Histogram of missing data","Pattern"))
        }
      })
      
      
      # ---- Les outputs de tab1
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
      
      output$tab_correlation <- renderTable({
        cor( select_if(base(), is.numeric) )
      })
      
      output$plot_correlation <- renderPlot({
        corrplot(cor(select_if(base(), is.numeric)))
      })
      
      output$plot_ggpairs <- renderPlot({
        ggpairs(base())
      })
      
      # ---- les outputs de tab2
      
      output$plot_univariate_cat <- renderPlot({
        select(base(), -names(select_if(base(), is.numeric) ) ) %>% 
          gather() %>%
          ggplot(aes(x=value)) +
          geom_bar() +
          facet_wrap(~key, scales = "free")
      })
      
      # ---- les outputs de tab3
    output$acp_var <- renderPlot({
      PCA(select_if(base(), is.numeric), 
          graph = TRUE)
    })
    
    output$acp_ind <- renderPlot({
      plot(PCA(select_if(base(), is.numeric), graph = FALSE), 
           choix = "ind", 
           autoLab = "yes")
    })
    
    output$acp_tab_var <- renderTable({
      PCA(select_if(base(), is.numeric), graph = FALSE)$var$contrib
    })
    
    output$acp_plot_tab_var <- renderPlot({
      barplot(CA(select_if(base(), is.numeric), graph = FALSE)$eig[, 2], 
              names.arg = 1:nrow(CA(select_if(base(), is.numeric), graph = FALSE)$eig), 
              main = "Variances Explained by Dimensions (%)",
              xlab = "Principal Dimensions",
              ylab = "Percentage of variances",
              col ="steelblue")
    })
    
    output$acp_tab_ind <- renderDataTable({
      data.frame(PCA(select_if(base(), is.numeric), graph = FALSE)$ind$coord)
    })
      
    }
  )
# }