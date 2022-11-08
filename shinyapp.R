# if(!"librarian" %in% rownames(installed.packages())) {
#   install.packages("librarian", repos = "https://cran.rstudio.com/", dep = TRUE)
# }
# librarian::shelf(shiny,
#                  shinydashboard,
#                  tidyverse,
#                  visdat,
#                  plotly,
#                  VIM,
#                  corrplot,
#                  # funModelling,
#                  DT,
#                  GGally,
#                  FactoMineR,
#                  gtsummary,
#                  pins,
#                  quiet = TRUE)

ui = dashboardPage(
    
    dashboardHeader(),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Général", tabName = "tab1", icon = icon("dashboard")),
        menuItem("Variables numériques", tabName = "tab2", icon = icon("th")),
        menuItem("Variables catégorielles", tabName = "tab3", icon = icon("th")),
        menuItem("Table1", tabName="tab7", icon = icon("th")),
        menuItem("ACP", tabName = "tab4", icon = icon("dashboard")),
        menuItem("Clusteurisation", tabName = "tab5", icon = icon("dashboard")),
        menuItem("Table globale", tabName = "tab6", icon = icon("th"))
      )
    ),
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "tab1",
                verbatimTextOutput("query_text"),
                
                fluidRow( # ---- Choix des variables
                  h2("Choix de la base et des varaibles"),
                  box(
                    # selectInput("database",
                    #               "Jeu de données : ",
                    #               choices = c("Ma base", "Jeu Orange", "Jeu iris", "Jeu mtcars", "Jeu swiss")),
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
                  # h2("Résumé"),
                  # plotOutput("plot_ggpairs"),
                  # 
                  h2("Etude des données manquantes"),
                  textOutput("text_NA"),
                  plotlyOutput("plot_NA_visdat"),
                  plotOutput("plot_NA_naniar")
                ),
         ),
        
        # --- Second tab content : variables nuémriques
        tabItem(tabName = "tab2",
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
        tabItem(tabName = "tab3",
                
                h2("Description univariées"),
                plotOutput("plot_univariate_cat")
                
          ), #,
        
        # 4e tab content : TableOne
        tabItem(tabName = "tab7",
                h2("Table 1 des données"),
                fluidRow( 
                  box ( uiOutput("ui_tabOne_choix_ventil") ),
                  box ( uiOutput("ui_tabOne_choix_aff") ),
                ),
                
                textOutput("tabOne_text_1"),
                
                h3("Table One"),
                print("Variable catégorielle : n (%); Variable nuémrique : Médiane (IQR)"),
                # dataTableOutput("tabOne")
                uiOutput("tabOne")
                ),
        
        # 4e tab content : ACP
        tabItem(tabName = "tab4",
                fluidRow(
                  h2("Analyse en composante princiaple (variables nuémriques seulement)"),
                  
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
        tabItem(tabName = "tab5",
                fluidRow(
                    h2("Clasification hiérarchique sur composante principale (variables numériques seulement)"),
                    box( numericInput("input_num_clust", 
                                      "Choisissez le nombre de cluster à considérer", 
                                      3  ) ),
                    box( plotOutput("plot_clust_factor_map") ) ,
                    box( plotOutput("plot_clust_map") ),
                    box( plotOutput("plot_clust_3D_map") ),
                    renderDataTable("tab_predict_clust")
                 )
              ),
        tabItem(tabName = "tab6", 
                DT::dataTableOutput("full_tab")
              )
          )
      )
    )

  # ------------------------------------------------------------------------------------------------------------------------------------
server = function(input, output, session) {
  session$onSessionEnded(stopApp)
  
    # --- Reactive des bases
  # database <- eventReactive(input$database, {
  database <- reactive({
    query <- parseQueryString(session$clientData$url_search)
      if (!is.null(query[['param']])) {
        updateTextInput(session, "InputLabel_A", value = query[['param']])
      }

      board_dataexplorer <- board_folder( path = paste0( query[['param']], "/board_dataexplorer") )
    
    return(pin_reactive_read(board_dataexplorer, "database", interval = 1000))
  })
    
    # base_pins_2 <<- reactive({
    #   query <- parseQueryString(session$clientData$url_search)
    #   if (!is.null(query[['param']])) {
    #     updateTextInput(session, "InputLabel_A", value = query[['param']])
    #   }
    # 
    #   board_dataexplorer <- board_folder( path = paste0( query[['param']], "/board_dataexplorer") )
    #   base_brut <- pin_reactive_read(board_dataexplorer, "database", interval = 1000)
    #   
    #   return(base_brut)
    # })
    # 
    # base_pins <- pin_reactive_read(board_dataexplorer, "database", interval = 1000)
    # 
    output$query_text <- renderText({
      query <- parseQueryString(session$clientData$url_search)
      if (!is.null(query[['param']])) {
        updateTextInput(session, "InputLabel_A", value = query[['param']])
      } 
      return(paste0("Dossier de travail : ", query[['param']], 
                    "; \nEmplacement du dossier de la base temporaire : ", paste0( query[['param']], "/board_dataexplorer"),
                    "; \nEssai de type : ", class(isolate({database()})) ) ) 
    })
    
    base_ref <- reactive({
      # aux <- switch(input$database,
      #               "Ma base" = database(),
      #               "Jeu Orange" = Orange,
      #               "Jeu iris" = iris,
      #               "Jeu mtcars" = mtcars,
      #               "Jeu swiss" = swiss)
      aux <- database()
      return(aux)
    }) # Switch refuse les réactives comme base de donnée
    
    base <- reactive({
      # base <- base_ref()[ ,colnames(base_ref()) %in% input$choix_var]
      base <- base_ref() %>%
        select(input$choix_var)
      
      return(base)
    })
    
    # ---- Les outputs de tab1 : infos G
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
    
    
    # ---- Les outputs de tab2 - Var numériques
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
    # 
    # output$plot_ggpairs <- renderPlot({
    #   ggpairs(base())
    # })
    
    # ---- les outputs de tab3 : var catégorielles
    
    output$plot_univariate_cat <- renderUI({
      select(base(), -names(select_if(base(), is.numeric) ) ) %>% 
        gather() %>%
        ggplot(aes(x=value)) +
        geom_bar() +
        facet_wrap(~key, scales = "free")
    })
    
    # ---- Les outputs tab7 : table One
    # Lien intéressant : https://www.danieldsjoberg.com/gtsummary/
    output$tabOne <- renderUI({
      
      if ( length(input$tabOne_choix_ventil) == 1 ) {
        base() %>%
                     select(input$tabOne_choix_aff, input$tabOne_choix_ventil)  %>%
                     tbl_summary(by = input$tabOne_choix_ventil, missing = "always" ) %>%
          modify_header(label = "**Variable**") %>%
          add_overall(last = TRUE)  %>%
            add_stat_label() %>%
            as_gt()
        
        
      } else if ( length(input$tabOne_choix_ventil) > 1) {
        base() %>%
                     select(input$tabOne_choix_aff, input$tabOne_choix_ventil) %>%
          tbl_strata(strata = c(input$tabOne_choix_ventil)[2],
                     .tbl_fun = ~ .x %>%
                       tbl_summary( c(input$tabOne_choix_ventil)[1]) %>%
                                    modify_header(label = "**Variable**") %>% 
                                      add_overall(last = TRUE) %>%
                                      add_stat_label() ,
                                                     missing = "always") %>%
          as_gt()
      
        
      } else {
        base() %>%
                     select(input$tabOne_choix_aff)  %>%
                     tbl_summary()  %>%
                     add_stat_label() %>%
          modify_header(label = "**Variable**") %>% 
                  as_gt()
        
      }
      
    })
    
    output$tabOne_text_1 <- renderText({
      input$tabOne_choix_ventil
    })
    
    output$ui_tabOne_choix_ventil <- renderUI({
      checkboxGroupInput("tabOne_choix_ventil", label = "Choisir la variable de ventilation", names( base() ) )
    })
    
    output$ui_tabOne_choix_aff <- renderUI({
      checkboxGroupInput("tabOne_choix_aff", label = "Choisir les variables à décrire", names( base() ) )
    })
    
    # ---- les outputs de tab4 : ACP
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
  
  # ---- Les outputs de tab5 : clusteurisation sur composante principales
  output$plot_clust_factor_map <- renderPlot({
      plot(HCPC(select_if(base(), is.numeric), input$input_num_clust), choice = "factor.map")
      })
  
  output$plot_clust_map <- renderPlot({
    plot(HCPC(select_if(base(), is.numeric), input$input_num_clust), choice = "map")
  })
  
  output$plot_clust_3D_map <- renderPlot({
    plot(HCPC(select_if(base(), is.numeric), input$input_num_clust), choice = "3D.map")
  })
  
  # ---- Les toutputs tab6 : la table
  output$full_tab <- renderDataTable({
    base()
  })
  
  }

shinyApp(ui = ui, server = server)
