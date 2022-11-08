# packages <- c("librarian")
# 
# installed_packages <- packages %in% row.names(installed.packages())
# if( any( installed_packages == FALSE ) ) {
#   install.packages(packages[!installed_packages])
# }
# rm(packages, installed_packages)
# 
# librarian::shelf("callr",
#                  "pins",
#                  quiet= TRUE)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           
           h4("Chemin d'accès"),
           htmlOutput("pass"),

           h4("table"),
           tableOutput("dbTable")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # ---- classic shiny app
  
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
    
    # ----- Pins -----
    chemin <- reactive({
      query <- parseQueryString(session$clientData$url_search)
      if (!is.null(query[['param']])) {
        updateTextInput(session, "InputLabel_A", value = query[['param']])
      }
      return(query[['param']])
    })

    observe({
      board_dataexplorer <- board_folder( paste( chemin(), "/board_dataexplorer/", sep = "") )
      data <- pin_reactive_read(board_dataexplorer, "database", interval = 1000)
      output$dbTable <- renderTable( data() )
    })
    
    # ----- Parameters by URL

    # Return the components of the URL in a string:
    # output$urlText <- renderText({
    #   paste(sep = "",
    #         "protocol: ", session$clientData$url_protocol, "\n",
    #         "hostname: ", session$clientData$url_hostname, "\n",
    #         "pathname: ", session$clientData$url_pathname, "\n",
    #         "port: ",     session$clientData$url_port,     "\n",
    #         "search: ",   session$clientData$url_search,   "\n"
    #   )
    # })
    
    output$pass <- renderUI({
      query <- parseQueryString(session$clientData$url_search)
      if (!is.null(query[['param']])) {
        updateTextInput(session, "InputLabel_A", value = query[['param']])
      }
      return(query[['param']])
    })
    
    # Forme de l'URL pour paramètre : http://127.0.0.1:8888/?param=hello
    # Si 2 paramètres, forme = http://localhost.com/?paramA=hello&?paramB=world
    
}

# Run the application
shinyApp(ui = ui, server = server)

# Liens : 
# Passing argument to shiny : https://stackoverflow.com/questions/65588060/how-can-i-pass-an-argument-to-my-shiny-app
