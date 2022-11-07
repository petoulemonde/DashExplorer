options(shiny.host = '0.0.0.0') # Non fonctionnel
options(shiny.port = 8888) # fonctionnel, mais pas très utile

library(shiny)

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
           textOutput("query"),
           h3("URL components"),
           verbatimTextOutput("urlText"),
           
           h3("Parsed query string"),
           verbatimTextOutput("queryText")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['paramA']])) {
      updateTextInput(session, "InputLabel_A", value = query[['paramA']])
    }
    if (!is.null(query[['paramB']])) {
      updateTextInput(session, "InputLabel_A", value = query[['paramB']])
    }
  })
  
  output$query <- renderText({
    paste(names(query), query, sep = "=", collapse=", ")
  })
  
  # corresponding to : http://localhost.com/?paramA=hello&?paramB=world
  # http://127.0.0.1:8888/?paramA=hello
  
  # ---- board pins
  
  # board <- bord_local()
  # board <- board_folder(getwd())
  # data <- pin_reactive_read(board, "shiny", interval = 1000)
  # output$table <- renderTable(data())
  
  # ---- clasic shiny app
  
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })

    # Return the components of the URL in a string:
    output$urlText <- renderText({
      paste(sep = "",
            "protocol: ", session$clientData$url_protocol, "\n",
            "hostname: ", session$clientData$url_hostname, "\n",
            "pathname: ", session$clientData$url_pathname, "\n",
            "port: ",     session$clientData$url_port,     "\n",
            "search: ",   session$clientData$url_search,   "\n"
      )
    })
    
    # Parse the GET query string
    output$queryText <- renderText({
      query <- parseQueryString(session$clientData$url_search)
      
      # Return a string with key-value pairs
      paste(names(query), query, sep = "=", collapse=", ")
    })
}

# Run the application
shinyApp(ui = ui, server = server)

# Liens : 
#   https://shiny.rstudio.com/articles/client-data.html
# https://callr.r-lib.org/ : Pour exéctuion en arrière plan
# job ackage : https://lindeloev.github.io/job/

# Reactive pins : https://rdrr.io/github/rstudio/pins/man/pin_reactive_read.html

# Passing argument to shiny : https://stackoverflow.com/questions/65588060/how-can-i-pass-an-argument-to-my-shiny-app
