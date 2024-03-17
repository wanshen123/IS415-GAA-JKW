library(shiny)
library(ggplot2)
library(dplyr)

# Define UI
ui <- fluidPage(
  titlePanel("Kernel Density Estimation"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV file",
                accept = c(".csv")),
      selectInput("variable", "Select a variable:",
                  choices = NULL),
      br(),
      helpText("Note: Uploaded CSV file should have a header.")
    ),
    mainPanel(
      plotOutput("kde_plot")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Read uploaded file
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath, header = TRUE)
  })
  
  # Update variable choices based on uploaded file
  observe({
    updateSelectInput(session, "variable",
                      choices = names(data()))
  })
  
  # Generate and render KDE plot
  output$kde_plot <- renderPlot({
    req(input$variable)
    
    ggplot(data(), aes(x = .data[[input$variable]])) +
      geom_density(fill = "skyblue", alpha = 0.5) +
      labs(title = paste("KDE Plot of", input$variable))
  })
}

# Run the app
shinyApp(ui = ui, server = server)
