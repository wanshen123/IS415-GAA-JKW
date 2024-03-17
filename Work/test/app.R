library(shiny)
library(leaflet)

# Define UI
ui <- navbarPage(
  title = "My Shiny App",
  
  tabPanel("Histogram",
           sidebarLayout(
             sidebarPanel(
               sliderInput("num", "Choose a number", 1, 100, 30)
             ),
             mainPanel(
               plotOutput("histogram")
             )
           )
  ),
  
  tabPanel("Singapore Map",
           fluidPage(
             titlePanel("Singapore Map"),
             leafletOutput("map")
           )
  )
)

# Define server logic
server <- function(input, output) {
  output$histogram <- renderPlot({
    hist(rnorm(input$num), main = "Histogram")
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 103.8198, lat = 1.3521, zoom = 12)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
