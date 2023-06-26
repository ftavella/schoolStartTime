# Here we will have all the app-specific code, separate from the circadian-specific code
library(shiny)

ui <- fluidPage(
  # Application title
  titlePanel("School Start Times"),
  
  # Side bar
  sidebarLayout(
    sidebarPanel(

    ),

    # Contents of the page
    mainPanel(

    )
  )
)

server <- function(input, output) {
  
}

# Run the application
shinyApp(ui = ui, server = server)