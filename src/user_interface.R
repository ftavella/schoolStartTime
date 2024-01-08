# our new file
# this is another comment

library(shiny)
library(shinythemes)
library(suncalc)
library(ggplot2)
library(maptools)
library(lubridate)
library(lutz)
library(tidygeocoder)
library(tibble)
library(dplyr, warn.conflicts = TRUE)
library(deSolve)
library(sf)
library(tidyverse)
library(scales)
library(plotly)
library(shinycssloaders)
library(waiter)
library(progress)
library(progressr)


ui <- fluidPage(

    # Application title
    titlePanel("School Start Times App by ArcaScope"),

    # Theme selector 
    shinythemes::themeSelector(),
    theme = bslib::bs_theme(bootswatch = "darkly"),

    # Layout
    
    navbarPage(
       "School Light App",

       
     tabPanel(
        "Your Information",

        
        sidebarPanel(
           helpText("Modeling Schoool Start Times....."),
           verbatimTextOutput("textout"),
           
           tags$h3("Which city do you live in?"),
           textInput("txt1", "City:"), 

           # maybe dropdown here?
           # selectInput("cit",
           #             label = "Choose the City Where You Reside In",
           #             choices = c(), # vector of U.S. cities - to do
           #             selected = ""),


        ),

        mainPanel(
           h1("Histogram Wake Times"),

        )
     )




    )  



)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$text1 <- renderText({ 


    })

    output$textout <- renderText({
        "This app calculates what your average sleep will be
         based on where you are in the world"
         
    })

    

    


}

# Run the application 
shinyApp(ui, server)
