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

    # find how to get avgSleepHours and how to plot  
    

    output$plot <- renderPlot({
      avgsleepHours <- # avg sleep times vector here
      wakeAM <- c("10", "5", "6", "7", "8", "9")
      ca.df <- data.frame(wakeAM, avgsleepHours)
      isolate(ggplot(ca.df, aes(x=wakeAM, y=avgsleepHours)) +
               geom_bar(stat="identity", fill="lightblue") +
               theme(
                 panel.grid.minor = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.background = element_blank(),
                 plot.background = element_blank())
      )


    })

    # 

    output$plot2 <- renderPlot({
       # for Histogram fall asleep time        
      fallasleeptimes <- # fall asleep times vector here
      # 1, 2 AM & 9, 10, 11, 12 PM
      fallAslAMPM <- c("1", "2", "9", "10", "11", "12") 
      ca.FA <- data.frame(wakeAM, avgsleepHours)
      isolate(ggplot(ca.FA, aes(x=fallAslAM, y=fallasleeptimes)) +
               geom_bar(stat="identity", fill="lightblue") +
               theme(
                 panel.grid.minor = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.background = element_blank(),
                 plot.background = element_blank())
     )


    })
    
    # find and print averages 

    output$txtwake <- renderText({
      "Average Wake Time: "
       # (sum of (fallasleep + sleepduration) % 12) / 6


    })

    output$txtfallasleep <- renderText({ 
      "Average Fall Asleep Time: "


    })

    output$txtsleepduration <- renderText({ 
      "Average Sleep Duration: "
      

    })


    

    


}

# Run the application 
shinyApp(ui, server)
