# our new file
# this is another comment

library(shiny)



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
      sidebarLayout(   
        
        sidebarPanel(
           tags$h3("Modeling Schoool Start Times....."),
           verbatimTextOutput("textout"),
           
           tags$h3("Which city do you live in?"),
          # textInput("txt1", "City:"),  

           # maybe dropdown here?
           selectInput("cit",
                        label = "Choose the City Where You Reside In",
                        choices = c("Chicago", "New York", "LA"), # vector of U.S. cities
                        selected = ""),


        ),

        mainPanel(
           h1("Histogram Wake Times"),
           plotOutput("plot"),

           h1("Histogram Fall Asleep Times"),
           plotOutput("plot2"),

           verbatimTextOutput("txtwake"),
           verbatimTextOutput("txtfallasleep"),
           verbatimTextOutput("txtsleepduration"), 

        )
       )   
     )




    )  



)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    wakeTime <- c()
    fallAsleepTime <- c()
    time <- c()
    
    sleepDuration <- c()

    for (i in 1:100) {
       wakeTime <- append(wakeTime, i)
       fallAsleepTime <- append(fallAsleepTime, i)
       time <- append(time, i)

    }
    
    
    sleepDurationTime <- wakTime - fallAsleepTime

    avgSleepDuration <- (sum(sleepDurationTime) %% 100) 
    avgWake <- (sum(wakeTime) %% 100) %% 12
    avgfallAsleep <- (sum(fallAsleepTime) %% 100) %% 12
     
    wakeMsg <- paste("Average Wake Time: ", avgWake)
    fallAsleepMsg <- paste("Average Fall Asleep Time: ", avgfallAsleep)
    sleepDurationMsg <- paste("Average Sleep Duration Time: ", avgSleepDuration) 


    output$text1 <- renderText({ 


    })

    output$textout <- renderText({
        "This app calculates what your average sleep will be
         based on where you are in the world"
         
    })

    # find how to get avgSleepHours and how to plot  
    

    output$plot <- renderPlot({
      ca.df <- data.frame(time, wakeTime)
      isolate(ggplot(ca.df, aes(x=time, y=wakeTime)) +
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
      ca.FA <- data.frame(time, fallAsleepTime)
      isolate(ggplot(ca.FA, aes(x=fallAslAM, y=fallAsleepTime)) +
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
      wakeMsg
       # (sum of (fallasleep + sleepduration) % 12) / 6


    })

    output$txtfallasleep <- renderText({ 
      fallAsleepMsg


    })

    output$txtsleepduration <- renderText({ 
      sleepDurationMsg
      

    })


    

    


}

# Run the application 
shinyApp(ui, server)
