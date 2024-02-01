library(shiny)
library(ggplot2)

ui <- fluidPage(

    titlePanel("School Start Times App by ArcaScope"),

    shinythemes::themeSelector(),
    theme = bslib::bs_theme(bootswatch = "darkly"),

    navbarPage(
       "School Light App",
     tabPanel(
        "Your Information",
      sidebarLayout(
        sidebarPanel(
           tags$h3("Modeling Schoool Start Times....."),
           verbatimTextOutput("textout"),
           tags$h3("Which city do you live in?"),
           selectInput("cit",
                        label = "Choose the City Where You Reside In",
                        choices = c("Chicago", "New York", "LA"),
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

server <- function(input, output) {
    wakeTime <- 1:100
    fallAsleepTime <- 1:100
    time <- 1:100
    sleepDurationTime <- wakeTime - fallAsleepTime

    avgSleepDuration <- mean(sleepDurationTime)
    avgWake <- mean(wakeTime) %% 12
    avgfallAsleep <- mean(fallAsleepTime) %% 12
     
    wakeMsg <- paste("Average Wake Time: ", avgWake)
    fallAsleepMsg <- paste("Average Fall Asleep Time: ", avgfallAsleep)
    sleepDurationMsg <- paste("Average Sleep Duration Time: ", avgSleepDuration)

    output$text1 <- renderText({})

    output$textout <- renderText({
        "This app calculates what your average sleep will be
         based on where you are in the world"
    })

    output$plot <- renderPlot({
      ca.df <- data.frame(time, wakeTime)
      isolate(ggplot(ca.df, aes(x = time, y = wakeTime)) +
               geom_bar(stat = "identity", fill = "lightblue") +
               theme(
                 panel.grid.minor = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.background = element_blank(),
                 plot.background = element_blank(),
                 axis.title.x = element_text(color = "blue",
				 							 size = 20,
				 							 face = "plain"),
                 axis.title.y = element_text(color = "blue",
				 							 size = 20,
											 face = "plain"),
                 axis.text = element_text(color = "black",
				 						  size = 10,
										  face = 1)) +
               xlab("Wake Time (hour of day)") +
               ylab("Counts")
      )
    })

    output$plot2 <- renderPlot({
      ca.FA <- data.frame(time, fallAsleepTime)
      isolate(ggplot(ca.FA, aes(x = time, y = fallAsleepTime)) +
               geom_bar(stat = "identity", fill = "lightblue") +
               theme(
                 panel.grid.minor = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.background = element_blank(),
                 plot.background = element_blank(),
                 axis.title.x = element_text(color = "blue",
				 							 size = 20,
											 face = "plain"),
                 axis.title.y = element_text(color = "blue",
				 							 size = 20,
											 face = "plain"),
                 axis.text = element_text(color = "black",
				 						  size = 10,
										  face = 1)) +
                xlab("Fall Asleep time (hour of day)") +
                ylab("Counts")
     )
    })
    output$txtwake <- renderText({wakeMsg})
    output$txtfallasleep <- renderText({fallAsleepMsg})
    output$txtsleepduration <- renderText({sleepDurationMsg})
}
shinyApp(ui, server)