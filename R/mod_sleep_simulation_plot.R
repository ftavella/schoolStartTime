#' sleep_simulation_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot geom_rect aes geom_line coord_cartesian
mod_sleep_simulation_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    titlePanel("Sleep simulation"),
    fluidRow(
      column(4,
        sliderInput(ns("simulation_time"), "Simulation time (hrs)", 1, 1000, 72),
      ),
      column(4,
        sliderInput(ns("sleep_start"), "Sleep start time (hrs)", 0, 24, 23),
      ),
      column(4,
        sliderInput(ns("sleep_end"), "Sleep end time (hrs)", 0, 24, 7),
      ),
    ),
    plotOutput(ns("sleep_plot")),
  )
}

#' sleep_simulation_plot Server Functions
#'
#' @noRd
mod_sleep_simulation_plot_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$sleep_plot <- renderPlot({
      timeArray <- seq(0, input$simulation_time, length.out = 1000)
      initialCondition <- c(A = 767.7, R1tot = 584.2)
      wakeCondition <- timeArray %% 24 >= input$sleep_end &
                       timeArray %% 24 <= input$sleep_start
      wakeArray <- ifelse(wakeCondition, 1, 0)
      result <- simulateSleepModel(timeArray, wakeArray, initialCondition,
                                   defaultParameters)
      data <- data.frame(result)
      # Shaded light regions
      shade_sleep <- data.frame(
        from=seq(input$sleep_start - 24, 24 * floor(input$simulation_time / 24) + input$sleep_start - 24, 24),
        to=seq(input$sleep_end, 24 * floor(input$simulation_time / 24) + input$sleep_end, 24)
      )
      data$sleep_drive <- getR1b(data$A, data$R1tot)
      ggplot() +
        geom_rect(
          data=shade_sleep,
          aes(xmin=.data$from, xmax=.data$to, ymin=-Inf, ymax=Inf),
          alpha=0.4) +
        geom_line(data=data, aes(x=.data$time, y=.data$sleep_drive)) +
        coord_cartesian(xlim=c(timeArray[1], timeArray[length(timeArray)]))
    })

  })
}

## To be copied in the UI
# mod_sleep_simulation_plot_ui("sleep_simulation_plot_1")

## To be copied in the server
# mod_sleep_simulation_plot_server("sleep_simulation_plot_1")
