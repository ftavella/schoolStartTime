#' circadian_simulation_plot UI Function
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
mod_circadian_simulation_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    titlePanel("Circadian simulation"),
    fluidRow(
      column(4,
        sliderInput(ns("simulation_time"), "Simulation time (hrs)", 1, 1000, 72),
      ),
      column(4,
        sliderInput(ns("light_start"), "Light start time (hrs)", 0, 24, 8),
      ),
      column(4,
        sliderInput(ns("light_end"), "Light end time (hrs)", 0, 24, 22),
      ),
    ),
    plotOutput(ns("circadian_plot")),
  )
}

#' circadian_simulation_plot Server Functions
#'
#' @noRd
mod_circadian_simulation_plot_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$circadian_plot <- renderPlot({
      timeArray <- seq(0, input$simulation_time, length.out = 1000)
      initialCondition <- c(R = 0.8, Psi = 2.5, n = 0.8)
      lightCondition <- timeArray %% 24 >= input$light_start &
                        timeArray %% 24 <= input$light_end
      lightArray <- ifelse(lightCondition, 1000, 0)
      result <- simulateCircadianModel(timeArray, lightArray, initialCondition,
                                       defaultParameters)
      data <- data.frame(result)
      # Shaded light regions
      shade_light <- data.frame(
        from=seq(input$light_start, 24 * floor(input$simulation_time / 24) + input$light_start, 24),
        to=seq(input$light_end, 24 * floor(input$simulation_time / 24) + input$light_end, 24)
      )
      data$circadian_state <- data$R * cos(data$Psi)
      ggplot() +
        geom_rect(
          data=shade_light,
          aes(xmin=.data$from, xmax=.data$to, ymin=-Inf, ymax=Inf),
          alpha=0.4, fill="yellow2") +
        geom_line(data=data, aes(x=.data$time, y=.data$circadian_state)) +
        coord_cartesian(xlim=c(timeArray[1], timeArray[length(timeArray)]))
    })

  })
}

## To be copied in the UI
# mod_circadian_simulation_plot_ui("circadian_simulation_plot_1")

## To be copied in the server
# mod_circadian_simulation_plot_server("circadian_simulation_plot_1")
