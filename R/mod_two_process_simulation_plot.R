#' two_process_simulation_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot geom_rect aes geom_line coord_cartesian geom_hline
mod_two_process_simulation_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    titlePanel("Two process simulation"),
    fluidRow(
      column(3,
        sliderInput(ns("simulation_time"), "Simulation time (hrs)", 1, 1000, 72),
      ),
      column(3,
        sliderInput(ns("time_step"), "Simulation time step (hrs)", 0.001, 1, 0.1),
      ),
      column(2,
        sliderInput(ns("light_start"), "Light start time (hrs)", 0, 24, 7),
      ),
      column(2,
        sliderInput(ns("light_end"), "Light end time (hrs)", 0, 24, 22),
      ),
      column(2,
        sliderInput(ns("school_start"), "School start time (hrs)", 5, 11, 8),
      ),
    ),
    plotOutput(ns("two_process_plot")),
    plotOutput(ns("sleep_drive_plot")),
    plotOutput(ns("sleep_state_plot")),
    plotOutput(ns("school_state_plot")),
  )
}

#' two_process_simulation_plot Server Functions
#'
#' @noRd
mod_two_process_simulation_plot_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    dataSim <- reactive({
      timeArray <- seq(0, input$simulation_time, by = input$time_step)
      initialCondition <- c(R = 0.8, Psi = 2.5, n = 0.8,
                            A = 760, R1tot = 580, S = 0)
      parameters <- defaultParameters
      parameters["schoolStart"] <- input$school_start

      lightCondition <- timeArray %% 24 >= input$light_start &
                        timeArray %% 24 <= input$light_end
      lightArray <- ifelse(lightCondition, 1000, 0)
      result <- simulateTwoProcessModel(timeArray, lightArray, initialCondition,
                                        parameters)
      data <- data.frame(result)
      data$circadian_state <- data$R * cos(data$Psi)
      R1b <- getR1b(data$A, data$R1tot)
      data$sleep_drive <- sleepDrive(R1b, data$Psi, parameters)
      simResult <- list(data = data, parameters = parameters)
      simResult
    })

    output$two_process_plot <- renderPlot({
      simResult <- dataSim()
      data <- simResult$data
      # Shaded light regions
      shade_light <- data.frame(
        from=seq(input$light_start, 24 * floor(input$simulation_time / 24) + input$light_start, 24),
        to=seq(input$light_end, 24 * floor(input$simulation_time / 24) + input$light_end, 24)
      )
      ggplot() +
        geom_rect(
          data=shade_light,
          aes(xmin=.data$from, xmax=.data$to, ymin=-Inf, ymax=Inf),
          alpha=0.4, fill="yellow2") +
        geom_line(data=data, aes(x=.data$time, y=.data$circadian_state)) +
        coord_cartesian(xlim=c(data$time[1], data$time[length(data$time)]))
    })

    output$sleep_drive_plot <- renderPlot({
      simResult <- dataSim()
      data <- simResult$data
      parameters <- simResult$parameters
      ggplot() +
        # TODO: Add shaded regions of sleep
        # geom_rect(
        #   data=shade_sleep,
        #   aes(xmin=.data$from, xmax=.data$to, ymin=-Inf, ymax=Inf),
        #   alpha=0.4) +
        geom_hline(yintercept=parameters["wakeThreshold"],
                   linetype="longdash", color="red") +
        geom_hline(yintercept=defaultParameters["sleepThreshold"],
                   linetype="longdash", color="red") +
        geom_line(data=data, aes(x=.data$time, y=.data$sleep_drive)) +
        coord_cartesian(xlim=c(data$time[1], data$time[length(data$time)]))
    })

    output$sleep_state_plot <- renderPlot({
      simResult <- dataSim()
      data <- simResult$data
      ggplot() +
        geom_line(data=data, aes(x=.data$time, y=.data$S)) +
        coord_cartesian(xlim=c(data$time[1], data$time[length(data$time)]))
    })

    output$school_state_plot <- renderPlot({
      simResult <- dataSim()
      data <- simResult$data
      parameters <- simResult$parameters
      schoolStart <- parameters["schoolStart"]
      schoolDuration <- parameters["schoolDuration"]
      school_state <- isWithinSchoolHours(data$time, schoolStart, schoolDuration)
      plotDataFrame <- data.frame(time=data$time, school_state=as.numeric(school_state))
      ggplot() +
        geom_line(data=plotDataFrame, aes(x=.data$time, y=.data$school_state)) +
        coord_cartesian(xlim=c(data$time[1], data$time[length(data$time)]))
    })
  })
}

## To be copied in the UI
# mod_two_process_simulation_plot_ui("two_process_simulation_plot_1")

## To be copied in the server
# mod_two_process_simulation_plot_server("two_process_simulation_plot_1")
