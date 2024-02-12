#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  mod_circadian_simulation_plot_server("circadian_simulation_plot_1")
  mod_sleep_simulation_plot_server("sleep_simulation_plot_1")
}
