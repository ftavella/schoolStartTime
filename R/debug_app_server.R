#' The debug application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
debug_app_server <- function(input, output, session) {
  mod_circadian_simulation_plot_server("circadian_simulation_plot_1")
  mod_sleep_simulation_plot_server("sleep_simulation_plot_1")
  mod_two_process_simulation_plot_server("two_process_simulation_plot_1")
}
