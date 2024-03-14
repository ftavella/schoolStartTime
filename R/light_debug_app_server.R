#' The light debug application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
light_debug_app_server <- function(input, output, session) {
  mod_light_exposure_per_city_plot_server("light_exposure_per_city_plot_1")
}
