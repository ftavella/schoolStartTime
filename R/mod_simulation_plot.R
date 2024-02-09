#' simulation_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_simulation_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' simulation_plot Server Functions
#'
#' @noRd 
mod_simulation_plot_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_simulation_plot_ui("simulation_plot_1")
    
## To be copied in the server
# mod_simulation_plot_server("simulation_plot_1")
