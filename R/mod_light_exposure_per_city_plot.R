#' light_exposure_per_city_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_light_exposure_per_city_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4,
        dateInput(ns("date"), "Date:")
      ),
      column(4,
        selectInput(ns("country"), "Country:", sort(unique(worldCities$country))),
      ),
      column(4,
        selectInput(ns("city"), "City:", NULL)
      ),
    ),
  )
}

#' light_exposure_per_city_plot Server Functions
#'
#' @noRd
mod_light_exposure_per_city_plot_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # Update city dropdown with selected country
    observeEvent(input$country,
                 {
                   newChoices <- sort(worldCities[worldCities$country %in% input$country,
                                                  "name", drop = TRUE])
                   updateSelectInput(session, input = "city",
                                     choices = newChoices)
                 })

  })
}

## To be copied in the UI
# mod_light_exposure_per_city_plot_ui("light_exposure_per_city_plot_1")

## To be copied in the server
# mod_light_exposure_per_city_plot_server("light_exposure_per_city_plot_1")
