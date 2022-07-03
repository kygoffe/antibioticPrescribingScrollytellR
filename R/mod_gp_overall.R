#' gp_overall UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_gp_overall_ui <- function(id) {
  ns <- NS(id)
  tagList()
}

#' gp_overall Server Functions
#'
#' @noRd
mod_gp_overall_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_gp_overall_ui("gp_overall_1")

## To be copied in the server
# mod_gp_overall_server("gp_overall_1")
