#' drug_list UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_drug_list_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2_tabstop("Presentations contributing to variation"),
    p(
      "Show drug presentation to explain the variation in the above metric and practices",
      "add data table?"
    ),
    nhs_card(
      heading = "Which drug were prescribed the most in month ending 2022?",
      tags$text("Use Open Data Portal data ")
    )
  )
}

#' drug_list Server Functions
#'
#' @noRd
mod_drug_list_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_drug_list_ui("drug_list_1")

## To be copied in the server
# mod_drug_list_server("drug_list_1")
