#' criteria_selection UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_criteria_selection_ui <- function(id) {
  ns <- NS(id)
  tagList(
    nhs_card(
      heading = "Select Metrics to explore",

      # requires three dropdown memu

      nhs_selectInput(
        inputId = ns("metric"),
        label = "Metric",
        choices = c(
          "Antibacterial items/STAR-PU" = "STAR_PU",
          "Co-amoxiclav, Cephalosporins & Quinolones" = "COAMOX"
        ),
        full_width = TRUE
      )
    )
  )
}

#' criteria_selection Server Functions
#'
#' @noRd
mod_criteria_selection_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    metric_sel <- reactive(input$metric)

    return(metric_sel)
    # observe(print(metric_sel()))
  })
}

## To be copied in the UI
# mod_criteria_selection_ui("criteria_selection_1")

## To be copied in the server
# mod_criteria_selection_server("criteria_selection_1")
