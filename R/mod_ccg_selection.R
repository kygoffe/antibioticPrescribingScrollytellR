#' ccg_selection UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ccg_selection_ui <- function(id) {
  ns <- NS(id)
  tagList(
    nhs_selectInput(
      inputId = ns("ccg"),
      label = "Sub ICB locations",
      choices = c(sort(unique(antibioticPrescribingScrollytellR::gp_merge_df$SUB_ICB_NAME))),
      full_width = TRUE
    )
  )
}

#' ccg_selection Server Functions
#'
#' @noRd
mod_ccg_selection_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ccg_sel <- reactive(input$ccg)

    return(ccg_sel)
  })
}

## To be copied in the UI
# mod_ccg_selection_ui("ccg_selection_1")

## To be copied in the server
# mod_ccg_selection_server("ccg_selection_1")
