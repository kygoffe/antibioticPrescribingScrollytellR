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
    h3_tabstop("Sub Integrated Care Board"),
    p(
      "NHS Sub-Integrated Care Boards (Sub-ICBs) have the responsibility ",
      "for management for healthcare provision within a given area. ",
      "Hover over the map above to identify the Sub-ICB of interest."
    ),
    br(),
    nhs_card(
      nhs_selectInput(
        inputId = ns("ccg"),
        label = "Select Sub-ICB:",
        # choices = c(sort(unique(antibioticPrescribingScrollytellR::gp_merge_df$SUB_ICB_NAME))),
        choices = NULL,
        full_width = TRUE
      )
    )
  )
}

#' ccg_selection Server Functions
#'
#' @noRd
mod_ccg_selection_server <- function(id, ccg_sel) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ccg_sel <- ccg_list
    # observe(print(paste0(ccg_sel(),"ccg_sel")))

    icb_list <- reactive({
      req(ccg_sel())
      antibioticPrescribingScrollytellR::gp_merge_df %>%
        dplyr::filter(SUB_ICB_NAME %in% ccg_sel()) %>%
        dplyr::distinct(SUB_ICB_NAME)
    })

    #



    # fill the name of CCG
    observeEvent(
      eventExpr = icb_list(),
      handlerExpr = {
        freezeReactiveValue(input, "ccg")
        updateSelectInput(
          inputId = "ccg",
          choices =
            unique(icb_list()$SUB_ICB_NAME) %>%
              na.omit() %>%
              sort()
        )
      }
    )

    ccg_selected <- reactive({
      input$ccg
    })

    return(ccg_selected)
  })
}

## To be copied in the UI
# mod_ccg_selection_ui("ccg_selection_1")

## To be copied in the server
# mod_ccg_selection_server("ccg_selection_1")
