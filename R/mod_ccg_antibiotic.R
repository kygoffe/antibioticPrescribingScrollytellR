#' ccg_antibiotic UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ccg_antibiotic_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2_tabstop("CCG profile"),
    p("The map and chart show anttibiotic prescribing SOF metrics by CCG."),

    # Chart: Select by Region as there are too many CCGs, click the CCG map
    # Show two charts; England, STP as reference line

    nhs_card(
      heading = "CCG explore",
      # two drop down menu
      nhs_grid_2_col(
        nhs_selectInput(
          inputId = ns("region"),
          label = "Region",
          choices = c(sort(unique(ccg$REGION[ccg$REGION != "England"]))),
          full_width = TRUE
        ),
        nhs_selectInput(
          inputId = ns("metric"),
          label = "Metric",
          choices = c(
            "Antibacterial items/STAR PU" = "ind44a",
            "Co-amoxiclav, Cephalosporins & Quinolones" = "ind46b"
          )
        )
      ),
      # column chart to show how many CCG meet the target/not meet the target
      highcharter::highchartOutput(
        outputId = ns("region_ccg_count"),
        height = "300px"
      ),
      nhs_grid_2_col(
        # CCG map to show the distribution.
        highcharter::highchartOutput(
          outputId = ns("map_region_ccg"),
          height = "300px"
        ),
        # selected CCG trend
        highcharter::highchartOutput(
          outputId = ns("ccg_trend"),
          height = "300px"
        )
      ),
      shiny::htmlOutput(ns("ccg_comment"))
    ),
    p("Dynamic text about the selected CCG"),
    nhs_card(
      highcharter::highchartOutput(
        outputId = ns("gp_count"),
        height = "400px"
      ),
      highcharter::highchartOutput(
        outputId = ns("gp_imd"),
        height = "400px"
      )
    )
  )
}

#' ccg_antibiotic Server Functions
#'
#' @noRd
mod_ccg_antibiotic_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_ccg_antibiotic_ui("ccg_antibiotic_1")

## To be copied in the server
# mod_ccg_antibiotic_server("ccg_antibiotic_1")
