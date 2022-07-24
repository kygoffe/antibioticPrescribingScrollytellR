#' intro_antibiotic UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_intro_antibiotic_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p(
      "Antimicrobial resistance is a focal topic of the ",
      enurl(
        text = "NHS Long Term Plan.",
        url = "https://www.longtermplan.nhs.uk/"
      ),
      "It sets out the need to optimise use, reduce the need for ",
      "and unintentional exposure to antibiotics, as well as supporting ",
      "the development of new antimicrobials. ",
      "Critical to support these objectives is monitoring and illustrating ",
      "the trends in antibiotic prescribing. "
    ),
    br(),
    p(
      "This visualisation tool guides you through the main metrics used for ",
      "monitoring antibiotic prescribing, enabling you to drill-down view the ",
      "geographical trends from region to general practice level. ",
      "Please check the glossary tab for any unfamiliar terms."
    ),
    br(),
    h3_tabstop("Medicines Optimisation Comparators"),
    p(
      "The NHS adopts a ",
      enurl(
        text = "medicines optimisation approach ",
        url = "https://www.england.nhs.uk/medicines-2/medicines-optimisation/"
      ),
      "to assess the value that medicines deliver, in both clinical-effectiveness ",
      "and cost-effectiveness. For antibiotics, the ",
      enurl(
        text = "two metrics ",
        url = "https://www.nhsbsa.nhs.uk/sites/default/files/2018-04/Master%20Specifications%20March%202018.pdf"
      ),
      "are: ",
      tags$ul(
        tags$li(
          tags$b("Antibacterial items per STAR-PU, "),
          "a population stanardised for age and gender measure of number of prescription items for antibacterial drugs per ",
          "antibacterial item. The target for each SICBL is to be equal or below value ",
          "of 0.871 items per STAR-PU."
        ),
        tags$li(
          tags$b("Co-amoxiclav, Cephalosporins and Quinolones % items, "),
          "the number of prescription items for co-amoxiclav, cephalosporins and quinolones (antibiotic drugs) ",
          "as a percentage of the total number of prescription items for selected antibacterial drugs. ",
          "The target to reach of each SICBL is to be 10% or below."
        )
      )
    ),
    nhs_card(
      nhs_selectInput(
        inputId = ns("metric"),
        label = "Select the antibiotic comparator of interest to see the geographical variation:",
        choices = c(
          "Antibacterial items/STAR-PU" = "STAR_PU",
          "Co-amoxiclav, Cephalosporins & Quinolones" = "COAMOX"
        ),
        full_width = TRUE
      )
    )
  )
}

#' intro_antibiotic Server Functions
#'
#' @noRd
mod_intro_antibiotic_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    metric_sel <- reactive(input$metric)
    return(metric_sel)
  })
}

## To be copied in the UI
# mod_intro_antibiotic_ui("intro_antibiotic_1")

## To be copied in the server
# mod_intro_antibiotic_server("intro_antibiotic_1")
