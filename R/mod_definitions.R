#' definitions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_definitions_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2_tabstop("Definitions"),
    p(tags$b("List of drugs used to define Amoxicillin, UTI, Co-amoxiclav, cephalosporin & quinolone")),
    p(
      "Amoxicillin",
      tags$ul(
        tags$li("Amoxicillin (0501013B0)")
      )
    ),
    p(
      "UTI",
      tags$li("Pivmecillinam hydrochloride (0501015P0)"),
      tags$li("Nitrofurantoin (0501130R0)"),
      tags$li("Trimethoprim (0501080W0)"),
      tags$li("Fosfomycin trometamol (0501070AE)"),
      tags$li("Fosfomycin calcium (0501130S0)")
    ),
    p(
      "Co-amoxiclav",
      tags$li("Amoxicillin/clavulanic acid) (0501013K0)"),
    ),
    p(
      "Cephalosporin",
      tags$li("Cefaclor (0501021A0)"),
      tags$li("Cefadroxil (0501021B0)"),
      tags$li("Cefixime (0501021C0)"),
      tags$li("Cefotaxime sodium (0501021D0)"),
      tags$li("Ceftriaxone sodium (0501021G0)"),
      tags$li("Cefuroxime sodium (0501021J0)"),
      tags$li("Cefuroxime axetil (0501021K0)"),
      tags$li("Cefalexin (0501021L0)"),
      tags$li("Cefradine (0501021M0)")
    ),
    p(
      "Quinolones",
      tags$li("Ciprofloxacin (0501120L0)"),
      tags$li("Ofloxacin (0501120P0)"),
      tags$li("Norfloxacin (0501120Q0)"),
      tags$li("Levofloxacin (0501120X0)"),
      tags$li("Moxifloxacin (0501120Y0)")
    )
  )
}

#' definitions Server Functions
#'
#' @noRd
mod_definitions_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_definitions_ui("definitions_ui_1")

## To be copied in the server
# mod_definitions_server("definitions_ui_1")
