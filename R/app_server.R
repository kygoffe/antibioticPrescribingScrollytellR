#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  mod_s07_copd_server("s07_copd_ui_1")
  mod_s08_saba_server("s08_saba_ui_1")
  mod_intro_antibiotic_server("intro_antibiotic_1")
  mod_ccg_antibiotic_server("ccg_antibiotic_1")
  mod_definitions_server("definitions_1")
}
