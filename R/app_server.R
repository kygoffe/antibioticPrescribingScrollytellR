#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  metric_sel <- mod_intro_antibiotic_server("intro_antibiotic_1")
  mod_intro_antibiotic_server("intro_antibiotic_1")
  mod_region_antibiotic_server("region_antibiotic_1", metric_sel)
  mod_ccg_selection_server("ccg_selection_1")
  ccg_sel <- mod_ccg_selection_server("ccg_selection_1")
  mod_gp_overall_server("gp_overall_1", metric_sel, ccg_sel)
  gp_val <- mod_gp_overall_server("gp_overall_1", metric_sel, ccg_sel)
  mod_drug_list_server("drug_list_1", gp_val)
  # mod_nhs_region_trend_server("nhs_region_trend_1")
  mod_definitions_server("definitions_1")
}
