#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Need this for accessibility
    tags$html(lang = "en"),
    bootstrapLib(),

    # Create skip link to jump to the main content
    tags$a(id = "skiplink", "Skip to Main Content", href = "#maincontent"),
    tags$style(HTML("
      #skiplink {
        position: absolute;
        transform: translateY(-100%);
      }
      #skiplink:focus {
      transform: translateY(0%);
      background-color: lightyellow;
      padding: 20px;
      z-index: 9999;
      }")),

    # Your application UI logic
    nhs_header(),
    br(),
    tags$div(
      class = "nhsuk-width-container",
      tags$div(
        class = "nhsuk-main-wrapper",
        id = "maincontent",
        role = "main",
        h1("Antibiotic prescribing in primary care in England"),
        hr(),
        nhs_navlistPanel(
          well = TRUE,
          widths = c(3, 9),
          selected = "Article",
          fluid = FALSE,
          tabPanel(
            title = "Article",
            mod_intro_antibiotic_ui("intro_antibiotic_1"),
            mod_region_antibiotic_ui("region_antibiotic_1"),
            mod_ccg_selection_ui("ccg_selection_1"),
            mod_gp_overall_ui("gp_overall_1"),
            mod_drug_list_ui("drug_list_1")

            # mod_nhs_region_trend_ui("nhs_region_trend_1"),
            # New try
          ),
          tabPanel(
            title = "Definitions",
            mod_definitions_ui("definitions_ui_1")
          )
        ),
        # reset vertical scroll bar when clicking tabpanel link
        # code cribbed from https://stackoverflow.com/questions/44686681/r-shiny-tabpanel-does-not-automatically-got-to-top-of-page-when-clicked
        tags$script("$(document).ready(function () {
          $('#maincontent a[data-toggle=\"tab\"]').on('click', function (e) {
          window.scrollTo(0, 0)});
          });")
      )
    ),
    br(),
    nhs_footer()
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www", app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Antibiotic prescribing in primary care in England"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
