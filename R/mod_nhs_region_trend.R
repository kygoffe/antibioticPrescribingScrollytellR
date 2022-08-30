#' nhs_region_trend UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_nhs_region_trend_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2_tabstop("AMR long term trends"),
    p(
      "Antimicrobial resistance (AMR) is one of the biggest threats to global public health. ",
      "NHS antimicrobial activity since COVID-19 pandemic.",
      "The first visualisation shows the total number of items in the seven NHS England regions"
    ),
    p(
      "add text here"
    ),
    nhs_card(
      heading = "Number of monthly antibiotic items (BNF 5.1) prescribed by NHS region",
      nhs_grid_2_col(
        nhs_selectInput(
          inputId = ns("region"),
          label = "Region",
          choices = c("All", unique(nhs_region$REGION)),
          full_width = TRUE
        ),
        nhs_selectInput(
          inputId = ns("age_band"),
          label = "Age band",
          choices = c("All", unique(nhs_region$AGE_BAND)) %>%
            purrr::discard(
              .p = stringr::str_detect(
                string = .,
                pattern = "Unknown"
              )
            ),
          full_width = TRUE
        )
      ),
      highcharter::highchartOutput(
        outputId = ns("nhs_region_trend"),
        height = "500px"
      ),
      mod_nhs_download_ui(
        id = ns("download_trend_nhs_region")
      )
    ),
    tags$div(
      style = "margin-top: 25vh" # add some buffer space after the chart
    )
  )
}

#' nhs_region_trend Server Functions
#'
#' @noRd
mod_nhs_region_trend_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    nhs_region_df <- reactive({
      req(input$age_band)
      req(input$region)
      if (input$region == "All" & input$age_band == "All") {
        antibioticPrescribingScrollytellR::nhs_region %>%
          dplyr::filter(YEAR_MONTH != 202003) %>%
          dplyr::group_by(YEAR_MONTH, REGION) %>%
          dplyr::summarise(ITEMS = sum(ITEMS)) %>%
          dplyr::ungroup()
      } else if (input$region != "All" & input$age_band == "All") {
        antibioticPrescribingScrollytellR::nhs_region %>%
          dplyr::filter(REGION == input$region) %>%
          dplyr::filter(YEAR_MONTH != 202003) %>%
          dplyr::group_by(YEAR_MONTH, REGION) %>%
          dplyr::summarise(ITEMS = sum(ITEMS)) %>%
          dplyr::ungroup()
      } else if (input$region == "All" & input$age_band != "All") {
        antibioticPrescribingScrollytellR::nhs_region %>%
          dplyr::filter(AGE_BAND == input$age_band) %>%
          dplyr::filter(YEAR_MONTH != 202003) %>%
          dplyr::group_by(YEAR_MONTH, REGION) %>%
          dplyr::summarise(ITEMS = sum(ITEMS)) %>%
          dplyr::ungroup()
      } else {
        antibioticPrescribingScrollytellR::nhs_region %>%
          dplyr::filter(AGE_BAND == input$age_band) %>%
          dplyr::filter(REGION == input$region) %>%
          dplyr::filter(YEAR_MONTH != 202003)
      }
    })
    
    output$nhs_region_trend <- highcharter::renderHighchart({
      # 20062022 - need to add line chart (similar to LIS chart)
      # Data processing
      nhs_region_df() %>%
        dplyr::mutate(YEAR_MONTH = as.character(YEAR_MONTH)) %>%
        highcharter::hchart(
          type = "line",
          highcharter::hcaes(
            x = YEAR_MONTH,
            y = ITEMS,
            group = REGION
          )
        ) %>%
        theme_nhsbsa(stack = NA) %>%
        highcharter::hc_yAxis(
          title = list(text = "Number of items")
        ) %>%
        highcharter::hc_xAxis(
          title = list(text = "Year month"),
          plotBands = list(
            list(
              label = list(
                text = "First lockdown",
                align = "middle",
                rotation = 90
              ),
              color = "rgba(100, 0, 0, 0.1)",
              from = 0,
              to = 2
            ),
            list(
              label = list(
                text = "Second lockdown",
                align = "middle",
                rotation = 90
              ),
              color = "rgba(100,0,0,0.1)",
              from = 6,
              to = 7
            ),
            list(
              label = list(
                text = "Third lockdown",
                align = "middle",
                rotation = 90
              ),
              color = "rgba(100,0,0,0.1)",
              from = 8,
              to = 10.5
            )
          )
        ) %>%
        highcharter::hc_tooltip(
          shared = TRUE,
          valueDecimals = 0
        )
    })
  })
}

## To be copied in the UI
# mod_nhs_region_trend_ui("nhs_region_trend_1")

## To be copied in the server
# mod_nhs_region_trend_server("nhs_region_trend_1")