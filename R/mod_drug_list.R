#' drug_list UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_drug_list_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3_tabstop("Antibiotic"),
    p(
      "Prescribing information is available by antibiotic drug groups: ",
      "amoxicillin, lower UTIs, co-amoxiclav, cephalosporins and ",
      "quinolones. Select a group below to see how the practice of ",
      "interest compares to those within its Sub-ICB region."
    ),
    nhs_card(
      heading = p(
        "Prescribing of",
        textOutput(
          outputId = ns("selected_drug_text"),
          inline = TRUE
        ), " at ",
        textOutput(
          outputId = ns("selected_gp_text"),
          inline = TRUE
        ),
        "in the 12 months to April 2022."
      ),
      nhs_selectInput(
        inputId = ns("drugs"),
        label = "Select group of antibiotic drugs:",
        choices = c(unique(antibioticPrescribingScrollytellR::antibiotic_practice_final$DRUG_OF_INTEREST) %>%
          purrr::discard(
            .p = stringr::str_detect(
              string = .,
              pattern = "Other"
            )
          )),
        full_width = TRUE
      ),
      # Bar chart first
      highcharter::highchartOutput(
        outputId = ns("drug_chart"),
        height = "250px"
      ),
      br(),
      # list of practices in the selected CCG (quintile bar chart)
      highcharter::highchartOutput(
        outputId = ns("drug_trend"),
        height = "300px"
      )
    )
  )
}

#' drug_list Server Functions
#'
#' @noRd
mod_drug_list_server <- function(id, gp_val) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$selected_gp <- reactive({
      req(gp_val())

      antibioticPrescribingScrollytellR::antibiotic_practice_final %>%
        dplyr::filter(YEAR_MONTH == "Apr-22") %>%
        dplyr::filter(PRACTICE_CODE == gp_val()) %>%
        dplyr::distinct(PRACTICE_NAME) %>%
        dplyr::pull()
    })

    output$selected_drug_text <- reactive({
      drug_sel <- input$drugs
      return(drug_sel)
    })

    output$selected_gp_text <- reactive({
      rep(gp_val())

      antibioticPrescribingScrollytellR::antibiotic_practice_final %>%
        dplyr::filter(YEAR_MONTH == "Apr-22") %>%
        dplyr::filter(PRACTICE_CODE == gp_val()) %>%
        dplyr::distinct(PRACTICE_NAME) %>%
        dplyr::pull()
    })
    # observe(print(gp_val()))

    # extract SUB_ICB_CODE from the gp_val()
    sel_sub_icb <- reactive({
      req(gp_val())

      antibioticPrescribingScrollytellR::antibiotic_practice_final %>%
        dplyr::filter(YEAR_MONTH == "Apr-22") %>%
        dplyr::filter(PRACTICE_CODE == gp_val()) %>%
        dplyr::distinct(SUB_ICB_CODE) %>%
        dplyr::pull()
    })

    # observe(print(sel_sub_icb()))


    plot_df <- reactive({
      req(input$drugs)
      req(gp_val())
      req(sel_sub_icb())

      # observe(print(gp_val()))

      dplyr::bind_rows(
        antibioticPrescribingScrollytellR::antibiotic_practice_final %>%
          dplyr::filter(YEAR_MONTH == "Apr-22") %>%
          dplyr::filter(PRACTICE_CODE == gp_val()) %>% # should be the same nameas GP practice name from the prvious section
          dplyr::filter(DRUG_OF_INTEREST == input$drugs) %>% # selected drug
          dplyr::select(YEAR_MONTH, DRUG_OF_INTEREST, TOTAL_ITEMS, STARPU_DENOM, PRACTICE_NAME, STAR_PU) %>%
          dplyr::rename(GEOGRAPHY = PRACTICE_NAME),
        antibioticPrescribingScrollytellR::antibiotic_icb_final %>%
          dplyr::filter(YEAR_MONTH == "Apr-22") %>%
          dplyr::filter(SUB_ICB_CODE == sel_sub_icb()) %>%
          dplyr::filter(DRUG_OF_INTEREST == input$drugs) %>%
          dplyr::select(YEAR_MONTH, DRUG_OF_INTEREST, TOTAL_ITEMS, STARPU_DENOM, SUB_ICB_NAME, STAR_PU) %>%
          dplyr::rename(GEOGRAPHY = SUB_ICB_NAME),
        antibioticPrescribingScrollytellR::antibiotic_eng_final %>%
          dplyr::filter(DRUG_OF_INTEREST == input$drugs) %>%
          dplyr::mutate(
            YEAR_MONTH = "Apr-22",
            GEOGRAPHY = "England",
            STARPU_DENOM = 34111023
          )
      )
    })

    # observe(print(plot_df()))

    # Create first bar chart
    output$drug_chart <- highcharter::renderHighchart({
      export <- list(
        list(
          text = "PNG",
          onclick = highcharter::JS("function () {
                   this.exportChart({ type: 'image/png' }); }")
        ),
        list(
          text = "JPEG",
          onclick = highcharter::JS("function () {
                   this.exportChart({ type: 'image/jpeg' }); }")
        ),
        list(
          text = "SVG",
          onclick = highcharter::JS("function () {
                   this.exportChart({ type: 'image/svg+xml' }); }")
        ),
        list(
          text = "PDF",
          onclick = highcharter::JS("function () {
                   this.exportChart({ type: 'application/pdf' }); }")
        )
      )

      plot_df() %>%
        highcharter::hchart(
          type = "column",
          highcharter::hcaes(
            x = GEOGRAPHY,
            y = STAR_PU
          )
        ) %>%
        theme_nhsbsa() %>%
        highcharter::hc_xAxis(
          title = list(text = "")
        ) %>%
        highcharter::hc_yAxis(
          title = list(text = paste("Items for", input$drugs, "STAR-PUs"))
        ) %>%
        highcharter::hc_tooltip(
          shared = FALSE,
          useHTML = TRUE,
          formatter = htmlwidgets::JS(
            "
              function(){
                outHTML =
                  '<b>' + (this.point.DRUG_OF_INTEREST) + '</b> <br>' +
                  '<b>' + (this.point.GEOGRAPHY) + '</b> <br>' +
                  '<b>' + 'Number of items: </b>' + Highcharts.numberFormat(this.point.TOTAL_ITEMS,0) + '<br>' +
                  '<b>' + 'STAR-PUs for oral antibiotics in April 2022: </b>' + Highcharts.numberFormat(this.point.STARPU_DENOM,0) + '<br>' +
                  '<b>' + 'Items STAR_PU: </b>' + Highcharts.numberFormat(this.point.STAR_PU,3) + '<br>'
                return outHTML
              }
            "
          )
        ) %>%
        highcharter::hc_exporting(
          enabled = TRUE,
          filename = "drug_items",
          buttons = list(
            contextButton = list(
              text = "Export",
              menuItems = export
            )
          )
        )
    })


    output$trend_text <- renderUI({
      tags$text(
        class = "highcharts-caption",
        "Add text here"
      )
    })


    trend_plot_df <- reactive({
      req(gp_val())

      antibioticPrescribingScrollytellR::antibiotic_practice_final %>%
        dplyr::filter(YEAR_MONTH == "Apr-22") %>%
        dplyr::filter(SUB_ICB_CODE == sel_sub_icb()) %>%
        dplyr::mutate(SELECTED = ifelse(test = PRACTICE_CODE == gp_val(), "Y", "N")) %>%
        # Getting a quantile rank
        dplyr::rename(
          GEOGRAPHY = PRACTICE_CODE,
          GEOGRAPHY_NAME = PRACTICE_NAME
        ) %>%
        # Join back pivot wider data
        dplyr::left_join(
          y = antibioticPrescribingScrollytellR::antibiotic_practice_final_pivot_wider,
          by = c("GEOGRAPHY", "GEOGRAPHY_NAME", "DRUG_OF_INTEREST")
        )
    })

    # observe(print(trend_plot_df()))
    # Maximum STAR-PU value for all the practices and CCG but exclude top and bottom 1%

    max_prac_val <- reactive({
      antibioticPrescribingScrollytellR::antibiotic_practice_final %>%
        dplyr::filter(YEAR_MONTH == "Apr-22") %>%
        dplyr::filter(DRUG_OF_INTEREST == input$drugs) %>%
        dplyr::filter(dplyr::between(STAR_PU, quantile(STAR_PU, .01), quantile(STAR_PU, .99))) %>%
        dplyr::summarise(max_val = max(STAR_PU)) %>%
        dplyr::pull()
    })



    # observe(print(trend_plot_df()))

    trend_bar_df <- reactive({
      req(input$drugs)
      trend_plot_df() %>%
        dplyr::filter(DRUG_OF_INTEREST == input$drugs) %>%
        dplyr::mutate(QUINTILE_RANK = dplyr::ntile(STAR_PU, 5)) %>%
        dplyr::mutate(COLOUR = dplyr::case_when(
          QUINTILE_RANK == 1 & SELECTED == "N" ~ "#00A9CE",
          QUINTILE_RANK == 2 & SELECTED == "N" ~ "#41B6E6",
          QUINTILE_RANK == 3 & SELECTED == "N" ~ "#0072CE",
          QUINTILE_RANK == 4 & SELECTED == "N" ~ "#005EB8",
          QUINTILE_RANK == 5 & SELECTED == "N" ~ "#003087",
          SELECTED == "Y" ~ "#ED8B00"
        )) %>%
        dplyr::arrange(STAR_PU)
    })


    output$drug_trend <- highcharter::renderHighchart({
      # req(input$drugs)
      # req(gp_val())

      export <- list(
        list(
          text = "PNG",
          onclick = highcharter::JS("function () {
                   this.exportChart({ type: 'image/png' }); }")
        ),
        list(
          text = "JPEG",
          onclick = highcharter::JS("function () {
                   this.exportChart({ type: 'image/jpeg' }); }")
        ),
        list(
          text = "SVG",
          onclick = highcharter::JS("function () {
                   this.exportChart({ type: 'image/svg+xml' }); }")
        ),
        list(
          text = "PDF",
          onclick = highcharter::JS("function () {
                   this.exportChart({ type: 'application/pdf' }); }")
        )
      )

      trend_bar_df() %>%
        highcharter::hchart(
          type = "column",
          highcharter::hcaes(
            x = GEOGRAPHY,
            y = STAR_PU,
            color = COLOUR
          )
        ) %>%
        theme_nhsbsa(stack = NA) %>%
        highcharter::hc_xAxis(
          title = list(text = "GP surgeries ranked"),
          labels = list(enabled = FALSE)
        ) %>%
        highcharter::hc_yAxis(
          title = list(text = paste(input$drugs, "STAR-PU (12 months to April 2022)")),
          max = max_prac_val()
        ) %>%
        highcharter::hc_tooltip(
          shared = FALSE,
          formatter = htmlwidgets::JS(
            paste0(
              "
            function() {

                outHTML =
                  '<b>' + this.point.DRUG_OF_INTEREST + '</b> <br>' +
                  '<b>Name: </b>' + this.point.GEOGRAPHY_NAME + '<br>' +
                  '<b>Quintile rank: </b>' + this.point.QUINTILE_RANK + '<br>' +
                  '<b>Compare with 12 months to April 2021: </b>' + this.point.CHANGE_DIRECTION + '<br>' +
                  '<b>Number of items: </b>' + Highcharts.numberFormat(this.point.TOTAL_ITEMS,0) + '<br>' +
                  '<b>Items for STAR-PU: </b>' + Highcharts.numberFormat(this.point.STAR_PU,3)
                return outHTML;
            }
            "
            )
          )
        ) %>%
        highcharter::hc_exporting(
          enabled = TRUE,
          filename = "drug_quintile",
          buttons = list(
            contextButton = list(
              text = "Export",
              menuItems = export
            )
          )
        )
    })
  })
}

## To be copied in the UI
# mod_drug_list_ui("drug_list_1")

## To be copied in the server
# mod_drug_list_server("drug_list_1")
