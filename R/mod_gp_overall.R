#' gp_overall UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_gp_overall_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2_tabstop("How has antibiotic prescribing changed in your GP practice?"),
    p(
      "In this section, user can select their GP practice and compare with sub ICB and England.",
      "Comment on their performance (lower than sub ICB/ England)",
      " Top 20% bottom 20% etc (split GP practice in quintiles?)",
      "using bold, colour to highlight the key message in the paragraph"
    ),
    nhs_card(
      heading = "Antimicrobial Stewardship data reporting against NHS AMR metrics",

      # requires three dropdown memu
      nhs_grid_3_col(
        nhs_selectInput(
          inputId = ns("metric"),
          label = "Metric",
          choices = c(
            "Antibacterial items/STAR PU" = "STAR_PU",
            "Co-amoxiclav, Cephalosporins & Quinolones" = "COAMOX"
          ),
          full_width = TRUE
        ),
        nhs_selectInput(
          inputId = ns("ccg"),
          label = "Sub ICB/CCG",
          choices = c(sort(unique(antibioticPrescribingScrollytellR::sub_icb_df$SUB_ICB))),
          full_width = TRUE
        ),
        nhs_selectInput(
          inputId = ns("gp"),
          label = "GP practice",
          choices = NULL, # dynamic
          full_width = TRUE
        )
      ),
      shiny::htmlOutput(ns("bar_chart_text")),
      # bar chart
      highcharter::highchartOutput(
        outputId = ns("bar_chart"),
        height = "400px"
      ),
      shiny::htmlOutput(ns("scatter_chart_text")),
      # scatterplot - practice starpu vs imd rank
      highcharter::highchartOutput(
        outputId = ns("scatter_chart"),
        height = "400px"
      ),
      shiny::htmlOutput(ns("trend_chart_text")),
      # trend chart
      highcharter::highchartOutput(
        outputId = ns("trend_chart"),
        height = "400px"
      )
    ),
    tags$div(
      style = "margin-top: 25vh" # add some buffer space after the chart
    )
  )
}

#' gp_overall Server Functions
#'
#' @noRd
mod_gp_overall_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # generate column chart data frame
    gp_sel <- reactive({
      req(input$metric)
      # req(input$ccg)
      antibioticPrescribingScrollytellR::gp_merge_df %>%
        dplyr::filter(YEAR_MONTH %in% "Apr-22") %>%
        dplyr::filter(SUB_ICB %in% input$ccg) %>%
        dplyr::filter(METRIC %in% input$metric) %>%
        dplyr::mutate(IMD_RANK = as.numeric(IMD_RANK))
    })


    gp_list <- reactive({
      # req(input$metric)
      req(input$ccg)
      antibioticPrescribingScrollytellR::gp_merge_df %>%
        dplyr::filter(YEAR_MONTH %in% "Apr-22") %>%
        dplyr::filter(SUB_ICB %in% input$ccg)
    })

    # fill the name of GP
    observeEvent(
      eventExpr = gp_list(),
      handlerExpr = {
        freezeReactiveValue(input, "gp")
        updateSelectInput(
          inputId = "gp",
          choices =
            unique(gp_list()$PRACTICE) %>%
              na.omit() %>%
              unique()
        )
      }
    )

    # filter data based on practice

    gp_df <- reactive({
      req(input$metric)
      req(input$ccg)
      req(input$gp)

      gp_sel() %>%
        dplyr::filter(PRACTICE %in% input$gp) %>%
        dplyr::select(YEAR_MONTH,
          GEOGRAPHY = PRACTICE,
          VALUE
        )
    })

    ccg_df <- reactive({
      req(input$metric)
      req(input$ccg)

      antibioticPrescribingScrollytellR::sub_icb_df %>%
        dplyr::filter(YEAR_MONTH %in% "Apr-22") %>%
        dplyr::filter(METRIC %in% input$metric) %>%
        dplyr::filter(SUB_ICB %in% input$ccg) %>%
        dplyr::select(YEAR_MONTH,
          GEOGRAPHY = SUB_ICB,
          VALUE
        )
    })

    eng_df <- reactive({
      req(input$metric)
      antibioticPrescribingScrollytellR::df_eng_pivot %>%
        dplyr::filter(YEAR_MONTH %in% "Apr-22") %>%
        dplyr::filter(METRIC %in% input$metric) %>%
        dplyr::select(-METRIC) %>%
        dplyr::mutate(GEOGRAPHY = "England")
    })

    # bind them
    plot_df <- reactive({
      dplyr::bind_rows(gp_df(), ccg_df(), eng_df())
    })


    reference_value <- reactive({
      switch(input$metric,
        "STAR_PU" = 0.87,
        "COAMOX" = 10
      )
    })

    max_val <- reactive({
      antibioticPrescribingScrollytellR::gp_merge_df %>%
        dplyr::filter(METRIC %in% input$metric) %>%
        dplyr::filter(dplyr::between(VALUE, quantile(VALUE, .01), quantile(VALUE, .99))) %>%
        dplyr::summarise(max(VALUE)) %>%
        dplyr::ungroup() %>%
        dplyr::pull()
    })

    observe(print(max_val()))


    # Create bar chart

    output$bar_chart <- highcharter::renderHighchart({
      validate(
        need(nrow(plot_df()) > 0, message = FALSE)
      ) # stopping to show temporary error message.

      plot_df() %>%
        highcharter::hchart(
          type = "column",
          highcharter::hcaes(
            x = GEOGRAPHY,
            y = VALUE
          )
        ) %>%
        theme_nhsbsa() %>%
        highcharter::hc_xAxis(
          title = list(text = "12 months to:")
        ) %>%
        highcharter::hc_yAxis(
          min = 0,
          max = max_val(),
          title = list(text = switch(input$metric,
            "STAR_PU" = "Items/STAR PU",
            "COAMOX" = "% of items"
          )),
          plotLines = list(list(
            value = reference_value(), color = "#8A1538", width = 1,
            dashStyle = "shortdash"
          ))
        ) %>%
        highcharter::hc_tooltip(
          shared = TRUE,
          pointFormat = switch(input$metric,
            "STAR_PU" = "<b>STAR_PU: {point.y:.2f}</b>",
            "COAMOX" = "<b>{point.y:.1f}%</b>"
          )
        )
    })

    # bar chart text

    output$bar_chart_text <- renderUI({
      tags$text(
        class = "highcharts-caption",
        switch(input$metric,
          "STAR_PU" = "12 months to April 2022, add text for the bar chart",
          "COAMOX" = "12 months to April 2022, add text for the bar chart"
        )
      )
    })


    # data for scatterplot

    observe(print(gp_sel()$IMD_RANK)) # list of GPs in the selected CCG


    # Define IMD tooltip text
    lsoa_metric_text <- reactive({
      if (input$metric == "STAR_PU") {
        paste0(
          "<b>Item STAR_PU:</b> {point.VALUE:,.2f}"
        )
      } else {
        paste0(
          "<b>Co-amoxiclav, Cephalosporins & Quinolones:</b> {point.VALUE:,.1f}%"
        )
      }
    })


    # Tooltip text
    gp_tooltip_text <- reactive({
      paste0(
        "<b>{point.PRACTICE}</b> <br>",
        "<b>SUB ICB/CCG:</b> {point.SUB_ICB} <br>",
        "<b>IMD Decile:</b> {point.IMD_DECILE} <br>",
        "<b>2019 IMD rank (out of 32,844):</b> {point.IMD_RANK:,.0f} <br>",
        lsoa_metric_text()
      )
    })


    # create chart object
    output$scatter_chart <- highcharter::renderHighchart({
      req(input$gp)

      gp_scatter_chart <- highcharter::highchart()

      # highlight different colour for the selected practice

      gp_scatter_chart <- gp_scatter_chart %>%
        highcharter::hc_add_series(
          data = gp_sel() %>%
            dplyr::filter(PRACTICE == input$gp),
          type = "scatter",
          highcharter::hcaes(
            x = IMD_RANK,
            y = VALUE
          ),
          name = "Selected practice",
          showInLegend = TRUE,
          marker = list(symbol = "circle", fillColor = "#003087")
        )

      # add remaining practices in the selected CCG
      gp_scatter_chart <- gp_scatter_chart %>%
        highcharter::hc_add_series(
          data = gp_sel() %>%
            dplyr::filter(!PRACTICE == input$gp),
          type = "scatter",
          highcharter::hcaes(
            x = IMD_RANK,
            y = VALUE
          ),
          name = "Other practices",
          showInLegend = TRUE,
          marker = list(symbol = "circle", fillColor = "#ED8B00")
        ) %>%
        highcharter::hc_xAxis(
          min = 0,
          max = 35000, # Pad to ensure we can see the final label
          categories = c("Most<br>deprived", rep(NA, 34999), "Least<br>deprived"),
          tickInterval = 35000,
          title = list(text = "Deprivation rank (based on practice location)"),
          endOnTick = TRUE
        ) %>%
        highcharter::hc_yAxis(
          min = 0,
          max = max_val(),
          title = list(text = switch(input$metric,
            "STAR_PU" = "Items/STAR PU",
            "COAMOX" = "% of items"
          )),
          plotLines = list(list(
            value = reference_value(), color = "#8A1538", width = 1,
            dashStyle = "shortdash"
          ))
        ) %>%
        theme_nhsbsa(stack = NA) %>%
        highcharter::hc_tooltip(
          headerFormat = "",
          pointFormat = gp_tooltip_text()
        )
    })

    # scatterplot chart text



    # data for trend chart

    gp_trend <- reactive({
      req(input$metric)
      req(input$gp)

      antibioticPrescribingScrollytellR::gp_merge_df %>%
        dplyr::filter(METRIC %in% input$metric) %>%
        dplyr::filter(PRACTICE %in% input$gp) %>%
        dplyr::select(
          YEAR_MONTH,
          GEOGRAPHY = PRACTICE,
          VALUE
        ) %>%
        dplyr::mutate(
          GEOGRAPHY_TYPE = "GP"
        )
    })

    ccg_trend <- reactive({
      req(input$metric)
      req(input$ccg)

      antibioticPrescribingScrollytellR::sub_icb_df %>%
        dplyr::filter(METRIC %in% input$metric) %>%
        dplyr::filter(SUB_ICB %in% input$ccg) %>%
        dplyr::select(
          YEAR_MONTH,
          GEOGRAPHY = SUB_ICB,
          VALUE
        ) %>%
        dplyr::mutate(GEOGRAPHY_TYPE = "Sub ICB/CCG")
    })

    # observe(print(ccg_trend()))

    eng_trend <- reactive({
      req(input$metric)

      antibioticPrescribingScrollytellR::df_eng_pivot %>%
        dplyr::filter(METRIC %in% input$metric) %>%
        dplyr::mutate(
          GEOGRAPHY = "England",
          GEOGRAPHY_TYPE = "England"
        )
    })


    plot_trend_df <- reactive({
      dplyr::bind_rows(gp_trend(), ccg_trend(), eng_trend())
    })

    # observe(print(plot_trend_df()))

    output$trend_chart <- highcharter::renderHighchart({
      validate(
        need(nrow(plot_trend_df()) > 0, message = FALSE)
      ) # stopping to show temporary error message.

      plot_trend_df() %>%
        highcharter::hchart(
          type = "line",
          highcharter::hcaes(
            x = YEAR_MONTH,
            y = VALUE,
            group = GEOGRAPHY_TYPE
          )
        ) %>%
        theme_nhsbsa(stack = NA) %>%
        highcharter::hc_tooltip(
          shared = TRUE,
          pointFormat = switch(input$metric,
            "STAR_PU" = "<b>{point.GEOGRAPHY}: {point.y:.2f} </b> <br>",
            "COAMOX" = "<b>{point.GEOGRAPHY}: {point.y:.1f}%</b> <br> "
          )
        ) %>%
        highcharter::hc_yAxis(
          min = 0,
          max = max_val(),
          title = list(text = switch(input$metric,
            "STAR_PU" = "Items/STAR PU",
            "COAMOX" = "% of items"
          )),
          plotLines = list(list(
            value = reference_value(), color = "#8A1538", width = 1,
            dashStyle = "shortdash"
          ))
        ) %>%
        highcharter::hc_xAxis(
          title = list(text = "12 months to:")
        )
    })

    # trend chart text

    output$trend_chart_text <- renderUI({
      tags$text(
        class = "highcharts-caption",
        switch(input$metric,
          "STAR_PU" = "add text for the trend chart",
          "COAMOX" = "add text for the trend chart"
        )
      )
    })
  })
}

## To be copied in the UI
# mod_gp_overall_ui("gp_overall_1")

## To be copied in the server
# mod_gp_overall_server("gp_overall_1")
