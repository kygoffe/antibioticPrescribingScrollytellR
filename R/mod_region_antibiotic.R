#' ccg_antibiotic UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_region_antibiotic_ui <- function(id) {
  ns <- NS(id)
  tagList(
    nhs_card(
      heading = p(textOutput(outputId = ns("region_text_title"), inline = TRUE)),
      br(),
      # shinyjs::useShinyjs(),
      shiny::htmlOutput(outputId = ns("region_text")), # try to give some sleep() to delay
      br(),
      nhs_selectInput(
        inputId = ns("region"),
        label = "Select region:",
        choices = c("All", sort(unique(antibioticPrescribingScrollytellR::map_df$REGION))),
        full_width = TRUE
      ),
      tags$text(
        class = "highcharts-caption",
        style = "font-size: 9pt",
        "Click map to see annual trend by Sub-ICB location."
      ),
      # map
      highcharter::highchartOutput(
        outputId = ns("map_chart"),
        height = "400px"
      ),
      nhs_grid_2_col(
        # trend chart depends on the clicked geography
        highcharter::highchartOutput(
          outputId = ns("sof_compare"),
          height = "320px"
        ),
        highcharter::highchartOutput(
          outputId = ns("sof_trend"),
          height = "350px"
        )
      )
    ) # ,
    # tags$div(
    #   style = "margin-top: 25vh" # add some buffer space after the chart
    # )
  )
}

#' ccg_antibiotic Server Functions
#'
#' @noRd
mod_region_antibiotic_server <- function(id, metric_sel = metric_sel) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # shinyjs::delay(3000, shinyjs::show("region_text") ) # not sure whether this works

    # add title
    output$region_text_title <- renderText({
      req(metric_sel())
      # observe(print(metric_sel()))

      text <- switch(metric_sel(),
        "STAR_PU" = "Antibacterial items/STAR-PU",
        "COAMOX" = "Co-amoxiclav, Cephalosporins & Quinolones"
      )
      return(text)
    })

    # observe(print(text()))


    output$region_text <- renderText({
      req(metric_sel())

      text <- switch(metric_sel(),
        "STAR_PU" = paste0(
          "Figures are presented for 12 months rolling period to April 2022.", br(), br(),
          "In only three regions did a majority of Sub-ICBs met ",
          "the NHS England & Improvement target. In the East of England, ",
          "only one Sub-ICB met the target."
        ),
        "COAMOX" = paste0(
          "Figures are presented for 12 months rolling period to April 2022.", br(), br(),
          "In all regions, a majority of Sub-ICBs met the NHS England & Improvement target. ",
          "In both the South West and North East & Yorkshire, all Sub-ICBs met the target."
        )
      )
    })

    geography_df <- reactive({
      req(input$region)
      req(metric_sel())

      # Three choices of geographies
      if (input$region == "All") {
        antibioticPrescribingScrollytellR::sub_icb_df %>%
          dplyr::filter(METRIC == metric_sel()) %>%
          dplyr::filter(YEAR_MONTH == "Apr-22")
      } else {
        antibioticPrescribingScrollytellR::sub_icb_df %>%
          dplyr::filter(REGION == input$region) %>%
          dplyr::filter(METRIC == metric_sel()) %>%
          dplyr::filter(YEAR_MONTH == "Apr-22")
      }
    })

    # observe(print(geography_df()))


    map_list <- reactive({
      req(input$region)


      if (input$region == "All") {
        antibioticPrescribingScrollytellR::map_df %>%
          geojsonsf::sf_geojson() %>%
          jsonlite::fromJSON(simplifyVector = FALSE)
      } else {
        antibioticPrescribingScrollytellR::map_df %>%
          dplyr::filter(REGION == input$region) %>%
          geojsonsf::sf_geojson() %>%
          jsonlite::fromJSON(simplifyVector = FALSE)
      }
    })

    # observe(print(map_list()))

    output$map_chart <- highcharter::renderHighchart({
      req(metric_sel())

      # Define export options

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

      if (metric_sel() == "STAR_PU") {
        highcharter::highchart() %>%
          highcharter::hc_add_series_map(
            df = geography_df(),
            map = map_list(),
            joinBy = "SUB_ICB_NAME",
            value = "MEET_TARGET",
            allowPointSelect = TRUE,
            cursur = "pointer",
            borderColor = "black",
            borderWidth = 0.2,
            tooltip = list(
              headerFormat = "",
              pointFormat = paste0(
                "<b> Sub ICB location: </b> {point.SUB_ICB_NAME}<br> <b>",
                switch(metric_sel(),
                  "STAR_PU" = "Antibacterial items/STAR-PU (12 months to {point.YEAR_MONTH}):</b> {point.VALUE:.2f}",
                  "COAMOX" = "Co-amoxiclav, Cephalosporins & Quinolones (12 months to {point.YEAR_MONTH}): </b> {point.VALUE:.2f}%"
                )
              )
            )
          ) %>%
          theme_nhsbsa(palette = "gender") %>%
          highcharter::hc_colorAxis(
            dataClassColor = "category",
            dataClasses = list(
              list(from = 0, to = 0, color = "#41B6E6", name = "Not met the target"), # didn't meet the target (orange)
              list(from = 1, to = 1, color = "#330072", name = "Met the target") # meet the target (blue)
            )
          ) %>%
          # highcharter::hc_legend(enabled = FALSE) %>%
          highcharter::hc_legend(
            enabled = TRUE,
            verticalAlign = "bottom" # ,
            # title = list(text = paste("12 months to", "Apr-22"))
          ) %>%
          highcharter::hc_plotOptions(
            map = list(
              events = list(
                click = htmlwidgets::JS(
                  paste0(
                    "
                function(event) {
                  Shiny.setInputValue('", id, "-mapclick_sof', event.point.SUB_ICB_NAME);
                }
                "
                  )
                )
              )
            )
          ) %>%
          highcharter::hc_exporting(
            enabled = TRUE,
            filename = "region",
            buttons = list(
              contextButton = list(
                text = "Export",
                menuItems = export
              )
            )
          )
      } else {
        highcharter::highchart() %>%
          highcharter::hc_add_series_map(
            df = geography_df(),
            map = map_list(),
            joinBy = "SUB_ICB_NAME",
            value = "MEET_TARGET",
            allowPointSelect = TRUE,
            cursur = "pointer",
            borderColor = "black",
            borderWidth = 0.2,
            tooltip = list(
              headerFormat = "",
              pointFormat = paste0(
                "<b> Sub ICB location: </b> {point.SUB_ICB_NAME}<br> <b>",
                switch(metric_sel(),
                  "STAR_PU" = "Antibacterial items/STAR-PU (12 months to {point.YEAR_MONTH}):</b> {point.VALUE:.2f}",
                  "COAMOX" = "Co-amoxiclav, Cephalosporins & Quinolones (12 months to {point.YEAR_MONTH}): </b> {point.VALUE:.2f}%"
                )
              )
            )
          ) %>%
          theme_nhsbsa(palette = "gender") %>%
          highcharter::hc_colorAxis(
            dataClassColor = "category",
            dataClasses = list(
              list(from = 0, to = 0, color = "#0072CE", name = "Not met the target"), # didn't meet the target (orange)
              list(from = 1, to = 1, color = "#AE2573", name = "Met the target") # meet the target (blue)
            )
          ) %>%
          # highcharter::hc_legend(enabled = FALSE) %>%
          highcharter::hc_legend(
            enabled = TRUE,
            verticalAlign = "bottom" # ,
            # title = list(text = paste("12 months to", "Apr-22"))
          ) %>%
          highcharter::hc_plotOptions(
            map = list(
              events = list(
                click = htmlwidgets::JS(
                  paste0(
                    "
                function(event) {
                  Shiny.setInputValue('", id, "-mapclick_sof', event.point.SUB_ICB_NAME);
                }
                "
                  )
                )
              )
            )
          ) %>%
          highcharter::hc_exporting(
            enabled = TRUE,
            filename = "region",
            buttons = list(
              contextButton = list(
                text = "Export",
                menuItems = export
              )
            )
          )
      }
    })

    # Trend chart
    observeEvent(input$mapclick_sof, {
      output$sof_trend <- highcharter::renderHighchart({
        req(input$mapclick_sof)

        reference_value <- switch(metric_sel(),
          "STAR_PU" = 0.871,
          "COAMOX" = 10
        )

        # add min, max value (28062022)
        max_val <- reactive({
          antibioticPrescribingScrollytellR::sub_icb_df %>%
            dplyr::filter(METRIC == metric_sel()) %>%
            dplyr::filter(YEAR_MONTH == "Apr-22") %>%
            dplyr::summarise(max(VALUE)) %>%
            dplyr::ungroup() %>%
            dplyr::pull()
        })


        # define the dataset for the selected geography
        antibioticPrescribingScrollytellR::sub_icb_df %>%
          dplyr::filter(SUB_ICB_NAME == input$mapclick_sof &
            METRIC == metric_sel() &
            !YEAR_MONTH %in% c("Feb-21", "Mar-21", "Apr-21")) %>%
          dplyr::mutate(
            MEET = ifelse(test = VALUE < reference_value, "Met the target", "Not met the target"),
            METRIC_TOOLTIP = ifelse(METRIC == "STAR_PU", "Antibacterial items/STAR-PU",
              "Proportion of co-amoxiclav, cephalosporin & quinolon items"
            )
          ) %>%
          highcharter::hchart(
            type = "column",
            highcharter::hcaes(
              x = YEAR_MONTH,
              y = VALUE,
              color = colour
            )
          ) %>%
          theme_nhsbsa(palette = "gender") %>%
          highcharter::hc_title(
            text = glue::glue({
              input$mapclick_sof
            })
          ) %>%
          highcharter::hc_yAxis(
            min = 0,
            max = max_val(),
            title = list(
              text = ""
            ),
            plotLines = list(list(
              value = reference_value, color = "#8A1538", width = 1,
              dashStyle = "shortdash"
            )),
            labels = list(format = switch(metric_sel(),
              "STAR_PU" = "{value:.1f}",
              "COAMOX" = "{value:.0f}%"
            ))
          ) %>%
          highcharter::hc_xAxis(
            # categories = antibioticPrescribingScrollytellR::sub_icb_df$YEAR_MONTH,
            title = list(text = "")
          ) %>%
          highcharter::hc_plotOptions(
            series = list(
              # marker = list(enabled = FALSE)
              pointWidth = 8,
              tickInterval = 1
            )
          ) %>%
          highcharter::hc_tooltip(
            shared = FALSE,
            formatter = highcharter::JS(
              "
              function () {
                if(this.point.METRIC == 'STAR_PU'){
                  outHTML =
                    '<b>12 months to: </b>' + this.point.YEAR_MONTH + '<br>' +
                    '<b>Metric: </b>' + this.point.METRIC_TOOLTIP + '<br>' +
                    '<b>Item STAR-PU: </b>' + Highcharts.numberFormat(this.point.y, 2)

                }else{
                outHTML =
                    '<b>12 months to: </b>' + this.point.YEAR_MONTH + '<br>' +
                    '<b>Metric: </b>' + this.point.METRIC_TOOLTIP + '<br>' +
                    '<b>% of Co-amoxiclav, Cephalosporins & Quinolones: </b>' + Highcharts.numberFormat(this.point.y, 1) + '%'
                }

                return outHTML
              }
              "
            )
          )
      })
    })

    # All : show by region number of SOF/CCG met/not met the target
    # selected geography: show each STP/CCG to
    # Take geography_df() and group by count


    geography_compare_df <- reactive({
      antibioticPrescribingScrollytellR::sub_icb_df %>%
        dplyr::filter(METRIC == metric_sel()) %>%
        dplyr::filter(YEAR_MONTH == "Apr-22") %>%
        dplyr::group_by(REGION) %>%
        dplyr::summarise(
          MEET = sum(MEET_TARGET),
          NOT_MEET = dplyr::n() - sum(MEET_TARGET)
        ) %>%
        dplyr::ungroup()
    })

    output$sof_compare <- highcharter::renderHighchart({
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

      if (metric_sel() == "STAR_PU") {
        highcharter::highchart() %>%
          highcharter::hc_chart(type = "bar") %>%
          highcharter::hc_plotOptions(series = list(stacking = "normal")) %>%
          highcharter::hc_xAxis(categories = geography_compare_df()$REGION) %>%
          highcharter::hc_yAxis(title = (list(text = "Sub-ICB count"))) %>%
          highcharter::hc_add_series(
            name = "Antibacterial items/STAR-PU greater than 0.871(Not met target)",
            data = geography_compare_df()$NOT_MEET,
            color = "#0072CE"
          ) %>%
          highcharter::hc_add_series(
            name = "Antibacterial items/STAR-PU 0.871 or less (Met target)",
            data = geography_compare_df()$MEET,
            color = "#330072"
          ) %>%
          theme_nhsbsa() %>%
          highcharter::hc_legend(enabled = FALSE) %>%
          highcharter::hc_tooltip(
            shared = TRUE
          ) %>%
          highcharter::hc_exporting(
            enabled = TRUE,
            filename = "region",
            buttons = list(
              contextButton = list(
                text = "Export",
                menuItems = export
              )
            )
          )
      } else {
        highcharter::highchart() %>%
          highcharter::hc_chart(type = "bar") %>%
          highcharter::hc_plotOptions(series = list(stacking = "normal")) %>%
          highcharter::hc_xAxis(categories = geography_compare_df()$REGION) %>%
          highcharter::hc_yAxis(title = (list(text = "Sub-ICB count"))) %>%
          highcharter::hc_add_series(
            name = "Co-amoxiclav, Cephalosporins & Quinolones greater than 10% (Not met target)",
            data = geography_compare_df()$NOT_MEET,
            color = "#0072CE"
          ) %>%
          highcharter::hc_add_series(
            name = "Co-amoxiclav, Cephalosporins & Quinolones 10% or less (Met target)",
            data = geography_compare_df()$MEET,
            color = "#AE2573"
          ) %>%
          theme_nhsbsa() %>%
          highcharter::hc_legend(enabled = FALSE) %>%
          highcharter::hc_tooltip(
            shared = TRUE
          ) %>%
          highcharter::hc_exporting(
            enabled = TRUE,
            filename = "region",
            buttons = list(
              contextButton = list(
                text = "Export",
                menuItems = export
              )
            )
          )
      }
    })


    # extract list of CCG

    ccg_sel <- reactive({
      if (input$region == "All") {
        antibioticPrescribingScrollytellR::sub_icb_df %>%
          dplyr::distinct(SUB_ICB_NAME) %>%
          dplyr::pull()
      } else {
        antibioticPrescribingScrollytellR::sub_icb_df %>%
          dplyr::filter(REGION == input$region) %>%
          dplyr::distinct(SUB_ICB_NAME) %>%
          dplyr::pull()
      }
    })

    return(ccg_sel)
  })
}

## To be copied in the UI
# mod_region_antibiotic_ui("region_antibiotic_1")

## To be copied in the server
# mod_region_antibiotic_server("region_antibiotic_1")
