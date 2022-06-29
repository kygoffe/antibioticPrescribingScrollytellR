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
    h2_tabstop("Breakdown by geography"),
    p("The map and chart show antibiotic prescribing SOF metrics by selected region and drill down to CCG level."),
    p("TEXT WILL BE ADDED"),
    # Chart: Select by Region as there are too many CCGs, click the CCG map
    # Show two charts; England, STP as reference line

    nhs_card(
      heading = "Antimicrobial Stewardship data reporting against NHS AMR metrics",
      # two drop down menu
      nhs_grid_3_col(
        nhs_selectInput(
          inputId = ns("metric"),
          label = "Metric",
          choices = c(
            "Antibacterial items/STAR PU" = "STARPU",
            "Co-amoxiclav, Cephalosporins & Quinolones" = "COAMOX"
          )
        ),
        nhs_selectInput(
          inputId = ns("region"),
          label = "Region",
          choices = c("All", sort(unique(antibioticPrescribingScrollytellR::region_stp_ccg_lookup$REGION))),
          full_width = TRUE
        ),
        nhs_selectInput(
          inputId = ns("stp_ccg_sel"),
          label = "Geography",
          choices = c(
            "STP/ICS" = "STP",
            "CCG" = "CCG"
          ),
          # selected = "STP/ICS",
          full_width = TRUE
        )
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
          height = "300px"
        ),
        highcharter::highchartOutput(
          outputId = ns("sof_trend"),
          height = "300px"
        )
      ),
      tags$text(
        class = "highcharts-caption",
        style = "font-size: 9pt",
        "Selected 12 months rolling period to March 2022. Click map to see trend by selected geography."),
      mod_nhs_download_ui(id = ns("map_sof_download"))
    ),
    tags$div(
      style = "margin-top: 25vh" # add some buffer space after the chart
    )
  )
}

#' ccg_antibiotic Server Functions
#'
#' @noRd
mod_region_antibiotic_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    geography_df <- reactive({
      req(input$region)
      req(input$stp_ccg_sel)
      req(input$metric)

      # Three choices of geographies
      if (input$region == "All" & input$stp_ccg_sel == "STP") {
        antibioticPrescribingScrollytellR::merge_df %>%
          dplyr::filter(REGION != "England") %>%
          dplyr::filter(SUB_GEOGRAPHY_TYPE == "STP") %>%
          dplyr::filter(METRIC == input$metric) %>%
          dplyr::filter(YEAR_MONTH == "Mar_22")
      } else if (input$region == "All" & input$stp_ccg_sel == "CCG") {
        antibioticPrescribingScrollytellR::merge_df %>%
          dplyr::filter(REGION != "England") %>%
          dplyr::filter(SUB_GEOGRAPHY_TYPE == "CCG") %>%
          dplyr::filter(METRIC == input$metric) %>%
          dplyr::filter(YEAR_MONTH == "Mar_22")
      } else if (input$region != "All" & input$stp_ccg_sel == "STP") {
        antibioticPrescribingScrollytellR::merge_df %>%
          dplyr::filter(REGION == input$region) %>%
          dplyr::filter(SUB_GEOGRAPHY_TYPE == "STP") %>%
          dplyr::filter(METRIC == input$metric) %>%
          dplyr::filter(YEAR_MONTH == "Mar_22")
      } else {
        antibioticPrescribingScrollytellR::merge_df %>%
          dplyr::filter(REGION == input$region) %>%
          dplyr::filter(SUB_GEOGRAPHY_TYPE == "CCG") %>%
          dplyr::filter(METRIC == input$metric) %>%
          dplyr::filter(YEAR_MONTH == "Mar_22")
      }
    })

    # observe(print(geography_df()))


    map_list <- reactive({
      req(input$region)
      req(input$stp_ccg_sel)

      if (input$region == "All") {
        antibioticPrescribingScrollytellR::map_df %>%
          dplyr::filter(GEOGRAPHY == input$stp_ccg_sel) %>%
          geojsonsf::sf_geojson() %>%
          jsonlite::fromJSON(simplifyVector = FALSE)
      } else {
        antibioticPrescribingScrollytellR::map_df %>%
          dplyr::filter(GEOGRAPHY == input$stp_ccg_sel & REGION == input$region) %>%
          geojsonsf::sf_geojson() %>%
          jsonlite::fromJSON(simplifyVector = FALSE)
      }
    })

    output$map_chart <- highcharter::renderHighchart({
      req(input$metric)
      req(input$stp_ccg_sel)

      highcharter::highchart() %>%
        highcharter::hc_add_series_map(
          df = geography_df(),
          map = map_list(),
          joinBy = "SUB_GEOGRAPHY_NAME",
          value = "MEET_TARGET",
          allowPointSelect = TRUE,
          cursur = "pointer",
          borderColor = "black",
          borderWidth = 0.2,
          tooltip = list(
            headerFormat = "",
            pointFormat = paste0(
              "<b>", input$stp_ccg_sel, ":</b> {point.SUB_GEOGRAPHY_NAME}<br> <b>",
              switch(input$metric,
                "STARPU" = "Antibacterial items/STAR PU (March 2022):</b> {point.VALUE:.2f}",
                "COAMOX" = "Co-amoxiclav, Cephalosporins & Quinolones (March 2022): </b> {point.VALUE:.2f}%"
              )
            )
          )
        ) %>%
        theme_nhsbsa() %>%
        highcharter::hc_colorAxis(
          dataClassColor = "category",
          dataClasses = list(
            list(from = 0, to = 0, color = "#ED8B00", name = "MEET_TARGET"),
            list(from = 1, to = 1, color = "#41B6E6", name = "MEET_TARGET")
          )
        ) %>%
        highcharter::hc_legend(enabled = FALSE) %>%
        highcharter::hc_plotOptions(
          map = list(
            events = list(
              click = htmlwidgets::JS(
                paste0(
                  "
                function(event) {
                  Shiny.setInputValue('", id, "-mapclick_sof', event.point.SUB_GEOGRAPHY_NAME);
                }
                "
                )
              )
            )
          )
        )
    })

    # Trend chart
    observeEvent(input$mapclick_sof, {
      output$sof_trend <- highcharter::renderHighchart({

        req(input$mapclick_sof)
        req(input$stp_ccg_sel)
        req(input$metric)
        
        
        plot_df <- reactive({antibioticPrescribingScrollytellR::merge_df %>%
            dplyr::filter(SUB_GEOGRAPHY_NAME == input$mapclick_sof &
                            SUB_GEOGRAPHY_TYPE == input$stp_ccg_sel &
                            METRIC == input$metric
            )})
        
        validate(
          need(nrow(plot_df()) > 0, message = FALSE)
        ) # stopping to show temporary error message.
        
        # observe(print(plot_df()))

        reference_value <- switch(input$metric,
          "STARPU" = 0.87,
          "COAMOX" = 10
        )

        # add min, max value (28062022)
        max_val <- reactive({
          antibioticPrescribingScrollytellR::merge_df %>% 
          dplyr::filter(METRIC == input$metric & 
                          SUB_GEOGRAPHY_TYPE == input$stp_ccg_sel) %>% 
          dplyr::summarise(max(VALUE)) %>% 
          dplyr::ungroup() %>% 
          dplyr::pull()
        })
        
        # observe(print(max_val()))  


        # define the dataset for the selected geography
        plot_df() %>% 
          highcharter::hchart(
            type = "coloredline",
            highcharter::hcaes(
              x = YEAR_MONTH,
              y = VALUE,
              segmentColor = colour
            )
          ) %>%
          highcharter::hc_add_dependency("plugins/multicolor_series.js") %>% 
          theme_nhsbsa() %>%
          highcharter::hc_title(
            text = glue::glue({
              input$mapclick_sof
            })
          ) %>%
          highcharter::hc_yAxis(
            min = 0,
            max = max_val(),
            title = list(
              text = "12 rolling month trend",
              align = "middle"
            ),
            plotLines = list(list(
              value = reference_value, color = "#8A1538", width = 1,
              dashStyle = "shortdash"
            )),
            labels = list(format = switch(input$metric,
              "STARPU" = "{value:.1f}",
              "COAMOX" = "{value:.0f}%"
            ))
          ) %>%
          highcharter::hc_xAxis(
            categories = unique(antibioticPrescribingScrollytellR::merge_df[,"YEAR_MONTH"]),
            title = list(text = "Year month")
          ) %>% 
          highcharter::hc_plotOptions(
            line = list(marker = (list(enabled = FALSE)))
          ) %>% 
          highcharter::hc_tooltip(
            shared = TRUE,
            useHTML = TRUE,
            pointFormat = switch(input$metric,
                                 "STARPU" = "<b>{point.y:.2f}</b>",
                                 "COAMOX" = "<b>{point.y:.1f}%</b>")
            
          )
      })
    })

    # All : show by region number of SOF/CCG met/not met the target
    # selected geography: show each STP/CCG to
    # Take geography_df() and group by count


    geography_compare_df <- reactive({
      if (input$stp_ccg_sel == "STP") {
        antibioticPrescribingScrollytellR::merge_df %>%
          dplyr::filter(REGION != "England") %>%
          dplyr::filter(SUB_GEOGRAPHY_TYPE == "STP") %>%
          dplyr::filter(METRIC == input$metric) %>%
          dplyr::filter(YEAR_MONTH == "Mar_22") %>%
          dplyr::group_by(REGION) %>%
          dplyr::summarise(
            MEET = sum(MEET_TARGET),
            NOT_MEET = dplyr::n() - sum(MEET_TARGET)
          )
      } else if (input$stp_ccg_sel == "CCG") {
        antibioticPrescribingScrollytellR::merge_df %>%
          dplyr::filter(REGION != "England") %>%
          dplyr::filter(SUB_GEOGRAPHY_TYPE == "CCG") %>%
          dplyr::filter(METRIC == input$metric) %>%
          dplyr::filter(YEAR_MONTH == "Mar_22") %>%
          dplyr::group_by(REGION) %>%
          dplyr::summarise(
            MEET = sum(MEET_TARGET),
            NOT_MEET = dplyr::n() - sum(MEET_TARGET)
          )
      }
    })

    observe(print(geography_compare_df()))

    output$sof_compare <- highcharter::renderHighchart({

      # highchart plot
      highcharter::highchart() %>%
        highcharter::hc_chart(type = "column") %>%
        highcharter::hc_plotOptions(column = list(stacking = "normal")) %>%
        highcharter::hc_xAxis(categories = geography_compare_df()$REGION) %>%
        highcharter::hc_add_series(
          name = "Met",
          data = geography_compare_df()$MEET,
          stack = "Not met target"
        ) %>%
        highcharter::hc_add_series(
          name = "Not Met",
          data = geography_compare_df()$NOT_MEET,
          stack = "Not met target"
        ) %>%
        theme_nhsbsa()
    })
  })
}

## To be copied in the UI
# mod_region_antibiotic_ui("region_antibiotic_1")

## To be copied in the server
# mod_region_antibiotic_server("region_antibiotic_1")
