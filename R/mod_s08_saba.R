#' s08 saba UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_s08_saba_ui <- function(id) {
  ns <- NS(id)


  # Section Introduction ----------------------------------------------------
  tagList(
    h3("Patients living in more deprived areas are more likely to be receiving excessive prescribing of short-acting beta agonist (SABA) inhalers"),
    p(
      "Well controlled asthma, a similar condition to COPD,  can be associated with little or no need for ",
      "short-acting bronchodilator (SABA or reliever) inhalers. The excessive ",
      "(e.g. more than six canisters in 12 months) use of Salbutamol and other ",
      "short acting bronchodilators is associated with poorer health outcomes and even ",
      enurl(
        url = "https://pubmed.ncbi.nlm.nih.gov/15649676/",
        text = "death."
      )
    ),
    p(
      "The prescribing of SABA inhaler products in relation to deprivation ",
      "has been defined based on the excessive SABA inhaler metric as reported ",
      "within the ",
      enurl(
        url = "https://www.nhsbsa.nhs.uk/access-our-data-products/epact2/dashboards-and-specifications/respiratory-dashboard",
        text = "NHSBSA ePACT2 Respiratory Dashboard."
      )
    ),

    # Scrolly Chart: SABA -----------------------------------------------------
    # Define the scrolly container that will hold the chart object
    scrollytell::scrolly_container(
      outputId = ns("saba_scrolly"),
      scrollytell::scrolly_graph(
        # Place the sticky part in the center of the page
        tags$div(
          style = "margin-top: 20vh" # change based on size of sticky graph
        ),
        nhs_card(
          heading = p("Saba Prescribing: ", textOutput(outputId = ns("chart_title"), inline = TRUE)),
          # dynamic chart based on current scroll section
          highcharter::highchartOutput(
            outputId = ns("saba_chart"),
            height = "450px"
          ),
          # caveat text below chart
          tags$text(
            class = "highcharts-caption",
            style = "font-size: 9pt",
            "SABA prescribing based on activity amongst patients also precribed preventer inhalers ",
            "but not prescribed an antimuscarinic."
          ),
          mod_nhs_download_ui(id = ns("saba_chart_download"))
        )
      ),
      # Define the text for each scroll section
      scrollytell::scrolly_sections(
        scrollytell::scrolly_section(
          id = "SABA_ONE",
          tags$div(style = "height: 30vh"), # bump text from top of section
          h4_tabstop("Any SABA prescribing"),
          p(
            "The rate of prescribing of any SABA inhalers decreases as deprivation decreases. ",
            "Figures range from 85.6% in the most deprived decile to 71.7% in the least deprived ",
            "decile."
          )
        ),
        scrollytell::scrolly_section(
          id = "SABA_SIX",
          tags$div(style = "height: 20vh"), # bump text from top of section
          h4_tabstop("Excess SABA prescribing"),
          p(
            "The correlation between SABA prescribing and deprivation is more ",
            "pronounced when looking at patients receiving excessive prescribing ",
            "of SABA, defined as 6 or more inhalers in a 12-month period."
          ),
          p(
            "In the most deprived area, the rate of excessive SABA prescribing in 2020/21 ",
            "(38.8%) was more than double the rate (17.9%) in the least deprived area."
          )
        ),
        scrollytell::scrolly_section(
          id = "SABA_THIRTEEN",
          tags$div(style = "height: 20vh"), # bump text from top of section
          h4_tabstop("Extreme SABA Prescribing"),
          p(
            "The ",
            enurl(
              url = "https://www.rcplondon.ac.uk/projects/national-review-asthma-deaths",
              text = "National Review of Asthma Deaths (NRAD)"
            ),
            " reported that 39% of the people who died of asthma-related causes, ",
            "and who were using a SABA inhaler at the time of death, had been ",
            "prescribed more than 12 SABA inhalers in the year before they died."
          ),
          p(
            "In the most deprived area, the rate of excessive SABA prescribing in 2020/21 ",
            "(11.7%) was more than triple the rate (3.5%) in the least deprived area."
          ),
          p(
            "These findings may suggest patients in the most deprived areas are at increased risk ",
            "of poor outcomes linked to SABA prescribing."
          )
        ),
        scrollytell::scrolly_section(
          id = "SABA_AGE_GENDER",
          tags$div(style = "height: 20vh"), # bump text from top of section
          h4_tabstop("Extreme SABA Prescribing"),
          p(
            "In addition to a relationship between deprivation and 'extreme' SABA prescribing, ",
            "taking the patient's age and gender into consideration also highlights a potential ",
            "relationship with the patient age and 'extreme' prescribing."
          ),
          p(
            "The proportion of patients on 13+ SABA inhalers increase with age, with this ",
            "increase being sharper for patients in the Core20 areas."
          ),
          p(
            "Rates are slightly higher for males up until the age of 80, with rates for females ",
            "being higher from ages 80 and above."
          ),
          tags$div(style = "height: 20vh"), # bump text from bottom of last section
        )
      )
    )
  )
}

#' s08_saba Server Functions
#'
#' @noRd
mod_s08_saba_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    # Define dynamic chart header ---------------------------------------------
    output$chart_title <- renderText({

      # require input object
      req(input$saba_scrolly)

      # Select chart title based on scroll section input
      switch(input$saba_scrolly,
        "SABA_ONE" = "Proportion of patients receiving one or more SABA inhalers, by IMD decile (2020/21)",
        "SABA_SIX" = "Proportion of patients receiving six or more SABA inhalers, by IMD decile (2020/21)",
        "SABA_THIRTEEN" = "Proportion of patients receiving 13 or more SABA inhalers, by IMD decile (2020/21)",
        "SABA_AGE_GENDER" = "Proportion of patients receiving 13 or more SABA inhalers, by age, gender and Core20 classification (2020/21)"
      )
    })




    # Define dynamic chart object ---------------------------------------------
    output$saba_chart <- highcharter::renderHighchart({

      # require input object
      req(input$saba_scrolly)

      # create different chart objects based on the scrolly section
      if (input$saba_scrolly %in% c("SABA_ONE", "SABA_SIX", "SABA_THIRTEEN")) {
        # create column chart split by IMD

        # base the caption on the Core20 summary data
        # select the relevant metric based on the scroll position
        caption_data <- antibioticPrescribingScrollytellR::saba_core %>%
          dplyr::mutate(METRIC = switch(input$saba_scrolly,
            "SABA_ONE" = RATE_1_PLUS_SABA,
            "SABA_SIX" = RATE_6_PLUS_SABA,
            "SABA_THIRTEEN" = RATE_13_PLUS_SABA
          )) %>%
          dplyr::select(CORE20_CLASSIFICATION, METRIC)

        # find the value for the Core20 point
        caption_data_core20 <- caption_data %>%
          dplyr::filter(CORE20_CLASSIFICATION == "CORE20") %>%
          dplyr::select(METRIC)

        # find the value for the non Core20 point
        caption_data_noncore20 <- caption_data %>%
          dplyr::filter(CORE20_CLASSIFICATION != "CORE20") %>%
          dplyr::select(METRIC)


        # create the caption object
        tags$text(
          class = "highcharts-caption",
          HTML(
            paste0(
              "Core20 overall rate: ", sprintf(caption_data_core20, fmt = "%#.1f"), "%", br(),
              "Non Core20 overall rate: ", sprintf(caption_data_noncore20, fmt = "%#.1f"), "%"
            )
          )
        )

        # define which metric to report on based on scroll input
        antibioticPrescribingScrollytellR::saba_deciles %>%
          dplyr::mutate(METRIC = switch(input$saba_scrolly,
            "SABA_ONE" = RATE_1_PLUS_SABA,
            "SABA_SIX" = RATE_6_PLUS_SABA,
            "SABA_THIRTEEN" = RATE_13_PLUS_SABA
          )) %>%
          # create the base chart
          highcharter::hchart(
            type = "column",
            highcharter::hcaes(
              x = IMD_DECILE,
              y = METRIC,
              group = CORE20_CLASSIFICATION
            )
          ) %>%
          # apply NHSBSA themes
          theme_nhsbsa() %>%
          # define y axis options
          highcharter::hc_yAxis(title = list(text = "Proportion of patients (%)")) %>%
          # define x axis options
          highcharter::hc_xAxis(
            min = 1,
            max = 11, # Pad to ensure we can see the 10 label
            categories = c(NA, "1<br>Most<br>deprived", rep(NA, 8), "10<br>Least<br>deprived"),
            labels = list(step = 9),
            title = list(text = "Deprivation decile")
          ) %>%
          # show highcharter credit
          highcharter::hc_credits(enabled = TRUE) %>%
          # create the tooltip
          highcharter::hc_tooltip(
            headerFormat = "",
            pointFormat = paste0(
              "<b>2019 IMD decile:</b> {point.x} <br>",
              "<b>Proportion of overall population:</b> {point.y:.1f}%"
            )
          ) %>%
          # disable the legend and create in subtitle
          highcharter::hc_legend(enabled = FALSE) %>%
          highcharter::hc_subtitle(
            useHTML = TRUE,
            text = paste0(
              "<span style = 'color:#003087; font-size: 20px'> &bull; </span> <b> <span style = font-size: 35px'> Core20",
              " (", sprintf(caption_data_core20, fmt = "%#.1f"), "%)</span> </b>",
              "<br>",
              "<span style = 'color:#ED8B00; font-size: 20px'> &bull; </span> <b> <span style = font-size: 35px'> Non Core20",
              " (", sprintf(caption_data_noncore20, fmt = "%#.1f"), "%)</span>"
            ),
            align = "right"
          )
      } else {
        # create line chart split by age band with separate series for gender and Core20 group

        # Manually define the icons for gender charts
        female_core20 <- fa_to_png_to_datauri(
          name = "female",
          width = 7,
          fill = "#003087"
        )
        male_core20 <- fa_to_png_to_datauri(
          name = "male",
          width = 6,
          fill = "#003087"
        )
        female_noncore20 <- fa_to_png_to_datauri(
          name = "female",
          width = 7,
          fill = "#ED8B00"
        )
        male_noncore20 <- fa_to_png_to_datauri(
          name = "male",
          width = 6,
          fill = "#ED8B00"
        )

        # Generate chart data
        chartdata <- antibioticPrescribingScrollytellR::saba_age_gender %>%
          dplyr::mutate(METRIC = RATE_13_PLUS_SABA) %>%
          dplyr::select(CORE20_CLASSIFICATION, AGE_BAND, GENDER, METRIC) %>%
          tidyr::pivot_wider(names_from = c(CORE20_CLASSIFICATION, GENDER), values_from = METRIC)

        # Define tooltip text
        tooltip_text <- paste0(
          "<b>{series.name} </b> <br>",
          "<b>Age Band:</b> {point.AGE_BAND} <br>",
          "<b>Proportion of patients:</b> £{point.y:,.1f}% <br><br>",
          "<b>Value Comparison ({point.AGE_BAND})</b><br>",
          "<b>Core 20 - Male: </b> {point.CORE20_M:,.1f}% <br>",
          "<b>Core 20 - Female: </b> {point.CORE20_F:,.1f}% <br>",
          "<b>Non Core - 20 Male: </b> {point.NON-CORE20_M:,.1f}% <br>",
          "<b>Non Core - 20 Female: </b> {point.NON-CORE20_F:,.1f}%"
        )

        # Generate chart data
        age_chart <- highcharter::highchart() %>%
          highcharter::hc_chart(type = "line") %>%
          highcharter::hc_xAxis(categories = unique(chartdata$AGE_BAND)) %>%
          highcharter::hc_yAxis(
            min = 0,
            title = list(text = "Proportion of patients (%)")
          ) %>%
          theme_nhsbsa(stack = NA) %>%
          highcharter::hc_add_series(
            data = chartdata,
            type = "line",
            highcharter::hcaes(
              x = AGE_BAND,
              y = CORE20_F
            ),
            name = "Core20 - Female",
            color = "#003087",
            marker = list(
              symbol = stringr::str_glue("url({data_uri})", data_uri = female_core20),
              radius = 2
            ),
            icon = female_core20
          ) %>%
          highcharter::hc_add_series(
            data = chartdata,
            type = "line",
            highcharter::hcaes(
              x = AGE_BAND,
              y = CORE20_M
            ),
            name = "Core20 - Male",
            color = "#003087",
            marker = list(
              symbol = stringr::str_glue("url({data_uri})", data_uri = male_core20),
              radius = 2
            ),
            icon = male_core20
          ) %>%
          highcharter::hc_add_series(
            data = chartdata,
            type = "line",
            highcharter::hcaes(
              x = AGE_BAND,
              y = `NON-CORE20_F`
            ),
            name = "Non Core20 - Female",
            color = "#ED8B00",
            marker = list(
              symbol = stringr::str_glue("url({data_uri})", data_uri = female_noncore20),
              radius = 2
            ),
            icon = female_noncore20
          ) %>%
          highcharter::hc_add_series(
            data = chartdata,
            type = "line",
            highcharter::hcaes(
              x = AGE_BAND,
              y = `NON-CORE20_M`
            ),
            name = "Non Core20 - Male",
            color = "#ED8B00",
            marker = list(
              symbol = stringr::str_glue("url({data_uri})", data_uri = male_noncore20),
              radius = 2
            ),
            icon = male_noncore20
          ) %>%
          highcharter::hc_plotOptions(series = list(animation = FALSE)) %>%
          highcharter::hc_credits(enabled = TRUE) %>%
          highcharter::hc_tooltip(
            headerFormat = "",
            pointFormat = tooltip_text
          )
      }
    })

    # downloadable dataset - include all figures from charts
    mod_nhs_download_server(
      id = "saba_chart_download",
      filename = "saba_prescribing_proportions.csv",
      export_data = antibioticPrescribingScrollytellR::saba_download
    )

    # Render scrolly object
    output$saba_scrolly <- scrollytell::renderScrollytell({
      scrollytell::scrollytell()
    })
  })
}
