#' s07_copd UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_s07_copd_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2_tabstop("NHS Prescribing Focus: Chronic obstructive pulmonary disease (COPD)"),
    p(
      enurl(
        url = "https://bnf.nice.org.uk/treatment-summary/chronic-obstructive-pulmonary-disease.html",
        text = "Chronic obstructive pulmonary disease (COPD)"
      ),
      " is a common, largely ",
      "preventable and treatable disease, characterised by persistent respiratory ",
      "symptoms and airflow limitation that is usually progressive and not fully ",
      "reversible."
    ),
    p(
      "The main risk factor for development and exacerbations of COPD is tobacco ",
      "smoking. Other risk factors include environmental pollution and occupational ",
      "exposures, genetic factors, and poor lung growth during childhood."
    ),
    p(
      "COPD is one of the biggest contributors to the life expectancy inequality gap between the ",
      "most and least deprived regions and ",
      enurl(
        url = "https://www.nature.com/articles/pcrj201232",
        text = "acute exacerbations of COPD account for roughly 1 in 8 emergency admissions"
      ),
      "in England. The NHS Long Term Plan (LTP) respiratory programme aims ",
      "to improve diagnosis, treatment and pulmonary rehab. As part of this the Core20PLUS5 ",
      "approach will include a focus on accelerating Flu, Covid-19 and Pneumonia vaccines uptake ",
      "to reduce premature mortality and emergency admissions due to exacerbation of COPD."
    ),
    p(
      "Previous studies have highlighted a relationship between COPD and deprivation. The ",
      enurl(
        url = "https://www.gov.uk/government/statistics/interactive-health-atlas-of-lung-conditions-in-england-inhale-november-2021-update/interactive-health-atlas-of-lung-conditions-in-england-inhale-november-2021-update#:~:text=More%20deprived%20areas%20have%20a,decile%20(30.9%20per%20100%2C000)",
        text = "rate of death from COPD as a contributory factor"
      ),
      " is five times higher in the most deprived decile compared to the least deprived decile and ",
      enurl(
        url = "https://www.nacap.org.uk/nacap/welcome.nsf/vwFiles/COPD+Clinical+Audit+2019-20/$File/NACAP_COPD_SC_Data_And_Methodology_Report_2019-20_Jun_2021.pdf#page=11",
        text = "emergency admissions for COPD exacerbations"
      ),
      " in the most deprived areas being more than triple that of the least deprived areas."
    ),
    p(
      "For this investigation prescribing relating to the treatment of ",
      "COPD has been identified based on any NHS prescriptions ",
      "issued that include products from the following section(s) of the BNF:"
    ),
    tags$ul(
      tags$li("BNF 3.1: Bronchodilators"),
      tags$li("BNF 3.2: Corticosteroids (respiratory)"),
      tags$li("BNF 3.3: Cromoglycate, leukotriene and phosphodesterase type-4 inhib")
    ),
    tags$div(
      style = "margin-top: 25vh" # add some buffer space after the chart
    ),

    # Chart One: trend line chart ----------------------------------------------

    h3_tabstop("COPD prescribing levels have not changed much over the past six years"),
    p(
      "The number of prescription items for COPD medication has increased ",
      "slightly over the past few years, rising from 53.4 million in 2015/16 to ",
      "55.5 million in 2020/21. However, this rate of growth (4.0%) is only marginally ",
      "higher than the English population growth over the same period (3.2%). A ",
      "similar pattern can be seen per thousand population in both Core20 and ",
      "non Core20 areas."
    ),
    p(
      "There is a slight jump in prescribing in 2019/20. This may be ",
      "linked to the Covid-19 pandemic as many patients received additional ",
      "prescribing in March 2020 ahead of expected social distancing measures."
    ),
    nhs_card(
      nhs_selectInput(
        inputId = ns("trend_metric"),
        label = "Select trend to view:",
        choices = c(
          "National: Number of COPD prescription items" = "national_items",
          "Core20 v Non Core20: COPD prescription items per thousand population" = "core_item_per_pop"
        ),
        full_width = TRUE
      ),
      # Text
      shiny::htmlOutput(ns("copd_trend_chart_text")),
      # include the dynamic chart
      highcharter::highchartOutput(
        outputId = ns("copd_trend_chart"),
        height = "300px"
      ),
      mod_nhs_download_ui(id = ns("copd_prescribing_trend_download"))
    ),
    tags$div(
      style = "margin-top: 25vh" # add some buffer space after the chart
    ),

    # Chart Two: exemption trend line chart ------------------------------------

    h3_tabstop("The proportion of COPD prescribing requiring some form of payment from patients has been gradually increasing"),
    p(
      "The most common scenario is that patients receive COPD prescribing free ",
      "of charge based on being age exempt from NHS prescription charges. This ",
      "is not surprising due to the link between COPD and patient age."
    ),
    p(
      "However, the proportion of COPD items attracting some form of payment ",
      "from patients increased from 16.1% in 2015/16 to 19.2% in 2019/20, ",
      "before dipping slightly in 2020/21, which is likely due to the Covid19 ",
      "pandemic and more people receiving financial assistance during lockdowns."
    ),
    nhs_card(
      heading = "Proportion of COPD prescription items, by financial year,
      split by prescription charge status (2015/16 to 2020/21)",
      # include the dynamic chart
      highcharter::highchartOutput(
        outputId = ns("copd_exemption_trend_chart"),
        height = "300px"
      ),
      mod_nhs_download_ui(id = ns("copd_exemption_trend_download")),
      tags$text(
        class = "highcharts-caption",
        style = "font-size: 9pt",
        "Prescription charge proportion splits exclude between 1%-3% of prescription ",
        "items where a exemption category could not be identified."
      )
    ),
    tags$div(
      style = "margin-top: 25vh" # add some buffer space after the chart
    ),

    # Chart Three: COPD Value boxes --------------------------------------------

    h3_tabstop("Patients living in Core20 areas receive more COPD prescribing than those in non Core20 areas"),
    p(
      "The number of COPD prescription items and COPD drug cost per patient (per year) is ",
      "slightly higher for patients in Core20 areas than non Core20 areas."
    ),
    p(
      "Core20 patients are also more likely to receive their COPD prescribing ",
      "free of charge due to non age based exemptions and less likely to pay for ",
      "their prescription items."
    ),
    nhs_card(
      heading = "COPD prescribing data metrics for Core20 and non-Core20 patients (2020/21)",
      nhs_grid_2_col(
        h4("Metric"),
        nhs_grid_2_col(
          h4(
            style = "text-align: center;",
            "Core20"
          ),
          h4(
            style = "text-align: center;",
            "Non-Core20"
          )
        )
      ),
      uiOutput(ns("value_boxes_copd"))
    ),
    tags$div(
      style = "margin-top: 25vh" # add some buffer space after the chart
    ),

    # Chart Four: COPD Prescribing by IMD Decile -------------------------------

    h3_tabstop("Patients in more deprived areas receive more prescribing for COPD than those in less deprived areas"),
    p(
      "The drug cost and number of COPD prescription items prescribed increases ",
      "as areas become more deprived."
    ),
    nhs_card(
      heading = "COPD prescribing by IMD Decile (2020/21)",
      nhs_selectInput(
        inputId = ns("metric_copd_decile"),
        label = "Metric:",
        choices = c(
          "Patients per thousand population" = "pat_per_pop",
          "Items per thousand population" = "item_per_pop",
          "Cost per thousand population" = "cost_per_pop",
          "Items per patient" = "item_per_pat",
          "Cost per patient" = "cost_per_pat"
        ),
        full_width = TRUE
      ),
      # text
      shiny::htmlOutput(ns("copd_imd_chart_text")),
      # include the dynamic chart
      highcharter::highchartOutput(
        outputId = ns("copd_imd_chart"),
        height = "300px"
      ),
      tags$text(
        class = "highcharts-caption",
        style = "font-size: 9pt",
        "Excluding 0.7% of COPD prescription items where the IMD decile could not be identified"
      ),
      mod_nhs_download_ui(id = ns("copd_imd_decile_metrics_download"))
    ),
    tags$div(
      style = "margin-top: 25vh" # add some buffer space after the chart
    ),

    # Chart Five: COPD Prescribing by age and gender ---------------------------

    h3_tabstop("COPD prescribing increases with patient age. Core20 patients typically receive slightly higher levels of prescribing across each age group"),
    p(
      "The risk of COPD is known to increase with patient age, and the breakdown ",
      "of COPD prescribing by age group supports this. Prescribing peaks in the ",
      "70 to 79 age group and starts to drop beyond the age of 79."
    ),
    p(
      "Patients from Core20 areas receive more COPD prescribing than those in ",
      "non Core20 areas. When looking at the number of COPD ",
      "patients compared to the population, it appears that more females ",
      "are receiving COPD prescribing than males."
    ),
    nhs_card(
      heading = "COPD prescribing by age and gender (2020/21)",
      nhs_selectInput(
        inputId = ns("metric_copd_gender"),
        label = "Metric:",
        choices = c(
          "Patients per thousand population" = "pat_per_pop",
          "Items per thousand population" = "item_per_pop",
          "Cost per thousand population" = "cost_per_pop",
          "Items per patient" = "item_per_pat",
          "Cost per patient" = "cost_per_pat"
        ),
        full_width = TRUE
      ),
      # text
      shiny::htmlOutput(ns("copd_age_gender_chart_text")),
      # include the dynamic chart
      highcharter::highchartOutput(
        outputId = ns("copd_age_gender_chart"),
        height = "500px"
      ),
      # mod_nhs_download_ui(id = ns("smi_trend_chart_download")),
      tags$text(
        class = "highcharts-caption",
        style = "font-size: 9pt",
        "Excluding 0.8% of prescription items where the age/gender could not be identified"
      ),
      mod_nhs_download_ui(id = ns("copd_age_gender_metrics_download"))
    ),
    tags$div(
      style = "margin-top: 25vh" # add some buffer space after the chart
    ),

    # Chart Six: COPD Local Authority charts -----------------------------------

    h3_tabstop("Local Authority Profile"),
    p(
      "The chart and map show prescribing metrics by local authority ",
      "area relative to the population and deprivation profile of a ",
      "local authority. Local authority deprivation is based on ",
      enurl(
        url = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/833947/IoD2019_Research_Report.pdf",
        text = "Ministry of Housing, Communities & Local Government (MHCLG) methodology."
      ),
      " The data can be explored by selecting a metric and region to view."
    ),
    p(
      "There are not any obvious patterns when considering deprivation on its ",
      "own in relation to COPD prescribing. Prescribing rates per thousand of the ",
      "population seem to be generally lower in London than in other regions, ",
      "but this may be link to additional factors and not just deprivation alone."
    ),
    nhs_card(
      heading = "COPD prescribing by Index of Multiple Deprivation
      ranking for English Local Authorities (2020/21)",
      nhs_grid_2_col(
        # include selection box for exemption certificate
        nhs_selectInput(
          inputId = ns("metric_copd_la"),
          label = "Metric",
          choices = c(
            "Patients per thousand population" = "pat_per_pop",
            "Items per thousand population" = "item_per_pop",
            "Cost per thousand population" = "cost_per_pop",
            "Items per patient" = "item_per_pat",
            "Cost per patient" = "cost_per_pat"
          ),
          full_width = TRUE
        ),
        # include selection box for region selections
        nhs_selectInput(
          inputId = ns("input_geography_region_copd"),
          label = "Highlight Region",
          choices = c(sort(unique(antibioticPrescribingScrollytellR::region_la_lookup$REG_NAME))),
          full_width = TRUE
        )
      ),
      shiny::htmlOutput(ns("chart_lad_scatter_text")),
      highcharter::highchartOutput(
        outputId = ns("chart_lad_scatter_copd"),
        height = "350px"
      ),
      nhs_grid_2_col(
        highcharter::highchartOutput(
          outputId = ns("map_selected_region_la_copd"),
          height = "300px"
        ),
        highcharter::highchartOutput(
          outputId = ns("chart_selected_la_imd_copd"),
          height = "300px"
        )
      ),
      tags$text(
        class = "highcharts-caption",
        style = "font-size: 9pt",
        "Deprivation data uses 2019 estimates. Click map to see IMD decile distribution by selected local authority."
      ),
      mod_nhs_download_ui(id = ns("copd_la_metrics_download"))
    ),
    tags$div(
      style = "margin-top: 25vh" # add some buffer space after the chart
    ),

    # Chart Seven: COPD LSOA Chart ---------------------------------------------

    h3_tabstop("LSOA Profile"),
    p(
      "This chart enables you to explore whether any relationships exist between the ",
      "general deprivation profile for an area and the levels of COPD prescribing ",
      "received by the residents of those areas. You can also compare this to the ",
      "expected rate given deprivation for all LSOAs nationally (denoted by the ",
      "black line). LSOAs falling outside the red dotted lines may be potential ",
      "outliers, where the metrics are either higher or lower than 95% of LSOAs, ",
      "given deprivation."
    ),
    p(
      "The Core20 areas are based on the IMD classifications as defined at ",
      "small geographic areas called Lower Super Output Areas (LSOAs). ",
      "On average each Local Authority in England will cover around 100 LSOAs, ",
      "but this could range from 6 LSOAs (City of London) to 639 LSOAs (Birmingham)."
    ),
    nhs_card(
      heading = "COPD prescribing by Index of Multiple Deprivation ranking for English LSOA (2020/21)",
      nhs_grid_2_col(
        nhs_selectInput(
          inputId = ns("input_metric_copd"),
          label = "Metric",
          choices = c(
            "Patients per thousand population" = "patients_per_1k_pop",
            "Items per thousand population" = "items_per_1k_pop",
            "Cost per thousand population" = "nic_per_1k_pop",
            "Items per patient" = "items_per_patient",
            "Cost per patient" = "nic_per_patient"
          ),
          full_width = TRUE
        ),
        nhs_selectInput(
          inputId = ns("input_geography_copd"),
          label = "Select Local Authority",
          choices = c(sort(unique(antibioticPrescribingScrollytellR::region_la_lookup$LAD_NAME))),
          full_width = TRUE
        )
      ),
      # text
      shiny::htmlOutput(ns("outlier_text")),
      # chart
      highcharter::highchartOutput(
        outputId = ns("output_lsoa_scatter_chart_copd"),
        height = "350px"
      ),
      # caption
      tags$text(
        class = "highcharts-caption",
        style = "font-size: 9pt",
        "Across all LSOAs in England, it is estimated that approximately 95% of ",
        "values fall within the dashed lines, and LSOAs well outside of these ",
        "lines could be considered as potential 'outliers'."
      ),
      tags$br(),
      tags$text(
        class = "highcharts-caption",
        style = "font-size: 9pt",
        "Excluding any LSOAs with less than 5 patients identified receiving applicable prescribing."
      ),
      mod_nhs_download_ui(id = ns("copd_lsoa_metrics_download"))
    ),
    tags$div(
      style = "margin-top: 25vh" # add some buffer space after the chart
    ),

    # Chart Eight: COPD Drug dumbbell chart ------------------------------------

    h3_tabstop("More than 80% of COPD patients received Salbutamol"),
    p(
      "More than 80% of COPD patients received Salbutamol and almost half ",
      "received Beclomethasone. These are the two most commonly prescribed ",
      "medications in both Core20 and non Core20 areas, accounting for roughly ",
      "60% of all COPD prescription items . Although Salbutamol accounts for a ",
      "much lower proportion of drug cost than Beclomethasone."
    ),
    nhs_card(
      heading = "Comparison of COPD drugs (chemical substance) prescribed to Core20 and Non Core20 patients (2020/21)",
      nhs_grid_2_col(
        nhs_selectInput(
          inputId = ns("metric"),
          label = "Metric",
          choices = c(
            # "Items per patient" = "item_per_pat",
            # "Cost per patient" = "cost_per_pat",
            # "Patients per thousand population" = "pat_per_pop",
            # "Items per thousand population" = "item_per_pop",
            # "Cost per thousand population" = "cost_per_pop",
            "Proportion of patients" = "pct_pats",
            "Proportion of items" = "pct_items",
            "Proportion of cost" = "pct_cost"
          ),
          full_width = TRUE
        ),
        nhs_selectInput(
          inputId = ns("sort_group"),
          label = "Order by",
          choices = c(
            "Core20" = "CORE20",
            "Non Core20" = "NONCORE20"
          ),
          full_width = TRUE
        )
      ),
      # text
      shiny::htmlOutput(ns("chart_drug_comparison_text")),
      # chart
      highcharter::highchartOutput(
        outputId = ns("chart_drug_comparison"),
        height = "400px"
      ),
      mod_nhs_download_ui(id = ns("copd_drug_metrics_download"))
    ),
    tags$div(
      style = "margin-top: 25vh" # add some buffer space after the chart
    )
  )
}



#' s07_copd Server Functions
#'
#' @noRd
mod_s07_copd_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Chart One: trend line chart ----------------------------------------------

    # build a dynamic title for the chart basic on the selected metric
    output$copd_trend_chart_title <- renderText({
      if (input$trend_metric == "national_items") {
        "NHS prescription items for COPD drugs (2015/16 to 2020/21)"
      } else {
        "NHS prescription items for COPD drugs per thousand population (2015/16 to 2020/21)"
      }
    })

    # build the chart object
    output$copd_trend_chart <- highcharter::renderHighchart({
      if (input$trend_metric == "national_items") {
        # create the plot data
        highcharter::highchart() %>%
          highcharter::hc_chart(type = "line") %>%
          highcharter::hc_xAxis(categories = unique(antibioticPrescribingScrollytellR::copd_prescribing_trend$FINANCIAL_YEAR)) %>%
          theme_nhsbsa(stack = NA) %>%
          highcharter::hc_add_series(
            data = antibioticPrescribingScrollytellR::copd_prescribing_trend %>%
              dplyr::filter(CORE20_CLASSIFICATION == "Overall"),
            type = "line",
            highcharter::hcaes(
              x = FINANCIAL_YEAR,
              y = ITEMS
            ),
            # name = "No. of prescription items per thousand population",
            marker = list(symbol = "circle")
          ) %>%
          highcharter::hc_yAxis(
            min = 0,
            title = list(text = "Prescription items")
          ) %>%
          highcharter::hc_credits(enabled = TRUE) %>%
          highcharter::hc_legend(enabled = FALSE) %>%
          highcharter::hc_tooltip(
            shared = FALSE,
            formatter = htmlwidgets::JS(
              paste0(
                "
            function() {

                outHTML =
                  '<b>Financial Year: </b>' + this.point.FINANCIAL_YEAR + '<br>' +
                  '<b>Prescription items: </b>' + Highcharts.numberFormat(this.point.y / 1000000, 1) + 'm'

                return outHTML;

            }
            "
              )
            )
          )
      } else {
        # create the plot data
        highcharter::highchart() %>%
          highcharter::hc_chart(type = "line") %>%
          highcharter::hc_xAxis(categories = unique(
            antibioticPrescribingScrollytellR::copd_prescribing_trend$FINANCIAL_YEAR
          )) %>%
          theme_nhsbsa(stack = NA) %>%
          highcharter::hc_add_series(
            data = antibioticPrescribingScrollytellR::copd_prescribing_trend %>%
              dplyr::arrange(FINANCIAL_YEAR) %>%
              dplyr::filter(CORE20_CLASSIFICATION != "Overall"),
            type = "line",
            highcharter::hcaes(
              x = FINANCIAL_YEAR,
              y = ITEMS_PER_1K_POPULATION,
              group = CORE20_CLASSIFICATION
            ),
            # name = "No. of prescription items per thousand population",
            marker = list(symbol = "circle")
          ) %>%
          highcharter::hc_yAxis(
            min = 0,
            title = list(text = "Prescription items per thousand population")
          ) %>%
          highcharter::hc_credits(enabled = TRUE) %>%
          highcharter::hc_tooltip(
            shared = FALSE,
            formatter = htmlwidgets::JS(
              paste0(
                "
            function() {
                outHTML =
                  '<b>' + this.series.name + '</b><br>' +
                  '<b>Financial Year: </b>' + this.point.FINANCIAL_YEAR + '<br>' +
                  '<b>Prescription items per thousand population: </b>' + Highcharts.numberFormat(this.point.y, decimalPoint = 0, thousandsSep = ',')
                return outHTML;
            }
            "
              )
            )
          )
      }
    })

    # create the dynamic text for the chart
    output$copd_trend_chart_text <- renderUI({
      tags$text(
        class = "highcharts-caption",
        switch(input$trend_metric,
          "national_items" = "The number of COPD items increased from 53.4 million in 2015/16 to 55.5 million in 2020/21.",
          "core_item_per_pop" = "Due to different population sizes, to compare Core20 to non Core20 areas the figures have been standardised based on the area populations.  Both groups show a similar pattern of slight growth with the Core20 areas showing slightly higher volumes of COPD items per population."
        )
      )
    })

    # downloadable dataset - include all figures from charts
    mod_nhs_download_server(
      id = "copd_prescribing_trend_download",
      filename = "copd_prescribing_trend.csv",
      export_data = antibioticPrescribingScrollytellR::copd_prescribing_trend_download
    )

    # Chart Two: exemption trend line chart ------------------------------------

    output$copd_exemption_trend_chart <- highcharter::renderHighchart({

      # create the plot data
      plot_data <- antibioticPrescribingScrollytellR::copd_prescribing_trend %>%
        dplyr::filter(CORE20_CLASSIFICATION == "Overall") %>%
        dplyr::arrange(FINANCIAL_YEAR)

      # create the plot
      highcharter::highchart() %>%
        highcharter::hc_chart(type = "line") %>%
        highcharter::hc_xAxis(categories = unique(
          antibioticPrescribingScrollytellR::copd_prescribing_trend$FINANCIAL_YEAR
        )) %>%
        highcharter::hc_yAxis(title = list(text = "Proportion of prescription items (%)")) %>%
        theme_nhsbsa(stack = NA) %>%
        highcharter::hc_add_series(
          data = plot_data,
          type = "line",
          highcharter::hcaes(
            x = FINANCIAL_YEAR,
            y = AE_ITEMS_PROP
          ),
          name = "Age Exemption",
          marker = list(symbol = "circle")
        ) %>%
        highcharter::hc_add_series(
          data = plot_data,
          type = "line",
          highcharter::hcaes(
            x = FINANCIAL_YEAR,
            y = OE_ITEMS_PROP
          ),
          name = "Other Exemption",
          marker = list(symbol = "circle")
        ) %>%
        highcharter::hc_add_series(
          data = plot_data,
          type = "line",
          highcharter::hcaes(
            x = FINANCIAL_YEAR,
            y = CP_ITEMS_PROP
          ),
          name = "Charge Paid",
          marker = list(symbol = "circle")
        ) %>%
        highcharter::hc_credits(enabled = TRUE) %>%
        highcharter::hc_tooltip(
          shared = FALSE,
          formatter = htmlwidgets::JS(
            paste0(
              "
            function() {
                outHTML =
                  '<b>Category: </b>' + this.series.name + '<br>' +
                  '<b>Financial Year: </b>' + this.point.FINANCIAL_YEAR + '<br>' +
                  '<b>Proportion of prescription items: </b>' + Highcharts.numberFormat(this.point.y, 1) + '%'
                return outHTML;
            }
            "
            )
          )
        )
    })

    # downloadable dataset - include all figures from charts
    mod_nhs_download_server(
      id = "copd_exemption_trend_download",
      filename = "copd_exemption_trend.csv",
      export_data = antibioticPrescribingScrollytellR::copd_exemption_trend_download
    )

    # Chart Three: COPD value boxes --------------------------------------------

    output$value_boxes_copd <- renderUI({

      # Create table
      tagList(
        nhs_grid_2_col(
          p(
            tippy(
              text = "COPD Prescription items per patient",
              tooltip = "Average number of prescrition items prescribed per patient in 2020/21"
            )
          ),
          nhs_grid_2_col(
            nhs_value_box(
              group = "Core20",
              value = antibioticPrescribingScrollytellR::copd_core20_v_non_core20_summary %>%
                dplyr::filter(patient_classification == "CORE20") %>%
                dplyr::pull(items_per_patient),
              icon = "prescription"
            ),
            nhs_value_box(
              group = "Non_Core20",
              value = antibioticPrescribingScrollytellR::copd_core20_v_non_core20_summary %>%
                dplyr::filter(patient_classification == "NON-CORE20") %>%
                dplyr::pull(items_per_patient),
              icon = "prescription"
            )
          )
        ),
        nhs_grid_2_col(
          p(
            tippy(
              text = "Drug cost per patient",
              tooltip = "Average net ingredient drug cost (£) per patient for prescribing in 2020/21"
            )
          ),
          nhs_grid_2_col(
            nhs_value_box(
              group = "Core20",
              value = antibioticPrescribingScrollytellR::copd_core20_v_non_core20_summary %>%
                dplyr::filter(patient_classification == "CORE20") %>%
                dplyr::mutate(nic_per_patient = paste0("£", nic_per_patient)) %>%
                dplyr::pull(nic_per_patient),
              icon = "coins"
            ),
            nhs_value_box(
              group = "Non_Core20",
              value = antibioticPrescribingScrollytellR::copd_core20_v_non_core20_summary %>%
                dplyr::filter(patient_classification == "NON-CORE20") %>%
                dplyr::mutate(nic_per_patient = paste0("£", nic_per_patient)) %>%
                dplyr::pull(nic_per_patient),
              icon = "coins"
            )
          )
        ),
        nhs_grid_2_col(
          p(
            tippy(
              text = "Percentage of prescribing paid for",
              tooltip = "Percentage of prescription items for 2020/21 where the patient made some payment, either by prescription charge or PPC certificate"
            )
          ),
          nhs_grid_2_col(
            nhs_value_box(
              group = "Core20",
              value = antibioticPrescribingScrollytellR::copd_core20_v_non_core20_summary %>%
                dplyr::filter(patient_classification == "CORE20") %>%
                dplyr::pull(prop_cp),
              icon = "pills"
            ),
            nhs_value_box(
              group = "Non_Core20",
              value = antibioticPrescribingScrollytellR::copd_core20_v_non_core20_summary %>%
                dplyr::filter(patient_classification == "NON-CORE20") %>%
                dplyr::pull(prop_cp),
              icon = "pills"
            )
          )
        ),
        nhs_grid_2_col(
          p(
            tippy(
              text = "Percentage of prescribing age exempt",
              tooltip = "Percentage of prescription items for 2020/21 where the patient claimed charge exemption based on age (aged less than 19 or 60 plus)"
            )
          ),
          nhs_grid_2_col(
            nhs_value_box(
              group = "Core20",
              value = antibioticPrescribingScrollytellR::copd_core20_v_non_core20_summary %>%
                dplyr::filter(patient_classification == "CORE20") %>%
                dplyr::pull(prop_ae),
              icon = "pills"
            ),
            nhs_value_box(
              group = "Non_Core20",
              value = antibioticPrescribingScrollytellR::copd_core20_v_non_core20_summary %>%
                dplyr::filter(patient_classification == "NON-CORE20") %>%
                dplyr::pull(prop_ae),
              icon = "pills"
            )
          )
        ),
        nhs_grid_2_col(
          p(
            tippy(
              text = "Percentage of prescribing other charge exempt",
              tooltip = "Percentage of prescription items for 2020/21 where the patient claimed charge exemption for any other reason than age or PPC"
            )
          ),
          nhs_grid_2_col(
            nhs_value_box(
              group = "Core20",
              value = antibioticPrescribingScrollytellR::copd_core20_v_non_core20_summary %>%
                dplyr::filter(patient_classification == "CORE20") %>%
                dplyr::pull(prop_oe),
              icon = "pills"
            ),
            nhs_value_box(
              group = "Non_Core20",
              value = antibioticPrescribingScrollytellR::copd_core20_v_non_core20_summary %>%
                dplyr::filter(patient_classification == "NON-CORE20") %>%
                dplyr::pull(prop_oe),
              icon = "pills"
            )
          )
        ),
        tags$text(
          class = "highcharts-caption",
          style = "font-size: 9pt",
          "Excluding 0.7% of COPD prescriptions in 2020/21 that could not be ",
          "assigned to a identifiable patient with a valid residential location."
        ),
        br(),
        tags$text(
          class = "highcharts-caption",
          style = "font-size: 9pt",
          "Prescription charge proportion splits exclude around 3% of prescription ",
          "items where a exemption category could not be identified."
        )
      )
    })


    # Chart Four: COPD Prescribing by IMD Decile -------------------------------

    # Define the selected metric label - for yAxis
    imd_metric_lbl <- reactive({
      switch(input$metric_copd_decile,
        "item_per_pat" = "Prescription items per patient",
        "cost_per_pat" = "Drug cost per patient (£)",
        "pat_per_pop" = "Patients per thousand population",
        "item_per_pop" = "Prescription items per thousand population",
        "cost_per_pop" = "Drug cost per thousand population (£)"
      )
    })

    # Edited metric label just - for tooltip
    imd_metric_lbl_edit <- reactive({
      trimws(gsub("[^[:alnum:][:space:]]", "", imd_metric_lbl()))
    })

    # Define IMD metric text
    imd_metric_text <- reactive({
      if (input$metric_copd_decile == "cost_per_pat" | input$metric_copd_decile == "cost_per_pop") {
        paste0(
          "<b>", imd_metric_lbl_edit(), ":</b> £{point.y:,.0f}"
        )
      } else if (input$metric_copd_decile == "item_per_pat") {
        paste0(
          "<b>", imd_metric_lbl_edit(), ":</b> {point.y:,.1f}"
        )
      } else {
        paste0(
          "<b>", imd_metric_lbl_edit(), ":</b> {point.y:,.0f}"
        )
      }
    })

    # Define IMD tooltip text
    imd_tooltip_text <- reactive({
      paste0(
        "<b>Core20 Classification:</b> {point.CORE20_CLASSIFICATION} <br>",
        "<b>2019 IMD decile:</b> {point.x} <br>",
        imd_metric_text()
      )
    })

    # create the chart object
    output$copd_imd_chart <- highcharter::renderHighchart({

      # define the dataset for the selected local authority
      antibioticPrescribingScrollytellR::copd_imd %>%
        dplyr::mutate(
          METRIC = switch(input$metric_copd_decile,
            "item_per_pat" = ITEMS_PER_PATIENT,
            "cost_per_pat" = NIC_PER_PATIENT,
            "pat_per_pop" = PATIENTS_PER_1K_POP,
            "item_per_pop" = ITEMS_PER_1K_POP,
            "cost_per_pop" = NIC_PER_1K_POP
          ),
          CORE20_CLASSIFICATION = stringr::str_to_title(CORE20_CLASSIFICATION)
        ) %>%
        highcharter::hchart(
          type = "column",
          highcharter::hcaes(
            x = IMD_DECILE,
            y = METRIC,
            group = CORE20_CLASSIFICATION
          )
        ) %>%
        theme_nhsbsa() %>%
        highcharter::hc_yAxis(title = list(text = imd_metric_lbl())) %>%
        highcharter::hc_xAxis(
          min = 1,
          max = 11, # Pad to ensure we can see the 10 label
          categories = c(NA, "1<br>Most<br>deprived", rep(NA, 8), "10<br>Least<br>deprived"),
          labels = list(step = 9),
          title = list(text = "Deprivation decile")
        ) %>%
        highcharter::hc_credits(enabled = TRUE) %>%
        highcharter::hc_tooltip(
          headerFormat = "",
          pointFormat = imd_tooltip_text()
        )
    })

    # create the dynamic text for the chart
    output$copd_imd_chart_text <- renderUI({
      tags$text(
        class = "highcharts-caption",
        switch(input$metric_copd_decile,
          "item_per_pat" = "COPD prescribing rates drop from 11.8 items per patient in the most deprived areas to 7.1 items per patient in the least deprived areas.",
          "cost_per_pat" = "The annual drug cost per patient for COPD prescribing ranges from £191 in the most deprived areas to £133 in the least deprived areas.",
          "pat_per_pop" = "In the most deprived areas 133 out of every 1000 people receive COPD prescribing, with this rate dropping to 94 people per 1000 in the least deprived areas.",
          "item_per_pop" = "In the most deprived areas 1567 COPD items are prescribed per 1000 of the population compared, which is more than double the rate for the least deprived areas (673).",
          "cost_per_pop" = "In the most deprived areas the drug cost of COPD prescribing per 1000 of the population (£25,473) is double the rate for the least deprived areas (£12,557)."
        )
      )
    })

    # downloadable dataset - include all figures from charts
    mod_nhs_download_server(
      id = "copd_imd_decile_metrics_download",
      filename = "copd_imd_decile_metrics.csv",
      export_data = antibioticPrescribingScrollytellR::copd_imd_download
    )

    # Chart Five: COPD Prescribing by age and gender ---------------------------

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

    # define the selected metric label
    age_metric_lbl <- reactive({
      switch(input$metric_copd_gender,
        "item_per_pat" = "Prescription items per patient",
        "cost_per_pat" = "Drug cost per patient (£)",
        "pat_per_pop" = "Patients per thousand population",
        "item_per_pop" = "Prescription items per thousand population",
        "cost_per_pop" = "Drug cost per thousand population (£)"
      )
    })

    # Edited metric label just - for tooltip
    age_metric_lbl_edit <- reactive({
      trimws(gsub("[^[:alnum:][:space:]]", "", age_metric_lbl()))
    })

    # Define tooltip text
    age_tooltip_text <- reactive({
      if (input$metric_copd_gender == "cost_per_pat" | input$metric_copd_gender == "cost_per_pop") {
        paste0(
          "<b>{series.name} </b> <br>",
          "<b>Age Band:</b> {point.AGE_BAND} <br>",
          "<b>", age_metric_lbl_edit(), ":</b> £{point.y:,.0f} <br><br>",
          "<b>Value Comparison ({point.AGE_BAND})</b><br>",
          "<b>Core 20 - Male: </b> £{point.Core20_M:,.0f} <br>",
          "<b>Core 20 - Female: </b> £{point.Core20_F:,.0f} <br>",
          "<b>Non Core - 20 Male: </b> £{point.Non Core20_M:,.0f} <br>",
          "<b>Non Core - 20 Female: </b> £{point.Non Core20_F:,.0f}"
        )
      } else if (input$metric_copd_gender == "item_per_pat") {
        paste0(
          "<b>{series.name} </b> <br>",
          "<b>Age Band:</b> {point.AGE_BAND} <br>",
          "<b>", age_metric_lbl_edit(), ":</b> {point.y:,.1f} <br><br>",
          "<b>Value Comparison ({point.AGE_BAND})</b><br>",
          "<b>Core 20 - Male: </b> {point.Core20_M:,.1f} <br>",
          "<b>Core 20 - Female: </b> {point.Core20_F:,.1f} <br>",
          "<b>Non Core - 20 Male: </b> {point.Non Core20_M:,.1f} <br>",
          "<b>Non Core - 20 Female: </b> {point.Non Core20_F:,.1f}"
        )
      } else {
        paste0(
          "<b>{series.name} </b> <br>",
          "<b>Age Band:</b> {point.AGE_BAND} <br>",
          "<b>", age_metric_lbl_edit(), ":</b> {point.y:,.0f} <br><br>",
          "<b>Value Comparison ({point.AGE_BAND})</b><br>",
          "<b>Core 20 - Male: </b> {point.Core20_M:,.0f} <br>",
          "<b>Core 20 - Female: </b> {point.Core20_F:,.0f} <br>",
          "<b>Non Core - 20 Male: </b> {point.Non Core20_M:,.0f} <br>",
          "<b>Non Core - 20 Female: </b> {point.Non Core20_F:,.0f}"
        )
      }
    })

    # Generate chart data
    chartdata <- reactive({
      antibioticPrescribingScrollytellR::copd_age_gender %>%
        dplyr::mutate(
          METRIC = switch(input$metric_copd_gender,
            "item_per_pat" = ITEMS_PER_PATIENT,
            "cost_per_pat" = NIC_PER_PATIENT,
            "pat_per_pop" = PATS_PER_1K_POPULATION,
            "item_per_pop" = ITEMS_PER_1K_POPULATION,
            "cost_per_pop" = NIC_PER_1K_POPULATION
          )
        ) %>%
        dplyr::select(CORE20_CLASSIFICATION, AGE_BAND, GENDER, METRIC) %>%
        tidyr::pivot_wider(names_from = c(CORE20_CLASSIFICATION, GENDER), values_from = METRIC)
    })

    # create the trend chart object
    output$copd_age_gender_chart <- highcharter::renderHighchart({

      # create the plot data
      age_chart <- highcharter::highchart() %>%
        highcharter::hc_chart(type = "line") %>%
        highcharter::hc_xAxis(categories = unique(chartdata()$AGE_BAND)) %>%
        highcharter::hc_yAxis(
          min = 0,
          title = list(text = age_metric_lbl())
        ) %>%
        theme_nhsbsa(stack = NA) %>%
        highcharter::hc_add_series(
          data = chartdata(),
          type = "line",
          highcharter::hcaes(
            x = AGE_BAND,
            y = Core20_F
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
          data = chartdata(),
          type = "line",
          highcharter::hcaes(
            x = AGE_BAND,
            y = Core20_M
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
          data = chartdata(),
          type = "line",
          highcharter::hcaes(
            x = AGE_BAND,
            y = `Non Core20_F`
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
          data = chartdata(),
          type = "line",
          highcharter::hcaes(
            x = AGE_BAND,
            y = `Non Core20_M`
          ),
          name = "Non Core20 - Male",
          color = "#ED8B00",
          marker = list(
            symbol = stringr::str_glue("url({data_uri})", data_uri = male_noncore20),
            radius = 2
          ),
          icon = male_noncore20
        ) %>%
        highcharter::hc_credits(enabled = TRUE) %>%
        highcharter::hc_plotOptions(series = list(animation = FALSE)) %>%
        highcharter::hc_tooltip(
          headerFormat = "",
          pointFormat = age_tooltip_text()
        )
    })

    # create the dynamic text for the chart
    output$copd_age_gender_chart_text <- renderUI({
      tags$text(
        class = "highcharts-caption",
        switch(input$metric_copd_gender,
          "item_per_pat" = "The number of items per patient rises in line with age before peaking at ages 70-79, with Core20 areas showing higher figures than non Core20 areas.",
          "cost_per_pat" = "The COPD drug cost per patient rises in line with age before peaking at ages 70-79, with Core20 areas typically showing higher figures than non Core20 areas except for patients aged 90 and over.",
          "pat_per_pop" = "There is a noticeable gap between female and male patients receiving COPD prescribing per thousand of the population suggesting COPD is either more prevalant or more often diagnosed in females.",
          "item_per_pop" = "There is a large gap between the number of COPD prescription items per thousand of the population when comparing Core20 areas to non Core20 areas.",
          "cost_per_pop" = "There is a large gap between the COPD drug cost per thousand of the population when comparing Core20 areas to non Core20 areas."
        )
      )
    })

    # downloadable dataset - include all figures from charts
    mod_nhs_download_server(
      id = "copd_age_gender_metrics_download",
      filename = "copd_age_gender_metrics.csv",
      export_data = antibioticPrescribingScrollytellR::copd_age_gender_download
    )

    # Chart Six: COPD Local Authority Charts -----------------------------------

    # Get trend data
    la_trend_data <- reactive({
      antibioticPrescribingScrollytellR::copd_local_authority_lm %>%
        dplyr::filter(
          metric == switch(input$metric_copd_la,
            "item_per_pat" = "items_per_patient",
            "cost_per_pat" = "nic_per_patient",
            "pat_per_pop" = "pats_per_1k_pop",
            "item_per_pop" = "items_per_1k_pop",
            "cost_per_pop" = "nic_per_1k_pop"
          )
        )
    })

    # define the chart data object
    la_chart_data <- reactive({
      antibioticPrescribingScrollytellR::copd_local_authority %>%
        dplyr::mutate(
          METRIC = switch(input$metric_copd_la,
            "item_per_pat" = ITEMS_PER_PATIENT,
            "cost_per_pat" = NIC_PER_PATIENT,
            "pat_per_pop" = PATS_PER_1K_POP,
            "item_per_pop" = ITEMS_PER_1K_POP,
            "cost_per_pop" = NIC_PER_1K_POP
          ),
          METRIC_DIFF = switch(input$metric_copd_la,
            "item_per_pat" = DIFF_ITEMS_PER_PATIENT,
            "cost_per_pat" = DIFF_NIC_PER_PATIENT,
            "pat_per_pop" = DIFF_PATS_PER_1K_POP,
            "item_per_pop" = DIFF_ITEMS_PER_1K_POP,
            "cost_per_pop" = DIFF_NIC_PER_1K_POP
          )
        )
    })

    # define the selected metric label
    la_metric_lbl <- reactive({
      switch(input$metric_copd_la,
        "item_per_pat" = "Prescription items per patient",
        "cost_per_pat" = "Drug cost per patient (£)",
        "pat_per_pop" = "Patients per thousand population",
        "item_per_pop" = "Prescription items per thousand population",
        "cost_per_pop" = "Drug cost per thousand population (£)"
      )
    })

    # Edited metric label just - for tooltip
    la_metric_lbl_edit <- reactive({
      trimws(gsub("[^[:alnum:][:space:]]", "", la_metric_lbl()))
    })

    # Define tooltip text for scatter
    la_metric_text <- reactive({
      if (input$metric_copd_la == "cost_per_pat" | input$metric_copd_la == "cost_per_pop") {
        paste0(
          "<b>", la_metric_lbl_edit(), ":</b> £{point.y:,.0f}"
        )
      } else if (input$metric_copd_la == "item_per_pat") {
        paste0(
          "<b>", la_metric_lbl_edit(), ":</b> {point.y:,.1f}"
        )
      } else {
        paste0(
          "<b>", la_metric_lbl_edit(), ":</b> {point.y:,.0f}"
        )
      }
    })

    # Define tooltip text for map
    la_map_metric_text <- reactive({
      if (input$metric_copd_la == "cost_per_pat" | input$metric_copd_la == "cost_per_pop") {
        paste0(
          "<b>", la_metric_lbl_edit(), ":</b> £{point.value:,.0f}"
        )
      } else if (input$metric_copd_la == "item_per_pat") {
        paste0(
          "<b>", la_metric_lbl_edit(), ":</b> {point.value:,.1f}"
        )
      } else {
        paste0(
          "<b>", la_metric_lbl_edit(), ":</b> {point.value:,.0f}"
        )
      }
    })

    # Tooltip text
    la_tooltip_text <- reactive({
      paste0(
        "<b>Region:</b> {point.REG_NAME} <br>",
        "<b>Local authority:</b> {point.LAD_NAME} <br>",
        "<b>2019 IMD rank:</b> {point.x} <br>",
        la_metric_text()
      )
    })

    # Map tooltip text
    la_map_tooltip_text <- reactive({
      paste0(
        "<b>Local Authority: </b> {point.LAD_NAME}<br>",
        "<b>IMD Rank: </b> {point.IMD_RANK}<br>",
        la_map_metric_text()
      )
    })

    # create the local authority scatter plot
    output$chart_lad_scatter_copd <- highcharter::renderHighchart({
      la_chart_data() %>%
        # Indicate selected region
        dplyr::mutate(
          GROUP = ifelse(
            test = REG_NAME == input$input_geography_region_copd,
            yes = "Selected region",
            no = "Other regions"
          )
        ) %>%
        highcharter::hchart(
          type = "scatter",
          highcharter::hcaes(
            x = IMD_RANK,
            y = METRIC,
            group = GROUP
          ),
          animation = FALSE
        ) %>%
        highcharter::hc_add_series(
          data = la_trend_data(),
          type = "line",
          highcharter::hcaes(
            x = imd_rank,
            y = value
          ),
          showInLegend = T,
          lineWidth = 1,
          name = "Expected rate (given deprivation) based on all English local authorities",
          color = "black",
          marker = list(radius = 0),
          dashStyle = "Solid"
        ) %>%
        theme_nhsbsa(stack = NA, palette = "highlight") %>%
        highcharter::hc_credits(enabled = TRUE) %>%
        highcharter::hc_legend(reversed = FALSE) %>%
        highcharter::hc_xAxis(
          min = 1,
          max = 340, # Pad to ensure we can see the 309 label
          categories = c(NA, "1<br>Most deprived", rep(NA, 307), "309<br>Least deprived"),
          labels = list(step = 308),
          title = list(text = "Deprivation rank")
        ) %>%
        highcharter::hc_yAxis(
          min = 0,
          max = max(la_chart_data()$METRIC),
          title = list(text = la_metric_lbl())
        ) %>%
        highcharter::hc_tooltip(
          headerFormat = "",
          pointFormat = la_tooltip_text()
        ) %>%
        highcharter::hc_caption(
          text = paste0(
            "Excluding 0.7% of prescription items where the patient's local authority could not be identified.",
            "<br>Excluding any Local Authorities with less than 5 patients receiving applicable prescribing."
          ),
          align = "left"
        ) %>%
        # prevent the lines from being picked up by the hover
        highcharter::hc_plotOptions(
          series = list(
            states = list(
              inactive = list(opacity = 1)
            )
          ),
          line = list(
            states = list(
              hover = list(enabled = F)
            ),
            enableMouseTracking = F
          )
        )
    })

    # dynamic text for scatter plot
    # Take-up of selected region
    # this part will change depends on the region selection from the reactive value.
    output$chart_lad_scatter_text <- renderUI({

      # dynamic text
      focus_data_df <- reactive({
        la_chart_data() %>%
          dplyr::filter(REG_NAME == input$input_geography_region_copd)
      })

      # which is the highest take-up local authority in the selected region.
      highest_take_up_la <- reactive({
        focus_data_df() %>%
          dplyr::filter(METRIC_DIFF == max(METRIC_DIFF)) %>%
          dplyr::filter(IMD_RANK == min(IMD_RANK)) %>% # add tie break
          dplyr::select(LAD_NAME, IMD_RANK, METRIC_DIFF)
      })

      # which is the lowest take-up local authority in the selected region.
      lowest_take_up_la <- reactive({
        focus_data_df() %>%
          dplyr::filter(METRIC_DIFF == min(METRIC_DIFF)) %>%
          dplyr::filter(IMD_RANK == min(IMD_RANK)) %>% # add tie break
          dplyr::select(LAD_NAME, IMD_RANK, METRIC_DIFF)
      })

      # create the main text object
      main_text <- paste(
        # Sentence 1
        "For ",
        tags$b(input$input_geography_region_copd), ", ",
        tags$b(highest_take_up_la()$LAD_NAME), " is the local authority furthest above the expected rate for ",
        tags$b(la_metric_lbl_edit()), " based on their deprivation rank (",
        tags$b(highest_take_up_la()$IMD_RANK), "/309). ",

        # Sentence 2
        tags$b(lowest_take_up_la()$LAD_NAME), " is the furthest below the expected rate for ",
        tags$b(la_metric_lbl_edit()), " in the ",
        tags$b(input$input_geography_region_copd), " region, with an IMD rank of ",
        tags$b(lowest_take_up_la()$IMD_RANK), " out of all 309 local authorities in England.",
        sep = ""
      )

      # create output object
      tags$text(
        class = "highcharts-caption",
        HTML(paste(main_text))
      )
    })

    # map data - filter to region
    map_data_copd <- reactive({
      la_chart_data() %>%
        dplyr::filter(REG_NAME == input$input_geography_region_copd)
    })

    # region focused map plot - click local authority and show decile distribution
    output$map_selected_region_la_copd <- highcharter::renderHighchart({
      highcharter::highchart() %>%
        highcharter::hc_add_series_map(
          map = antibioticPrescribingScrollytellR::la_map %>%
            dplyr::inner_join(map_data_copd(), by = c("LAD_NAME" = "LAD_NAME")) %>%
            dplyr::select(LAD_NAME) %>%
            sf::st_transform(crs = 27700) %>%
            geojsonsf::sf_geojson() %>%
            jsonlite::fromJSON(simplifyVector = FALSE),
          df = map_data_copd(),
          joinBy = "LAD_NAME",
          value = "METRIC",
          tooltip = list(
            headerFormat = "",
            pointFormat = la_map_tooltip_text()
          ),
          animation = FALSE
        ) %>%
        theme_nhsbsa() %>%
        highcharter::hc_credits(enabled = TRUE) %>%
        highcharter::hc_legend(
          enabled = TRUE,
          verticalAlign = "bottom",
          title = list(text = la_metric_lbl())
        ) %>%
        highcharter::hc_colorAxis(
          min = min(la_chart_data()$METRIC),
          max = max(la_chart_data()$METRIC)
        ) %>%
        highcharter::hc_plotOptions(
          map = list(
            events = list(
              click = htmlwidgets::JS(
                paste0(
                  "
                function(event) {
                  Shiny.setInputValue('", id, "-mapclick_copd', event.point.LAD_NAME);
                }
                "
                )
              )
            )
          )
        )
    })

    # IMD chart
    observeEvent(input$mapclick_copd, {
      output$chart_selected_la_imd_copd <- highcharter::renderHighchart({
        req(input$mapclick_copd)

        # define the dataset for the selected local authority
        antibioticPrescribingScrollytellR::ons_population_lad %>%
          dplyr::filter(LAD_NAME == input$mapclick_copd) %>%
          highcharter::hchart(
            type = "column",
            highcharter::hcaes(
              x = IMD_DECILE,
              y = PROP_ONS_POPULATION_0_90,
              group = CORE20_CLASSIFICATION
            )
          ) %>%
          theme_nhsbsa() %>%
          highcharter::hc_title(text = glue::glue({
            input$mapclick_copd
          })) %>%
          highcharter::hc_yAxis(
            title = list(
              text = "Percentage of population in decile",
              align = "middle"
            ),
            labels = list(format = "{value:.0f}%")
          ) %>%
          highcharter::hc_xAxis(
            min = 1,
            max = 11, # Pad to ensure we can see the 10 label
            categories = c(NA, "1<br>Most<br>deprived", rep(NA, 8), "10<br>Least<br>deprived"),
            labels = list(step = 9),
            title = list(text = "Deprivation decile")
          ) %>%
          highcharter::hc_credits(enabled = TRUE) %>%
          highcharter::hc_tooltip(
            shared = FALSE,
            formatter = highcharter::JS(
              "
              function () {
                outHTML =
                  '<b>2019 IMD decile: </b>' + this.point.x + '<br>' +
                  '<b>Percentage: </b>' + Highcharts.numberFormat(this.point.y, 1) + '%'
                return outHTML
              }
              "
            )
          )
      })
    })

    # downloadable dataset - include all figures from charts
    mod_nhs_download_server(
      id = "copd_la_metrics_download",
      filename = "copd_la_metrics.csv",
      export_data = antibioticPrescribingScrollytellR::copd_local_authority_download
    )

    # Chart Seven: COPD LSOA Chart ---------------------------------------------

    # Get trend data
    lsoa_text <- reactive({
      antibioticPrescribingScrollytellR::copd_lsoa_lm_summary_la %>%
        dplyr::filter(lad_name == input$input_geography_copd) %>%
        dplyr::filter(metric == input$input_metric_copd)
    })

    # Full text with bold added
    lsoa_text_edit <- reactive({
      paste0(
        "Of the ", tags$b(lsoa_text()$lsoa_count),
        " LSOAs in ", tags$b(lsoa_text()$lad_name),
        " Local Authority, ", tags$b(lsoa_text()$core20_count),
        " are classified as Core20 areas. ", br(),
        "For the ", tags$b(lsoa_metric_lbl()),
        " metric, ", tags$b(lsoa_text()$potential_outliers),
        " LSOAs may be considered potential outliers.", br(),
        "Typically in ", tags$b(lsoa_text()$lad_name),
        ", LSOA values are ", tags$b(lsoa_text()$trend),
        " what may be expected, given their IMD rank."
      )
    })

    # define the dataset for the LSOA scatter points
    scatter_data <- reactive({
      antibioticPrescribingScrollytellR::copd_lsoa %>%
        # filter to only the selected Local Authority
        dplyr::filter(lad_name == input$input_geography_copd) %>%
        # define which column to use as the reporting metric
        dplyr::mutate(metric = .[[input$input_metric_copd]])
    })

    # define the dataset for the national trend lines
    line_data <- reactive({
      antibioticPrescribingScrollytellR::copd_lsoa_lm %>%
        # filter to only results for the chosen metric
        dplyr::filter(metric == input$input_metric_copd)
    })

    # define the selected metric label
    lsoa_metric_lbl <- reactive({
      switch(input$input_metric_copd,
        "items_per_patient" = "Prescription items per patient",
        "nic_per_patient" = "Drug cost per patient (£)",
        "patients_per_1k_pop" = "Patients per thousand population",
        "items_per_1k_pop" = "Prescription items per thousand population",
        "nic_per_1k_pop" = "Drug cost per thousand population (£)"
      )
    })

    # Edited metric label just - for tooltip
    lsoa_metric_lbl_edit <- reactive({
      trimws(gsub("[^[:alnum:][:space:]]", "", lsoa_metric_lbl()))
    })

    # Define IMD tooltip text
    lsoa_metric_text <- reactive({
      if (input$input_metric_copd == "nic_per_1k_pop" | input$input_metric_copd == "nic_per_patient") {
        paste0(
          "<b>", lsoa_metric_lbl_edit(), ":</b> £{point.metric:,.0f}"
        )
      } else if (input$input_metric_copd == "items_per_patient") {
        paste0(
          "<b>", lsoa_metric_lbl_edit(), ":</b> {point.metric:,.1f}"
        )
      } else {
        paste0(
          "<b>", lsoa_metric_lbl_edit(), ":</b> {point.metric:,.0f}"
        )
      }
    })

    # Tooltip text
    lsoa_tooltip_text <- reactive({
      paste0(
        "<b>{point.lsoa_name}</b> <br>",
        "<b>Local Authority:</b> {point.lad_name} <br><br>",
        "<b>Urban/Rural:</b> {point.urban_rural_group} <br>",
        "<b>IMD Decile:</b> {point.imd_decile} <br>",
        "<b>2019 IMD rank (out of 32,844):</b> {point.imd_rank:,.0f} <br>",
        lsoa_metric_text()
      )
    })

    # create the chart object
    output$output_lsoa_scatter_chart_copd <- highcharter::renderHighchart({

      # create the chart
      lsoa_scatter_chart <- highcharter::highchart()

      # check if the selected Local Authority has any Core20 points (captured in text object)
      if (lsoa_text()$core20_count != 0) {
        lsoa_scatter_chart <- lsoa_scatter_chart %>%
          # add scatter series for Core20 LSOAs
          highcharter::hc_add_series(
            data = scatter_data() %>%
              dplyr::filter(core20_classification == "CORE20"),
            type = "scatter",
            highcharter::hcaes(
              x = imd_rank,
              y = metric
            ),
            name = "Core20",
            showInLegend = T,
            marker = list(symbol = "circle", fillColor = "#003087")
          )
      }

      # add non Core20 and line series to chart
      lsoa_scatter_chart <- lsoa_scatter_chart %>%
        # add scatter series for non-Core20 LSOAs
        highcharter::hc_add_series(
          data = scatter_data() %>%
            dplyr::filter(core20_classification == "NON-CORE20"),
          type = "scatter",
          highcharter::hcaes(
            x = imd_rank,
            y = metric
          ),
          name = "Non Core20",
          showInLegend = T,
          marker = list(symbol = "circle", fillColor = "#ED8B00")
        ) %>%
        # add the central trend line
        highcharter::hc_add_series(
          line_data(),
          type = "spline",
          highcharter::hcaes(imd_rank, value),
          showInLegend = TRUE,
          lineWidth = 1,
          name = "Expected rate (given deprivation) based on all English LSOAs",
          color = "black",
          marker = list(radius = 0),
          dashStyle = "Solid"
        ) %>%
        highcharter::hc_add_series(
          line_data(),
          "spline",
          highcharter::hcaes(imd_rank, value_lower),
          showInLegend = F,
          lineWidth = 1,
          color = "red",
          marker = list(radius = 0),
          dashStyle = "longdash"
        ) %>%
        highcharter::hc_add_series(
          line_data(),
          "spline",
          highcharter::hcaes(imd_rank, value_upper),
          showInLegend = F,
          lineWidth = 1,
          color = "red",
          marker = list(radius = 0),
          dashStyle = "longdash"
        ) %>%
        # apply x axis formatting
        highcharter::hc_xAxis(
          min = 0,
          max = 35000, # Pad to ensure we can see the final label
          categories = c("Most<br>deprived", rep(NA, 34999), "Least<br>deprived"),
          tickInterval = 35000,
          title = list(text = "Deprivation rank"),
          endOnTick = TRUE,
          plotBands = list(
            list(
              label = list(text = "Core20"),
              from = 0,
              to = 6568,
              color = "#f0f0f0"
            )
          )
        ) %>%
        # apply y axis formatting
        highcharter::hc_yAxis(
          min = round(min(antibioticPrescribingScrollytellR::copd_lsoa[input$input_metric_copd]), 0),
          max = round(max(antibioticPrescribingScrollytellR::copd_lsoa[input$input_metric_copd]), 0),
          title = list(text = lsoa_metric_lbl())
        ) %>%
        # prevent the lines from being picked up by the hover
        highcharter::hc_plotOptions(
          spline = list(
            states = list(
              hover = list(enabled = F)
            ),
            enableMouseTracking = F
          ),
          series = list(
            states = list(
              inactive = list(opacity = 1)
            )
          )
        ) %>%
        highcharter::hc_credits(enabled = TRUE) %>%
        # define the hover tooltip
        highcharter::hc_tooltip(
          headerFormat = "",
          pointFormat = lsoa_tooltip_text()
        ) %>%
        # apply NHSBSA theme
        theme_nhsbsa(stack = NA)
    })

    # downloadable dataset - include all figures from charts
    mod_nhs_download_server(
      id = "copd_lsoa_metrics_download",
      filename = "copd_lsoa_metrics.csv",
      export_data = antibioticPrescribingScrollytellR::copd_lsoa_download
    )

    # create the dynamic text for the chart
    output$outlier_text <- renderUI({
      tags$text(
        class = "highcharts-caption",
        HTML(lsoa_text_edit())
      )
    })

    # Chart Eight: drug dumbbell chart -----------------------------------------

    # define the chart data object
    chart_data <- reactive({
      antibioticPrescribingScrollytellR::copd_chemsub %>%
        # define which fields are to be used as the reporting metrics
        dplyr::mutate(
          CORE20_METRIC = switch(input$metric,
            "item_per_pat" = CORE20_ITEMS_PER_PATIENT,
            "cost_per_pat" = CORE20_NIC_PER_PATIENT,
            "pat_per_pop" = CORE20_PATS_PER_1K_POP,
            "item_per_pop" = CORE20_ITEMS_PER_1K_POP,
            "cost_per_pop" = CORE20_NIC_PER_1K_POP,
            "pct_pats" = CORE20_PCT_PATS,
            "pct_items" = CORE20_PCT_ITEMS,
            "pct_cost" = CORE20_PCT_NIC
          ),
          NONCORE20_METRIC = switch(input$metric,
            "item_per_pat" = NONCORE20_ITEMS_PER_PATIENT,
            "cost_per_pat" = NONCORE20_NIC_PER_PATIENT,
            "pat_per_pop" = NONCORE20_PATS_PER_1K_POP,
            "item_per_pop" = NONCORE20_ITEMS_PER_1K_POP,
            "cost_per_pop" = NONCORE20_NIC_PER_1K_POP,
            "pct_pats" = NONCORE20_PCT_PATS,
            "pct_items" = NONCORE20_PCT_ITEMS,
            "pct_cost" = NONCORE20_PCT_NIC
          )
        ) %>%
        # define a sort order based on selection
        dplyr::arrange(desc(switch(input$sort_group,
          "CORE20" = CORE20_METRIC,
          "NONCORE20" = NONCORE20_METRIC
        )))
    })

    # define the selected metric label
    drug_metric_lbl <- reactive({
      switch(input$metric,
        "item_per_pat" = "Prescription items per patient",
        "cost_per_pat" = "Drug cost per patient (£)",
        "pat_per_pop" = "Patients per thousand population",
        "item_per_pop" = "Prescription items per thousand population",
        "cost_per_pop" = "Drug cost per thousand population (£)",
        "pct_pats" = "Proportion of patients",
        "pct_items" = "Proportion of items",
        "pct_cost" = "Proportion of costs"
      )
    })

    # create the chart object
    output$chart_drug_comparison <- highcharter::renderHighchart({
      # create chart object
      highcharter::highchart() %>%
        # add the data series
        highcharter::hc_add_series(
          data = chart_data(),
          type = "dumbbell",
          highcharter::hcaes(
            low = CORE20_METRIC,
            high = NONCORE20_METRIC
          ),
          lowColor = "#003087",
          color = "#003087",
          marker = list(fillColor = "#ED8B00")
        ) %>%
        highcharter::hc_subtitle(
          useHTML = TRUE,
          text =
            "
            <span style = 'color:#003087; font-size: 20px'> &bull; </span> <b> <span style = font-size: 35px'> Core20 </span> </b>
            <span style = 'color:#ED8B00; font-size: 20px'> &bull; </span> <b> <span style = font-size: 35px'> Non-Core20 </span>
            ",
          align = "center"
        ) %>%
        highcharter::hc_chart(
          inverted = TRUE,
          marginLeft = 200
        ) %>%
        highcharter::hc_scrollbar(enabled = TRUE) %>%
        theme_nhsbsa() %>%
        highcharter::hc_xAxis(
          categories = unique(chart_data()$CHEM_SUB),
          max = 9 # it shows n + 1 = 10
        ) %>%
        highcharter::hc_yAxis(
          min = 0,
          max = max(
            max(chart_data()$CORE20_METRIC),
            max(chart_data()$NONCORE20_METRIC)
          ),
          title = list(text = paste0(drug_metric_lbl(), " (%)"))
        ) %>%
        highcharter::hc_legend(enabled = FALSE) %>%
        highcharter::hc_credits(enabled = TRUE) %>%
        # define the hover tooltip
        highcharter::hc_tooltip(
          headerFormat = "",
          pointFormat = paste0(
            "<b>{point.CHEM_SUB}</b> <br><br>",
            "<b>", drug_metric_lbl(), "</b><br>",
            "<b>Core20:</b> {point.CORE20_METRIC:,.1f}% <br>",
            "<b>Non Core20:</b> {point.NONCORE20_METRIC:,.1f}%"
          )
        ) %>%
        highcharter::hc_caption(
          text = paste0(
            "Excluding 0.7% of prescription items where the patient's location could not be identified.",
            "<br>Excluding any drugs prescribed to less than 100 patients in either Core20 or non Core20 areas."
          ),
          align = "left"
        )
    })

    # create the dynamic text for the chart
    output$chart_drug_comparison_text <- renderUI({
      tags$text(
        class = "highcharts-caption",
        switch(input$metric,
          "pct_pats" = "Most patients will be receiving either Salbutamol and/or Beclomethasone prescribing with only relatively small proportions of patients receiving any other COPD medicines",
          "pct_items" = "Salbutamol makes up a slightly higher proportion of COPD prescription items in the Core20 areas than in the non Core20 areas, where Beclomethasone has a higher proportion than seen in the Core20 areas.",
          "pct_cost" = "Despite Salbutamol being the most common COPD drug, there are seven other medicines that account for a higher proportion of the overall COPD drug cost."
        )
      )
    })

    # downloadable dataset - include all figures from charts
    mod_nhs_download_server(
      id = "copd_drug_metrics_download",
      filename = "copd_drug_metrics.csv",
      export_data = antibioticPrescribingScrollytellR::copd_chemsub_download
    )
  })
}
