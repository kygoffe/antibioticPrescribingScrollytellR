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

    # Chart: Select by Region as there are too many CCGs, click the CCG map
    # Show two charts; England, STP as reference line

    nhs_card(
      heading = "Breakdown by geography",
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
          choices = c(sort(unique(ccg$REGION[ccg$REGION != "England"]))), # Exclude England
          full_width = TRUE
        ),
        nhs_selectInput(
          inputId = ns("stp_ccg_sel"),
          label = "Sub geography",
          choices = c(
            "STP/ICS" = "STP_NAME", 
            "CCG" = "CCG_NAME"
            ), 
          full_width = TRUE
        )
      ),
      # map
      highcharter::highchartOutput(
        outputId = ns("map_metric"),
        height = "300px"
      ),
      nhs_grid_2_col(
        # either STP or CCG trend chart 
        highcharter::highchartOutput(
          outputId = ns("map_region_ccg"),
          height = "300px"
        ),
        # selected CCG trend
        highcharter::highchartOutput(
          outputId = ns("ccg_trend"),
          height = "300px"
        )
      ),
      shiny::htmlOutput(ns("ccg_comment"))
    ),
    p("Dynamic text about the selected CCG"),
    nhs_card(
      highcharter::highchartOutput(
        outputId = ns("gp_count"),
        height = "400px"
      ),
      highcharter::highchartOutput(
        outputId = ns("gp_imd"),
        height = "400px"
      )
    )
  )
}

#' ccg_antibiotic Server Functions
#'
#' @noRd
mod_region_antibiotic_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Filter data for selected geography and metrics
    
    geography_df <- reactive({
      req(input$region)
      req(input$stp_ccg_sel)
      req(input$metric)
      
      # Two choices of geographies
      if(input$stp_ccg_sel == "STP_NAME"){
        antibioticPrescribingScrollytellR::stp %>% 
          dplyr::filter(REGION == input$region) %>% 
          dplyr::filter(METRIC == input$metric)
      }else{
        antibioticPrescribingScrollytellR::ccg %>% 
          dplyr::filter(REGION == input$region) %>% 
          dplyr::filter(METRIC == input$metric)
      }
    })
    
    output$map_region_ccg <- highcharter::renderHighchart({
      highcharter::highchart() %>% 
        highcharter::hc_add_series_map(
          if(input$stp_ccg_sel == "STP_NAME"){
            map = antibioticPrescribingScrollytellR::stp_map 
          }
          
        )
    })
    
    
    # observe(print(geography_df()))
    

    # observeEvent(
    #   eventExpr = geography_df(),
    #   handlerExpr = {
    #     freezeReactiveValue(input, "stp")
    #     updateSelectInput(
    #       inputId = "stp",
    #       choices = geography_df()$STP_NAME %>% 
    #         na.omit() %>% 
    #         unique()
    #     )
    #   }
    # )
    
    
    # data for map
      
    
    
    
    
    
  })
}

## To be copied in the UI
# mod_region_antibiotic_ui("region_antibiotic_1")

## To be copied in the server
# mod_region_antibiotic_server("region_antibiotic_1")
