#' intro_antibiotic UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_intro_antibiotic_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p(
      "Antimicrobial resistance is a focal topic of the ",
      enurl(
        text = "NHS Long Term Plan.",
        url = "https://www.longtermplan.nhs.uk/"
      ),
      "It sets out the need to optimise use, reduce the need for ",
      "and unintentional exposure to antibiotics, as well as supporting ",
      "the development of new antimicrobials. ",
      "Critical to support these objectives is monitoring and illustrating ",
      "the trends in antibiotic prescribing. "
    ),
    br(),
    h3_tabstop("Medicines Optimisation Comparators"),
    p(
      "The NHS adopts a ",
      enurl(
        text = "medicines optimisation approach ",
        url = "https://www.england.nhs.uk/medicines-2/medicines-optimisation/"
      ),
      "to assess the value that medicines deliver, in both clinical-effectiveness ",
      "and cost-effectiveness. For antibiotics, the ",
      enurl(
        text = "two metrics ",
        url = "https://www.nhsbsa.nhs.uk/sites/default/files/2018-04/Master%20Specifications%20March%202018.pdf"
      ),
      "are: ",
      tags$ul(
        tags$li(
          tags$b("Antibacterial items per STAR-PU, "),
          "a population stanardised for age and gender measure of the number of prescription items for antibacterial drugs per ",
          "antibacterial item. The target for each Sub-ICB location is to be equal or below value ",
          "of 0.871 items per STAR-PU."
        ),
        tags$li(
          tags$b("Co-amoxiclav, Cephalosporins and Quinolones % items, "),
          "the number of prescription items for co-amoxiclav, cephalosporins and quinolones (antibiotic drugs) ",
          "as a percentage of the total number of prescription items for selected antibacterial drugs. ",
          "The target to reach of each Sub-ICB location is to be 10% or below."
        )
      )
    ),
    scrollytell::scrolly_container(
      outputId = ns("england_scrolly"),
      scrollytell::scrolly_graph(
        # This is for the sticky chart
        # It will show dual chart for items star-pu and 10% of board spectrum antibiotic
        tags$div(
          style = "margin-top: 20vh" # change based on size of sticky graph
        ),
        nhs_card(
          heading = p(textOutput(outputId = ns("england_chart_title"), inline = TRUE)),
          # dynamic chart based on current scroll section
          highcharter::highchartOutput(
            outputId = ns("england_chart"),
            height = "450px"
          ),
          # caveat text below chart
          tags$text(
            class = "highcharts-caption",
            style = "font-size: 9pt",
            "Calculation is based on CCG data and 12 months rolling period."
          ),
          mod_nhs_download_ui(id = ns("england_chart_download"))
        )
      ),
      # Define text for each scroll section
      scrollytell::scrolly_sections(
        scrollytell::scrolly_section(
          id = "ENG_44a",
          tags$div(style = "height: 30vh"), # bump text from top of section
          h4_tabstop("Antibiotic items star-pu in England"),
          p("England items STAR-PU")
          # tags$div(style = "height: 20vh"), # bump text from bottom of last section
        ),
        scrollytell::scrolly_section(
          id = "ENG_46b",
          tags$div(style = "height: 10vh"), # bump text from top of section,
          h4_tabstop("Antibiotic 10% over the broad antibiotics in England"),
          p("comments on 10% of broad antibiotics"),
          tags$div(style = "height: 20vh"), # bump text from bottom of last section
        )
      )
    )
    
  )
  
}

#' intro_antibiotic Server Functions
#'
#' @noRd
mod_intro_antibiotic_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # First chart title
    output$england_chart_title <- renderText({
      
      # require input ojbect
      req(input$england_scrolly)
      
      # Select two possible chart title based on scroll section input
      switch(input$england_scrolly,
             "ENG_44a" = "Anti-microbial resistance: total prescribing of antibiotics in primary care",
             "ENG_46b" = "Anti-microbial resistance: proportion of broad-spectrum antibiotic prescribing in primary care"
      )
    })
    
    
    # Create dual chart based on the selection of indicator
    output$england_chart <- highcharter::renderHighchart({
      # require input object as it will change depends on the
      req(input$england_scrolly)
      
      if (input$england_scrolly == "ENG_44a") {
        df <- antibioticPrescribingScrollytellR::eng_trend |>
          dplyr::select(YEAR_MONTH, LINE = STAR_PU, COL = TOTAL_ITEMS)
      } else {
        df <- antibioticPrescribingScrollytellR::eng_trend |>
          dplyr::select(YEAR_MONTH, LINE = COAMOX, COL = TOTAL_COAMOX)
      }
      
      yaxis_text1 <- switch(input$england_scrolly,
                            "ENG_44a" = "Items/STAR_PU",
                            "ENG_46b" = "% of items"
      )
      
      reference_value <- switch(input$england_scrolly,
                                "ENG_44a" = 0.87,
                                "ENG_46b" = 10
      )
      
      
      highcharter::highchart() |>
        highcharter::hc_xAxis(categories = unique(df$YEAR_MONTH)) |>
        highcharter::hc_yAxis_multiples(
          list(title = list(text = "Number of items"), lineWidth = 1, opposite = FALSE),
          list(
            title = list(text = yaxis_text1), lineWidth = 1,
            showLastLAbel = FALSE, opposite = TRUE, min = 0,
            plotLines = list(list(
              value = reference_value, color = "#8A1538", width = 1,
              dashStyle = "shortdash"
            ))
          )
        ) |>
        highcharter::hc_add_series(
          data = df$COL, type = "column", yAxis = 0, showInLegend = FALSE,
          name = "Number of items"
        ) |>
        highcharter::hc_add_series(
          data = round(df$LINE, digit = 2), type = "line", yAxis = 1,
          showInLegend = FALSE,
          name = yaxis_text1
        ) |>
        theme_nhsbsa() |>
        highcharter::hc_tooltip(
          shared = TRUE,
          useHTML = TRUE
        )
    })
    
    
    # Render scrolly object
    output$england_scrolly <- scrollytell::renderScrollytell({
      scrollytell::scrollytell()
    })
    
    
    
    # add here dual axis chart
    # nhs card chart title change dynamically
  })
}

## To be copied in the UI
# mod_intro_antibiotic_ui("intro_antibiotic_1")

## To be copied in the server
# mod_intro_antibiotic_server("intro_antibiotic_1")
