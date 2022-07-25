#' definitions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_definitions_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2_tabstop("Glossary"),
    p(
      tags$b(
        enurl(
          text = "Antibiotics",
          url = "https://www.nhs.uk/conditions/antibiotics/"
        )
      ),
      br(),
      "Antibiotics are used to treat or prevent some types of bacterial infection.",
      "they work by killing bacteria or preventing "
    ),
    p(
      tags$b(
        enurl(
          text = "Co-amoxiclav",
          url = "https://www.nhs.uk/medicines/co-amoxiclav/"
        )
      ),
      br(),
      "Co-amoxiclav is a combination antibiotic used for bacterial infections. ",
      "It contains amoxicillin (an antibiotic from the penicillin group of medicines) ",
      "mixed with clavulanic acid."
    ),
    p(
      tags$b(
        enurl(
          text = "Cephalosporins",
          url = "https://bnf.nice.org.uk/treatment-summaries/cephalosporins/"
        )
      ),
      br(),
      "The cephalosporins are broad-spectrum antibiotics which are used for ",
      "the treatment of septicaemia, pneumonia, meningitis, ",
      "biliary-tract infections, peritonitis, and urinary-tract infections. ",
      "The pharmacology of the cephalosporins is similar to that of the penicillins."
    ),
    p(
      tags$b(
        enurl(
          text = "Quinolones",
          url = "https://bnf.nice.org.uk/treatment-summaries/quinolones/"
        )
      ),
      br(),
      "In the UK, only fluoroquinolones are available, such as ",
      enurl(
        text = "ciprofloxacin.",
        url = "https://www.nhs.uk/medicines/ciprofloxacin/"
      ),
      "They are used to treat serious infections, or infections when other antibiotics ",
      "have not worked."
    ),
    p(
      tags$b(
        enurl(
          text = "STAR-PU",
          url = "https://www.nhs.uk/scorecard/12030"
        )
      ),
      br(),
      "Average daily quantity of Hypnotics prescribed per Specific Therapeutic group ",
      "Age-sex Related Prescribing Unit (STAR PU)"
    ),
    p(
      tags$b(
        enurl(
          text = "Integrated Care Boards",
          url = "https://www.england.nhs.uk/integratedcare/what-is-integrated-care/"
        )
      ),
      br(),
      "These are statutory NHS organisations responsible for developing a plan for meeting the ",
      "health needs, managing the NHS budget and arranging for the provision of health services ",
      "of a specific geographical population. These are replacing clinical commissioning groups as ",
      "a result of the Health and Care Act (2022)."
    ),
    p(
      tags$b(
        enurl(
          text = "NHS Long Term Plan",
          url = "https://www.longtermplan.nhs.uk/"
        )
      ),
      br(),
      "Published in 2019 by NHS England, the NHS Long Term Plan set out the healthcare priorities ",
      "for the following 10 years."
    )
  )
}

#' definitions Server Functions
#'
#' @noRd
mod_definitions_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_definitions_ui("definitions_ui_1")

## To be copied in the server
# mod_definitions_server("definitions_ui_1")
