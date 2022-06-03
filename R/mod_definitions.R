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
    h2_tabstop("Definitions"),
    p(
      "This section will highlight definitions and caveats of any datasets used and any metrics ",
      "reported throughout this article."
    ),
    p(
      "Care should be taken when interpreting the figures presented within this report as there ",
      "could be multiple underlying causes impacting figures that would require specific, ",
      "in-depth analysis to identify. To help interpret some general relationships many of the ",
      "sections of this report include multiple metrics to provide a range of views of the data."
    ),
    p(
      "Information has been split across various groups and presented using the tabs below:"
    ),
    navbarPage(
      "Categories:",

      # Datasets Menu
      navbarMenu(
        "Datasets",
        tabPanel(
          title = "NHS Prescription Data",
          htmlOutput(outputId = ns("ds_prescriptions"))
        ),
        tabPanel(
          title = "COPD Prescription Data",
          htmlOutput(outputId = ns("ds_copd"))
        ),
        tabPanel(
          title = "Hypertension Prescription Data",
          htmlOutput(outputId = ns("ds_hypertension"))
        ),
        tabPanel(
          title = "SMI Prescription Data",
          htmlOutput(outputId = ns("ds_smi"))
        ),
        tabPanel(
          title = "Low Income Scheme Certificates",
          htmlOutput(outputId = ns("ds_lis"))
        ),
        tabPanel(
          title = "MATEX, MEDEX and Tax Credit Certificates",
          htmlOutput(outputId = ns("ds_ppc"))
        ),
        tabPanel(
          title = "Base Population Figures",
          htmlOutput(outputId = ns("ds_population"))
        ),
        tabPanel(
          title = "Live Birth Figures",
          htmlOutput(outputId = ns("ds_births"))
        )
      ),

      # Key Terminology Menu
      navbarMenu(
        "Key Terminology",
        tabPanel(
          title = "Prescription Exemptions",
          htmlOutput(outputId = ns("term_px_exemptions"))
        ),
        tabPanel(
          title = "Prescribing Measures (Items, Cost...)",
          htmlOutput(outputId = ns("term_px_measures"))
        ),
        tabPanel(
          title = "Exemption Certificates",
          htmlOutput(outputId = ns("term_exemption_certificates"))
        )
      ),

      # Metrics Menu
      navbarMenu(
        "Metrics",
        tabPanel(
          title = "2. Introduction",
          htmlOutput(outputId = ns("metric_sec_intro"))
        ),
        tabPanel(
          title = "3. Prescription Exemptions",
          htmlOutput(outputId = ns("metric_sec_px"))
        ),
        tabPanel(
          title = "4. COPD Prescribing",
          htmlOutput(outputId = ns("metric_sec_copd"))
        ),
        tabPanel(
          title = "5. Hypertension Prescribing",
          htmlOutput(outputId = ns("metric_sec_hypertension"))
        ),
        tabPanel(
          title = "6. SMI Prescribing",
          htmlOutput(outputId = ns("metric_sec_smi"))
        ),
        tabPanel(
          title = "7. NHSBSA Exemption Certificates",
          htmlOutput(outputId = ns("metric_sec_certificates"))
        )
      )
    )
  )
}

#' definitions Server Functions
#'
#' @noRd
mod_definitions_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns



    # Text - Dataset - Prescription Data ----------------------------------------------------------

    output$ds_prescriptions <- renderUI({
      tagList(
        br(),
        h3_tabstop("NHS Prescription Data"),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Source:"
          ),
          tags$li(
            style = "margin-left: 20px",
            "Taken from copy of NHS Prescription data transferred from NHSBSA Data Warehouse ",
            "to NHSBSA Data Analytics Learning Laboratory (DALL)."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Analysis is based on primary care prescription data, collected for the ",
            "operational purpose of reimbursing and remunerating dispensing contractors for ",
            "the costs of supplying drugs and devices, along with essential and advanced ",
            "services, to NHS patients."
          )
        ),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Restrictions Applied:"
          ),
          tags$li(
            style = "margin-left: 20px",
            "Excludes prescriptions that were issued but not presented for dispensing."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Excludes prescriptions that were not submitted to the NHSBSA for processing ",
            "and reimbursement."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Excludes prescriptions issued and dispensed in prisons, hospitals and private ",
            "prescriptions."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Limited to prescription items both prescribed and dispensed in England."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Prescription data relates to prescription batches submitted to the NHSBSA for ",
            "payment between April 2016 and March 2021, although most analysis is limited ",
            "the 2020/21 financial year. The part month in NHSBSA data relates to the ",
            "dispensing month for which the prescription batch was submitted. This is ",
            "generally, but not always, the month in which the prescription was dispensed. ",
            "This means that there may be dispensing that has not been submitted to the ",
            "NHSBSA for payment and is therefore not included. There may also be ",
            "prescriptions included that were dispensed prior to the dispensing month."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Where analysis is based on patient information this is limited to prescribing ",
            "where the patient can be identified from the prescription data. This includes ",
            "any analysis reported to a geographical area or Core20 classification, as these ",
            "classifications are based on patient information. NHS numbers are captured for ",
            "100% electronic prescription messages. The estimated NHS number capture rate ",
            "for paper prescription forms is 83.7%."
          ),
          tags$li(
            style = "margin-left: 20px",
            "All analysis based on geographical location, including Core20 classification, ",
            "limited to prescribing for patients identified with English residential addresses."
          )
        ),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Data Classifications (General):"
          ),
          tags$li(
            style = "margin-left: 20px",
            "Due to manual processes involved in the processing of prescriptions there may ",
            "be inaccuracies in capturing prescription information which are then reflected ",
            "in the data. NHS Prescription Services have a variety of validation streams ",
            "throughout prescription processing to support accurate capture of the data. In ",
            "addition, a retrospective sample is completed in the month following ",
            "reimbursement to identify the accuracy of prescription processing information. ",
            "The check includes the accuracy of prescriber, practice and drug information, ",
            "but does not include the personal details of the patient. The latest reported ",
            "Prescription Processing Information Accuracy is 99.9%, which covers the ",
            "12-month rolling period ending November 2021. The sample may not be ",
            "representative at a more granular level; as such the level of accuracy is ",
            "undetermined for specific groups such as drugs, geographies, time periods etc. ",
            "It should also be noted that the identification of errors in the accuracy ",
            "checking sample does not result in amendments to data held in NHSBSA systems. ",
            "Further Prescription Processing Information Accuracy can be found ",
            enurl(
              url = "https://www.nhsbsa.nhs.uk/pharmacies-gp-practices-and-appliance-contractors/payments-and-pricing/how-we-process-prescriptions",
              text = "here."
            )
          ),
          tags$li(
            style = "margin-left: 20px",
            "When NHS prescriptions are dispensed, the patient will need to identify whether ",
            "they need to pay a prescription charge (£9.35 per prescription item as of April ",
            "2021) or whether they are eligible for free NHS prescriptions. Within the ",
            "dataset the exemption category represents what has been claimed by the patient ",
            "and captured during prescription processing activities."
          )
        ),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Data Classifications (Patient):"
          ),
          tags$li(
            style = "margin-left: 20px",
            "The NHSBSA periodically investigate the accuracy of NHS numbers captured ",
            "from paper forms. The personal details captured (NHS number, date of birth and ",
            "age) are compared against those on the prescription form for a random sample ",
            "of 50,000 prescription forms. The NHS number captured typically matches that ",
            "on the prescription form for over 99.9% of forms. The results represent the ",
            "accuracy for all items processed; as such the level of accuracy is undetermined ",
            "for specific medicines, geographies, time periods and other factors. By ",
            "contrast, the accuracy of captured NHS numbers in electronic prescribing is ",
            "estimated to be 100%."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Patient age was determined using a mixture of patient information from ",
            "prescription forms and ",
            enurl(
              url = "https://digital.nhs.uk/services/demographics",
              text = "Personal Demographic Service (PDS)"
            ),
            ", based on ",
            enurl(
              url = "https://www.nhsbsa.nhs.uk/sites/default/files/2018-02/180115%20Age%20Logic%20Summary%20Flow%20Chart%20-%20Revised%20Layout.pdf",
              text = "logic"
            ),
            " determined by NHSBSA Data Warehouse Team. Both patient age and patient age band ",
            "were recorded as the age at the time of prescribing."
          ),
          tags$li(
            style = "margin-left: 20px",
            "The NHSBSA does not capture information relating to a patient’s sex or gender ",
            "from a prescription during processing activities. Gender is instead obtained ",
            "from PDS, where a patient can be matched based on the captured NHS number. For ",
            "gender-specific analyses, prescription forms with an unknown patient gender ",
            "were not included and in a small number of cases a patient’s reported gender ",
            "may change over time and therefore it is possible for a patient to be recorded ",
            "against multiple genders."
          )
        ),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Data Classifications (Geography):"
          ),
          tags$li(
            style = "margin-left: 20px",
            "Geographic reporting is based on the patient's residential address, as captured ",
            "from either the prescription data or identified from data reported by PDS. The ",
            "residential address was prioritised over the location of prescribing and ",
            "dispensing accounts due to the fact patients could use multiple accounts and ",
            "reporting on deprivation was deemed to be more appropriate based on the ",
            "patient's location."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Activity could be assigned to Lower Super Output Area (LSOA) based on mapping ",
            "information published for the ",
            enurl(
              url = "https://geoportal.statistics.gov.uk/search?collection=Dataset&sort=name&tags=all(PRD_NSPL)",
              text = "National Statistics Postcode Lookup (NSPL)."
            ),
            "Each postcode was assigned to a LSOA based on the latest published mapping for ",
            "the postcode, which in most cases was the mapping as of February 2022."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Local Authority and Region classifications were based on mapping from LSOA as ",
            "defined with the ",
            enurl(
              url = "https://geoportal.statistics.gov.uk",
              text = "ONS Open Geography Portal,"
            ),
            "based on the Local Authority and Region structures as of April 2021."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Deprivation profiles are based on classifications based on the ",
            enurl(
              url = "https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019",
              text = "Index of Multiple Deprivation (IMD),"
            ),
            "which combines scoring across seven deprivation indicators. IMD scoring is ",
            "assigned at a LSOA level and can be used to either rank LSOAs based on ",
            "relative deprivation or assign a LSOA to an IMD decile, grouping LSOAs into 10% ",
            "bandings with an IMD decile of '1' representing the 10% of LSOAs that are the ",
            "most deprived in England."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Core20 classification is based on combining all LSOAs that represent the 20% ",
            "most deprived areas, which represents all LSOAs that are ",
            "classified as assigned to IMD deciles 1 or 2."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Deprivation profiles for Local Authorities have been based on assigning an IMD ",
            "rank to each local authority based on methodology as defined by the",
            enurl(
              url = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/833947/IoD2019_Research_Report.pdf",
              text = "Ministry of Housing, Communities & Local Government (MHCLG)."
            ),
            "It should be noted that as these ranks are calculated across the full local ",
            "authority it may not represent smaller areas within the local authority."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Urban and Rural classifications are available, at LSOA level only, based on ",
            "mappings as published by ",
            enurl(
              url = "https://www.ons.gov.uk/methodology/geography/geographicalproducts/ruralurbanclassifications/2011ruralurbanclassification",
              text = "ONS."
            )
          )
        )
      )
    })



    # Text - Dataset - COPD -----------------------------------------------------------------------
    output$ds_copd <- renderUI({
      tagList(
        br(),
        h3_tabstop("COPD Prescribing"),
        p(
          "For COPD prescribing, the same definitions were used as defined for NHS ",
          "Prescription Data, but with additional restrictions to limit results to specific drugs ",
          "from the BNF chapter for 'Respiratory System', as listed below."
        ),
        p(
          "The NHSBSA do not capture the clinical indication of a prescription and therefore do ",
          "not know the reason why a prescription was issued, or the condition it is intended to ",
          "treat. Many drugs have multiple uses, and although classified in the BNF by their ",
          "primary therapeutic use may be issued to treat a condition outside of this."
        ),
        nhs_card(
          heading = "List of drugs used to define COPD prescribing",
          # include the dynamic chart
          reactable::reactableOutput(outputId = ns("copd_drug_table")),
          tags$text(
            class = "highcharts-caption",
            style = "font-size: 9pt",
            "Full drug list down to drug presentation level available in data download"
          ),
          mod_nhs_download_ui(id = ns("download_copd_drug_list"))
        )
      )
    })



    # Text - Dataset - Hypertension ---------------------------------------------------------------
    output$ds_hypertension <- renderUI({
      tagList(
        br(),
        h3_tabstop("Hypertension Prescribing"),
        p(
          "For hypertension prescribing, the same definitions were used as defined for NHS ",
          "Prescription Data, but with additional restrictions to limit results to specific drugs ",
          "from the BNF chapter for 'Cardiovascular System', as listed below."
        ),
        p(
          "The NHSBSA do not capture the clinical indication of a prescription and therefore do ",
          "not know the reason why a prescription was issued, or the condition it is intended to ",
          "treat. Many drugs have multiple uses, and although classified in the BNF by their ",
          "primary therapeutic use may be issued to treat a condition outside of this."
        ),
        nhs_card(
          heading = "List of drugs used to define hypertension prescribing",
          # include the dynamic chart
          reactable::reactableOutput(outputId = ns("hypertension_drug_table")),
          tags$text(
            class = "highcharts-caption",
            style = "font-size: 9pt",
            "Full drug list down to drug presentation level available in data download"
          ),
          mod_nhs_download_ui(id = ns("download_hypertension_drug_list"))
        )
      )
    })



    # Text - Dataset - SMI ------------------------------------------------------------------------
    output$ds_smi <- renderUI({
      tagList(
        br(),
        h3_tabstop("SMI Prescribing"),
        p(
          "For SMI prescribing, the same definitions were used as defined for NHS ",
          "Prescription Data, but with additional restrictions to limit results to specific drugs ",
          "from the BNF chapter for 'Central Nervous System', as listed below."
        ),
        p(
          "It is difficult to identify a definitive list of medicines that would represent SMI ",
          "prescribing as some drugs are used in combination to treat severe anxiety or ",
          "depression, but they are also used for other conditions such as pain management or ",
          "dementia related psychosis. For this investigation, following collaboration with ",
          "pharmacy and mental health experts, prescribing relating to the treatment of severe ",
          "mental illness is based on any NHS prescriptions for medicines identified for the ",
          "treatment of psychoses or second and third line treatment of depression. By focusing ",
          "on drugs used for second and third line depression treatment, this should help exclude ",
          "treatment for mild-moderate depression."
        ),
        p(
          "The NHSBSA do not capture the clinical indication of a prescription and therefore do ",
          "not know the reason why a prescription was issued, or the condition it is intended to ",
          "treat. Many drugs have multiple uses, and although classified in the BNF by their ",
          "primary therapeutic use may be issued to treat a condition outside of this."
        ),
        nhs_card(
          heading = "List of drugs used to define SMI prescribing",
          # include the dynamic chart
          reactable::reactableOutput(outputId = ns("smi_drug_table")),
          tags$text(
            class = "highcharts-caption",
            style = "font-size: 9pt",
            "Full drug list down to drug presentation level available in data download"
          ),
          mod_nhs_download_ui(id = ns("download_smi_drug_list"))
        )
      )
    })



    # Text - Dataset - LIS ------------------------------------------------------------------------
    output$ds_lis <- renderUI({
      tagList(
        br(),
        h3_tabstop("Low Income Scheme (LIS) Certificate Data"),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Source:"
          ),
          tags$li(
            style = "margin-left: 20px",
            "Snapshot of production CRS database taken on 01 December 2021."
          ),
          tags$li(
            style = "margin-left: 20px",
            "This database contains application and award data relating to LIS certificates."
          )
        ),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Restrictions Applied:"
          ),
          tags$li(
            style = "margin-left: 20px",
            "Limited to applications received between 01 April 2016 and 31 March 2021"
          ),
          tags$li(
            style = "margin-left: 20px",
            "Limited to applications that resulted in a HC2 or HC3 certificate being issued ",
            "to the applicant."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Excluding any certificates where the applicant’s personal information cannot ",
            "be determined or where the applicant’s address cannot be assigned to an English ",
            "LSOA."
          )
        ),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Data Classifications:"
          ),
          tags$li(
            style = "margin-left: 20px",
            "Only the latest applicant information is retained in the source system and ",
            "therefore demographic data will represent the last known information supplied ",
            "for the application."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Applicants to the Low Income Scheme can apply either for themselves only or include ",
            "dependants within the same household. The database only includes details of a ",
            "partner as dependent (not children) and does not include any demographic ",
            "information about the partner. Therefore throughout this report only the lead ",
            "applicant has been considered for reporting and figures based on person counts will ",
            "exlude any dependants."
          ),
          tags$li(
            style = "margin-left: 20px",
            "No unique applicant identifier is captured within the database and therefore an ",
            "identifier has been established based on a combination of the applicant’s ",
            "personal information.  Therefore, where applicants have very similar personal ",
            "information they may be counted as a single individual, or where a single ",
            "applicant provides different personal information across multiple applications ",
            "they may be counted as multiple individuals."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Unique applicant identifier based on first three characters of forename, full ",
            "surname, full date of birth and postcode area code."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Gender is not captured as part of the application process and has therefore ",
            "been assigned, where possible, based on the applicant’s title (Mr = Male, ",
            "Mrs = Female …).  Where no title has been captured (common for digital ",
            "applications) or the title is not typically gender specific (Dr …), the gender ",
            "has been classified as unknown."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Applicant age has been calculated as the age at middle (30th September) for each ",
            "financial year. This is to support reporting for active certificates that could be ",
            "active for several years."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Applicant assigned to an LSOA based on the latest available mapping based on ",
            "postcode lookups published by ONS."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Where an applicant (defined by created unique identifier) has more than one ",
            "activity record within a single reporting period (financial year), the ",
            "demographics will be assigned based on the latest identified demographics for ",
            "the applicant to prevent the applicant being reported across multiple locations ",
            "and/or age groups."
          )
        )
      )
    })



    # Text - Dataset - MATEX_MEDEX_TC -------------------------------------------------------------
    output$ds_ppc <- renderUI({
      tagList(
        br(),
        h3_tabstop("Maternity Exemption (MATEX), Medical Exemption (MEDEX) and Tax Credit Certificate Data"),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Source:"
          ),
          tags$li(
            style = "margin-left: 20px",
            "Snapshot of production PPC database taken on 14 December 2021."
          ),
          tags$li(
            style = "margin-left: 20px",
            "This database contains application and award data relating to exemption ",
            "certificates, including MATEX, MEDEX and Tax Credits."
          )
        ),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Restrictions Applied:"
          ),
          tags$li(
            style = "margin-left: 20px",
            "Limited to applications received between 01 April 2016 and 31 March 2021."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Excluding any certificates where the applicant’s personal information cannot ",
            "be determined or where the applicant’s address cannot be assigned to an ",
            "English LSOA."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Excluding any test records identified as created for staff training purposes."
          )
        ),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Data Classifications:"
          ),
          tags$li(
            style = "margin-left: 20px",
            "Demographic data will reflect the latest available applicant information for ",
            "each application."
          ),
          tags$li(
            style = "margin-left: 20px",
            "No unique applicant identifier is captured within the database and therefore ",
            "an identifier has been established based on a combination of the applicant’s ",
            "personal information. Where applicants have very similar personal ",
            "information they may be counted as a single individual, or where a single ",
            "applicant provides different personal information across multiple applications ",
            "they may be counted as multiple individuals."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Unique applicant identifier based on first three characters of forename, full ",
            "surname, full date of birth and postcode area code."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Gender is not captured as part of the application process and has therefore ",
            "been assigned, where possible, based on the applicant’s title (Mr = Male, ",
            "Mrs = Female …).  Where no title has been captured (common for digital ",
            "applications) or the title is not typically gender specific (Dr …), the gender ",
            "has been classified as unknown. For MEDEX certificates all applicants are ",
            "assumed to be female regardless of title."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Applicant age has been calculated as the age at middle (30th September) for each ",
            "financial year. This is to support reporting for active certificates that could be ",
            "active for several years."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Applicant assigned to an LSOA based on the latest available mapping based on ",
            "postcode lookups published by ONS."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Where an applicant (defined by created unique identifier) has more than one ",
            "activity record within a single reporting period (financial year), the ",
            "demographics will be assigned based on the latest identified demographics for ",
            "the applicant to prevent the applicant being reported across multiple locations ",
            "and/or age groups."
          )
        )
      )
    })



    # Text - Dataset - Base Population ------------------------------------------------------------
    output$ds_population <- renderUI({
      tagList(
        br(),
        h3_tabstop("Base Population Figures"),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Source:"
          ),
          tags$li(
            style = "margin-left: 20px",
            "Based on mid year population estimates published by ONS and download via the ",
            enurl(
              url = "https://www.nomisweb.co.uk/api/v01/help",
              text = "NOMIS API."
            )
          )
        ),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Data Classifications:"
          ),
          tags$li(
            style = "margin-left: 20px",
            "Figures taken from report for 'Population estimates - small area based by ",
            "single year of age - England and Wales' (NM_2010_1), with figures extracted ",
            "at LSOA level, split by age and gender."
          ),
          tags$li(
            style = "margin-left: 20px",
            "ONS population figures are based on mid-year estimates and for reporting against ",
            "2020/21 figures the mid-year estimate for 2020 has been used."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Population estimates has been used as the denominator in calculations ",
            "throughout the report to standardise figures, and where possible the age ",
            "groups have been restricted to most closely align with a figures being reported."
          ),
          tags$li(
            style = "margin-left: 40px; list-style-type: circle;",
            "Paid for prescribing: population aged 19-59."
          ),
          tags$li(
            style = "margin-left: 40px; list-style-type: circle;",
            "Age exempt prescribing: population aged 0-18 and 60+."
          ),
          tags$li(
            style = "margin-left: 40px; list-style-type: circle;",
            "Other exemption prescribing: population aged 19-59."
          ),
          tags$li(
            style = "margin-left: 40px; list-style-type: circle;",
            "Age Exempt Prescribing: population aged 0-18 and 60+."
          ),
          tags$li(
            style = "margin-left: 40px; list-style-type: circle;",
            "MATEX certificates: based on live birth figures (see below)."
          ), tags$li(
            style = "margin-left: 40px; list-style-type: circle;",
            "MEDEX certificates: population aged 16-60."
          ),
          tags$li(
            style = "margin-left: 40px; list-style-type: circle;",
            "LIS certificates: population aged 16+."
          ),
          tags$li(
            style = "margin-left: 40px; list-style-type: circle;",
            "Tax Credit certificates: population aged 16+."
          ),
        )
      )
    })



    # Text - Dataset - Live Births ----------------------------------------------------------------
    output$ds_births <- renderUI({
      tagList(
        br(),
        h3_tabstop("Live Birth Figures"),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Source:"
          ),
          tags$li(
            style = "margin-left: 20px",
            enurl(
              url = "https://www.nomisweb.co.uk/datasets/lebirthsla",
              text = "NOMIS:"
            ),
            "Live births in England and Wales down to local authority local area (2013 to 2020)"
          )
        ),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Data Classifications:"
          ),
          tags$li(
            style = "margin-left: 20px",
            "To support calculation of uptake of MATEX certificates, live birth statistics ",
            "have been used as the population denominator to try and represent the ",
            "population who would be eligible for a MATEX."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Live birth figures will not include figures for abortions or stillbirths, where ",
            "a MATEX may have been awarded prior to this event.  However, these figures ",
            "could not be identified down to the required level of detail (local authority ",
            "and age group."
          ),
          tags$li(
            style = "margin-left: 20px",
            "To align with active certificates for 2020/21 the birth statistics across a ",
            "number of years needed to be considered.  As a MATEX certificate is valid from ",
            "conception to one year following birth, MATEX certificates active in 2020/21 ",
            "could include anybody who had an valid MATEX certificate for at least one day ",
            "in 2020/21 and will therefore include:"
          ),
          tags$li(
            style = "margin-left: 40px; list-style-type: circle;",
            "any live births in 2019/20"
          ),
          tags$li(
            style = "margin-left: 40px; list-style-type: circle;",
            "any live births in 2020/21"
          ),
          tags$li(
            style = "margin-left: 40px; list-style-type: circle;",
            "any live births in 2021/22 until around September 2021 (based on conceptions ",
            "in 2020/21)"
          ),
          tags$li(
            style = "margin-left: 20px",
            "As seen above, active certificates for a year will cover births over a period ",
            "of ~30 months. Live birth figures are published based on calendar years, with ",
            "2020 being the latest available at the time of analysis. Therefore, to attempt ",
            "to align figures, live births figures have been combined for 2019, 2020 and an ",
            "additional 50% of 2020."
          )
        )
      )
    })



    # Text - Terminology - Prescription Exemptions ------------------------------------------------
    output$term_px_exemptions <- renderUI({
      tagList(
        br(),
        h3_tabstop("Prescription Exemptions"),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Age Exempt:"
          ),
          tags$li(
            style = "list-style:none; margin-left: 20px",
            "NHS prescriptions are free to all patients aged under 16, 60 or over and to ",
            "students aged 16-18 in full time education.  Although these may be claimed as ",
            "separate exemption categories, they have been combined for reporting purposes ",
            "within this article."
          )
        ),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Paid For:"
          ),
          tags$li(
            style = "list-style:none; margin-left: 20px",
            "Patients without any valid payment exemption will pay a prescription charge for ",
            "each prescription item they receive, although a Prescription Prepayment ",
            "Certificate (PPC) can be purchased that allows the holder to receive as many ",
            "prescription items as required for a set price (£108.10 for 12 months). As both ",
            "scenarios require the patient to make some form of payment, these have been ",
            "combined for reporting purposes within this article."
          )
        ),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Other Charge Exempt:"
          ),
          tags$li(
            style = "list-style:none; margin-left: 20px",
            "All other prescription charge exemptions, once PPC and age-related exemptions ",
            "have been removed, have been combined as a single reporting group."
          )
        ),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Unknown Exemption:"
          ),
          tags$li(
            style = "list-style:none; margin-left: 20px",
            "In a small proportion of cases (3% of prescription items in 2020/21), a ",
            "prescription may be identified as charge exempt but without a clear ",
            "identification of the exemption category claimed by the patient. As these could ",
            "include PPC exemptions they have been excluded from reporting as in this article ",
            "PPC have been included as part of the 'paid for' group."
          )
        )
      )
    })



    # Text - Terminology - Prescribing Measures ---------------------------------------------------
    output$term_px_measures <- renderUI({
      tagList(
        br(),
        h3_tabstop("Prescribing Measures: Patients, Items and Cost"),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Number of patients:"
          ),
          tags$li(
            style = "list-style:none; margin-left: 20px",
            "Relates to the number of unique patients that could be identified within the ",
            "data based on distinct NHS numbers.  NHS numbers cannot be captured from all ",
            "prescriptions, although in 2020/21 over 97% of prescription items could be ",
            "attributed to an identifiable NHS number. Where items or drug cost have been ",
            "reported as a rate per patient, only prescribing linked to identifiable NHS ",
            "numbers has been included.  Additional definitions about patient demographics ",
            "can be found within the Prescription Data definitions."
          )
        ),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Number of prescription items:"
          ),
          tags$li(
            style = "list-style:none; margin-left: 20px",
            "Relates to the number of times a product, such as a drug or appliance, appears ",
            "on a prescription form. It does not account for dosage, quantity prescribed or ",
            "length of treatment. For example, a patient could receive 100 x 50mg tablets ",
            "as an item and another could receive 7 x 5 mg tablets as an item. Both would ",
            "be counted as 1 item."
          ),
          tags$li(
            style = "list-style:none; margin-left: 20px",
            "The frequency of prescribing will strongly influence the reported number of ",
            "prescription items. For example, a patient may receive a single prescription item ",
            "with a sufficient quantity to cover three months of prescribing, whilst another ",
            "patient could receive the same prescribing but spread out across 12 weekly ",
            "prescriptions, which would be reported as 12 separate prescription items."
          )
        ),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Drug cost:"
          ),
          tags$li(
            style = "list-style:none; margin-left: 20px",
            "Relates solely to the basic price of the drugs, in the quantity prescribed on ",
            "a prescription form. It does not include any additional fees or discounts that ",
            "were paid to the dispensing contractors. Also known as Net Ingredient Cost ",
            "(NIC)."
          )
        ),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Items v Drug Cost:"
          ),
          tags$li(
            style = "list-style:none; margin-left: 20px",
            "In some scenarios considering both the number of prescription items and the ",
            "drug cost may provide a more rounded view of activity.  For example, if two ",
            "groups have similar levels of prescription items but one has a notably higher ",
            "drug cost, this may suggest that the higher cost is due to either more ",
            "expensive products being prescribed, or a higher quantity of drug being ",
            "prescribed per prescription item."
          )
        )
      )
    })



    # Text - Terminology - Exemption Certificates -------------------------------------------------
    output$term_exemption_certificates <- renderUI({
      tagList(
        br(),
        h3_tabstop("NHSBSA Issued Exemption Certificates"),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Number of people with active certificates:"
          ),
          tags$li(
            style = "list-style:none; margin-left: 20px",
            "Represents the number of people who held a valid exemption certificate that was ",
            "valid for at any point during the reported period. This will include ",
            "certificates that were issued during the reported period and any ",
            "certificates issued in previous years that were still valid. This measure has ",
            "been used as the main reporting measure for NHSBSA issued exemption ",
            "certificates as it best represents the population who are receiving some form ",
            "of support."
          )
        ),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Number of people issued certificates:"
          ),
          tags$li(
            style = "list-style:none; margin-left: 20px",
            "Represents the number of people who applied and were issued ",
            "an exemption certificate during the reported period."
          )
        ),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "HC2 (Low Income Scheme):"
          ),
          tags$li(
            style = "list-style:none; margin-left: 20px",
            "A HC2 certificate will provide the holder with full help with health costs ",
            "covering:"
          ),
          tags$li(
            style = "list-style:none; margin-left: 20px",
            "A HC2 certificate will provide the holder with full help with health costs, ",
            "including free NHS prescriptions, free NHS dental treatment and ",
            enurl(
              url = "https://www.nhsbsa.nhs.uk/nhs-low-income-scheme/hc2-certificates-full-help-health-costs",
              text = "other support."
            )
          )
        ),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "HC3 (Low Income Scheme):"
          ),
          tags$li(
            style = "list-style:none; margin-left: 20px",
            "Where applicants do not qualify for the full level of support offered by a ",
            "HC2 certificate, they may be awarded a HC3 certificate which offers partial ",
            "support. Although a HC3 will not provide any support for NHS prescription costs ",
            "it could help towards NHS dental costs or NHS sight tests. The ",
            "level of support provided is based on the applicants' circumstances and the ",
            "certificate will identify how much money the applicant will get towards ",
            "treatment costs."
          )
        ),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Live Births:"
          ),
          tags$li(
            style = "list-style:none; margin-left: 20px",
            "Live birth statistics have been chosen as the most appropriate population ",
            "denominator to reflect people who may be applying for MATEX certificates. ",
            "Although live birth figures do not include numbers for abortions, stillbirths, ",
            "or miscarriages, they are the most appropriate figures that could be sourced ",
            "that would allow reporting down to Local Authority areas and age band. See ",
            "'Live Birth Figures' section of the Datasets definitions for additional details."
          )
        )
      )
    })


    # Text - Metric - 2_Introduction --------------------------------------------------------------
    output$metric_sec_intro <- renderUI({
      tagList(
        br(),
        h3_tabstop("2. Introduction"),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Life Expectancy (LE) and Healthy Life Expectancy (HLE) 2018-2020 across ",
            "deprivation deciles, split by gender:"
          ),
          tags$li(
            style = "margin-left: 20px",
            "This metric is based on figures ",
            enurl(
              url = "https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/healthinequalities/bulletins/healthstatelifeexpectanciesbyindexofmultipledeprivationimd/2018to2020",
              text = "published by ONS. "
            )
          ),
          tags$li(
            style = "margin-left: 20px",
            "Healthy life expectancy is based on estimates of lifetime spent in 'very good' or ",
            "'good' health, based on how individuals perceive their general health."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Figures represent life expectancy and healthy life expectancy at birth, where people ",
            "experience the same IMD decile throughout their life."
          )
        )
      )
    })


    # Text - Metric - 3_Prescription_Exemptions ---------------------------------------------------
    output$metric_sec_px <- renderUI({
      tagList(
        br(),
        h3_tabstop("3. Prescription Exemptions"),
        # Trend for exemption groups
        br(),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Proportion of prescription items split by charge status, by financial year:"
          ),
          tags$li(
            style = "margin-left: 20px",
            "Prescriptions items, prescribed and dispensed in England, by ",
            "financial year. The exemption category reflects that claimed by the patient ",
            "based on declarations on the back of the prescription form, with categories ",
            "grouped as defined in the 'Key Terminology' section."
          )
        ),
        # Core v non Core comparison
        br(), br(),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "NHS prescribing data metrics for Core20 and non-Core20 patients (2020/21):"
          ),
          tags$li(
            style = "margin-left: 20px",
            "Based on prescriptions items, prescribed and dispensed in England, by ",
            "financial year. The exemption category reflects that claimed by the patient ",
            "based on declarations on the back of the prescription form, with categories ",
            "grouped as defined in the 'Key Terminology' section. Core20 classification is ",
            "based on the patient's identified residential address."
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Prescription items per patient"),
            "= (total number of prescription items) / (total number of distinct identified ",
            "patients)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Drug cost per patient"),
            "= (total net ingredient drug cost) / (total ",
            "number of distinct identified patients)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Percentage of prescribing paid for"),
            "= (prescription items with some payment from patient) / (total prescription ",
            "items)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Percentage of prescribing age exempt"),
            "= (prescription items claimed as age exempt) / (total prescription ",
            "items)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Percentage of prescribing other charge exempt"),
            "= (prescription items claimed as non age related exemption) / (total ",
            "prescription items)"
          )
        ),
        # Core v non Core (age) - Population distribution
        br(),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Core20 vs Non Core20: Population distribution by age band (2020/21):"
          ),
          tags$li(
            style = "margin-left: 20px",
            "Identifies how general population is distributed by age band within Core20 and ",
            "non Core20 areas. Based on mid year population estimates for 2020 published by ",
            "ONS, with figures extracted at LSOA level to allow Core20 classification prior ",
            "to aggregation. For each age group, shows the number of people as a proportion ",
            "of the overall population within the Core20 group."
          )
        ),
        # Core v non Core (age) - Item distribution
        br(),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Core20 vs Non Core20: Prescription items distribution by age band (2020/21):"
          ),
          tags$li(
            style = "margin-left: 20px",
            "Identifies how prescription items are distributed by age band within Core20 and ",
            "non Core20 areas. Based on prescription items, prescribed and dispensed in ",
            "England, excluding 3% of prescription items where either the patients age or ",
            "residential cannot be identified. For each age group, shows the number of ",
            "prescription items as a proportion of all items prescribed to patients within ",
            "the Core20 group."
          )
        ),
        # Core v non Core (age) - Items per person
        br(),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Core20 vs Non Core20: Prescription items per person by age band (2020/21)"
          ),
          tags$li(
            style = "margin-left: 20px",
            "Identifies the volume of prescription items per person by age band within ",
            "Core20 and non Core20 areas. Based on prescription items, prescribed and ",
            "dispensed in England, excluding 3% of prescription items where either the ",
            "patients age or residential cannot be identified. Figures calculated as a rate ",
            "per population head as identified from ONS mid year (2020) estimates. Please ",
            "note, as the denominator is based on overall population figures this will ",
            "include people who may not have received any prescribing."
          )
        ),
        # Core v non Core (age) - Unique medicines
        br(),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Core20 vs Non Core20: Proportion of patients on 10 or more unique medicines by ",
            "age band (2020/21)"
          ),
          tags$li(
            style = "margin-left: 20px",
            "Identifies the proportion of patients receiving prescribing in 2020/21 who ",
            "were prescribined ten or more unique medicines. Based on prescription items, ",
            "prescribed and dispensed in England, excluding 3% of prescription items where ",
            "either the patients age or residential cannot be identified. Unique medicines ",
            "are defined as one or more medicine with the same chemical substance (limited ",
            "to BNF chapters 1-4 and 6-10), regardless of presentation or strength. For ",
            "example, a patient receiving 'Warfarin 5mg tablets' and 'Warfarin 5mg/5ml oral ",
            "solution' would be classed as receiving a single unique medicine."
          )
        ),
        # Distance Analysis
        br(), br(),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Average distance (in miles) between patient's address and prescribing location ",
            "(2020/21):"
          ),
          tags$li(
            style = "margin-left: 20px",
            "Based on prescription items, prescribed and dispensed in England, where the ",
            "postcode coordinates could be identified for the patient's address and the ",
            "prescribing organisation address.  Core20 and Urban/Rural classifications are ",
            "based on the LSOA as identified for the patient's address. Distance is ",
            "calculated in miles based on a straight line distance between the two points. ",
            "Each combination of patient and prescriber is considered as a single record, ",
            "regardless of the volume of activity. No consideration is taken as to the type ",
            "of prescribing to differentiate between in person and remote prescribing."
          )
        ),
        # Local Authority
        br(), br(),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "NHS prescribing activity by Index of Multiple Deprivation ranking for English ",
            "Local Authorities (2020/21):"
          ),
          tags$li(
            style = "margin-left: 20px",
            "Based on prescriptions items, prescribed and dispensed in England, by ",
            "financial year. The exemption category reflects that claimed by the patient ",
            "based on declarations on the back of the prescription form, with categories ",
            "grouped as defined in the 'Key Terminology' section. Local Authority ",
            "classification is based on the patient's identified residential address, with ",
            "reporting limited to patient's identified against an English Local AUthority."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Each point on the scatter chart will represent a local authority and the value ",
            "for the chosen metric. The solid line represents the results of a simple regression ",
            "model based on the metric figures and the IMD ranking across all local ",
            "authorities. This line will suggest the value that may be expected for the ",
            "local authority based on any relationship between the results and IMD rank."
          ),
          tags$li(
            style = "margin-left: 20px",
            "The map view will show the values for the chosen metric and chosen region. ",
            "Selecting a point on the map will update the deprivation decile chart to ",
            "reflect the selected local authority."
          ),
          tags$li(
            style = "margin-left: 20px",
            "The deprivation decile chart will show, for the chosed local authority, the ",
            "overall population distribution (based on ONS mid 2020 population estimates) ",
            "by IMD decile. The most appropriate population figures will be used to match ",
            "the chosen metric. For example, when reporting age exempt prescribing ",
            "proportions, the deprivation decile chart will be based on the population aged ",
            "0 to 18 and 60 plus, reflecting the population who could claim an age exemption."
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Prescription items per patient"),
            "= (total number of prescription items) / (total number of distinct identified ",
            "patients)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Drug cost per patient"),
            "= (total net ingredient drug cost) / (total ",
            "number of distinct identified patients)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Percentage of prescribing paid for"),
            "= (prescription items with some payment from patient) / (total prescription ",
            "items)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Percentage of prescribing age exempt"),
            "= (prescription items claimed as age exempt) / (total prescription ",
            "items)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Percentage of prescribing other charge exempt"),
            "= (prescription items claimed as non age related exemption) / (total ",
            "prescription items)"
          )
        ),
        # LSOA
        br(), br(),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "NHS prescribing activity (2020/21) split by LSOA area, highlighting Core20 and ",
            "non Core20 areas:"
          ),
          tags$li(
            style = "margin-left: 20px",
            "Based on prescriptions items, prescribed and dispensed in England, by ",
            "financial year. The exemption category reflects that claimed by the patient ",
            "based on declarations on the back of the prescription form, with categories ",
            "grouped as defined in the 'Key Terminology' section. LSOA classification is ",
            "based on the patient's identified residential address, with reporting limited ",
            "to patient's identified against an English LSOAs."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Each point on the scatter chart will represent a LSOA, with LSOAs plotted for ",
            "the selected local authority. The lines represent the results of a simple ",
            "regression model based on the metric figures and the IMD ranking across all LSOAs ",
            "nationally. The solid line will suggest the value that may be expected for the ",
            "LSOA based on any relationship between the results and IMD rank. Across all ",
            "LSOAs in England, it is estimated that approximately 95% of values fall within ",
            "the dashed lines, and LSOAs well outside of these lines could be considered ",
            "as potential 'outliers'."
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Prescription items per patient"),
            "= (total number of prescription items) / (total number of distinct identified ",
            "patients)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Drug cost per patient"),
            "= (total net ingredient drug cost) / (total ",
            "number of distinct identified patients)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Percentage of prescribing paid for"),
            "= (prescription items with some payment from patient) / (total prescription ",
            "items)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Percentage of prescribing age exempt"),
            "= (prescription items claimed as age exempt) / (total prescription ",
            "items)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Percentage of prescribing other charge exempt"),
            "= (prescription items claimed as non age related exemption) / (total ",
            "prescription items)"
          )
        )
      )
    })



    # Text - Metric - 4_COPD_Prescribing ----------------------------------------------------------
    output$metric_sec_copd <- renderUI({
      tagList(
        br(),
        h3_tabstop("4. COPD Prescribing"),
        # Financial Year Trend
        br(),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Financial Year Trend:"
          ),
          tags$li(
            style = "margin-left: 20px",
            "Based on prescriptions items identified as representing prescribing for COPD (see ",
            "'Datasets' definitions), prescribed and dispensed in England, by financial year."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Figures for Core20 v Non Core20 comparisons have been standardised by population ",
            "size to allow comparison as Core20 areas will only represent 20% of the population. ",
            "Base populations for calculated rates are based on population estimates for England, ",
            "published by ONS, with the closest calendar year being applied (e.g. 2019 mid year ",
            "population estimate used for 2019/20 data)."
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("National: Number of COPD prescription items"),
            "= COPD prescription items"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Core20 v Non Core20: COPD prescription items per thousand population"),
            "= ((COPD prescription items) / (ONS population estimate) * 1000)"
          )
        ),
        # Exemption Category Trend
        br(), br(),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Proportion of COPD prescription items, by financial year, split by prescription ",
            "charge status (2015/16 to 2020/21):"
          ),
          tags$li(
            style = "margin-left: 20px",
            "Based on prescriptions items identified as representing prescribing for COPD (see ",
            "'Datasets' definitions), prescribed and dispensed in England, during 2020/21. ",
            "The exemption category reflects that claimed by the patient based on declarations ",
            "on the back of the prescription form, with categories grouped as defined in the ",
            "'Key Terminology' section."
          )
        ),
        # Core20 v Non Core20
        br(), br(),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "COPD prescribing data metrics for Core20 and non-Core20 patients (2020/21):"
          ),
          tags$li(
            style = "margin-left: 20px",
            "Based on prescriptions items identified as representing prescribing for COPD (see ",
            "'Datasets' definitions), prescribed and dispensed in England, during 2020/21. ",
            "The exemption category reflects that claimed by the patient based on declarations ",
            "on the back of the prescription form, with categories grouped as defined in the ",
            "'Key Terminology' section. Core20 classification is based on the patient's ",
            "identified residential address."
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("COPD Prescription items per patient"),
            "= (total number of COPD prescription items) / (total number of distinct identified ",
            "patients receiving COPD prescribing)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Drug cost per patient"),
            "= (total net ingredient drug cost for COPD prescription items) / (total ",
            "number of distinct identified patients receiving COPD prescribing)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Percentage of prescribing paid for"),
            "= (COPD prescription items with some payment from patient) / (total COPD ",
            "prescription items)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Percentage of prescribing age exempt"),
            "= (COPD prescription items claimed as age exempt) / (total COPD prescription ",
            "items)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Percentage of prescribing other charge exempt"),
            "= (COPD prescription items claimed as non age related exemption) / (total COPD ",
            "prescription items)"
          )
        ),
        # IMD Decile
        br(), br(),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "COPD prescribing by IMD Decile (2020/21):"
          ),
          tags$li(
            style = "margin-left: 20px",
            "Based on prescriptions items identified as representing prescribing for COPD (see ",
            "'Datasets' definitions), prescribed and dispensed in England, for 2020/21."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Rates per thousand population are based on English mid year population estimates ",
            "2020, as published by ONS."
          ),
          tags$li(
            style = "margin-left: 20px",
            "IMD decile classification based on the LSOA associated to the patient's ",
            "residential address, with results limited to records where the patient's address ",
            "can be identified."
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Patients per thousand population"),
            "= ((distinct patients receiving COPD prescribing) / (ONS population estimate) * 1000)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Items per thousand population"),
            "= ((COPD prescription items) / (ONS population estimate) * 1000)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Patients per thousand population"),
            "= ((drug cost for COPD prescription items) / (ONS population estimate) * 1000)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Items per patient"),
            "= (COPD prescription items) / (distinct patients receiving COPD prescribing)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Cost per patient"),
            "= (drug cost for COPD prescription items) / (distinct patients receiving COPD prescribing)"
          )
        ),
        # Age and Gender
        br(), br(),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "COPD prescribing by age and gender (2020/21):"
          ),
          tags$li(
            style = "margin-left: 20px",
            "Based on prescriptions items identified as representing prescribing for COPD (see ",
            "'Datasets' definitions), prescribed and dispensed in England, for 2020/21."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Rates per thousand population are based on English mid year population estimates ",
            "2020, as published by ONS."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Results limited to records where the patient's age, gender and residential location ",
            "can be identified."
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Patients per thousand population"),
            "= ((distinct patients receiving COPD prescribing) / (ONS population estimate) * 1000)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Items per thousand population"),
            "= ((COPD prescription items) / (ONS population estimate) * 1000)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Patients per thousand population"),
            "= ((drug cost for COPD prescription items) / (ONS population estimate) * 1000)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Items per patient"),
            "= (COPD prescription items) / (distinct patients receiving COPD prescribing)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Cost per patient"),
            "= (drug cost for COPD prescription items) / (distinct patients receiving COPD prescribing)"
          )
        ),
        # Local Authority
        br(), br(),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "COPD prescribing by Index of Multiple Deprivation ranking for English Local ",
            "Authorities (2020/21):"
          ),
          tags$li(
            style = "margin-left: 20px",
            "Based on prescriptions items identified as representing prescribing for COPD (see ",
            "'Datasets' definitions), prescribed and dispensed in England, for 2020/21."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Rates per thousand population are based on English mid year population estimates ",
            "2020, as published by ONS."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Local authority classification based on the patient's residential address, with ",
            "results limited to records where the patient's address can be identified. Excluding ",
            "any local authorities with less than five patients receiving COPD prescriptions ",
            "to prevent any potential patient disclosure."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Each point on the scatter chart will represent a local authority and the value ",
            "for the chosen metric. The solid line represents the results of a simple regression ",
            "model based on the metric figures and the IMD ranking across all local ",
            "authorities. This line will suggest the value that may be expected for the ",
            "local authority based on any relationship between the results and IMD rank."
          ),
          tags$li(
            style = "margin-left: 20px",
            "The map view will show the values for the chosen metric and chosen region. ",
            "Selecting a point on the map will update the deprivation decile chart to ",
            "reflect the selected local authority."
          ),
          tags$li(
            style = "margin-left: 20px",
            "The deprivation decile chart will show, for the chosen local authority, the ",
            "overall population distribution (based on ONS mid 2020 population estimates) ",
            "by IMD decile."
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Patients per thousand population"),
            "= ((distinct patients receiving COPD prescribing) / (ONS population estimate) * 1000)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Items per thousand population"),
            "= ((COPD prescription items) / (ONS population estimate) * 1000)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Patients per thousand population"),
            "= ((drug cost for COPD prescription items) / (ONS population estimate) * 1000)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Items per patient"),
            "= (COPD prescription items) / (distinct patients receiving COPD prescribing)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Cost per patient"),
            "= (drug cost for COPD prescription items) / (distinct patients receiving COPD prescribing)"
          )
        ),
        # LSOA
        br(), br(),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "COPD prescribing by Index of Multiple Deprivation ranking for English LSOA ",
            "(2020/21):"
          ),
          tags$li(
            style = "margin-left: 20px",
            "Based on prescriptions items identified as representing prescribing for COPD (see ",
            "'Datasets' definitions), prescribed and dispensed in England, for 2020/21."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Rates per thousand population are based on English mid year population estimates ",
            "2020, as published by ONS."
          ),
          tags$li(
            style = "margin-left: 20px",
            "LSOA classification based on the patient's residential address, with results ",
            "limited to records where the patient's address can be identified. Excluding ",
            "any LSOAs with less than five patients receiving COPD prescriptions to prevent any ",
            "potential patient disclosure."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Each point on the scatter chart will represent a LSOA, with LSOAs plotted for ",
            "the selected local authority. The lines represent the results of a simple ",
            "regression model based on the metric figures and the IMD ranking across all LSOAs ",
            "nationally. The solid line will suggest the value that may be expected for the ",
            "LSOA based on any relationship between the results and IMD rank. Across all ",
            "LSOAs in England, it is estimated that approximately 95% of values fall within ",
            "the dashed lines, and LSOAs well outside of these lines could be considered ",
            "as potential 'outliers'."
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Patients per thousand population"),
            "= ((distinct patients receiving COPD prescribing) / (ONS population estimate) * 1000)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Items per thousand population"),
            "= ((COPD prescription items) / (ONS population estimate) * 1000)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Patients per thousand population"),
            "= ((drug cost for COPD prescription items) / (ONS population estimate) * 1000)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Items per patient"),
            "= (COPD prescription items) / (distinct patients receiving COPD prescribing)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Cost per patient"),
            "= (drug cost for COPD prescription items) / (distinct patients receiving COPD prescribing)"
          )
        ),
        # Drug Split
        br(), br(),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Comparison of COPD drugs (chemical substance) prescribed to Core20 and Non Core20 ",
            "patients (2020/21):"
          ),
          tags$li(
            style = "margin-left: 20px",
            "Based on prescriptions items identified as representing prescribing for COPD (see ",
            "'Datasets' definitions), prescribed and dispensed in England, during 2020/21."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Drug classification based on chemical substance regardless of presentation or ",
            "strength. Any COPD drugs issued to less than 100 patients in 2020/21 have been ",
            "excluded to limit results to drugs more frequently prescribed."
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Proportion of patients"),
            "= (number of distinct patients prescribed drug) / (number of distinct patients prescribed any COPD drugs)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Proportion of items"),
            "= (number of prescription items for specific drug) / (number of COPD prescription items)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Proportion of cost"),
            "= (drug cost for specific drug) / (drug cost for COPD prescription items)"
          )
        ),
        # SABA
        br(), br(),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "SABA Prescribing:"
          ),
          tags$li(
            style = "margin-left: 20px",
            "This metric is based on a metric published in the ",
            enurl(
              url = "https://www.nhsbsa.nhs.uk/access-our-data-products/epact2/dashboards-and-specifications/respiratory-dashboard",
              text = "NHSBSA ePACT2 Respiratory Dashboard."
            )
          ),
          tags$li(
            style = "margin-left: 20px",
            "This metric identifies where patients may be potentially overusing SABA medication, ",
            "by identifying the proportion of patients prescribed preventer inhalers without ",
            "antimuscarinics who were also prescribed (1/6/13) or more SABA inhalers."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Figures are presented as a proportion of all patients prescribed preventer inhalers ",
            "without antimuscarinics."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Combinations of prescribing for each patient have been aggregated for all their ",
            "activity across the full financial year. For example, where patients have moved ",
            "across IMD deciles all activity has been taken into account. However, each patient ",
            "will only be reported against a single IMD decile (or age/gender group), based ",
            "on where most activity could be identified for the patient."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Separate reporting measures can be viewed by scrolling through the report"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Proportion of patients receiving one or more SABA inhalers, by IMD decile (2020/21)"),
            "= (People receiving a preventer inhalers and one or more SABA inhalers without an antimuscarinic) / (People receiving a preventer inhalers without an antimuscarinic)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Proportion of patients receiving six or more SABA inhalers, by IMD decile (2020/21)"),
            "= (People receiving a preventer inhalers and six or more SABA inhalers without an antimuscarinic) / (People receiving a preventer inhalers without an antimuscarinic)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Proportion of patients receiving 13 or more SABA inhalers, by IMD decile (2020/21)"),
            "= (People receiving a preventer inhalers and 13 or more SABA inhalers without an antimuscarinic) / (People receiving a preventer inhalers without an antimuscarinic)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Proportion of patients receiving 13 or more SABA inhalers, by age, gender and Core20 classification (2020/21)"),
            "= (People receiving a preventer inhalers and 13 or more SABA inhalers without an antimuscarinic) / (People receiving a preventer inhalers without an antimuscarinic)"
          ),
          br(),
          tags$li(
            style = "margin-left: 20px",
            "The full list of products used to define 'preventer inhalers', 'SABA' and ",
            "'antimuscarinics' can be seen below."
          ),
          nhs_card(
            heading = "List of drugs used to define SABA prescribing",
            # include the dynamic chart
            reactable::reactableOutput(outputId = ns("saba_drug_table")),
            mod_nhs_download_ui(id = ns("download_saba_drug_list"))
          )
        )
      )
    })



    # Text - Metric - 5_Hypertension_Prescribing --------------------------------------------------
    output$metric_sec_hypertension <- renderUI({
      tagList(
        br(),
        h3_tabstop("5. Hypertension Prescribing"),
        # Financial Year Trend
        br(),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Financial Year Trend:"
          ),
          tags$li(
            style = "margin-left: 20px",
            "Based on prescriptions items identified as representing prescribing for hypertension (see ",
            "'Datasets' definitions), prescribed and dispensed in England, by financial year."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Figures for Core20 v Non Core20 comparisons have been standardised by population ",
            "size to allow comparison as Core20 areas will only represent 20% of the population. ",
            "Base populations for calculated rates are based on population estimates for England, ",
            "published by ONS, with the closest calendar year being applied (e.g. 2019 mid year ",
            "population estimate used for 2019/20 data)."
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("National: Number of hypertension prescription items"),
            "= hypertension prescription items"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Core20 v Non Core20: hypertension prescription items per thousand population"),
            "= ((hypertension prescription items) / (ONS population estimate) * 1000)"
          )
        ),
        # Exemption Category Trend
        br(), br(),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Proportion of hypertension prescription items, by financial year, split by prescription ",
            "charge status (2015/16 to 2020/21):"
          ),
          tags$li(
            style = "margin-left: 20px",
            "Based on prescriptions items identified as representing prescribing for hypertension (see ",
            "'Datasets' definitions), prescribed and dispensed in England, during 2020/21. ",
            "The exemption category reflects that claimed by the patient based on declarations ",
            "on the back of the prescription form, with categories grouped as defined in the ",
            "'Key Terminology' section."
          )
        ),
        # Core20 v Non Core20
        br(), br(),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Hypertension prescribing data metrics for Core20 and non-Core20 patients (2020/21):"
          ),
          tags$li(
            style = "margin-left: 20px",
            "Based on prescriptions items identified as representing prescribing for hypertension (see ",
            "'Datasets' definitions), prescribed and dispensed in England, during 2020/21. ",
            "The exemption category reflects that claimed by the patient based on declarations ",
            "on the back of the prescription form, with categories grouped as defined in the ",
            "'Key Terminology' section. Core20 classification is based on the patient's ",
            "identified residential address."
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Hypertension prescription items per patient"),
            "= (total number of hypertension prescription items) / (total number of distinct identified ",
            "patients receiving hypertension prescribing)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Drug cost per patient"),
            "= (total net ingredient drug cost for hypertension prescription items) / (total ",
            "number of distinct identified patients receiving hypertension prescribing)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Percentage of prescribing paid for"),
            "= (hypertension prescription items with some payment from patient) / (total hypertension ",
            "prescription items)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Percentage of prescribing age exempt"),
            "= (hypertension prescription items claimed as age exempt) / (total hypertension prescription ",
            "items)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Percentage of prescribing other charge exempt"),
            "= (hypertension prescription items claimed as non age related exemption) / (total hypertension ",
            "prescription items)"
          )
        ),
        # IMD Decile
        br(), br(),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Hypertension prescribing by IMD Decile (2020/21):"
          ),
          tags$li(
            style = "margin-left: 20px",
            "Based on prescriptions items identified as representing prescribing for hypertension (see ",
            "'Datasets' definitions), prescribed and dispensed in England, for 2020/21."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Rates per thousand population are based on English mid year population estimates ",
            "2020, as published by ONS."
          ),
          tags$li(
            style = "margin-left: 20px",
            "IMD decile classification based on the LSOA associated to the patient's ",
            "residential address, with results limited to records where the patient's address ",
            "can be identified."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Looking at IMD decile on its own may mask some relationships where both deprivation ",
            "and age are contributing factors as there is a standalone relationship between ",
            "deprivation and age as the least deprived areas of the country have a higher ",
            "proportion of older people."
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Patients per thousand population"),
            "= ((distinct patients receiving hypertension prescribing) / (ONS population estimate) * 1000)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Items per thousand population"),
            "= ((hypertension prescription items) / (ONS population estimate) * 1000)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Patients per thousand population"),
            "= ((drug cost for hypertension prescription items) / (ONS population estimate) * 1000)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Items per patient"),
            "= (hypertension prescription items) / (distinct patients receiving hypertension prescribing)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Cost per patient"),
            "= (drug cost for hypertension prescription items) / (distinct patients receiving hypertension prescribing)"
          )
        ),
        # Age and Gender
        br(), br(),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Hypertension prescribing by age and gender (2020/21):"
          ),
          tags$li(
            style = "margin-left: 20px",
            "Based on prescriptions items identified as representing prescribing for hypertension (see ",
            "'Datasets' definitions), prescribed and dispensed in England, for 2020/21."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Rates per thousand population are based on English mid year population estimates ",
            "2020, as published by ONS."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Results limited to records where the patient's age, gender and residential location ",
            "can be identified."
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Patients per thousand population"),
            "= ((distinct patients receiving hypertension prescribing) / (ONS population estimate) * 1000)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Items per thousand population"),
            "= ((hypertension prescription items) / (ONS population estimate) * 1000)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Patients per thousand population"),
            "= ((drug cost for hypertension prescription items) / (ONS population estimate) * 1000)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Items per patient"),
            "= (hypertension prescription items) / (distinct patients receiving hypertension prescribing)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Cost per patient"),
            "= (drug cost for hypertension prescription items) / (distinct patients receiving hypertension prescribing)"
          )
        ),
        # Local Authority
        br(), br(),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Hypertension prescribing by Index of Multiple Deprivation ranking for English Local ",
            "Authorities (2020/21):"
          ),
          tags$li(
            style = "margin-left: 20px",
            "Based on prescriptions items identified as representing prescribing for Hypertension (see ",
            "'Datasets' definitions), prescribed and dispensed in England, for 2020/21."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Rates per thousand population are based on English mid year population estimates ",
            "2020, as published by ONS."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Local authority classification based on the patient's residential address, with ",
            "results limited to records where the patient's address can be identified. Excluding ",
            "any local authorities with less than five patients receiving hypertension prescriptions ",
            "to prevent any potential patient disclosure."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Each point on the scatter chart will represent a local authority and the value ",
            "for the chosen metric. The solid line represents the results of a simple regression ",
            "model based on the metric figures and the IMD ranking across all local ",
            "authorities. This line will suggest the value that may be expected for the ",
            "local authority based on any relationship between the results and IMD rank."
          ),
          tags$li(
            style = "margin-left: 20px",
            "The map view will show the values for the chosen metric and chosen region. ",
            "Selecting a point on the map will update the deprivation decile chart to ",
            "reflect the selected local authority."
          ),
          tags$li(
            style = "margin-left: 20px",
            "The deprivation decile chart will show, for the chosed local authority, the ",
            "overall population distribution (based on ONS mid 2020 population estimates) ",
            "by IMD decile."
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Patients per thousand population"),
            "= ((distinct patients receiving hypertension prescribing) / (ONS population estimate) * 1000)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Items per thousand population"),
            "= ((hypertension prescription items) / (ONS population estimate) * 1000)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Patients per thousand population"),
            "= ((drug cost for hypertension prescription items) / (ONS population estimate) * 1000)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Items per patient"),
            "= (hypertension prescription items) / (distinct patients receiving hypertension prescribing)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Cost per patient"),
            "= (drug cost for hypertension prescription items) / (distinct patients receiving hypertension prescribing)"
          )
        ),
        # LSOA
        br(), br(),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Hypertension prescribing by Index of Multiple Deprivation ranking for English LSOA ",
            "(2020/21):"
          ),
          tags$li(
            style = "margin-left: 20px",
            "Based on prescriptions items identified as representing prescribing for hypertension (see ",
            "'Datasets' definitions), prescribed and dispensed in England, for 2020/21."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Rates per thousand population are based on English mid year population estimates ",
            "2020, as published by ONS."
          ),
          tags$li(
            style = "margin-left: 20px",
            "LSOA classification based on the patient's residential address, with results ",
            "limited to records where the patient's address can be identified. Excluding ",
            "any LSOAs with less than five patients receiving hypertension prescriptions to prevent any ",
            "potential patient disclosure."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Each point on the scatter chart will represent a LSOA, with LSOAs plotted for ",
            "the selected local authority. The lines represent the results of a simple ",
            "regression model based on the metric figures and the IMD ranking across all LSOAs ",
            "nationally. The solid line will suggest the value that may be expected for the ",
            "LSOA based on any relationship between the results and IMD rank. Across all ",
            "LSOAs in England, it is estimated that approximately 95% of values fall within ",
            "the dashed lines, and LSOAs well outside of these lines could be considered ",
            "as potential 'outliers'."
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Patients per thousand population"),
            "= ((distinct patients receiving hypertension prescribing) / (ONS population estimate) * 1000)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Items per thousand population"),
            "= ((hypertension prescription items) / (ONS population estimate) * 1000)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Patients per thousand population"),
            "= ((drug cost for hypertension prescription items) / (ONS population estimate) * 1000)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Items per patient"),
            "= (hypertension prescription items) / (distinct patients receiving hypertension prescribing)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Cost per patient"),
            "= (drug cost for hypertension prescription items) / (distinct patients receiving hypertension prescribing)"
          )
        ),
        # Drug Split
        br(), br(),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Comparison of hypertension drugs (chemical substance) prescribed to Core20 and Non Core20 ",
            "patients (2020/21):"
          ),
          tags$li(
            style = "margin-left: 20px",
            "Based on prescriptions items identified as representing prescribing for hypertension (see ",
            "'Datasets' definitions), prescribed and dispensed in England, during 2020/21."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Drug classification based on chemical substance regardless of presentation or ",
            "strength. Any hypertension drugs issued to less than 100 patients in 2020/21 have been ",
            "excluded to limit results to drugs more frequently prescribed."
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Proportion of patients"),
            "= (number of distinct patients prescribed drug) / (number of distinct patients prescribed any hypertension drugs)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Proportion of items"),
            "= (number of prescription items for specific drug) / (number of hypertension prescription items)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Proportion of cost"),
            "= (drug cost for specific drug) / (drug cost for hypertension prescription items)"
          )
        )
      )
    })



    # Text - Metric - 6_SMI_Prescribing -----------------------------------------------------------
    output$metric_sec_smi <- renderUI({
      tagList(
        br(),
        h3_tabstop("6. SMI Prescribing"),
        # Financial Year Trend
        br(),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Financial Year Trend:"
          ),
          tags$li(
            style = "margin-left: 20px",
            "Based on prescriptions items identified as representing prescribing for SMI (see ",
            "'Datasets' definitions), prescribed and dispensed in England, by financial year."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Figures for Core20 v Non Core20 comparisons have been standardised by population ",
            "size to allow comparison as Core20 areas will only represent 20% of the population. ",
            "Base populations for calculated rates are based on population estimates for England, ",
            "published by ONS, with the closest calendar year being applied (e.g. 2019 mid year ",
            "population estimate used for 2019/20 data)."
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("National: Number of SMI prescription items"),
            "= SMI prescription items"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Core20 v Non Core20: SMI prescription items per thousand population"),
            "= ((SMI prescription items) / (ONS population estimate) * 1000)"
          )
        ),
        # Exemption Category Trend
        br(), br(),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Proportion of SMI prescription items, by financial year, split by prescription ",
            "charge status (2015/16 to 2020/21):"
          ),
          tags$li(
            style = "margin-left: 20px",
            "Based on prescriptions items identified as representing prescribing for SMI (see ",
            "'Datasets' definitions), prescribed and dispensed in England, during 2020/21. ",
            "The exemption category reflects that claimed by the patient based on declarations ",
            "on the back of the prescription form, with categories grouped as defined in the ",
            "'Key Terminology' section."
          )
        ),
        # Core20 v Non Core20
        br(), br(),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "SMI prescribing data metrics for Core20 and non-Core20 patients (2020/21):"
          ),
          tags$li(
            style = "margin-left: 20px",
            "Based on prescriptions items identified as representing prescribing for SMI (see ",
            "'Datasets' definitions), prescribed and dispensed in England, during 2020/21. ",
            "The exemption category reflects that claimed by the patient based on declarations ",
            "on the back of the prescription form, with categories grouped as defined in the ",
            "'Key Terminology' section. Core20 classification is based on the patient's ",
            "identified residential address."
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("SMI prescription items per patient"),
            "= (total number of SMI prescription items) / (total number of distinct identified ",
            "patients receiving SMI prescribing)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Drug cost per patient"),
            "= (total net ingredient drug cost for SMI prescription items) / (total ",
            "number of distinct identified patients receiving SMI prescribing)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Percentage of prescribing paid for"),
            "= (SMI prescription items with some payment from patient) / (total SMI ",
            "prescription items)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Percentage of prescribing age exempt"),
            "= (SMI prescription items claimed as age exempt) / (total SMI prescription ",
            "items)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Percentage of prescribing other charge exempt"),
            "= (SMI prescription items claimed as non age related exemption) / (total SMI ",
            "prescription items)"
          )
        ),
        # IMD Decile
        br(), br(),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "SMI prescribing by IMD Decile (2020/21):"
          ),
          tags$li(
            style = "margin-left: 20px",
            "Based on prescriptions items identified as representing prescribing for SMI (see ",
            "'Datasets' definitions), prescribed and dispensed in England, for 2020/21."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Rates per thousand population are based on English mid year population estimates ",
            "2020, as published by ONS."
          ),
          tags$li(
            style = "margin-left: 20px",
            "IMD decile classification based on the LSOA associated to the patient's ",
            "residential address, with results limited to records where the patient's address ",
            "can be identified."
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Patients per thousand population"),
            "= ((distinct patients receiving SMI prescribing) / (ONS population estimate) * 1000)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Items per thousand population"),
            "= ((SMI prescription items) / (ONS population estimate) * 1000)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Patients per thousand population"),
            "= ((drug cost for SMI prescription items) / (ONS population estimate) * 1000)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Items per patient"),
            "= (SMI prescription items) / (distinct patients receiving SMI prescribing)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Cost per patient"),
            "= (drug cost for SMI prescription items) / (distinct patients receiving SMI prescribing)"
          )
        ),
        # Age and Gender
        br(), br(),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "SMI prescribing by age and gender (2020/21):"
          ),
          tags$li(
            style = "margin-left: 20px",
            "Based on prescriptions items identified as representing prescribing for SMI (see ",
            "'Datasets' definitions), prescribed and dispensed in England, for 2020/21."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Rates per thousand population are based on English mid year population estimates ",
            "2020, as published by ONS."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Results limited to records where the patient's age, gender and residential location ",
            "can be identified."
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Patients per thousand population"),
            "= ((distinct patients receiving SMI prescribing) / (ONS population estimate) * 1000)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Items per thousand population"),
            "= ((SMI prescription items) / (ONS population estimate) * 1000)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Patients per thousand population"),
            "= ((drug cost for SMI prescription items) / (ONS population estimate) * 1000)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Items per patient"),
            "= (SMI prescription items) / (distinct patients receiving SMI prescribing)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Cost per patient"),
            "= (drug cost for SMI prescription items) / (distinct patients receiving SMI prescribing)"
          )
        ),
        # Local Authority
        br(), br(),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "SMI prescribing by Index of Multiple Deprivation ranking for English Local ",
            "Authorities (2020/21):"
          ),
          tags$li(
            style = "margin-left: 20px",
            "Based on prescriptions items identified as representing prescribing for SMI (see ",
            "'Datasets' definitions), prescribed and dispensed in England, for 2020/21."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Rates per thousand population are based on English mid year population estimates ",
            "2020, as published by ONS."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Local authority classification based on the patient's residential address, with ",
            "results limited to records where the patient's address can be identified. Excluding ",
            "any local authorities with less than five patients receiving SMI prescriptions ",
            "to prevent any potential patient disclosure."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Each point on the scatter chart will represent a local authority and the value ",
            "for the chosen metric. The solid line represents the results of a simple regression ",
            "model based on the metric figures and the IMD ranking across all local ",
            "authorities. This line will suggest the value that may be expected for the ",
            "local authority based on any relationship between the results and IMD rank."
          ),
          tags$li(
            style = "margin-left: 20px",
            "The map view will show the values for the chosen metric and chosen region. ",
            "Selecting a point on the map will update the deprivation decile chart to ",
            "reflect the selected local authority."
          ),
          tags$li(
            style = "margin-left: 20px",
            "The deprivation decile chart will show, for the chosen local authority, the ",
            "overall population distribution (based on ONS mid 2020 population estimates) ",
            "by IMD decile."
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Patients per thousand population"),
            "= ((distinct patients receiving SMI prescribing) / (ONS population estimate) * 1000)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Items per thousand population"),
            "= ((SMI prescription items) / (ONS population estimate) * 1000)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Patients per thousand population"),
            "= ((drug cost for SMI prescription items) / (ONS population estimate) * 1000)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Items per patient"),
            "= (SMI prescription items) / (distinct patients receiving SMI prescribing)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Cost per patient"),
            "= (drug cost for SMI prescription items) / (distinct patients receiving SMI prescribing)"
          )
        ),
        # LSOA
        br(), br(),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "SMI prescribing by Index of Multiple Deprivation ranking for English LSOA ",
            "(2020/21):"
          ),
          tags$li(
            style = "margin-left: 20px",
            "Based on prescriptions items identified as representing prescribing for SMI (see ",
            "'Datasets' definitions), prescribed and dispensed in England, for 2020/21."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Rates per thousand population are based on English mid year population estimates ",
            "2020, as published by ONS."
          ),
          tags$li(
            style = "margin-left: 20px",
            "LSOA classification based on the patient's residential address, with results ",
            "limited to records where the patient's address can be identified. Excluding ",
            "any LSOAs with less than five patients receiving SMI prescriptions to prevent any ",
            "potential patient disclosure."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Each point on the scatter chart will represent a LSOA, with LSOAs plotted for ",
            "the selected local authority. The lines represent the results of a simple ",
            "regression model based on the metric figures and the IMD ranking across all LSOAs ",
            "nationally. The solid line will suggest the value that may be expected for the ",
            "LSOA based on any relationship between the results and IMD rank. Across all ",
            "LSOAs in England, it is estimated that approximately 95% of values fall within ",
            "the dashed lines, and LSOAs well outside of these lines could be considered ",
            "as potential 'outliers'."
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Patients per thousand population"),
            "= ((distinct patients receiving SMI prescribing) / (ONS population estimate) * 1000)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Items per thousand population"),
            "= ((SMI prescription items) / (ONS population estimate) * 1000)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Patients per thousand population"),
            "= ((drug cost for SMI prescription items) / (ONS population estimate) * 1000)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Items per patient"),
            "= (SMI prescription items) / (distinct patients receiving SMI prescribing)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Cost per patient"),
            "= (drug cost for SMI prescription items) / (distinct patients receiving SMI prescribing)"
          )
        ),
        # Drug Split
        br(), br(),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Comparison of SMI drugs (chemical substance) prescribed to Core20 and Non Core20 ",
            "patients (2020/21):"
          ),
          tags$li(
            style = "margin-left: 20px",
            "Based on prescriptions items identified as representing prescribing for SMI (see ",
            "'Datasets' definitions), prescribed and dispensed in England, during 2020/21."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Drug classification based on chemical substance regardless of presentation or ",
            "strength. Any SMI drugs issued to less than 100 patients in 2020/21 have been ",
            "excluded to limit results to drugs more frequently prescribed."
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Proportion of patients"),
            "= (number of distinct patients prescribed drug) / (number of distinct patients prescribed any SMI drugs)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Proportion of items"),
            "= (number of prescription items for specific drug) / (number of SMI prescription items)"
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Proportion of cost"),
            "= (drug cost for specific drug) / (drug cost for SMI prescription items)"
          )
        )
      )
    })



    # Text - Metric - 7_NHSBSA_Exemption_Certificates ---------------------------------------------
    output$metric_sec_certificates <- renderUI({
      tagList(
        br(),
        h3_tabstop("7. NHSBSA Exemption Certificates"),
        # MEDEX Trend
        br(),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Number of people issued and holding active MEDEX certificates (2016/17 to 2020/21):"
          ),
          tags$li(
            style = "margin-left: 20px",
            "The number of people issued certificates will reflect the people receiving a ",
            "certificate in any of the reported financial years."
          ),
          tags$li(
            style = "margin-left: 20px",
            "The number of people with active certificates will reflect the people with a MEDEX ",
            "certificate that was valid for at least one day during the reported financial year."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Data retention schedules have required historic certificate data to be deleted ",
            "within specific timeframes, resulting in active certificates only being possible to ",
            "identify from 2019/20 as records for certificates that may have been active prior ",
            "to this have been deleted."
          ),
          tags$li(
            style = "margin-left: 20px",
            "See 'Datasets' for restrictions and classifications applied to MEDEX dataset, ",
            "including identification of distinct certificate holders."
          )
        ),
        # MATEX Rate Drop
        br(), br(),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Drop in MEDEX issue rate between 2019/20 and 2020/21 by IMD decile:"
          ),
          tags$li(
            style = "margin-left: 20px",
            "Based on the number of people issued MEDEX certificates in 2019/20 and 2020/21, with ",
            "issue rate based on certificates per 10 thousand base population."
          ),
          tags$li(
            style = "margin-left: 20px",
            "The base population figure reflects the population estimates published by ONS, with ",
            "only people aged 16-60 included to reflect the age groups that would typically ",
            "apply for MEDEX support. For certificates issued in 2019/20 the rate is based on ",
            "mid-year population estimates for 2019 whilst population estimates for 2020 were ",
            "used for certificates issued in 2020/21."
          ),
          tags$li(
            style = "margin-left: 40px",
            tags$b("Drop in certificate issue rate"),
            "= ((Certificate issue rate 2020/21) - (Certificate Issue rate 2019/20)) / (Certificate Issue rate 2019/20)"
          )
        ),
        # MATEX Trend
        br(), br(),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Number of people issued and holding active MATEX certificates (2018/19 to 2020/21):"
          ),
          tags$li(
            style = "margin-left: 20px",
            "The number of people issued certificates will reflect the people receiving a ",
            "certificate in any of the reported financial years."
          ),
          tags$li(
            style = "margin-left: 20px",
            "The number of people with active certificates will reflect the people with a MATEX ",
            "certificate that was valid for at least one day during the reported financial year."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Data retention schedules have required historic certificate data to be deleted ",
            "within specific timeframes. This has removed records for certificates issued prior ",
            "to 2018/19 and also resulted in active certificates only being possible to ",
            "identify from 2019/20 onwards as records for certificates that may have been active ",
            "prior to this have been deleted."
          ),
          tags$li(
            style = "margin-left: 20px",
            "See 'Datasets' for restrictions and classifications applied to MATEX dataset, ",
            "including identification of distinct certificate holders."
          )
        ),
        # LIS Trend
        br(), br(),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Number of people issued and holding active LIS (HC2&HC3) certificates (2016/17 to 2020/21):"
          ),
          tags$li(
            style = "margin-left: 20px",
            "The number of people issued certificates will reflect the people receiving a HC2 or HC3 ",
            "certificate in any of the reported financial years."
          ),
          tags$li(
            style = "margin-left: 20px",
            "The number of people with active certificates will reflect the people with a HC2 or HC3 ",
            "certificate that was valid for at least one day during the reported financial year."
          ),
          tags$li(
            style = "margin-left: 20px",
            "See 'Datasets' for restrictions and classifications applied to LIS dataset, ",
            "including identification of distinct certificate holders."
          )
        ),
        # Tax Credit Trend
        br(), br(),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Number of people issued and holding active Tax Credit certificates (2016/17 to 2020/21):"
          ),
          tags$li(
            style = "margin-left: 20px",
            "The number of people issued certificates will reflect the people receiving a Tax Credit ",
            "certificate in any of the reported financial years."
          ),
          tags$li(
            style = "margin-left: 20px",
            "The number of people with active certificates will reflect the people with a Tax Credit ",
            "certificate that was valid for at least one day during the reported financial year."
          ),
          tags$li(
            style = "margin-left: 20px",
            "See 'Datasets' for restrictions and classifications applied to LIS dataset, ",
            "including identification of distinct certificate holders."
          )
        ),
        # Age and Gender
        br(), br(),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Age and Gender split:"
          ),
          tags$li(
            style = "margin-left: 20px",
            "The number of people with active certificates during 2020/21, split by age and gender."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Figures presented as a rate per 10 thousand of the applicable base population, based ",
            "on figures published by ONS."
          ),
          tags$li(
            style = "margin-left: 40px",
            "For MEDEX, rates are based on English population estimates (2020) for people aged 16-60."
          ),
          tags$li(
            style = "margin-left: 40px",
            "For MATEX, rates are based on English live birth figures (see 'Datasets' for more information)."
          ),
          tags$li(
            style = "margin-left: 40px",
            "For LIS and Tax Credits, rates are based on English population estimates (2020) for people aged 16+."
          )
        ),
        # IMD
        br(), br(),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "IMD Decile split:"
          ),
          tags$li(
            style = "margin-left: 20px",
            "The number of people with active certificates during 2020/21, split by IMD decile."
          ),
          tags$li(
            style = "margin-left: 20px",
            "IMD decile assigned based on the certificate holder's residential address."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Figures presented as a rate per 10 thousand of the applicable base population, based ",
            "on figures published by ONS."
          ),
          tags$li(
            style = "margin-left: 40px",
            "For MEDEX, rates are based on English population estimates (2020) for people aged 16-60."
          ),
          tags$li(
            style = "margin-left: 40px",
            "For MATEX, rates are based on English live birth figures (see 'Datasets' for more information)."
          ),
          tags$li(
            style = "margin-left: 40px",
            "For LIS and Tax Credits, rates are based on English population estimates (2020) for people aged 16+."
          )
        ),
        # Local Authority
        br(), br(),
        tags$ul(
          tags$li(
            style = "list-style:none; font-weight:bold;",
            "Estimated take-up of NHSBSA Exemption Certificates by Index of Multiple Deprivation ",
            "for English Local Authorities (2020/21):"
          ),
          tags$li(
            style = "margin-left: 20px",
            "The number of people with active certificates during 2020/21, split by local authority."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Local authority assigned based on the certificate holder's residential address."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Figures presented as a rate per 10 thousand of the applicable base population, based ",
            "on figures published by ONS."
          ),
          tags$li(
            style = "margin-left: 40px",
            "For MEDEX, rates are based on English population estimates (2020) for people aged 16-60."
          ),
          tags$li(
            style = "margin-left: 40px",
            "For MATEX, rates are based on English live birth figures (see 'Datasets' for more information)."
          ),
          tags$li(
            style = "margin-left: 40px",
            "For LIS and Tax Credits, rates are based on English population estimates (2020) for people aged 16+."
          ),
          tags$li(
            style = "margin-left: 20px",
            "Each point on the scatter chart will represent a local authority and the value ",
            "for the chosen exemption certificate. The solid line represents the results of a ",
            "simple regression model based on the certificate figures and the IMD ranking across all ",
            "local authorities. This line will suggest the value that may be expected for the ",
            "local authority based on any relationship between the results and IMD rank."
          ),
          tags$li(
            style = "margin-left: 20px",
            "The map view will show the values for the chosen certificate and chosen region. ",
            "Selecting a point on the map will update the deprivation decile chart to ",
            "reflect the selected local authority."
          ),
          tags$li(
            style = "margin-left: 20px",
            "The deprivation decile chart will show, for the chosen local authority, the ",
            "overall population distribution (based on ONS mid 2020 population estimates) ",
            "by IMD decile."
          )
        )
      )
    })

    # Table - Drug Classification - COPD ----------------------------------------------------------
    # reactable table aggregated to BNF Section to BNF Product
    output$copd_drug_table <- reactable::renderReactable({
      antibioticPrescribingScrollytellR::copd_drug_list_table %>%
        reactable::reactable(
          style = list(fontFamily = "Work Sans, sans-serif", fontSize = "11px"),
          filterable = TRUE,
          pagination = FALSE,
          searchable = TRUE,
          highlight = TRUE,
          bordered = TRUE,
          compact = TRUE,
          height = 400,
          columns = list(
            SECTION_NAME = reactable::colDef(name = "BNF Section"),
            PARAGRAPH_NAME = reactable::colDef(name = "BNF Paragraph"),
            SUB_PARAGRAPH_NAME = reactable::colDef(name = "BNF Sub-paragraph"),
            CHEMICAL_SUBSTANCE_NAME = reactable::colDef(name = "BNF Chemical Substance"),
            PRODUCT_NAME = reactable::colDef(name = "BNF Product")
          )
        )
    })


    # Download - Drug Classification - COPD -------------------------------------------------------
    mod_nhs_download_server(
      id = "download_copd_drug_list",
      filename = "copd_drug_list.csv",
      export_data = antibioticPrescribingScrollytellR::copd_drug_list_download
    )


    # Table - Drug Classification - Hypertension --------------------------------------------------
    # reactable table aggregated to BNF Section to BNF Product
    output$hypertension_drug_table <- reactable::renderReactable({
      antibioticPrescribingScrollytellR::hypertension_drug_list_table %>%
        reactable::reactable(
          style = list(fontFamily = "Work Sans, sans-serif", fontSize = "11px"),
          filterable = TRUE,
          pagination = FALSE,
          searchable = TRUE,
          highlight = TRUE,
          bordered = TRUE,
          compact = TRUE,
          height = 400,
          columns = list(
            SECTION_NAME = reactable::colDef(name = "BNF Section"),
            PARAGRAPH_NAME = reactable::colDef(name = "BNF Paragraph"),
            SUB_PARAGRAPH_NAME = reactable::colDef(name = "BNF Sub-paragraph"),
            CHEMICAL_SUBSTANCE_NAME = reactable::colDef(name = "BNF Chemical Substance"),
            PRODUCT_NAME = reactable::colDef(name = "BNF Product")
          )
        )
    })


    # Download - Drug Classification - Hypertension -----------------------------------------------
    mod_nhs_download_server(
      id = "download_hypertension_drug_list",
      filename = "hypertension_drug_list.csv",
      export_data = antibioticPrescribingScrollytellR::hypertension_drug_list_download
    )


    # Table - Drug Classification - SMI -----------------------------------------------------------
    # reactable table aggregated to BNF Section to BNF Product
    output$smi_drug_table <- reactable::renderReactable({
      antibioticPrescribingScrollytellR::smi_drug_list_table %>%
        reactable::reactable(
          style = list(fontFamily = "Work Sans, sans-serif", fontSize = "11px"),
          filterable = TRUE,
          pagination = FALSE,
          searchable = TRUE,
          highlight = TRUE,
          bordered = TRUE,
          compact = TRUE,
          height = 400,
          columns = list(
            SECTION_NAME = reactable::colDef(name = "BNF Section"),
            PARAGRAPH_NAME = reactable::colDef(name = "BNF Paragraph"),
            SUB_PARAGRAPH_NAME = reactable::colDef(name = "BNF Sub-paragraph"),
            CHEMICAL_SUBSTANCE_NAME = reactable::colDef(name = "BNF Chemical Substance"),
            PRODUCT_NAME = reactable::colDef(name = "BNF Product")
          )
        )
    })


    # Download - Drug Classification - SMI --------------------------------------------------------
    mod_nhs_download_server(
      id = "download_smi_drug_list",
      filename = "smi_drug_list.csv",
      export_data = antibioticPrescribingScrollytellR::smi_drug_list_download
    )



    # Table - Drug Classification - SABA ----------------------------------------------------------
    # reactable table
    output$saba_drug_table <- reactable::renderReactable({
      antibioticPrescribingScrollytellR::saba_drug_list_table %>%
        reactable::reactable(
          style = list(fontFamily = "Work Sans, sans-serif", fontSize = "11px"),
          filterable = TRUE,
          pagination = FALSE,
          searchable = TRUE,
          highlight = TRUE,
          bordered = TRUE,
          compact = TRUE,
          height = 400,
          columns = list(
            SABA_DRUG_GROUP = reactable::colDef(name = "Drug Group"),
            PRESENTATION_BNF_DESCR = reactable::colDef(name = "BNF Presentation Name"),
            BNF_CODE = reactable::colDef(name = "BNF Presentation Code")
          )
        )
    })


    # Download - Drug Classification - SABA -------------------------------------------------------
    mod_nhs_download_server(
      id = "download_saba_drug_list",
      filename = "saba_drug_list.csv",
      export_data = antibioticPrescribingScrollytellR::saba_drug_list_download
    )
  })
}
