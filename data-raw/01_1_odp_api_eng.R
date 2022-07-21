library(dplyr)
devtools::load_all()


# Define the url for the API call
base_endpoint <- "https://opendata.nhsbsa.net/api/3/action/"
package_list_method <- "package_list" # List of data-sets in the portal
package_show_method <- "package_show?id=" # List all resources of a data-set
action_method <- "datastore_search_sql?" # SQL action method
dataset_id <- "english-prescribing-data-epd"

# API calls for data for multiple months ------------------------------------

# Firstly we need to get a list of all of the names and resource IDs for every
# EPD file. We therefore extract the metadata for the EPD dataset.
metadata_repsonse <- jsonlite::fromJSON(paste0(
  base_endpoint,
  package_show_method,
  dataset_id
))

# Resource names and IDs are kept within the resources table returned from the
# package_show_method call.
resources_table <- metadata_repsonse$result$resources

# Rolling 12months data (same way as NHS improvement)
# April 21 (contains 12 months rolling average), May21 etc until March 2022
resource_name_list <- resources_table$name[89:101] # Covers from May 2020 - April 2022 (need two time period for dumbbell chart)

# 5.1. For loop ----------------------------------------------------------------

# We can do this with a for loop that makes all of the individual API calls for
# you and combines the data together into one dataframe

# Initialise dataframe that data will be saved to
antibiotic_df <- data.frame()

# Loop through resource_name_list and make call to API to extract data, then
# bind each month together to make a single data-set
for (month in resource_name_list) {

  # Build temporary SQL query
  tmp_query <- paste0(
    "
  SELECT
      YEAR_MONTH,
      REGIONAL_OFFICE_NAME,
      REGIONAL_OFFICE_CODE,
      STP_NAME,
      STP_CODE,
      PCO_NAME,
      PCO_CODE,
      PRACTICE_NAME,
      PRACTICE_CODE,
      POSTCODE,
      BNF_CHEMICAL_SUBSTANCE,
      CHEMICAL_SUBSTANCE_BNF_DESCR,
      QUANTITY,
      ITEMS,
      TOTAL_QUANTITY,
      ADQUSAGE,
      NIC,
      ACTUAL_COST

  FROM `",
    month, "`
  WHERE
    BNF_CHEMICAL_SUBSTANCE LIKE '0501%'
  "
  )

  # Build temporary API call
  tmp_api_call <- paste0(
    base_endpoint,
    action_method,
    "resource_id=",
    month,
    "&",
    "sql=",
    URLencode(tmp_query) # Encode spaces in the url
  )

  # Grab the response JSON as a temporary list
  tmp_response <- jsonlite::fromJSON(tmp_api_call)

  # Extract records in the response to a temporary dataframe
  tmp_df <- data.table::fread(tmp_response$result$gc_urls$url[1])

  # Bind the temporary data to the main dataframe
  antibiotic_df <- dplyr::bind_rows(antibiotic_df, tmp_df)
}


# Good place to check
# https://openprescribing.net/analyse/#org=practice&orgIds=A86016&numIds=0501013B0&denom=star_pu.oral_antibacterials_item&selectedTab=summary
# and it matches
# antibiotic_df %>% filter(PRACTICE_NAME == 'LANE END SURGERY' & YEAR_MONTH == 202204 & CHEMICAL_SUBSTANCE_BNF_DESCR == 'Amoxicillin') %>% summarise(sum(ITEMS))


# Currently, our data contains OOH, community as well as old CCG

antibiotic_df <- antibiotic_df %>%
  filter(stringr::str_detect(PCO_NAME, "CCG")) %>%
  filter(YEAR_MONTH > 202105) # only for 12 months to May 22


antibiotic_eng_final <- antibiotic_df %>%
  mutate(DRUG_OF_INTEREST_LUTI = case_when(
    BNF_CHEMICAL_SUBSTANCE %in% c(
      "0501015P0", "0501021A0", "0501021B0", "0501021C0",
      "0501130R0", "0501021K0", "0501021L0", "0501021M0",
      "0501080W0", "0501070AE", "0501130S0"
    ) ~ "Lower UTI",
    TRUE ~ "Other drugs"
  )) %>%
  mutate(DRUG_OF_INTEREST = case_when(
    BNF_CHEMICAL_SUBSTANCE == "0501013B0" ~ "Amoxicillin",
    BNF_CHEMICAL_SUBSTANCE == "0501013K0" ~ "Co-amoxiclav",
    BNF_CHEMICAL_SUBSTANCE %in% c(
      "0501021A0", "0501021B0",
      "0501021C0", "0501021D0", "0501021G0",
      "0501021H0", "0501021J0", "0501021K0",
      "0501021L0", "0501021M0"
    ) ~ "Cephalosporins",
    BNF_CHEMICAL_SUBSTANCE %in% c(
      "0501021L0", "0501021M0", "0501120L0",
      "0501120P0", "0501120Q0", "0501120X0",
      "0501120Y0"
    ) ~ "Quinolones",
    TRUE ~ "Other drugs"
  ))


antibiotic_eng_luti <- antibiotic_eng_final %>%
  group_by(DRUG_OF_INTEREST_LUTI) %>%
  summarise(TOTAL_ITEMS = sum(ITEMS, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(DRUG_OF_INTEREST_LUTI == "Lower UTI") %>%
  rename(DRUG_OF_INTEREST = DRUG_OF_INTEREST_LUTI)
# exclude if practice code is empty
# filter(PRACTICE_NAME != "UNIDENTIFIED DOCTORS")

antibiotic_eng <- antibiotic_eng_final %>%
  group_by(DRUG_OF_INTEREST) %>%
  summarise(TOTAL_ITEMS = sum(ITEMS, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(DRUG_OF_INTEREST != "Other drugs")


antibiotic_eng <- bind_rows(antibiotic_eng, antibiotic_eng_luti) %>%
  mutate(STAR_PU = TOTAL_ITEMS / 34153292.65)


# similar enough so will keep it.......
usethis::use_data(antibiotic_eng_final, overwrite = TRUE)

usethis::use_data(antibiotic_eng, overwrite = TRUE)
