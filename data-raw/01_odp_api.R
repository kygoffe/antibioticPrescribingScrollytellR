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
resource_name_list <- resources_table$name[77:100] # Covers from May 2020 - April 2022 (need two time period for dumbbell chart)

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
  filter(stringr::str_detect(PCO_NAME, "CCG"))


# CCG Merge happended in april 2021 - therefore, deal two time period separately
antibiotic_df_2021 <- antibiotic_df %>%
  filter(YEAR_MONTH %in% c(
    202005, 202006, 202007, 202008, 202009, 202010, 202011, 202012,
    202101, 202102, 202103, 202104
  ))


antibiotic_df_2021 <- antibiotic_df_2021 %>%
  mutate(SUB_ICB_CODE = gsub("0*$", "", PCO_CODE)) %>% # First remove trailing 00s
  # deal with merged CCG issue - add all merged one to keep 106 CCGs at the end that is my aim
  mutate(SUB_ICB_CODE = case_when(
    PCO_CODE %in% c("06F00", "06P00", "04F00") ~ "M1J4Y",
    PCO_CODE %in% c("10K00", "10L00", "10J00", "10V00", "10X00", "11A00") ~ "D9Y0V",
    PCO_CODE %in% c("15D00", "99M00", "10C00") ~ "D4U1Y",
    PCO_CODE %in% c("07L00", "07T00", "08F00", "08M00", "08N00", "08V00", "08W00") ~ "A3A8R",
    PCO_CODE %in% c("07P00", "09A00", "07W00", "08C00", "08E00", "08G00", "07Y00", "08Y00") ~ "W2U3Z",
    PCO_CODE %in% c("05N00", "05X00") ~ "M2L0M",
    PCO_CODE %in% c("05C00", "05L00", "05Y00", "06A00") ~ "D2P2L",
    PCO_CODE %in% c("05A00", "05R00", "05H00") ~ "B2M3M",
    PCO_CODE %in% c("03A00", "03J00") ~ "X2C4Y",
    TRUE ~ SUB_ICB_CODE
  ))

# Just use SUB_ICB_CODE

# Keep list of CCG with 12 months of data and I will deal with merged one to keep same as the latest merged CCGs

antibiotic_df_2122 <- antibiotic_df %>%
  filter(YEAR_MONTH %in% c(
    202105, 202106, 202107, 202108, 202109, 202110, 202111,
    202112, 202201, 202202, 202203, 202204
  ))


antibiotic_df_2122 <- antibiotic_df_2122 %>%
  mutate(SUB_ICB_CODE = gsub("0*$", "", PCO_CODE)) # keep 106 SUB_ICB which will join and keep only for these CCGs

rm(antibiotic_df) # remove as it is too big


# add lookup to have SUB ICB
# Main selection is based on SUB_ICB
antibiotic_df_2021 <- antibiotic_df_2021 %>%
  inner_join(
    y = icb_lookup,
    by = "SUB_ICB_CODE"
  )

antibiotic_df_2122 <- antibiotic_df_2122 %>%
  inner_join(
    y = icb_lookup,
    by = "SUB_ICB_CODE"
  )

antibiotic_final_df <- bind_rows(antibiotic_df_2021, antibiotic_df_2122)


# usethis::use_data(antibiotic_final_df, overwrite = TRUE)


# Now, I need to extract drugs of interest prior to calculate STAR_PU for below list


# antibiotic_final_df <- antibiotic_final_df %>%
#   filter(BNF_CHEMICAL_SUBSTANCE %in% c("0501013B0","0501015P0","0501130R0",
#                                        "0501080W0","0501070AE","0501130S0",
#                                        "0501013K0","0501021A0","0501021B0",
#                                        "0501021C0","0501021D0","0501021G0",
#                                        "0501021H0","0501021J0","0501021K0",
#                                        "0501021L0","0501021M0","0501120L0",
#                                        "0501120P0","0501120Q0","0501120X0",
#                                        "0501120Y0"))

antibiotic_final_df <- antibiotic_final_df %>%
  mutate(DRUG_OF_INTEREST = case_when(
    BNF_CHEMICAL_SUBSTANCE == "0501013B0" ~ "Amoxicillin",
    BNF_CHEMICAL_SUBSTANCE %in% c(
      "0501015P0", "0501130R0",
      "0501080W0", "0501070AE", "0501130S0"
    ) ~ "UTI",
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


# Prior to calculate STAR-PU per practice, I need to aggregate number of items per month per practice
antibiotic_practice <- antibiotic_final_df %>%
  group_by(
    YEAR_MONTH, SUB_ICB_CODE, SUB_ICB_NAME, PRACTICE_NAME,
    PRACTICE_CODE, DRUG_OF_INTEREST
  ) %>%
  summarise(TOTAL_ITEMS = sum(ITEMS, na.rm = TRUE)) %>%
  ungroup() # %>%
# exclude if practice code is empty
# filter(PRACTICE_NAME != "UNIDENTIFIED DOCTORS")


# Amoxicillin (0501013B0)
## UTI related
# Pivmecillinam hydrochloride (0501015P0)
# Nitrofurantoin (0501130R0)
# Trimethoprim (0501080W0)
# Fosfomycin trometamol (0501070AE)
# Fosfomycin calcium (0501130S0)
## 3Cs
# Co-amoxiclav (Amoxicillin/clavulanic acid) (0501013K0)
# Cephalosporins (BNF CODE 0501021 list)
# 13              0501021A0                                      Cefaclor
# 14              0501021B0                                    Cefadroxil
# 15              0501021C0                                      Cefixime
# 16              0501021D0                             Cefotaxime sodium
# 17              0501021G0                            Ceftriaxone sodium
# 18              0501021H0                      Ceftazidime pentahydrate
# 19              0501021J0                             Cefuroxime sodium
# 20              0501021K0                             Cefuroxime axetil
# 21              0501021L0                                     Cefalexin
# 22              0501021M0                                     Cefradine
# Quinolones (BNF Code 050112)
# 74              0501120L0                                 Ciprofloxacin
# 75              0501120P0                                     Ofloxacin
# 76              0501120Q0                                   Norfloxacin
# 77              0501120X0                                  Levofloxacin
# 78              0501120Y0                                  Moxifloxacin


# Create STAR_PU table
# 1_practice_df.R has STARPU denominator
# join and retain denominator for each practice by year_month

# join with denominator
# antibiotic_practice %>%
#   dplyr::filter(PRACTICE_CODE == "B84016") %>%
#   filter(YEAR_MONTH > 202004 & YEAR_MONTH <= 202104) %>%
#   summarise(sum(TOTAL_ITEMS
#                 ))

# I need to get 12 months to value to April 2021 (Values for 202005 - 202104 and 202105 - 202204)

antibiotic_practice_final <-
  antibiotic_practice %>%
  dplyr::mutate("YEAR_MONTH" = case_when(
    YEAR_MONTH >= 202005 & YEAR_MONTH <= 202104 ~ "Apr-21",
    YEAR_MONTH >= 202105 & YEAR_MONTH <= 202204 ~ "Apr-22"
  )) %>%
  group_by(YEAR_MONTH, SUB_ICB_CODE, SUB_ICB_NAME, PRACTICE_CODE, DRUG_OF_INTEREST) %>%
  summarise(TOTAL_ITEMS = sum(TOTAL_ITEMS, na.rm = TRUE)) %>%
  ungroup() %>%
  inner_join(
    gp_star_pu_denom,
    by = c("YEAR_MONTH", "PRACTICE_CODE")
  ) %>%
  mutate(STAR_PU = TOTAL_ITEMS / STARPU_DENOM) %>%
  mutate(STAR_PU_100 = TOTAL_ITEMS / STARPU_DENOM * 100) # not sure....



antibiotic_practice_final_pivot_wider <- antibiotic_practice_final %>%
  tidyr::pivot_wider(
    id_cols = SUB_ICB_CODE:DRUG_OF_INTEREST,
    names_from = c(YEAR_MONTH),
    values_from = STAR_PU
  )

antibiotic_practice_final_pivot_wider <- antibiotic_practice_final_pivot_wider %>%
  mutate(CHANGE = `Apr-22` - `Apr-21`) %>%
  mutate(CHANGE_DIRECTION = case_when(
    CHANGE > 0 ~ "STAR-PU increased compared to 12 months to April 2021 value",
    CHANGE < 0 ~ "STAR-PU decreased compared to 12 months to April 2021 value"
  )) %>%
  select(GEOGRAPHY = PRACTICE_CODE, DRUG_OF_INTEREST, CHANGE_DIRECTION)






# continue with this: add up down element (for tooltip)
# count practices
# data for dumbbell chart

# also process for icb.



antibiotic_icb_final <-
  antibiotic_practice %>%
  dplyr::mutate("YEAR_MONTH" = case_when(
    YEAR_MONTH >= 202005 & YEAR_MONTH <= 202104 ~ "Apr-21",
    YEAR_MONTH >= 202105 & YEAR_MONTH <= 202204 ~ "Apr-22"
  )) %>%
  group_by(YEAR_MONTH, SUB_ICB_CODE, SUB_ICB_NAME, DRUG_OF_INTEREST) %>%
  summarise(TOTAL_ITEMS = sum(TOTAL_ITEMS, na.rm = TRUE)) %>%
  ungroup() %>%
  inner_join(
    sub_icb_df %>%
      distinct(YEAR_MONTH, SUB_ICB_CODE, STARPU_DENOM),
    by = c("YEAR_MONTH", "SUB_ICB_CODE")
  ) %>%
  mutate(STAR_PU = TOTAL_ITEMS / STARPU_DENOM)



antibiotic_icb_final_pivot_wider <- antibiotic_icb_final %>%
  tidyr::pivot_wider(
    id_cols = SUB_ICB_CODE:DRUG_OF_INTEREST,
    names_from = c(YEAR_MONTH),
    values_from = STAR_PU
  )

antibiotic_icb_final_pivot_wider <- antibiotic_icb_final_pivot_wider %>%
  mutate(CHANGE = `Apr-22` - `Apr-21`) %>%
  mutate(CHANGE_DIRECTION = case_when(
    CHANGE > 0 ~ "STAR-PU increased compared to 12 months to April 2021 value",
    CHANGE < 0 ~ "STAR-PU decreased compared to 12 months to April 2021 value"
  )) %>%
  select(GEOGRAPHY = SUB_ICB_CODE, DRUG_OF_INTEREST, CHANGE_DIRECTION)




## Only for checking
tst_icb <- sub_icb_df %>%
  filter(YEAR_MONTH %in% c("Apr-21", "Apr-22")) %>%
  distinct(YEAR_MONTH, SUB_ICB_CODE, STARPU_NUM, STARPU_DENOM)

check <- tst %>% left_join(tst_icb,
  by = c("YEAR_MONTH", "SUB_ICB_CODE")
)

##### After putting into the chart. need to check this.
##### I need to manually check to get drug count is correct (when I report practice. ccg. england level )
check_with_open_prescribing <-
  check %>%
  filter(ODP_TOTALS != STARPU_NUM) %>%
  mutate(p1 = ODP_TOTALS / STARPU_DENOM, p2 = STARPU_NUM / STARPU_DENOM)

# GP items breakdown (a, uti, 3cs )
usethis::use_data(antibiotic_practice_final, overwrite = TRUE)
usethis::use_data(antibiotic_icb_final, overwrite = TRUE)
usethis::use_data(antibiotic_practice_final_pivot_wider, overwrite = TRUE)
usethis::use_data(antibiotic_icb_final_pivot_wider, overwrite = TRUE)
