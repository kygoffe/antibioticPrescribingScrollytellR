# ODP does not have information about the age band so had to go back to ePACT2 to extract BNF 5.1 chapter by 7 regions and also age_band
# Save as csv file
library(dplyr)
devtools::load_all()

nhs_region <- readr::read_csv("./data-raw/BNF5_1_by_region.csv") %>%
  janitor::clean_names() %>%
  mutate(age_band = case_when(
    x10_years_age_band == "80-89" ~ "80+",
    x10_years_age_band == "90-99" ~ "80+",
    x10_years_age_band == "100-109" ~ "80+",
    x10_years_age_band == "110+" ~ "80+",
    TRUE ~ x10_years_age_band
  )) %>%
  select(
    YEAR_MONTH = year_month,
    REGION = hs_region,
    AGE_BAND = age_band,
    ITEMS = items
  ) %>%
  group_by(
    YEAR_MONTH,
    REGION,
    AGE_BAND
  ) %>%
  summarise(ITEMS = sum(ITEMS)) %>%
  ungroup()

# # need to get for all age group
# nhs_region_all <- nhs_region %>%
#   group_by(YEAR_MONTH,REGION) %>%
#   summarise(ITEMS = sum(ITEMS)) %>%
#   ungroup() %>%
#   mutate(AGE_BAND = 'All')
#
# # merge them
# nhs_region <- bind_rows(nhs_region, nhs_region_all)






# save to data folder
usethis::use_data(nhs_region, overwrite = TRUE)
