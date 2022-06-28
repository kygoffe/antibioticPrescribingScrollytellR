library(dplyr)
# Read STP and pivot longer

stp <- read.csv("./data-raw/stp_csv.csv", check.names = FALSE)

stp <- stp |>
  tidyr::pivot_longer(
    cols = !c(REGION:STP_CODE),
    names_to = c("METRIC", "YEAR_MONTH"),
    names_sep = "-",
    values_to = "VALUE"
  )

# STP_NAME tidy up
# Remove CCG
# Upper lower
# & change to and

stp <- stp |>
  mutate(
    REGION = stringr::str_to_title(REGION),
    REGION = gsub("And", "and", REGION),
    REGION = gsub("Of", "of", REGION)
  ) %>%
  mutate(
    STP_NAME = trimws(STP_NAME)
  ) %>%
  mutate(
    SUB_GEOGRAPHY_NAME = STP_NAME,
    SUB_GEOGRAPHY_TYPE = "STP"
  ) %>%
  select(REGION, SUB_GEOGRAPHY_NAME, SUB_GEOGRAPHY_TYPE, YEAR_MONTH, METRIC, VALUE)


# Merge ccg and stp (I have put this one inside the mod but too slow render so moved here)
merge_df <- dplyr::bind_rows(
  antibioticPrescribingScrollytellR::ccg,
  antibioticPrescribingScrollytellR::stp
) %>%
  mutate(MEET_TARGET = case_when(
    METRIC == "STARPU" & VALUE <= 0.871 ~ 1,
    METRIC == "COAMOX" & VALUE <= 10 ~ 1,
    METRIC == "ITEMS" ~ 99, # not relevant
    TRUE ~ 0 # not meet
  ))



usethis::use_data(stp, overwrite = TRUE)
usethis::use_data(merge_df, overwrite = TRUE)
