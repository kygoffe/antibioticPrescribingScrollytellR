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
  )


usethis::use_data(stp, overwrite = TRUE)
