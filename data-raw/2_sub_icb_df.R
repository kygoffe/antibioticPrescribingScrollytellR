# Read CCG 12 months data from epact2

library(dplyr)
library(purrr)
library(stringr)

sub_icb_df <- list.files(
  path = "./data-raw/epact2/sub_icb_ccg",
  pattern = "\\.csv$",
  full.names = TRUE
) %>%
  set_names() %>%
  map_dfr(read_csv, .id = "file_name") %>%
  mutate(file_name = basename(file_name)) %>%
  mutate(file_name = sub(".csv", "", file_name)) %>%
  janitor::clean_names() %>%
  select(
    YEAR_MONTH = file_name,
    SUB_ICB = sicbl,
    STAR_PU = antibacterial_items_per_star_pu,
    COAMOX = percent_co_amoxiclav_cephalosporins_quinolones_items,
    ICB = icb,
    REGION = region
  ) %>%
  filter(!str_detect(SUB_ICB, "(C 01-Apr-21)") & !str_detect(SUB_ICB, "(C 01-Apr-20)")) %>%
  relocate(ICB, .before = SUB_ICB) %>%
  relocate(REGION, .before = ICB) %>%
  tidyr::pivot_longer(
    cols = !c(YEAR_MONTH:SUB_ICB),
    names_to = "METRIC",
    values_to = "VALUE"
  ) %>%
  mutate(YEAR_MONTH = gsub("_", "-", YEAR_MONTH))

usethis::use_data(sub_icb_df, overwrite = TRUE)
