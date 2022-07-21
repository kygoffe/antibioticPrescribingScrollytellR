# Read CCG 12 months data from epact2

library(dplyr)
library(purrr)
library(stringr)
library(readr)

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
    STARPU_NUM = antibacterial_items_bnf_5_1,
    STARPU_DENOM = oral_antibacterial_item_based_star_p_us,
    STAR_PU = antibacterial_items_per_star_pu,
    COAMOX = percent_co_amoxiclav_cephalosporins_quinolones_items,
    ICB = icb,
    REGION = region
  ) %>%
  filter(!str_detect(SUB_ICB, "(C 01-Apr-21)") & !str_detect(SUB_ICB, "(C 01-Apr-20)")) %>%
  relocate(ICB, .before = SUB_ICB) %>%
  relocate(REGION, .before = ICB) %>%
  tidyr::pivot_longer(
    cols = !c(YEAR_MONTH:STARPU_DENOM),
    names_to = "METRIC",
    values_to = "VALUE"
  ) %>%
  mutate(YEAR_MONTH = gsub("_", "-", YEAR_MONTH))

x <- c("Feb-21", "Mar-21", "Apr-21", "May-21", "Jun-21", "Jul-21", "Aug-21", "Sep-21", "Oct-21", "Nov-21", "Dec-21", "Jan-22", "Feb-22", "Mar-22", "Apr-22")

sub_icb_df <- sub_icb_df %>%
  mutate(YEAR_MONTH = factor(YEAR_MONTH, levels = x)) %>%
  arrange(YEAR_MONTH) %>%
  mutate(YEAR_MONTH = as.character(YEAR_MONTH))


sub_icb_df <- sub_icb_df %>%
  mutate(
    SUB_ICB_CODE = sub(" *\\(.*", "", SUB_ICB),
    SUB_ICB_CODE = trimws(stringr::str_replace(SUB_ICB_CODE, ".+-(.+)", "\\1"))
  )


# join with icb_lookup to keep the better name
sub_icb_df <- sub_icb_df %>%
  left_join(
    y = icb_lookup,
    by = "SUB_ICB_CODE"
  ) %>%
  mutate(MEET_TARGET = case_when(
    METRIC == "STAR_PU" & VALUE <= 0.871 ~ 1,
    METRIC == "COAMOX" & VALUE <= 10 ~ 1,
    TRUE ~ 0 # not meet
  )) %>%
  mutate(
    colour = case_when(
      METRIC == "STAR_PU" & VALUE <= 0.871 ~ "#330072", # met target
      METRIC == "STAR_PU" & VALUE > 0.871 ~ "#41B6E6", # met target
      METRIC == "COAMOX" & VALUE <= 10 ~ "#AE2573", # met target
      METRIC == "COAMOX" & VALUE > 10 ~ "#0072CE", # met target
    )
  ) %>%
  mutate(
    REGION = stringr::str_to_title(REGION),
    REGION = gsub("Of", "of", REGION),
    REGION = gsub("And", "and", REGION)
  )


usethis::use_data(sub_icb_df, overwrite = TRUE)
