# practice_metric_df

library(dplyr)
library(readr)

# Apr 22
gp_apr_df <- list.files(
  path = "./data-raw/epact2/apr_22",
  pattern = "*.csv",
  full.names = TRUE
) %>%
  lapply(read_csv) %>%
  bind_rows() %>%
  janitor::clean_names() %>%
  select(
    SUB_ICB = commissioner_provider_plus_code,
    PRACTICE = practice_plus_code,
    ICB = icb,
    STARPU_NUM = antibacterial_items_bnf_5_1,
    STARPU_DENOM = oral_antibacterial_item_based_star_p_us,
    STAR_PU = antibacterial_items_per_star_pu,
    COAMOX = percent_co_amoxiclav_cephalosporins_quinolones_items
  ) %>%
  mutate(YEAR_MONTH = "Apr_22")


# Mar 22
gp_mar_df <- list.files(
  path = "./data-raw/epact2/mar_22",
  pattern = "*.csv",
  full.names = TRUE
) %>%
  lapply(read_csv) %>%
  bind_rows() %>%
  janitor::clean_names() %>%
  select(
    SUB_ICB = commissioner_provider_plus_code,
    PRACTICE = practice_plus_code,
    ICB = icb,
    STARPU_NUM = antibacterial_items_bnf_5_1,
    STARPU_DENOM = oral_antibacterial_item_based_star_p_us,
    STAR_PU = antibacterial_items_per_star_pu,
    COAMOX = percent_co_amoxiclav_cephalosporins_quinolones_items
  ) %>%
  mutate(YEAR_MONTH = "Mar_22")


# Feb 22
gp_feb_df <- list.files(
  path = "./data-raw/epact2/feb_22",
  pattern = "*.csv",
  full.names = TRUE
) %>%
  lapply(read_csv) %>%
  bind_rows() %>%
  janitor::clean_names() %>%
  select(
    SUB_ICB = commissioner_provider_plus_code,
    PRACTICE = practice_plus_code,
    ICB = icb,
    STARPU_NUM = antibacterial_items_bnf_5_1,
    STARPU_DENOM = oral_antibacterial_item_based_star_p_us,
    STAR_PU = antibacterial_items_per_star_pu,
    COAMOX = percent_co_amoxiclav_cephalosporins_quinolones_items
  ) %>%
  mutate(YEAR_MONTH = "Feb_22")


# Jan 22
gp_jan_df <- list.files(
  path = "./data-raw/epact2/jan_22",
  pattern = "*.csv",
  full.names = TRUE
) %>%
  lapply(read_csv) %>%
  bind_rows() %>%
  janitor::clean_names() %>%
  select(
    SUB_ICB = commissioner_provider_plus_code,
    PRACTICE = practice_plus_code,
    ICB = icb,
    STARPU_NUM = antibacterial_items_bnf_5_1,
    STARPU_DENOM = oral_antibacterial_item_based_star_p_us,
    STAR_PU = antibacterial_items_per_star_pu,
    COAMOX = percent_co_amoxiclav_cephalosporins_quinolones_items
  ) %>%
  mutate(YEAR_MONTH = "Jan_22")



# Dec 21

gp_dec_df <- list.files(
  path = "./data-raw/epact2/dec_21",
  pattern = "*.csv",
  full.names = TRUE
) %>%
  lapply(read_csv) %>%
  bind_rows() %>%
  janitor::clean_names() %>%
  select(
    SUB_ICB = commissioner_provider_plus_code,
    PRACTICE = practice_plus_code,
    ICB = icb,
    STARPU_NUM = antibacterial_items_bnf_5_1,
    STARPU_DENOM = oral_antibacterial_item_based_star_p_us,
    STAR_PU = antibacterial_items_per_star_pu,
    COAMOX = percent_co_amoxiclav_cephalosporins_quinolones_items
  ) %>%
  mutate(YEAR_MONTH = "Dec_21")

# Nov 21

gp_nov_df <- list.files(
  path = "./data-raw/epact2/nov_21",
  pattern = "*.csv",
  full.names = TRUE
) %>%
  lapply(read_csv) %>%
  bind_rows() %>%
  janitor::clean_names() %>%
  select(
    SUB_ICB = commissioner_provider_plus_code,
    PRACTICE = practice_plus_code,
    ICB = icb,
    STARPU_NUM = antibacterial_items_bnf_5_1,
    STARPU_DENOM = oral_antibacterial_item_based_star_p_us,
    STAR_PU = antibacterial_items_per_star_pu,
    COAMOX = percent_co_amoxiclav_cephalosporins_quinolones_items
  ) %>%
  mutate(YEAR_MONTH = "Nov_21")

# Oct 21
gp_oct_df <- list.files(
  path = "./data-raw/epact2/oct_21",
  pattern = "*.csv",
  full.names = TRUE
) %>%
  lapply(read_csv) %>%
  bind_rows() %>%
  janitor::clean_names() %>%
  select(
    SUB_ICB = commissioner_provider_plus_code,
    PRACTICE = practice_plus_code,
    ICB = icb,
    STARPU_NUM = antibacterial_items_bnf_5_1,
    STARPU_DENOM = oral_antibacterial_item_based_star_p_us,
    STAR_PU = antibacterial_items_per_star_pu,
    COAMOX = percent_co_amoxiclav_cephalosporins_quinolones_items
  ) %>%
  mutate(YEAR_MONTH = "Oct_21")

# Sep 21
gp_sep_df <- list.files(
  path = "./data-raw/epact2/sep_21",
  pattern = "*.csv",
  full.names = TRUE
) %>%
  lapply(read_csv) %>%
  bind_rows() %>%
  janitor::clean_names() %>%
  select(
    SUB_ICB = commissioner_provider_plus_code,
    PRACTICE = practice_plus_code,
    ICB = icb,
    STARPU_NUM = antibacterial_items_bnf_5_1,
    STARPU_DENOM = oral_antibacterial_item_based_star_p_us,
    STAR_PU = antibacterial_items_per_star_pu,
    COAMOX = percent_co_amoxiclav_cephalosporins_quinolones_items
  ) %>%
  mutate(YEAR_MONTH = "Sep_21")


# Aug 21
gp_aug_df <- list.files(
  path = "./data-raw/epact2/aug_21",
  pattern = "*.csv",
  full.names = TRUE
) %>%
  lapply(read_csv) %>%
  bind_rows() %>%
  janitor::clean_names() %>%
  select(
    SUB_ICB = commissioner_provider_plus_code,
    PRACTICE = practice_plus_code,
    ICB = icb,
    STARPU_NUM = antibacterial_items_bnf_5_1,
    STARPU_DENOM = oral_antibacterial_item_based_star_p_us,
    STAR_PU = antibacterial_items_per_star_pu,
    COAMOX = percent_co_amoxiclav_cephalosporins_quinolones_items
  ) %>%
  mutate(YEAR_MONTH = "Aug_21")



# Jul 21
gp_jul_df <- list.files(
  path = "./data-raw/epact2/jul_21",
  pattern = "*.csv",
  full.names = TRUE
) %>%
  lapply(read_csv) %>%
  bind_rows() %>%
  janitor::clean_names() %>%
  select(
    SUB_ICB = commissioner_provider_plus_code,
    PRACTICE = practice_plus_code,
    ICB = icb,
    STARPU_NUM = antibacterial_items_bnf_5_1,
    STARPU_DENOM = oral_antibacterial_item_based_star_p_us,
    STAR_PU = antibacterial_items_per_star_pu,
    COAMOX = percent_co_amoxiclav_cephalosporins_quinolones_items
  ) %>%
  mutate(YEAR_MONTH = "Jul_21")

# Jun 21
gp_jun_df <- list.files(
  path = "./data-raw/epact2/jun_21",
  pattern = "*.csv",
  full.names = TRUE
) %>%
  lapply(read_csv) %>%
  bind_rows() %>%
  janitor::clean_names() %>%
  select(
    SUB_ICB = commissioner_provider_plus_code,
    PRACTICE = practice_plus_code,
    ICB = icb,
    STARPU_NUM = antibacterial_items_bnf_5_1,
    STARPU_DENOM = oral_antibacterial_item_based_star_p_us,
    STAR_PU = antibacterial_items_per_star_pu,
    COAMOX = percent_co_amoxiclav_cephalosporins_quinolones_items
  ) %>%
  mutate(YEAR_MONTH = "Jun_21")


# May 21
gp_may_df <- list.files(
  path = "./data-raw/epact2/may_21",
  pattern = "*.csv",
  full.names = TRUE
) %>%
  lapply(read_csv) %>%
  bind_rows() %>%
  janitor::clean_names() %>%
  select(
    SUB_ICB = commissioner_provider_plus_code,
    PRACTICE = practice_plus_code,
    ICB = icb,
    STARPU_NUM = antibacterial_items_bnf_5_1,
    STARPU_DENOM = oral_antibacterial_item_based_star_p_us,
    STAR_PU = antibacterial_items_per_star_pu,
    COAMOX = percent_co_amoxiclav_cephalosporins_quinolones_items
  ) %>%
  mutate(YEAR_MONTH = "May_21")


# April 21
gp_apr_21_df <- list.files(
  path = "./data-raw/epact2/apr_21",
  pattern = "*.csv",
  full.names = TRUE
) %>%
  lapply(read_csv) %>%
  bind_rows() %>%
  janitor::clean_names() %>%
  select(
    SUB_ICB = commissioner_provider_plus_code,
    PRACTICE = practice_plus_code,
    ICB = icb,
    STARPU_NUM = antibacterial_items_bnf_5_1,
    STARPU_DENOM = oral_antibacterial_item_based_star_p_us,
    STAR_PU = antibacterial_items_per_star_pu,
    COAMOX = percent_co_amoxiclav_cephalosporins_quinolones_items
  ) %>%
  mutate(YEAR_MONTH = "Apr_21")

# Merge all

gp_merge_df <- bind_rows(gp_apr_21_df, gp_may_df, gp_jun_df, gp_jul_df, gp_aug_df, gp_sep_df, gp_oct_df, gp_nov_df, gp_dec_df, gp_jan_df, gp_feb_df, gp_mar_df, gp_apr_df)


# reloacte and pivot longer
gp_merge_df <- gp_merge_df %>%
  relocate(YEAR_MONTH, .before = SUB_ICB) %>%
  tidyr::pivot_longer(
    cols = !c(YEAR_MONTH:STARPU_DENOM),
    names_to = "METRIC",
    values_to = "VALUE"
  )

# change YEAR_MONTH to May-21 etc

gp_merge_df <- gp_merge_df %>%
  mutate(YEAR_MONTH = gsub("_", "-", YEAR_MONTH))

# remove closed GP practices and extract GP code for attaching IMD

gp_merge_df <- gp_merge_df %>%
  dplyr::filter(!grepl(" C 01-", PRACTICE)) %>%
  dplyr::filter(!grepl(" D 0", PRACTICE)) %>%
  dplyr::filter(!grepl(" D 1", PRACTICE)) %>%
  dplyr::filter(!grepl(" D 2", PRACTICE)) %>%
  dplyr::filter(!grepl(" D 3", PRACTICE))

gp_merge_df$PRACTICE_CODE_EXT <- sapply(stringr::str_extract_all(
  gp_merge_df$PRACTICE,
  "(?<=\\()[^)(]+(?=\\))"
),
paste0,
collapse = ","
)

# cleaning to get proper GP code

gp_merge_df <- gp_merge_df %>%
  tidyr::separate(PRACTICE_CODE_EXT, c("PRACTICE_CODE1", "PRACTICE_CODE2"), sep = ",") %>%
  mutate(PRACTICE_CODE = case_when(
    !is.na(PRACTICE_CODE1) & is.na(PRACTICE_CODE2) ~ PRACTICE_CODE1,
    !is.na(PRACTICE_CODE2) ~ PRACTICE_CODE2
  )) %>%
  select(-c(PRACTICE_CODE1, PRACTICE_CODE2))




# read GP lookup contains IMD rank/decile
gp_lookup <- readr::read_csv("./data-raw/gp_list.csv")

gp_merge_df <- gp_merge_df %>%
  left_join(
    y = gp_lookup,
    by = "PRACTICE_CODE"
  )

# Better naming by adding ICB lookup
icb_lookup <- readr::read_csv("./data-raw/icb_lookup.csv")

# extract code from gp_merge_df

gp_merge_df <- gp_merge_df %>%
  mutate(
    SUB_ICB_CODE = sub(" *\\(.*", "", SUB_ICB),
    SUB_ICB_CODE = trimws(stringr::str_replace(SUB_ICB_CODE, ".+-(.+)", "\\1"))
  )


# join with icb_lookup to keep the better name
gp_merge_df <- gp_merge_df %>%
  left_join(
    y = icb_lookup,
    by = "SUB_ICB_CODE"
  )

# GP practice name bit better?

gp_merge_df <- gp_merge_df %>%
  mutate(PRACTICE_NAME = stringr::str_to_title(PRACTICE)) %>%
  filter(!stringr::str_detect(PRACTICE, "WALK-IN"))

# Keep denominator for april 2022, april 2021.
gp_star_pu_denom <- gp_merge_df %>%
  filter(YEAR_MONTH %in% c("Apr-21", "Apr-22")) %>%
  distinct(YEAR_MONTH, PRACTICE_CODE, PRACTICE_NAME, STARPU_NUM, STARPU_DENOM)

# remove denom, numerator make the data bit smaller.
gp_merge_df <- gp_merge_df %>%
  select(-c(STARPU_DENOM, STARPU_NUM, SUB_ICB_CODE, SUB_ICB))




usethis::use_data(gp_merge_df, overwrite = TRUE)
usethis::use_data(gp_star_pu_denom, overwrite = TRUE)
usethis::use_data(icb_lookup, overwrite = TRUE)
