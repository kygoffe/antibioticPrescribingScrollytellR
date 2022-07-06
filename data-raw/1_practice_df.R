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
    STAR_PU = antibacterial_items_per_star_pu,
    COAMOX = percent_co_amoxiclav_cephalosporins_quinolones_items
  ) %>%
  mutate(YEAR_MONTH = "May_21")

# Merge all

gp_merge_df <- bind_rows(gp_may_df, gp_jun_df, gp_jul_df, gp_aug_df, gp_sep_df, gp_oct_df, gp_nov_df, gp_dec_df, gp_jan_df, gp_feb_df, gp_mar_df, gp_apr_df)


# reloacte and pivot longer
gp_merge_df <- gp_merge_df %>%
  relocate(YEAR_MONTH, .before = SUB_ICB) %>%
  tidyr::pivot_longer(
    cols = !c(YEAR_MONTH:ICB),
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


usethis::use_data(gp_merge_df, overwrite = TRUE)
