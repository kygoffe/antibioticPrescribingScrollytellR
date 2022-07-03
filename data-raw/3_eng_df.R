# England

library(dplyr)

df_eng_pivot <- read_csv("./data-raw/epact2/Data National Initiatives - National.csv") %>%
  janitor::clean_names() %>%
  select(
    YEAR_MONTH = month,
    STAR_PU = antibacterial_items_per_star_pu,
    ITEMS = antibacterial_items_bnf_5_1,
    COAMOX_ITEMS = co_amoxiclav_cephalosporins_quinolones_items_5,
    COAMOX = co_amoxiclav_cephalosporins_quinolones_items_7
  ) %>%
  tidyr::pivot_longer(
    cols = !YEAR_MONTH,
    names_to = "METRIC",
    values_to = "VALUE"
  )

df_eng <- read_csv("./data-raw/epact2/Data National Initiatives - National.csv") %>%
  janitor::clean_names() %>%
  select(
    YEAR_MONTH = month,
    STAR_PU = antibacterial_items_per_star_pu,
    ITEMS = antibacterial_items_bnf_5_1,
    COAMOX_ITEMS = co_amoxiclav_cephalosporins_quinolones_items_5,
    COAMOX = co_amoxiclav_cephalosporins_quinolones_items_7
  )



usethis::use_data(df_eng_pivot, overwrite = TRUE)
usethis::use_data(df_eng, overwrite = TRUE)
