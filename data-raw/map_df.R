library(dplyr)

# NHS region map
# region_map <- sf::read_sf("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/NHS_England_Regions_APR_2021_EN_BGC/FeatureServer/0/query?where=1%3D1&outFields=NHSER21CD,NHSER21NM&outSR=27700&f=json") %>%
#   select(-NHSER21CD) %>%
#   rename(REGION = NHSER21NM)

# Need to extract new SICBL name

ccg_name <- readr::read_csv("./data-raw/ccg_name.csv") %>%
  janitor::clean_names() %>%
  mutate(
    CCG_NAME = gsub("NHS", "", ccg_name),
    CCG_NAME = gsub("CCG", "", CCG_NAME),
    CCG_NAME = stringr::str_to_title(CCG_NAME),
    CCG_NAME = gsub("And", "and", CCG_NAME),
    CCG_NAME = gsub("&", "and", CCG_NAME),
    CCG_NAME = gsub("Of", "of", CCG_NAME),
    CCG_NAME = gsub("On", "on", CCG_NAME),
    CCG_NAME = gsub("With", "with", CCG_NAME),
    CCG_NAME = stringr::str_trim(CCG_NAME)
  ) %>%
  rename(CCG_CODE = ccg_code)

ccg_region_lookup <- readr::read_csv("./data-raw/sicbl_region_lookup.csv") %>%
  mutate(
    REGION = gsub(" COMMISSIONING REGION", "", REGION),
    REGION = stringr::str_to_title(REGION),
    REGION = gsub("Of", "of", REGION),
    REGION = gsub("And", "and", REGION)
  )


map_df <- sf::read_sf("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Clinical_Commissioning_Groups_April_2021_EN_BUC/FeatureServer/0/query?where=1%3D1&outFields=CCG21CD,CCG21NM&outSR=27700&f=json") %>%
  mutate(
    CCG21NM = gsub("NHS", "", CCG21NM),
    CCG21NM = gsub("CCG", "", CCG21NM),
    CCG21NM = trimws(CCG21NM)
  ) %>%
  mutate(GEOGRAPHY = "SUB_ICB") %>%
  select(
    GEOGRAPHY,
    SUB_GEOGRAPHY_NAME = CCG21NM,
    GEOMETRY = geometry
  ) %>%
  left_join(
    ccg_name %>% distinct(CCG_NAME, CCG_CODE),
    by = c("SUB_GEOGRAPHY_NAME" = "CCG_NAME")
  ) %>%
  left_join(
    icb_lookup,
    by = c("CCG_CODE" = "SUB_ICB_CODE")
  ) %>%
  left_join(
    ccg_region_lookup,
    by = c("CCG_CODE" = "SUB_ICB_CODE")
  ) %>%
  rename(SUB_ICB_CODE = CCG_CODE)


usethis::use_data(map_df, overwrite = TRUE)
