library(dplyr)

# CCG boundary


ccg_map <- sf::read_sf("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Clinical_Commissioning_Groups_April_2021_EN_BUC/FeatureServer/0/query?where=1%3D1&outFields=CCG21CD,CCG21NM&outSR=27700&f=json") |>
  janitor::clean_names() |>
  select(CCG_NAME = ccg21nm, geometry) |>
  mutate(
    CCG_NAME = gsub("NHS", "", CCG_NAME),
    CCG_NAME = gsub("CCG", "", CCG_NAME),
    CCG_NAME = stringr::str_to_title(CCG_NAME),
    CCG_NAME = gsub("And", "and", CCG_NAME),
    CCG_NAME = gsub("Of", "of", CCG_NAME),
    CCG_NAME = gsub("With", "with", CCG_NAME),
    CCG_NAME = stringr::str_trim(CCG_NAME)
  )


usethis::use_data(ccg_map, overwrite = TRUE)
