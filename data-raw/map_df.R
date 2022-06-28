library(dplyr)

# NHS region map
# region_map <- sf::read_sf("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/NHS_England_Regions_APR_2021_EN_BGC/FeatureServer/0/query?where=1%3D1&outFields=NHSER21CD,NHSER21NM&outSR=27700&f=json") %>%
#   select(-NHSER21CD) %>%
#   rename(REGION = NHSER21NM)

# STP
map_df <- bind_rows(
  sf::read_sf("https://ons-inspire.esriuk.com/arcgis/rest/services/Health_Boundaries/Sustainability_and_Transformation_Partnerships_April_2020_Boundaries_EN_BUC/MapServer/0/query?where=1%3D1&outFields=stp20cd,stp20nm&outSR=27700&f=json") %>%
    select(-stp20cd) %>%
    mutate(GEOGRAPHY = "STP") %>%
    select(
      GEOGRAPHY,
      SUB_GEOGRAPHY_NAME = stp20nm,
      GEOMETRY = geometry
    ) %>%
    left_join(
      region_stp_ccg_lookup %>% distinct(REGION, STP_NAME),
      by = c("SUB_GEOGRAPHY_NAME" = "STP_NAME")
    ),

  # CCG
  sf::read_sf("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Clinical_Commissioning_Groups_April_2021_EN_BUC/FeatureServer/0/query?where=1%3D1&outFields=CCG21CD,CCG21NM&outSR=27700&f=json") %>%
    mutate(
      CCG21NM = gsub("NHS", "", CCG21NM),
      CCG21NM = gsub("CCG", "", CCG21NM),
      CCG21NM = trimws(CCG21NM)
    ) %>%
    mutate(GEOGRAPHY = "CCG") %>%
    select(
      GEOGRAPHY,
      SUB_GEOGRAPHY_NAME = CCG21NM,
      GEOMETRY = geometry
    ) %>%
    left_join(
      region_stp_ccg_lookup %>% distinct(REGION, CCG_NAME),
      by = c("SUB_GEOGRAPHY_NAME" = "CCG_NAME")
    )
)



# Add to data-raw/
# usethis::use_data(stp_map, overwrite = TRUE)
# usethis::use_data(ccg_map, overwrite = TRUE)


usethis::use_data(map_df, overwrite = TRUE)
