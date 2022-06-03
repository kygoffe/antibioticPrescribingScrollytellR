# Read CCG and pivot longer

ccg <- read.csv("./data-raw/ccg.csv", check.names = FALSE)

ccg <- ccg |>
  tidyr::pivot_longer(
    cols = !c(REGION:CCG_CODE),
    names_to = c("METRIC", "YEAR_MONTH"),
    names_sep = "-",
    values_to = "VALUE"
  )

# CCG_NAME tidy up
# Remove CCG
# Upper lower
# & change to and

ccg <- ccg |>
  mutate(
    CCG_NAME = gsub("CCG", "", CCG_NAME),
    CCG_NAME = stringr::str_to_title(CCG_NAME),
    CCG_NAME = gsub("And", "and", CCG_NAME),
    CCG_NAME = gsub("&", "and", CCG_NAME),
    CCG_NAME = gsub("Of", "of", CCG_NAME),
    CCG_NAME = gsub("With", "with", CCG_NAME),
    CCG_NAME = stringr::str_trim(CCG_NAME)
  ) |>
  mutate(CCG_NAME = case_when(
    CCG_NAME == "Banes Swindon and Wiltshire" ~ "Bath and North East Somerset, Swindon and Wiltshire",
    CCG_NAME == "Bristol, North Somerset and S Glos" ~ "Bristol, North Somerset and South Gloucestershire",
    CCG_NAME == "Hampshire,Southampton andIsle of Wight" ~ "Hampshire, Southampton and Isle of Wight",
    CCG_NAME == "Se Staffs and Seisdon Peninsular" ~ "South East Staffordshire and Seisdon Peninsula",
    TRUE ~ CCG_NAME
  )) |>
  mutate(
    REGION = stringr::str_to_title(REGION),
    REGION = gsub("And", "and", REGION),
    REGION = gsub("Of", "of", REGION)
  )


usethis::use_data(ccg, overwrite = TRUE)
