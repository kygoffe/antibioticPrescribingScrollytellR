library(dplyr)
library(data.table)
devtools::load_all()


may_2020 <- fread("https://files.digital.nhs.uk/7F/C526F5/gp-reg-pat-prac-quin-age.csv") %>%
  filter(ORG_TYPE == "GP") %>%
  mutate(YEAR_MONTH = 202005)

june_2020 <- fread("https://files.digital.nhs.uk/48/B12E95/gp-reg-pat-prac-quin-age.csv") %>%
  filter(ORG_TYPE == "GP") %>%
  mutate(YEAR_MONTH = 202006)

july_2020 <- fread("https://files.digital.nhs.uk/7A/C3CD13/gp-reg-pat-prac-quin-age.csv") %>%
  filter(ORG_TYPE == "GP") %>%
  mutate(YEAR_MONTH = 202007)

august_2020 <- fread("https://files.digital.nhs.uk/16/3B7597/gp-reg-pat-prac-quin-age.csv") %>%
  filter(ORG_TYPE == "GP") %>%
  mutate(YEAR_MONTH = 202008)

september_2020 <- fread("https://files.digital.nhs.uk/07/D922F2/gp-reg-pat-prac-quin-age.csv") %>%
  filter(ORG_TYPE == "GP") %>%
  mutate(YEAR_MONTH = 202009)

october_2020 <- fread("https://files.digital.nhs.uk/F4/728CF9/gp-reg-pat-prac-quin-age.csv") %>%
  filter(ORG_TYPE == "GP") %>%
  mutate(YEAR_MONTH = 202010)

november_2020 <- fread("https://files.digital.nhs.uk/01/6D3875/gp-reg-pat-prac-quin-age.csv") %>%
  filter(ORG_TYPE == "GP") %>%
  mutate(YEAR_MONTH = 202011)

december_2020 <- fread("https://files.digital.nhs.uk/46/EFD4CB/gp-reg-pat-prac-quin-age.csv") %>%
  filter(ORG_TYPE == "GP") %>%
  mutate(YEAR_MONTH = 202012)

january_2021 <- fread("https://files.digital.nhs.uk/01/C1E9D5/gp-reg-pat-prac-quin-age.csv") %>%
  filter(ORG_TYPE == "GP") %>%
  mutate(YEAR_MONTH = 202101)

february_2021 <- fread("https://files.digital.nhs.uk/94/B835D4/gp-reg-pat-prac-quin-age.csv") %>%
  filter(ORG_TYPE == "GP") %>%
  mutate(YEAR_MONTH = 202102)

march_2021 <- fread("https://files.digital.nhs.uk/9B/99C8B8/gp-reg-pat-prac-quin-age.csv") %>%
  filter(ORG_TYPE == "GP") %>%
  mutate(EXTRACT_DATE = as.character(EXTRACT_DATE)) %>%
  mutate(YEAR_MONTH = 202103)

april_2021 <- fread("https://files.digital.nhs.uk/9C/54B8FC/gp-reg-pat-prac-quin-age.csv") %>%
  filter(ORG_TYPE == "GP") %>%
  mutate(EXTRACT_DATE = as.character(EXTRACT_DATE)) %>%
  mutate(YEAR_MONTH = 202104)

may_2021 <- fread("https://files.digital.nhs.uk/A2/49CF7C/gp-reg-pat-prac-quin-age.csv") %>%
  filter(ORG_TYPE == "GP") %>%
  mutate(EXTRACT_DATE = as.character(EXTRACT_DATE)) %>%
  mutate(YEAR_MONTH = 202105)

june_2021 <- fread("https://files.digital.nhs.uk/79/FC50B1/gp-reg-pat-prac-quin-age.csv") %>%
  filter(ORG_TYPE == "GP") %>%
  mutate(EXTRACT_DATE = as.character(EXTRACT_DATE)) %>%
  mutate(YEAR_MONTH = 202106)

july_2021 <- fread("https://files.digital.nhs.uk/EE/4EF23F/gp-reg-pat-prac-quin-age.csv") %>%
  filter(ORG_TYPE == "GP") %>%
  mutate(EXTRACT_DATE = as.character(EXTRACT_DATE)) %>%
  mutate(YEAR_MONTH = 202107)

august_2021 <- fread("https://files.digital.nhs.uk/2C/B12641/gp-reg-pat-prac-quin-age.csv") %>%
  filter(ORG_TYPE == "GP") %>%
  mutate(EXTRACT_DATE = as.character(EXTRACT_DATE)) %>%
  mutate(YEAR_MONTH = 202108)

september_2021 <- fread("https://files.digital.nhs.uk/31/618CE6/gp-reg-pat-prac-quin-age-v2.csv") %>%
  filter(ORG_TYPE == "GP") %>%
  mutate(EXTRACT_DATE = as.character(EXTRACT_DATE)) %>%
  mutate(YEAR_MONTH = 202109)

october_2021 <- fread("https://files.digital.nhs.uk/02/231C6E/gp-reg-pat-prac-quin-age.csv") %>%
  filter(ORG_TYPE == "GP") %>%
  mutate(EXTRACT_DATE = as.character(EXTRACT_DATE)) %>%
  mutate(YEAR_MONTH = 202110)

november_2021 <- fread("https://files.digital.nhs.uk/AF/D1DC73/gp-reg-pat-prac-quin-age.csv") %>%
  filter(ORG_TYPE == "GP") %>%
  mutate(EXTRACT_DATE = as.character(EXTRACT_DATE)) %>%
  mutate(YEAR_MONTH = 202111)

december_2021 <- fread("https://files.digital.nhs.uk/52/E55571/gp-reg-pat-prac-quin-age.csv") %>%
  filter(ORG_TYPE == "GP") %>%
  mutate(YEAR_MONTH = 202112)

january_2022 <- fread("https://files.digital.nhs.uk/61/55F8A6/gp-reg-pat-prac-quin-age.csv") %>%
  filter(ORG_TYPE == "GP") %>%
  mutate(YEAR_MONTH = 202201)

february_2022 <- fread("https://files.digital.nhs.uk/7B/5C0583/gp-reg-pat-prac-quin-age.csv") %>%
  filter(ORG_TYPE == "GP") %>%
  mutate(YEAR_MONTH = 202202)

march_2022 <- fread("https://files.digital.nhs.uk/98/EF683D/gp-reg-pat-prac-quin-age.csv") %>%
  filter(ORG_TYPE == "GP") %>%
  mutate(YEAR_MONTH = 202203)

april_2022 <- fread("https://files.digital.nhs.uk/CA/3F03AD/gp-reg-pat-prac-quin-age.csv") %>%
  filter(ORG_TYPE == "GP") %>%
  mutate(YEAR_MONTH = 202204)

may_2022 <- fread("https://files.digital.nhs.uk/BC/22B544/gp-reg-pat-prac-quin-age.csv") %>%
  filter(ORG_TYPE == "GP") %>%
  mutate(YEAR_MONTH = 202204)



gp_size <- bind_rows(
  may_2020, june_2020, july_2020, august_2020, september_2020,
  october_2020, november_2020, december_2020, january_2021, february_2021,
  march_2021, april_2021, may_2021, june_2021, july_2021, august_2021, september_2021,
  october_2021, november_2021, december_2021, january_2022, february_2022, march_2022, april_2022
)




# Don't restrict, include all GP practices for STP and CCG

gp_full_months <- gp_size %>%
  filter(SEX != "ALL" & AGE_GROUP_5 != "ALL") %>% # exclude all as we don't need to use them
  mutate(STAR_PU_AGE_GROUP = case_when(
    AGE_GROUP_5 == "0_4" ~ "0_4",
    AGE_GROUP_5 == "5_9" ~ "5_14",
    AGE_GROUP_5 == "10_14" ~ "5_14",
    AGE_GROUP_5 == "15_19" ~ "15_24",
    AGE_GROUP_5 == "20_24" ~ "15_24",
    AGE_GROUP_5 == "25_29" ~ "25_34",
    AGE_GROUP_5 == "30_34" ~ "25_34",
    AGE_GROUP_5 == "35_39" ~ "35_44",
    AGE_GROUP_5 == "40_44" ~ "35_44",
    AGE_GROUP_5 == "45_49" ~ "45_54",
    AGE_GROUP_5 == "50_54" ~ "45_54",
    AGE_GROUP_5 == "55_59" ~ "55_64",
    AGE_GROUP_5 == "60_64" ~ "55_64",
    AGE_GROUP_5 == "65_69" ~ "65_74",
    AGE_GROUP_5 == "70_74" ~ "65_74",
    AGE_GROUP_5 == "75_79" ~ "75+",
    AGE_GROUP_5 == "80_84" ~ "75+",
    AGE_GROUP_5 == "85_89" ~ "75+",
    AGE_GROUP_5 == "90_94" ~ "75+",
    AGE_GROUP_5 == "95+" ~ "75+"
  ))

usethis::use_data(gp_full_months, overwrite = TRUE)


# Calculate STAR-PU for antibiotic items calculation (paper Pouwels has different weighting??)
# Used https://www.england.nhs.uk/wp-content/uploads/2014/06/mo-dash-supp-info.pdf

STAR_PU_AGE_GROUP <- c("0_4", "5_14", "15_24", "25_34", "35_44", "45_54", "55_64", "65_74", "75+")
MALE <- c(0.8, 0.3, 0.3, 0.2, 0.3, 0.3, 0.4, 0.7, 1.0)
FEMALE <- c(0.8, 0.4, 0.6, 0.6, 0.6, 0.6, 0.7, 1.0, 1.3)

star_pu <- data.frame(STAR_PU_AGE_GROUP, MALE, FEMALE) %>%
  tidyr::pivot_longer(
    cols = c(MALE, FEMALE),
    names_to = "SEX",
    values_to = "STAR_PU"
  )

usethis::use_data(star_pu, overwrite = TRUE)

gp_star_pu <- gp_full_months %>%
  inner_join(
    star_pu,
    by = c("STAR_PU_AGE_GROUP", "SEX")
  ) %>%
  mutate(STAR_PU_PAT = NUMBER_OF_PATIENTS * STAR_PU) %>%
  group_by(YEAR_MONTH, ORG_CODE) %>%
  summarise(GP_STAR_PU_PAT_TOTAL = sum(STAR_PU_PAT)) %>%
  ungroup() %>%
  rename(PRACTICE_CODE = ORG_CODE)

antibiotic_practice_star_pu <- antibiotic_practice %>%
  left_join(gp_star_pu, by = c("YEAR_MONTH", "PRACTICE_CODE"))

# 12 months to April 2021, 12 months to April 2022
antibiotic_practice_star_pu <- antibiotic_practice_star_pu %>%
  mutate(REPORTING_PERIOD = case_when(
    YEAR_MONTH > 202004 & YEAR_MONTH < 202105 ~ "12 months to 2021 April",
    YEAR_MONTH > 202104 & YEAR_MONTH < 202205 ~ "12 months to 2022 April"
  ))


# check - for 12 months per practice - sorted


practice_rolling_month <- antibiotic_practice_star_pu %>%
  group_by(PRACTICE_CODE, REPORTING_PERIOD) %>%
  summarise(
    TOT_ITEMS = sum(TOTAL_ITEMS),
    ITEM_BASED_STARPU = mean(GP_STAR_PU_PAT_TOTAL)
  ) %>%
  ungroup() %>%
  mutate(VALUE = TOT_ITEMS / ITEM_BASED_STARPU)

# Calculation for SUB_ICB

gp_star_pu_sub_icb <- gp_full_months %>%
  inner_join(
    star_pu,
    by = c("STAR_PU_AGE_GROUP", "SEX")
  ) %>%
  select(
    PRACTICE_CODE = ORG_CODE,
    NUMBER_OF_PATIENTS,
    SEX,
    YEAR_MONTH,
    STAR_PU_AGE_GROUP,
    STAR_PU
  ) %>%
  mutate(REPORTING_PERIOD = case_when(
    YEAR_MONTH > 202004 & YEAR_MONTH < 202105 ~ "12 months to 2021 April",
    YEAR_MONTH > 202104 & YEAR_MONTH < 202205 ~ "12 months to 2022 April"
  ))


# Give SUB_ICB code & practice
# read gp sub_icb lookup first
gp_icb_lookup <- readr::read_csv("./data-raw/gp_sub_icb_lookup.csv")


gp_star_pu_sub_icb <- gp_star_pu_sub_icb %>%
  left_join(
    gp_icb_lookup,
    by = "PRACTICE_CODE"
  ) %>%
  filter(GP_TYPE == 4) # only GP

# Calculate STAR_PU
gp_star_pu_sub_icb_cal <- gp_star_pu_sub_icb %>%
  group_by(REPORTING_PERIOD, SUB_ICB, SEX, STAR_PU_AGE_GROUP, STAR_PU) %>%
  summarise(TOTAL_SUB_ICB_PATS = sum(NUMBER_OF_PATIENTS)) %>%
  ungroup() %>%
  mutate(ITEM_STAR_PU = STAR_PU * TOTAL_SUB_ICB_PATS) %>%
  group_by(REPORTING_PERIOD, SUB_ICB) %>%
  summarise(ITEMS_STAR_PU = mean(ITEM_STAR_PU)) %>%
  ungroup() %>%
  rename(SUB_ICB_CODE = SUB_ICB)


gp_star_pu_sub_icb_cal2 <- gp_star_pu_sub_icb %>%
  # getting a average age_band, sex by practice in the time period
  group_by(PRACTICE_CODE, REPORTING_PERIOD, SEX, STAR_PU_AGE_GROUP) %>%
  summarise(mean_pats = mean(NUMBER_OF_PATIENTS)) %>%
  ungroup() %>%
  inner_join(star_pu,
    by = c("SEX", "STAR_PU_AGE_GROUP")
  ) %>%
  mutate(ITEM_STAR_PU = STAR_PU * mean_pats) %>%
  left_join(
    gp_icb_lookup,
    by = "PRACTICE_CODE"
  ) %>%
  group_by(REPORTING_PERIOD, SUB_ICB) %>%
  summarise(ITEMS_STAR_PU = sum(ITEM_STAR_PU)) %>%
  ungroup() %>%
  rename(SUB_ICB_CODE = SUB_ICB)





antibiotic_sub_icb <- antibiotic_practice %>%
  mutate(REPORTING_PERIOD = case_when(
    YEAR_MONTH > 202004 & YEAR_MONTH < 202105 ~ "12 months to 2021 April",
    YEAR_MONTH > 202104 & YEAR_MONTH < 202205 ~ "12 months to 2022 April"
  )) %>%
  group_by(REPORTING_PERIOD, SUB_ICB_CODE, SUB_ICB_NAME, DRUG_OF_INTEREST) %>%
  summarise(TOTAL_ITEMS = sum(TOTAL_ITEMS)) %>%
  ungroup()

left_join(gp_star_pu_sub_icb_cal,
  by = c("PRACTICE_CODE", "YEAR_MONTH")
)


usethis::use_data(practice_rolling_month, overwrite = TRUE)
