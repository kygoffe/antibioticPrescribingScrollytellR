library(dplyr)
library(data.table)
devtools::load_all()

# Read 12 months of GP practice size data
# append them
# keep only if practices remained for whole 12 months
# Based on other research papers, remove practices if size is too small etc

april_2020 <- fread("https://files.digital.nhs.uk/98/764116/gp-reg-pat-prac-quin-age.csv") |>
  filter(ORG_TYPE == "GP")

may_2020 <- fread("https://files.digital.nhs.uk/7F/C526F5/gp-reg-pat-prac-quin-age.csv") |>
  filter(ORG_TYPE == "GP")

june_2020 <- fread("https://files.digital.nhs.uk/48/B12E95/gp-reg-pat-prac-quin-age.csv") |>
  filter(ORG_TYPE == "GP")

july_2020 <- fread("https://files.digital.nhs.uk/7A/C3CD13/gp-reg-pat-prac-quin-age.csv") |>
  filter(ORG_TYPE == "GP")

august_2020 <- fread("https://files.digital.nhs.uk/16/3B7597/gp-reg-pat-prac-quin-age.csv") |>
  filter(ORG_TYPE == "GP")

september_2020 <- fread("https://files.digital.nhs.uk/07/D922F2/gp-reg-pat-prac-quin-age.csv") |>
  filter(ORG_TYPE == "GP")

october_2020 <- fread("https://files.digital.nhs.uk/F4/728CF9/gp-reg-pat-prac-quin-age.csv") |>
  filter(ORG_TYPE == "GP")

november_2020 <- fread("https://files.digital.nhs.uk/01/6D3875/gp-reg-pat-prac-quin-age.csv") |>
  filter(ORG_TYPE == "GP")

december_2020 <- fread("https://files.digital.nhs.uk/46/EFD4CB/gp-reg-pat-prac-quin-age.csv") |>
  filter(ORG_TYPE == "GP")

january_2021 <- fread("https://files.digital.nhs.uk/01/C1E9D5/gp-reg-pat-prac-quin-age.csv") |>
  filter(ORG_TYPE == "GP")

february_2021 <- fread("https://files.digital.nhs.uk/94/B835D4/gp-reg-pat-prac-quin-age.csv") |>
  filter(ORG_TYPE == "GP")

march_2021 <- fread("https://files.digital.nhs.uk/9B/99C8B8/gp-reg-pat-prac-quin-age.csv") |>
  filter(ORG_TYPE == "GP") |>
  mutate(EXTRACT_DATE = as.character(EXTRACT_DATE))

april_2021 <- fread("https://files.digital.nhs.uk/9C/54B8FC/gp-reg-pat-prac-quin-age.csv") |>
  filter(ORG_TYPE == "GP") |>
  mutate(EXTRACT_DATE = as.character(EXTRACT_DATE))

may_2021 <- fread("https://files.digital.nhs.uk/A2/49CF7C/gp-reg-pat-prac-quin-age.csv") |>
  filter(ORG_TYPE == "GP") |>
  mutate(EXTRACT_DATE = as.character(EXTRACT_DATE))

june_2021 <- fread("https://files.digital.nhs.uk/79/FC50B1/gp-reg-pat-prac-quin-age.csv") |>
  filter(ORG_TYPE == "GP") |>
  mutate(EXTRACT_DATE = as.character(EXTRACT_DATE))

july_2021 <- fread("https://files.digital.nhs.uk/EE/4EF23F/gp-reg-pat-prac-quin-age.csv") |>
  filter(ORG_TYPE == "GP") |>
  mutate(EXTRACT_DATE = as.character(EXTRACT_DATE))

august_2021 <- fread("https://files.digital.nhs.uk/2C/B12641/gp-reg-pat-prac-quin-age.csv") |>
  filter(ORG_TYPE == "GP") |>
  mutate(EXTRACT_DATE = as.character(EXTRACT_DATE))

september_2021 <- fread("https://files.digital.nhs.uk/31/618CE6/gp-reg-pat-prac-quin-age-v2.csv") |>
  filter(ORG_TYPE == "GP") |>
  mutate(EXTRACT_DATE = as.character(EXTRACT_DATE))

october_2021 <- fread("https://files.digital.nhs.uk/02/231C6E/gp-reg-pat-prac-quin-age.csv") |>
  filter(ORG_TYPE == "GP") |>
  mutate(EXTRACT_DATE = as.character(EXTRACT_DATE))

november_2021 <- fread("https://files.digital.nhs.uk/AF/D1DC73/gp-reg-pat-prac-quin-age.csv") |>
  filter(ORG_TYPE == "GP") |>
  mutate(EXTRACT_DATE = as.character(EXTRACT_DATE))

december_2021 <- fread("https://files.digital.nhs.uk/52/E55571/gp-reg-pat-prac-quin-age.csv") |>
  filter(ORG_TYPE == "GP") |>
  mutate(EXTRACT_DATE = as.character(EXTRACT_DATE))

january_2022 <- fread("https://files.digital.nhs.uk/61/55F8A6/gp-reg-pat-prac-quin-age.csv") |>
  filter(ORG_TYPE == "GP") |>
  mutate(EXTRACT_DATE = as.character(EXTRACT_DATE))

february_2022 <- fread("https://files.digital.nhs.uk/7B/5C0583/gp-reg-pat-prac-quin-age.csv") |>
  filter(ORG_TYPE == "GP") |>
  mutate(EXTRACT_DATE = as.character(EXTRACT_DATE))

march_2022 <- fread("https://files.digital.nhs.uk/98/EF683D/gp-reg-pat-prac-quin-age.csv") |>
  filter(ORG_TYPE == "GP") |>
  mutate(EXTRACT_DATE = as.character(EXTRACT_DATE))











# bind them and keep if GP practice appeared for whole 12 months

gp_size <- bind_rows(
  april_2020, may_2020, june_2020, july_2020, august_2020, september_2020,
  october_2020, november_2020, december_2020, january_2021, february_2021,
  march_2021, april_2021, may_2021, june_2021, july_2021, august_2021, september_2021,
  october_2021, november_2021, december_2021, january_2022, february_2022, march_2022
)


# Don't restrict, include all GP practices for STP and CCG

gp_full_months <- gp_size |>
  filter(SEX != "ALL" & AGE_GROUP_5 != "ALL") |> # exclude all as we don't need to use them
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



# # keep if practice last for 12 months
#
# gp_full_months <- gp_size |>
#   semi_join(
#     gp_size |> group_by(ORG_CODE) |>
#       summarise(practice_count = n_distinct(EXTRACT_DATE)) |>
#       ungroup() |>
#       filter(practice_count == 12) |> # return 6,622 practices (6,801 total so lost 179 practices)
#       select(ORG_CODE)
#   )
#
#
# gp_full_months <- gp_full_months |>
#   semi_join(
#     gp_full_months |>
#       filter(SEX == "ALL") |>
#       group_by(ORG_CODE) |>
#       summarise(m = mean(NUMBER_OF_PATIENTS)) |>
#       filter(m > 750) |> # ref: Wang K.Y, Molter paper (as these practices were assumed to be newly opened or about to be closed.)
#       select(ORG_CODE)
#   ) |> # lost further 53 practices (total of 6,569 practices are used)
#   filter(SEX !='ALL' & AGE_GROUP_5 != 'ALL') |> # exclude all as we don't need to use them
#   mutate(STAR_PU_AGE_GROUP = case_when(
#     AGE_GROUP_5 == '0_4' ~ '0_4',
#     AGE_GROUP_5 == '5_9' ~ '5_14',
#     AGE_GROUP_5 == '10_14' ~ '5_14',
#     AGE_GROUP_5 == '15_19' ~ '15_24',
#     AGE_GROUP_5 == '20_24' ~ '15_24',
#     AGE_GROUP_5 == '25_29' ~ '25_34',
#     AGE_GROUP_5 == '30_34' ~ '25_34',
#     AGE_GROUP_5 == '35_39' ~ '35_44',
#     AGE_GROUP_5 == '40_44' ~ '35_44',
#     AGE_GROUP_5 == '45_49' ~ '45_54',
#     AGE_GROUP_5 == '50_54' ~ '45_54',
#     AGE_GROUP_5 == '55_59' ~ '55_64',
#     AGE_GROUP_5 == '60_64' ~ '55_64',
#     AGE_GROUP_5 == '65_69' ~ '65_74',
#     AGE_GROUP_5 == '70_74' ~ '65_74',
#     AGE_GROUP_5 == '75_79' ~ '75+',
#     AGE_GROUP_5 == '80_84' ~ '75+',
#     AGE_GROUP_5 == '85_89' ~ '75+',
#     AGE_GROUP_5 == '90_94' ~ '75+',
#     AGE_GROUP_5 == '95+' ~ '75+'
#   ))


# Calculate STAR-PU for antibiotic items calculation (paper Pouwels )

STAR_PU_AGE_GROUP <- c("0_4", "5_14", "15_24", "25_34", "35_44", "45_54", "55_64", "65_74", "75+")
MALE <- c(0.8, 0.3, 0.3, 0.2, 0.3, 0.3, 0.4, 0.7, 1.0)
FEMALE <- c(0.8, 0.4, 0.6, 0.6, 0.6, 0.6, 0.7, 1.0, 0.3)

star_pu <- data.frame(STAR_PU_AGE_GROUP, MALE, FEMALE) |>
  tidyr::pivot_longer(
    cols = c(MALE, FEMALE),
    names_to = "SEX",
    values_to = "STAR_PU"
  )

gp_star_pu <- gp_full_months |>
  inner_join(
    star_pu,
    by = c("STAR_PU_AGE_GROUP", "SEX")
  ) |>
  mutate(STAR_PU_PAT = NUMBER_OF_PATIENTS * STAR_PU) |>
  group_by(EXTRACT_DATE, ORG_CODE) |>
  summarise(GP_STAR_PU_PAT_TOTAL = sum(STAR_PU_PAT)) |>
  ungroup() |>
  mutate(
    YEAR_MONTH = case_when(
      EXTRACT_DATE == "01-Apr-20" ~ 202004,
      EXTRACT_DATE == "01MAY2020" ~ 202005,
      EXTRACT_DATE == "01JUN2020" ~ 202006,
      EXTRACT_DATE == "01JUL2020" ~ 202007,
      EXTRACT_DATE == "01AUG2020" ~ 202008,
      EXTRACT_DATE == "01SEP2020" ~ 202009,
      EXTRACT_DATE == "01OCT2020" ~ 202010,
      EXTRACT_DATE == "01NOV2020" ~ 202011,
      EXTRACT_DATE == "01DEC2020" ~ 202012,
      EXTRACT_DATE == "01JAN2021" ~ 202101,
      EXTRACT_DATE == "01FEB2021" ~ 202102,
      EXTRACT_DATE == "2021-03-01" ~ 202103,
      EXTRACT_DATE == "01APR2021" ~ 202104,
      EXTRACT_DATE == "01MAY2021" ~ 202105,
      EXTRACT_DATE == "01Jun2021" ~ 202106,
      EXTRACT_DATE == "01Jul2021" ~ 202107,
      EXTRACT_DATE == "01Aug2021" ~ 202108,
      EXTRACT_DATE == "01Sep2021" ~ 202109,
      EXTRACT_DATE == "01Oct2021" ~ 202110,
      EXTRACT_DATE == "01Nov2021" ~ 202111,
      EXTRACT_DATE == "01Dec2021" ~ 202112,
      EXTRACT_DATE == "01Jan2022" ~ 202201,
      EXTRACT_DATE == "01Feb2022" ~ 202202,
      EXTRACT_DATE == "01Mar2022" ~ 202203
    )
  ) |>
  rename(PRACTICE_CODE = ORG_CODE)

gp_star_pu_ccg_stp <- gp_star_pu |>
  left_join(gp_ccg_stp, by = c("YEAR_MONTH", "PRACTICE_CODE"))



# QR - broadly ok so happy with the process
d <- gp_star_pu_ccg_stp |>
  filter(YEAR_MONTH >= 202104 & YEAR_MONTH < 202204) |>
  summarise(sp = sum(GP_STAR_PU_PAT_TOTAL) / n_distinct(YEAR_MONTH))

n <- antibiotic_pm |> filter(YEAR_MONTH >= 202104 & YEAR_MONTH < 202204) |> # & PCO_CODE=='07P00') |>
  summarise(itms = sum(TOTAL_ITEMS))


n / d

usethis::use_data(gp_star_pu_ccg_stp, overwrite = TRUE)
