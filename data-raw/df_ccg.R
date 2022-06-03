# Read CCG and pivot longer

ccg <- read.csv("./data-raw/ccg.csv", check.names = FALSE)

ccg <- ccg |> 
  tidyr::pivot_longer(
    cols = !c(REGION:CCG_CODE),
    names_to = c("METRIC", "YEAR_MONTH"),
    names_sep = "-",
    values_to = "VALUE"
  )
