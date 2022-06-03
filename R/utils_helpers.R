#' Custom NHSBSA highcharter theme
#'
#' Based on the nhsbsaR highcharter theme, since it returns a list we can edit
#' it to the specific theme for this shiny app.
#'
#' @param palette Which colour palette to use from the `nhsbsaR` package.
#' @param stack Stack option for highcharter.
#'
#' @return
#' @export
theme_nhsbsa <- function(hc, palette = NA, stack = "normal") {

  # Load theme from nhsbsaR package
  theme_nhsbsa_hc <- nhsbsaR::theme_nhsbsa_hc(family = "Frutiger W01")

  # Add the plot options
  theme_nhsbsa_hc$plotOptions <- list(
    series = list(stacking = stack, borderWidth = 0),
    bar = list(groupPadding = 0.1)
  )

  # Add the palettes (hack the highlight palette to have a lighter grey)
  theme_nhsbsa_hc$colors <- nhsbsaR::palette_nhsbsa(palette = palette)
  theme_nhsbsa_hc$colors[theme_nhsbsa_hc$colors == "#768692"] <- "#d1d5d6"
  theme_nhsbsa_hc$colAxis <- list(
    min = 0,
    minColor = nhsbsaR::palette_nhsbsa(palette = "gradient")[1],
    maxColor = nhsbsaR::palette_nhsbsa(palette = "gradient")[2]
  )

  # Style based on the NHS frontend toolkit
  theme_nhsbsa_hc$xAxis$className <- "nhsuk-body-s"
  theme_nhsbsa_hc$yAxis$className <- "nhsuk-body-s"

  # Add the theme to the chart and then remove the credits afterwards (currently
  # does not work to do this within the theme)
  hc %>%
    highcharter::hc_add_theme(hc_thm = theme_nhsbsa_hc)
  # %>%    highcharter::hc_colors(colors = nhsbsaR::palette_nhsbsa(palette = palette))
}

#' fontawesome save to datauri
#' taken from https://jkunst.com/highcharter/articles/fontawesome.html
#'
#' @param name fontawsome name
#' @param vars Grouping variables
#'
#' @return
#' @export

fa_to_png_to_datauri <- function(name, ...) {
  tmpfl <- tempfile(fileext = ".png")

  fontawesome::fa_png(name, file = tmpfl, ...)

  knitr::image_uri(tmpfl)
}


#' fit_lm_poly function
#'
#' Apply linear model to supplied data based on IMD rankings.
#'
#' Output will reflect the "expected" value based on all supplied values and IMD rank,
#' including expected value as well as approximate 95% confidence interval levels.
#'
#' Results will be filtered to only include points every ~1000 values to support graph rendering
#'
#' @param data dataset including geographic rankings classified as imd_rank
#' @param vars metric to create model for
#'
#' @return
#' @export
fit_lm_poly <- function(data, vars) {

  # Prepare model data
  data <- data %>%
    dplyr::filter(!is.na(imd_rank)) %>%
    dplyr::select({{ vars }}, imd_rank) %>%
    dplyr::rename(y := {{ vars }})

  # Fit model
  mod <- lm(
    data = data,
    formula = y ~ poly(imd_rank, 2)
  )

  # Calculate 'potential outlier' boundaries
  sd <- 1.96 * sd(data$y)

  # Create potential outlier boundaries df
  line <- data.frame(
    imd_rank = data %>% dplyr::select(imd_rank),
    value = mod$fitted.values
  ) %>%
    dplyr::arrange(imd_rank) %>%
    dplyr::mutate(
      value_upper = value + sd,
      value_lower = value - sd
    ) %>%
    dplyr::filter(
      dplyr::row_number() %% 1000 == 1 | dplyr::row_number() == nrow(.)
    ) %>%
    dplyr::mutate(metric := {{ vars }})

  # Re-format rownumbers
  rownames(line) <- NULL

  # Return output
  return(line)
}

#' fit_lm_poly_outliers function
#'
#' Summarise LSOA values, aggregated to Local Authority, based on linear model
#'
#' Linear model will reflect expected results based on IMD rankings, identifying
#' any potential outliers based on approximate 95% confidence intervals
#'
#'
#' @param data dataset including geographic rankings classified as imd_rank
#' @param vars metric to create model for
#'
#' @return
#' @export
fit_lm_poly_outliers <- function(data, vars) {

  # Format model df
  mod_df <- data %>%
    dplyr::filter(!is.na(imd_rank)) %>%
    dplyr::select({{ vars }}, imd_rank) %>%
    dplyr::rename(y := {{ vars }})

  # Simple linear model
  mod <- lm(
    data = mod_df,
    formula = y ~ poly(imd_rank, 2)
  )

  # Calculate SD to generate 'potential outlier' bounds
  sd <- 1.96 * sd(mod_df$y)

  # Create df with fitted values for trend line
  line <- data.frame(
    imd_rank = mod_df %>% dplyr::select(imd_rank),
    value = mod$fitted.values,
    diff = mod$residuals
  ) %>%
    dplyr::arrange(imd_rank) %>%
    dplyr::mutate(
      value_upper = value + sd,
      value_lower = value - sd
    ) %>%
    dplyr::select(-value)

  # Rejoin back to original df and sum outliers / calculate net residual value
  data <- data %>%
    dplyr::select(lad_name, lsoa_name, imd_rank, core20_classification, {{ vars }}) %>%
    dplyr::rename(y := {{ vars }}) %>%
    dplyr::inner_join(line, by = "imd_rank") %>%
    dplyr::mutate(
      potential_outliers = ifelse(
        y < value_lower | y > value_upper, 1, 0
      )
    ) %>%
    dplyr::group_by(lad_name) %>%
    dplyr::summarise(
      potential_outliers = sum(potential_outliers),
      lsoa_count = dplyr::n_distinct(lsoa_name),
      core20_count = data.table::uniqueN(lsoa_name[core20_classification == "CORE20"]),
      trend = ifelse(sum(diff) <= 0, "below", "above")
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(metric := {{ vars }})

  # Return
  return(data)
}




#' fit_lm_poly_vals function
#'
#' Apply linear model to supplied data based on IMD rankings
#' Output will reflect the "expected" value based on all supplied values and IMD rank
#'
#' @param data dataset including geographic rankings classified as imd_rank
#' @param vars metric to create model for
#'
#' @return
#' @export
fit_lm_poly_vals <- function(data, vars) {

  # Lower case column names
  names(data) <- tolower(names(data))

  # Select vars for model df
  data <- data %>%
    dplyr::filter(!is.na(imd_rank)) %>%
    dplyr::select({{ vars }}, imd_rank) %>%
    dplyr::rename(y := {{ vars }})

  # Fit model
  mod <- lm(
    data = data,
    formula = y ~ poly(imd_rank, 2)
  )

  # Format Output
  line <- data.frame(
    imd_rank = data %>% dplyr::select(imd_rank),
    value = mod$fitted.values
  ) %>%
    dplyr::arrange(imd_rank) %>%
    dplyr::mutate(metric := {{ vars }})

  # Re-format rownumbers
  rownames(line) <- NULL

  # Return data
  return(line)
}
