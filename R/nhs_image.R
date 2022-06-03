#' nhs_image Function
#'
#' @noRd
#'
#' @importFrom shiny tagList
nhs_image <- function(src, alt = NULL, caption = NULL, width = "66.66667%") {
  tagList(
    tags$figure(
      class = "nhsuk-image",
      style = paste0("width:", width),
      tags$img(
        class = "nhsuk-image__img",
        src = src,
        alt = alt
      ),
      if (!is.null(caption)) {
        tags$figcaption(
          class = "nhsuk-image__caption",
          caption
        )
      }
    )
  )
}
