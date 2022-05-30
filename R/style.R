#' Custom CSS to tweak the rendering.
#' @noRd
custom_css <- function() {
  tags$head(
    tags$style(".nav-hidden { height: 0 }")
  )
}
