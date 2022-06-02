#' Custom CSS to tweak the rendering.
#' @noRd
custom_css <- function() {
  tags$head(
    tags$style(HTML(
      ".nav-hidden { height: 0 }",
      ".flow-layout > div {",
      "display: inline-block;",
      "vertical-align: top;",
      "margin-right: 12px;",
      "}"
    ))
  )
}
