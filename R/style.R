#' Custom CSS to tweak the rendering.
#' @noRd
custom_css <- function() {
  tags$head(
    tags$style(HTML(
      ".nav-hidden { height: 0 }",
      ".flow-layout > div {",
      "    display: inline-block;",
      "    vertical-align: top;",
      "    margin-right: 12px;",
      "}",
      ".container h2, .container h3 {",
      "     margin-top: 64px;",
      "}",
      "",
      ".container table {",
      "     margin-top: 32px;",
      "     margin-bottom: 32px;",
      "}",
      "",
      ".container th, .container td {",
      "     padding: 4px;",
      "}"
    ))
  )
}
