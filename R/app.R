#' Run the application server.
#'
#' @param host The hostname to serve the application on.
#' @param port The port to serve the application on.
#' @param custom_dataset This allows to set a custom dataset (return value of
#'   [analyze()]) as the default dataset of the UI.
#' @param show_api_docs Whether to show the API documentation. Use [run_api()]
#'   to actually serve the API.
#'
#' @seealso [app()] for retrieving a Shiny app object.
#'
#' @export
run_app <- function(host = "127.0.0.1",
                    port = 3464,
                    custom_dataset = NULL,
                    show_api_docs = FALSE) {
  runApp(
    app(
      custom_dataset = custom_dataset,
      show_api_docs = show_api_docs
    ),
    host = host,
    port = port
  )
}

#' Create a shiny application for Ubigen.
#'
#' @param custom_dataset This allows to set a custom dataset (return value of
#'   [analyze()]) as the default dataset of the UI.
#' @param show_api_docs Whether to show the API documentation. Use [run_api()]
#'   to actually serve the API.
#'
#' @seealso [run_app()] for immediately running the application.
#'
#' @export
app <- function(custom_dataset = NULL, show_api_docs = FALSE) {
  shinyApp(
    ui(
      custom_dataset = custom_dataset,
      show_api_docs = show_api_docs
    ),
    server(
      custom_dataset = custom_dataset
    )
  )
}

#' Run the Ubigen API.
#'
#' This requires the `plumber` package to be installed.
#'
#' @param host The hostname to serve the API on.
#' @param port The port to serve the API on.
#'
#' @export
run_api <- function(host = "127.0.0.1", port = 3465) {
  if (!requireNamespace("plumber", quietly = TRUE)) {
    stop("Please install \"plumber\" to use this function.")
  }

  plumber::plumb(file = system.file(
    "plumber", "ubigen", "plumber.R",
    package = "ubigen"
  )) |>
    plumber::pr_run(host = host, port = port, docs = FALSE)
}
