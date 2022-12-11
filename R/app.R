#' Run the application server.
#'
#' @param host The hostname to serve the application on.
#' @param port The port to serve the application on.
#' @param custom_dataset This allows to set a custom dataset (return value of
#'   [analyze()]) as the default dataset of the UI.
#'
#' @export
run_app <- function(host = "127.0.0.1",
                    port = 3464,
                    custom_dataset = NULL) {
  runApp(
    shinyApp(
      ui(custom_dataset = custom_dataset),
      server(custom_dataset = custom_dataset)
    ),
    host = host,
    port = port
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
