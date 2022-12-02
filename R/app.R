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
