#' Run the application server.
#'
#' @param host The hostname to serve the application on.
#' @param port The port to serve the application on.
#'
#' @export
run_app <- function(host = "127.0.0.1", port = 3464) {
    runApp(shinyApp(ui, server), host = host, port = port)
}
