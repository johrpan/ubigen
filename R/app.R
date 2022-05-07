#' Run the application server.
#'
#' @param port The port to serve the application on.
#'
#' @export
run_app <- function(port = 3464) {
    runApp(shinyApp(ui, server), port = port)
}
