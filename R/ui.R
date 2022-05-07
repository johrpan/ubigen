#' Function for creating the main user interface.
#' @noRd
ui <- function() {
    navbarPage(
        theme = bslib::bs_theme(
            version = 5,
            bootswatch = "united",
            primary = "#7d19bf"
        ),
        title = "Ubigen",
        tabPanel(
            "Explore",
            sidebarLayout(
                sidebarPanel(
                    width = 3,
                ),
                mainPanel(
                    width = 9,
                )
            )
        ),
        tabPanel(
            title = "Data",
            DT::dataTableOutput("all_data")
        ),
        tabPanel(
            title = "Publication"
        )
    )
}
