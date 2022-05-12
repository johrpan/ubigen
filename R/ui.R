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
                    h3("Criteria"),
                    sliderInput(
                        "above_median",
                        "Expressed above median",
                        min = -1.0,
                        max = 1.0,
                        step = 0.01,
                        value = 1.0
                    ),
                    sliderInput(
                        "mean_expression",
                        "Mean expression",
                        min = -1.0,
                        max = 1.0,
                        step = 0.01,
                        value = 1.0
                    ),
                    sliderInput(
                        "sd_expression",
                        "Standard deviation",
                        min = -1.0,
                        max = 1.0,
                        step = 0.01,
                        value = -1.0
                    )
                ),
                mainPanel(
                    width = 9,
                    h3("Distribution of scores"),
                    div(paste0(
                        "Note: Click or drag within the figure to select ",
                        "genes of interest."
                    )),
                    plotly::plotlyOutput("scores_plot"),
                    h3("Detailed ranking"),
                    div(class = "p-1"),
                    DT::dataTableOutput("selected_genes")
                )
            )
        ),
        tabPanel(
            title = "Help"
        ),
        tabPanel(
            title = "Publication"
        )
    )
}
