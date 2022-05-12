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
                    h3("Features"),
                    sliderInput(
                        "above_zero",
                        verticalLayout(
                            strong("Expressed"),
                            paste0(
                                "Percentage of samples in which the gene is ",
                                "expressed."
                            )
                        ),
                        min = -1.0,
                        max = 1.0,
                        step = 0.01,
                        value = 0.33
                    ),
                    sliderInput(
                        "above_median",
                        verticalLayout(
                            strong("Expressed above median"),
                            paste0(
                                "Percentage of samples that express the gene ",
                                "more than the median of expression within ",
                                "that sample."
                            )
                        ),
                        min = -1.0,
                        max = 1.0,
                        step = 0.01,
                        value = 0.33
                    ),
                    sliderInput(
                        "above_95",
                        verticalLayout(
                            strong("Expressed above 95%"),
                            paste0(
                                "Percentage of samples that express the gene ",
                                "more than the 95. percentile of expression ",
                                "within that sample."
                            )
                        ),
                        min = -1.0,
                        max = 1.0,
                        step = 0.01,
                        value = 0.33
                    ),
                    sliderInput(
                        "mean_expression",
                        verticalLayout(
                            strong("Mean expression"),
                            div(paste0(
                                "Average of the gene's expression across all ",
                                "samples."
                            ))
                        ),
                        min = -1.0,
                        max = 1.0,
                        step = 0.01,
                        value = 1.0
                    ),
                    sliderInput(
                        "sd_expression",
                        verticalLayout(
                            strong("Standard deviation"),
                            paste0(
                                "Standard deviation of the gene's expression ",
                                "across all samples."
                            )
                        ),
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
                        "Click or drag within the figure to select genes of ",
                        "interest."
                    )),
                    plotly::plotlyOutput("scores_plot"),
                    h3("Detailed ranking"),
                    div(paste0(
                        "Click on gene names to view them using the Ensembl ",
                        "genome browser."
                    )),
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
