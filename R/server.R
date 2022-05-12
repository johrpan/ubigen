#' Server implementing the main user interface.
#' @noRd
server <- function(input, output) {
    ranked_data <- reactive({
        total_weight <- abs(input$above_median) +
            abs(input$mean_expression) +
            abs(input$sd_expression)

        data <- data.table::copy(ubigen::genes)

        data[, score :=
            (input$above_median * above_median +
                input$mean_expression * mean_expression_normalized +
                input$sd_expression * sd_expression_normalized) /
                total_weight]

        data.table::setorder(data, -score)
        data[, rank := .I]

        data
    })

    output$ranked_data <- DT::renderDataTable(genes_table(ranked_data()))
}

#' Create a displayable data table from the gene results data.
#' @noRd
genes_table <- function(data) {
    data <- data[, .(
        "Gene" = glue::glue_data(
            data,
            "<a href=\"https://www.ensembl.org/Homo_sapiens/Gene/Summary",
            "?db=core;g={gene}\" target=\"_blank\">{hgnc_name}</a>"
        ),
        "Rank" = rank,
        "Score" = score,
        "Median" = median_expression,
        "Mean" = mean_expression,
        "Standard deviation" = sd_expression,
        "Expressed" = above_zero,
        "Above 50 TPM" = above_threshold,
        "Above median" = above_median,
        "Above 95%" = above_95
    )]

    DT::datatable(
        data,
        options = list(pageLength = 100),
        rownames = FALSE,
        escape = FALSE
    ) |>
        DT::formatPercentage(
            c(
                "Score",
                "Expressed",
                "Above 50 TPM",
                "Above median",
                "Above 95%"
            ),
            digits = 2,
        ) |>
        DT::formatRound(c(
            "Median",
            "Mean",
            "Standard deviation"
        ))
}
