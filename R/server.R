#' Server implementing the main user interface.
#' @noRd
server <- function(input, output) {
    output$all_data <- DT::renderDataTable({
        data <- ubigen::genes[, .(
            "Gene" = glue::glue_data(
                ubigen::genes,
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
            DT::formatPercentage(c(
                "Score",
                "Expressed",
                "Above 50 TPM",
                "Above median",
                "Above 95%"
            )) |>
            DT::formatRound(c(
                "Median",
                "Mean",
                "Standard deviation"
            ))
    })
}
