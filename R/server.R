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
        data[, percentile := 1 - rank / max(rank)]

        data
    })

    output$scores_plot <- plotly::renderPlotly(scores_plot(ranked_data()))
    output$ranked_data <- DT::renderDataTable(genes_table(ranked_data()))
}

#' Create plot showing the distribution of scores using `plotly`.
#'
#' @param ranked_data Data on genes with precomputed ranks.
#' @param ranks How may ranks the x-axis should include. If this parameter is
#'   `NULL`, all ranks will be shown.
#'
#' @return A `plotly` figure for rendering.
#' @noRd
scores_plot <- function(ranked_data, ranks = 1000) {
    data <- if (is.null(ranks)) {
        ranked_data
    } else {
        ranked_data[1:ranks]
    }

    ranks_label <- if (is.null(ranks)) {
        "Ranks"
    } else {
        glue::glue("Ranks (1 to {ranks})")
    }

    plotly::plot_ly() |>
        plotly::add_markers(
            data = data,
            x = ~rank,
            y = ~score,
            text = ~hgnc_name,
            customdata = ~percentile,
            hovertemplate = paste0(
                "<b>%{text}</b><br>",
                "Rank: %{x}<br>",
                "Score: %{y:.2}<br>",
                "Percentile: %{customdata:.2%}",
                "<extra></extra>"
            )
        ) |>
        plotly::layout(
            xaxis = list(title = ranks_label),
            yaxis = list(title = "Score")
        )
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
        "Percentile" = percentile,
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
                "Percentile",
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
