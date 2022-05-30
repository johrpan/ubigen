#' Server implementing the main user interface.
#' @noRd
server <- function(input, output) {
  ranked_data <- reactive({
    total_weight <- abs(input$cross_sample_weight) + abs(input$sd_expression)
    data <- data.table::copy(ubigen::genes)

    data[, score :=
      (input$cross_sample_weight * get(input$cross_sample_metric) +
        input$sd_expression * sd_expression_normalized) /
        total_weight]

    data.table::setorder(data, -score)
    data[, rank := .I]
    data[, percentile := 1 - rank / max(rank)]

    data
  })

  output$overview_plot <- plotly::renderPlotly(overview_plot(ranked_data()))
  output$scores_plot <- plotly::renderPlotly(scores_plot(ranked_data()))

  output$selected_genes <- DT::renderDataTable({
    selected_points <- plotly::event_data("plotly_selected")

    data <- if (is.null(selected_points)) {
      ranked_data()
    } else {
      ranked_data()[rank %in% selected_points$x]
    }

    genes_table(data)
  })
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
    "Above median" = above_median,
    "Above 95%" = above_95
  )]

  DT::datatable(
    data,
    options = list(
      buttons = list(
        list(
          extend = "copy",
          text = "Copy to clipboard"
        ),
        list(
          extend = "csv",
          text = "Download CSV"
        )
      ),
      dom = "fBrtip",
      pageLength = 100
    ),
    rownames = FALSE,
    escape = FALSE,
    selection = "none",
    extensions = "Buttons"
  ) |>
    DT::formatPercentage(
      c(
        "Percentile",
        "Score",
        "Expressed",
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
