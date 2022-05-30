#' Create a plot showing an overview over the provided ranking.
#'
#' @param ranked_data The ranking to visualize.
#' @param sample_proportion Proportion of rows to use as the shown sample.
#'
#' @return A `plotly` figure.
#' @noRd
overview_plot <- function(ranked_data, sample_proportion = 0.05) {
  plotly::plot_ly() |>
    plotly::add_lines(
      data = ranked_data[sample(
        nrow(ranked_data),
        sample_proportion * nrow(ranked_data)
      )],
      x = ~rank,
      y = ~score
    ) |>
    plotly::layout(
      xaxis = list(title = "Ranks"),
      yaxis = list(title = "Score")
    )
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
      yaxis = list(title = "Score"),
      clickmode = "event+select",
      dragmode = "select"
    )
}
