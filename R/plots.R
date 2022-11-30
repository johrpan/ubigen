#' Create a plot showing an overview over the provided ranking.
#'
#' @param ranked_data The ranking to visualize.
#' @param highlighted_genes Genes that will be marked.
#' @param sample_proportion Proportion of rows to use as the shown sample.
#'
#' @return A `plotly` figure.
#'
#' @export
overview_plot <- function(ranked_data,
                          highlighted_genes = NULL,
                          sample_proportion = 0.05) {
  figure <- plotly::plot_ly() |>
    plotly::add_lines(
      data = ranked_data[sample(
        nrow(ranked_data),
        sample_proportion * nrow(ranked_data)
      )],
      x = ~percentile,
      y = ~score,
      hoverinfo = "skip"
    ) |>
    plotly::layout(
      xaxis = list(
        title = "Percentile",
        tickformat = ".1%"
      ),
      yaxis = list(title = "Score"),
      shapes = list(
        vline(0.95),
        vline(0.75),
        vline(0.50),
        vline(0.25),
        vline(0.05)
      ),
      annotations = list(
        vlineannotation(0.95),
        vlineannotation(0.75),
        vlineannotation(0.50),
        vlineannotation(0.25),
        vlineannotation(0.05)
      )
    )

  if (length(highlighted_genes) > 0) {
    figure <- figure |>
      plotly::add_markers(
        data = ranked_data[gene %chin% highlighted_genes],
        x = ~percentile,
        y = ~score,
        text = ~ glue::glue(
          "<b>{hgnc_name}</b><br>",
          "Score: {round(score, digits = 2)}<br>",
          "Rank: {rank}<br>",
          "Percentile: {round(percentile * 100, digits = 2)}%"
        ),
        hoverinfo = "text",
        showlegend = FALSE
      )
  }

  figure
}

#' Create a plot comparing some genes with the overall ranking.
#'
#' @param ranked_data The ranking to visualize.
#' @param highlighted_genes Genes that will be compared.
#'
#' @return A `plotly` figure.
#'
#' @export
box_plot <- function(ranked_data, highlighted_genes) {
  data <- data.table::copy(ranked_data)
  data[, group := data.table::fifelse(
    gene %chin% highlighted_genes,
    "Your genes",
    "Other genes"
  )]

  plotly::plot_ly() |>
    plotly::add_boxplot(
      data = data,
      x = ~score,
      y = ~group,
      boxpoints = FALSE
    ) |> plotly::layout(
      xaxis = list(title = "Score"),
      yaxis = list(title = "")
    )
}

#' Create plot showing the distribution of scores using `plotly`.
#'
#' @param ranked_data Data on genes with precomputed ranks.
#' @param highlighted_genes Genes that will be marked.
#' @param ranks How may ranks the x-axis should include. If this parameter is
#'   `NULL`, all ranks will be shown.
#'
#' @return A `plotly` figure for rendering.
#' 
#' @export
scores_plot <- function(ranked_data, highlighted_genes = NULL, ranks = 1000) {
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

  data[, group := data.table::fifelse(
    gene %chin% highlighted_genes,
    "Your genes",
    "All genes"
  )]

  plotly::plot_ly() |>
    plotly::add_markers(
      data = data,
      x = ~rank,
      y = ~score,
      name = ~group,
      text = ~ glue::glue(
        "<b>{hgnc_name}</b><br>",
        "Score: {round(score, digits = 2)}<br>",
        "Rank: {rank}<br>",
        "Percentile: {round(percentile * 100, digits = 2)}%"
      ),
      hoverinfo = "text",
      showlegend = FALSE
    ) |>
    plotly::layout(
      xaxis = list(
        title = ranks_label,
        autorange = "reversed"
      ),
      yaxis = list(title = "Score"),
      clickmode = "event+select",
      dragmode = "select"
    )
}

#' Helper function for creating a vertical line for plotly.
#' @noRd
vline <- function(x) {
  list(
    type = "line",
    y0 = 0,
    y1 = 1,
    yref = "paper",
    x0 = x,
    x1 = x,
    line = list(
      color = "#00000080",
      opacity = 0.5,
      dash = "dot"
    )
  )
}

#' Helper function for creating annotations for lines created using [vline()].
#' @noRd
vlineannotation <- function(x) {
  list(
    text = glue::glue("{round(x * 100)}%"),
    showarrow = FALSE,
    yref = "paper",
    x = x,
    y = 1,
    xanchor = "left",
    xshift = 4,
    align = "left",
    font = list(
      color = "#00000080"
    )
  )
}
