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
      line = list(color = base_color()),
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
        marker = list(color = highlight_color()),
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
  fig <- plotly::plot_ly() |>
    plotly::add_boxplot(
      data = ranked_data[!gene %chin% highlighted_genes],
      x = ~score,
      y = "Other genes",
      line = list(color = base_color()),
      fillcolor = transparent(base_color()),
      showlegend = FALSE,
      boxpoints = FALSE
    )

  if (length(highlighted_genes) >= 1) {
    fig <- fig |> plotly::add_boxplot(
      data = ranked_data[gene %chin% highlighted_genes],
      x = ~score,
      y = "Your genes",
      line = list(color = highlight_color()),
      fillcolor = transparent(highlight_color()),
      showlegend = FALSE,
      boxpoints = FALSE
    )
  }

  fig |> plotly::layout(
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

  data[, color := data.table::fifelse(
    gene %chin% highlighted_genes,
    highlight_color(),
    base_color()
  )]

  data[, size := data.table::fifelse(
    gene %chin% highlighted_genes,
    8,
    4
  )]

  # Draw "Your genes" on top of "All genes".
  setorder(data, group)

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
      marker = ~ list(
        color = color,
        size = size,
        opacity = 1,
        line = list(width = 0)
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

#' Create a scatter plot for comparing two different rankings.
#'
#' @param ranking_x The ranking to be shown on the X-axis.
#' @param ranking_y The ranking to be shown on the Y-axis.
#' @param label_x Axis title for the X-axis.
#' @param label_y Axis title for the Y-axis.
#' @param highlighted_genes Gene IDs for genes that should be highlighted
#' @param limit_genes Only show highlighted genes.
#' @param use_percentiles Display percentiles instead of scores.
#'
#' @return A `plotly` figure for rendering.
#'
#' @export
rankings_comparison_plot <- function(ranking_x,
                                     ranking_y,
                                     label_x = "Ranking X",
                                     label_y = "Ranking Y",
                                     highlighted_genes = NULL,
                                     limit_genes = FALSE,
                                     use_percentiles = FALSE) {
  data <- merge(
    ranking_x[, .(gene, score, percentile)],
    ranking_y[, .(gene, score, percentile)],
    by = "gene",
    suffixes = c(x = "_x", y = "_y")
  )

  data <- merge(
    data,
    ubigen::genes,
    by = "gene"
  )

  data[, group := data.table::fifelse(
    gene %chin% highlighted_genes,
    "Your genes",
    "All genes"
  )]

  data[, color := data.table::fifelse(
    gene %chin% highlighted_genes,
    highlight_color(),
    base_color()
  )]

  data[, size := data.table::fifelse(
    gene %chin% highlighted_genes,
    8,
    4
  )]

  # Draw "Your genes" on top of "All genes".
  setorder(data, group)

  threshold_x <- data[percentile_x >= 0.95, min(score_x)]
  threshold_y <- data[percentile_y >= 0.95, min(score_y)]

  if (limit_genes) {
    data <- data[gene %chin% highlighted_genes]
  }

  plotly::plot_ly() |>
    plotly::add_markers(
      data = data,
      x = if (use_percentiles) ~percentile_x else ~score_x,
      y = if (use_percentiles) ~percentile_y else ~score_y,
      name = ~group,
      marker = ~ list(
        color = color,
        size = size,
        opacity = 1,
        line = list(width = 0)
      ),
      text = ~hgnc_name,
      hoverinfo = "text",
      customdata = ~gene,
      showlegend = FALSE
    ) |>
    plotly::layout(
      xaxis = list(
        title = label_x,
        tickformat = if (use_percentiles) ".1%" else NULL,
        range = c(0.0, 1.0)
      ),
      yaxis = list(
        title = label_y,
        range = c(0.0, 1.0)
      ),
      shapes = if (!use_percentiles) {
        list(
          vline(threshold_x),
          hline(threshold_y)
        )
      } else {
        NULL
      },
      annotations = if (!use_percentiles) {
        list(
          list(
            text = "95%",
            x = threshold_x,
            y = 1,
            xshift = 2,
            yshift = 3,
            yref = "paper",
            xanchor = "left",
            yanchor = "top",
            showarrow = FALSE
          ),
          list(
            text = "95%",
            x = 1,
            y = threshold_y,
            yshift = 2,
            xref = "paper",
            xanchor = "right",
            yanchor = "bottom",
            showarrow = FALSE
          )
        )
      } else {
        NULL
      },
      clickmode = "event+select",
      dragmode = "lasso"
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

#' Helper function for creating a horizontal line for plotly.
#' @noRd
hline <- function(y) {
  list(
    type = "line",
    y0 = y,
    y1 = y,
    x0 = 0,
    x1 = 1,
    xref = "paper",
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

#' Base color for plots.
#' @noRd
base_color <- function() "#7d19bf"

#' Highlight color for plots.
#' @noRd
highlight_color <- function() "#ff7f2a"

#' Return the half-transparent version of the color.
#' @noRd
transparent <- function(color) {
  paste0(color, "80")
}
