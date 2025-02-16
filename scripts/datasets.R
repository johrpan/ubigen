library(data.table)
library(here)

i_am("scripts/datasets.R")

ranking_gtex_all <- ubigen::rank_genes(ubigen::gtex_all)
ranking_gtex_tissues <- ubigen::rank_genes(ubigen::gtex_tissues)
ranking_hpa <- ubigen::rank_genes(ubigen::hpa_tissues)
ranking_cmap <- ubigen::rank_genes(ubigen::cmap)

cor_ranking <- function(ranking_x, ranking_y) {
  data <- merge(ranking_x, ranking_y, by = "gene")

  stats::cor(
    data$score.x,
    data$score.y,
  ) |>
    round(digits = 4) |>
    format(nsmall = 4)
}

cat("Ranking correlations:")
cat("\nGTEx (tissues) ~ GTEx (all): ")
cat(cor_ranking(ranking_gtex_all, ranking_gtex_tissues))
cat("\nHPA ~ GTEx (all): ")
cat(cor_ranking(ranking_gtex_all, ranking_hpa))
cat("\nCMap ~ GTEx (all): ")
cat(cor_ranking(ranking_gtex_all, ranking_cmap))
cat("\nCMap ~ GTEx (tissues): ")
cat(cor_ranking(ranking_gtex_tissues, ranking_cmap))
cat("\nHPA ~ GTEx (tissues): ")
cat(cor_ranking(ranking_gtex_tissues, ranking_hpa))
cat("\nCMap ~ HPA: ")
cat(cor_ranking(ranking_hpa, ranking_cmap))

# Sample rankings for plots

sample_ranking <- function(ranking) {
  ranking <- ranking[percentile >= 0.8]
  ranking[sample(nrow(ranking), 500)]
}

fig <- plotly::plot_ly() |>
  plotly::add_lines(
    data = ranking_gtex_tissues |> sample_ranking(),
    x = ~percentile,
    y = ~score,
    name = "GTEx (tissues)",
    line = list(
      color = "#2275d4",
      width = 4
    )
  ) |>
  plotly::add_lines(
    data = ranking_hpa |> sample_ranking(),
    x = ~percentile,
    y = ~score,
    name = "HPA (tissues)",
    line = list(
      color = "#30c422",
      width = 4
    )
  ) |>
  plotly::add_lines(
    data = ranking_cmap |> sample_ranking(),
    x = ~percentile,
    y = ~score,
    name = "CMap (drugs)",
    line = list(
      color = "#ff7f2a",
      width = 4
    )
  ) |>
  plotly::add_lines(
    data = ranking_gtex_all |> sample_ranking(),
    x = ~percentile,
    y = ~score,
    name = "GTEx (all)",
    line = list(
      color = "#7d19bf",
      width = 4
    )
  ) |>
  plotly::layout(
    xaxis = list(
      title = "Percentile",
      tickformat = ".1%",
      range = c(0.8, 1)
    ),
    yaxis = list(
      title = "Score",
      range = c(0, 1)
    ),
    font = list(size = 8),
    margin = list(
      pad = 2,
      l = 36,
      r = 0,
      t = 0,
      b = 36
    ),
    showlegend = FALSE
  )

plotly::save_image(
  fig,
  file = here("scripts/output/datasets.svg"),
  width = 6.27 * 72,
  height = 3.135 * 72,
  scale = 96 / 72
)

plotly::save_image(
  fig |> plotly::layout(showlegend = TRUE),
  file = here("scripts/output/datasets_legend.svg"),
  width = 6.27 * 72,
  height = 3.135 * 72,
  scale = 96 / 72
)
