library(data.table)
library(here)

i_am("scripts/gtex_vs_glioma.R")

ranking_gtex <- ubigen::rank_genes(
  ubigen::gtex_all,
  level_weight = 0.5,
  cross_sample_weight = 0.5,
  variation_weight = 0.0
)

ranking_glioma <- ubigen::rank_genes(
  ubigen::glioma,
  level_weight = 0.5,
  cross_sample_weight = 0.5,
  variation_weight = 0.0
)

data <- merge(
  ranking_gtex[, .(gene, score, percentile)],
  ranking_glioma[, .(gene, score, percentile)],
  by = "gene",
  suffixes = c(x = "_gtex", y = "_glioma")
)

cat("N =", nrow(data))

threshold_gtex <- data[percentile_gtex >= 0.95, min(score_gtex)]
threshold_glioma <- data[percentile_glioma >= 0.95, min(score_glioma)]

fig <- plotly::plot_ly() |>
  plotly::add_markers(
    data = data,
    x = ~score_gtex,
    y = ~score_glioma,
    showlegend = FALSE,
    marker = list(size = 4),
    cliponaxis = FALSE
  ) |>
  plotly::layout(
    xaxis = list(
      title = "Ranking based on TCGA (Glioma)",
      range = c(0, 1)
    ),
    yaxis = list(
      title = "Ranking based on glioma",
      range = c(0, 1)
    ),
    annotations = list(
      list(
        text = "95%",
        x = threshold_gtex,
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
        y = threshold_glioma,
        yshift = 2,
        xref = "paper",
        xanchor = "right",
        yanchor = "bottom",
        showarrow = FALSE
      )
    ),
    shapes = list(
      list(
        type = "line",
        y0 = 0,
        y1 = 1,
        yref = "paper",
        x0 = threshold_gtex,
        x1 = threshold_gtex,
        line = list(
          color = "#00000080",
          opacity = 0.5,
          width = 1,
          dash = "dot"
        )
      ),
      list(
        type = "line",
        y0 = threshold_glioma,
        y1 = threshold_glioma,
        x0 = 0,
        x1 = 1,
        xref = "paper",
        line = list(
          color = "#00000080",
          width = 1,
          opacity = 0.5,
          dash = "dot"
        )
      )
    ),
    font = list(size = 8),
    margin = list(
      pad = 2,
      l = 36,
      r = 0,
      t = 0,
      b = 36
    )
  )

# Plotly specifies all sizes in pixels, including font size. Because of
# that, we can actually think of these pixels as points. One point is defined as
# 1/72 inch and SVG uses 96 DPI as the standard resolution.
#
# 1 plotly pixel = 1 point = 1/72 inch = 1 1/3 actual pixels
#
# So, we specify width and height in points (= plotly pixels) and scale up the
# image by 96/72 to convert everything from points to pixels at 96 DPI.

plotly::save_image(
  fig,
  file = here("scripts/output/gtex_vs_glioma.svg"),
  width = 6.27 * 72,
  height = 6.27 * 72,
  scale = 96 / 72
)
