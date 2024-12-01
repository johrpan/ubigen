library(data.table)
library(here)

i_am("scripts/gtex_vs_cmap.R")

ranking_gtex <- ubigen::rank_genes(ubigen::gtex_all)
ranking_cmap <- ubigen::rank_genes(ubigen::cmap)

data <- merge(
  ranking_gtex[, .(gene, score, percentile)],
  ranking_cmap[, .(gene, score, percentile)],
  by = "gene",
  suffixes = c(x = "_gtex", y = "_cmap")
)

cat("N =", nrow(data))

# OPTICS clustering

result_optics <- dbscan::optics(
  data[, .(score_gtex, score_cmap)],
  minPts = 0.01 * nrow(data)
)

result_cluster <- result_optics |> dbscan::extractXi(0.05)
data[, cluster := result_cluster$cluster]

cluster_means <- data[,
  .(
    score_gtex = mean(score_gtex),
    score_cmap = mean(score_cmap)
  ),
  by = cluster
]

cluster_means[, distance_1_1 := sqrt(
  (1.0 - score_gtex)^2 + (1.0 - score_cmap)^2
)]

cluster_1_1 <- cluster_means[which.min(distance_1_1), cluster]
genes_optics <- data[cluster == cluster_1_1, gene]
write(genes_optics, here("scripts/output/genes_optics.txt"))

# Percentiles

data[percentile_gtex < 0.95 & percentile_cmap < 0.95, group := "0_0"]
data[percentile_gtex < 0.95 & percentile_cmap >= 0.95, group := "0_1"]
data[percentile_gtex >= 0.95 & percentile_cmap < 0.95, group := "1_0"]
data[percentile_gtex >= 0.95 & percentile_cmap >= 0.95, group := "1_1"]

fwrite(data, file = here("scripts/output/gtex_vs_cmap_groups.csv"))

write(data[group == "0_0", gene], here("scripts/output/genes_0_0.txt"))
write(data[group == "0_1", gene], here("scripts/output/genes_0_1.txt"))
write(data[group == "1_0", gene], here("scripts/output/genes_1_0.txt"))
write(data[group == "1_1", gene], here("scripts/output/genes_1_1.txt"))

threshold_gtex <- data[percentile_gtex >= 0.95, min(score_gtex)]
threshold_cmap <- data[percentile_cmap >= 0.95, min(score_cmap)]

fig <- plotly::plot_ly() |>
  plotly::add_markers(
    data = data,
    x = ~score_gtex,
    y = ~score_cmap,
    name = ~cluster,
    showlegend = FALSE,
    marker = list(size = 4),
    cliponaxis = FALSE
  ) |>
  plotly::layout(
    xaxis = list(
      title = "Ranking based on GTEx",
      range = c(0, 1)
    ),
    yaxis = list(
      title = "Ranking based on CMap",
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
        y = threshold_cmap,
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
        y0 = threshold_cmap,
        y1 = threshold_cmap,
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
  file = here("scripts/output/gtex_vs_cmap.svg"),
  width = 6.27 * 72,
  height = 6.27 * 72,
  scale = 96 / 72
)
