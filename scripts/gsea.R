library(data.table)
library(here)

i_am("scripts/gsea.R")

ranking_gtex <- ubigen::rank_genes(ubigen::gtex_all)
ranking_cmap <- ubigen::rank_genes(ubigen::cmap)

data <- merge(
  ranking_gtex[, .(gene, score, percentile)],
  ranking_cmap[, .(gene, score, percentile)],
  by = "gene",
  suffixes = c(x = "_gtex", y = "_cmap")
)

data[, score := score_gtex * score_cmap]
setorder(data, -score)
data[, percentile := (.N - .I) / .N]

gsea_1_0 <- gprofiler2::gost(
  data[percentile_gtex >= 0.95 & percentile_cmap < 0.95, gene],
  domain_scope = "custom_annotated",
  custom_bg = data[, gene]
)

gsea_1_1 <- gprofiler2::gost(
  data[percentile_gtex >= 0.95 & percentile_cmap >= 0.95, gene],
  domain_scope = "custom_annotated",
  custom_bg = data[, gene]
)

# This code is based on gostplot.R from the gprofiler2 package.

gsea_sources <- c(
  "GO:MF",
  "GO:BP",
  "GO:CC",
  "KEGG",
  "REAC",
  "WP",
  "TF",
  "MIRNA",
  "HPA",
  "CORUM",
  "HP"
)

gsea_source_colors <- data.table(
  source = gsea_sources,
  color = c(
    "#dc3912",
    "#ff9900",
    "#109618",
    "#dd4477",
    "#3366cc",
    "#0099c6",
    "#5574a6",
    "#22aa99",
    "#6633cc",
    "#66aa00",
    "#990099"
  )
)

lerp <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

gsea_plot <- function(
    gsea_result,
    sources = c("GO:MF", "GO:BP", "GO:CC", "KEGG", "REAC", "WP", "TF", "HP")) {
  source_data <- gsea_source_colors[source %chin% sources]

  source_data[,
    width := gsea_result$meta$result_metadata[[source]]$number_of_terms,
    by = source
  ]

  source_data[seq_len(.N - 1), width := width + 2000]
  source_data[, source_x := cumsum(width) - width]
  source_data[, source_center := source_x + width / 2]

  data <- gsea_result$result |> as.data.table()
  data <- merge(data, source_data, by = "source")
  data[, x := source_x + source_order]
  data[, y := -log10(p_value)]
  data[y > 16, y := 17]

  plotly::plot_ly() |>
    plotly::add_markers(
      data = data,
      x = ~x,
      y = ~y,
      text = ~term_name,
      marker = list(
        size = ~ 4 + 6 * lerp(term_size),
        color = ~color,
        line = list(width = 0)
      ),
      cliponaxis = FALSE
    ) |>
    plotly::layout(
      xaxis = list(
        title = "",
        range = c(0, source_data[.N, source_x + width]),
        tickmode = "array",
        tickvals = source_data[, source_center],
        ticktext = source_data[, source],
        showgrid = FALSE,
        zeroline = FALSE
      ),
      yaxis = list(
        title = "−log₁₀(p)",
        range = c(0, 18),
        tickmode = "array",
        tickvals = c(2, 4, 6, 8, 10, 12, 14, 16),
        ticktext = c("2", "4", "6", "8", "10", "12", "14", "≥ 16")
      ),
      font = list(size = 8),
      margin = list(
        pad = 2,
        l = 0,
        r = 0,
        t = 0,
        b = 0
      )
    )
}

fig_gsea_1_0 <- gsea_plot(gsea_1_0)
fig_gsea_1_1 <- gsea_plot(gsea_1_1)

# Plotly specifies all sizes in pixels, including font size. Because of
# that, we can actually think of these pixels as points. One point is defined as
# 1/72 inch and SVG uses 96 DPI as the standard resolution.
#
# 1 plotly pixel = 1 point = 1/72 inch = 1 1/3 actual pixels
#
# So, we specify width and height in points (= plotly pixels) and scale up the
# image by 96/72 to convert everything from points to pixels at 96 DPI.

plotly::save_image(
  fig_gsea_1_0,
  file = here("scripts/output/gsea_1_0.svg"),
  width = 6.27 * 72,
  height = 3.135 * 72,
  scale = 96 / 72
)

plotly::save_image(
  fig_gsea_1_1,
  file = here("scripts/output/gsea_1_1.svg"),
  width = 6.27 * 72,
  height = 3.135 * 72,
  scale = 96 / 72
)
