library(data.table)
library(here)

i_am("scripts/sliding_gsea_publication.R")

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

sliding_gsea <- function(ranking, bucket_size_percent = 2.5) {
  data <- copy(ranking)
  data[, bucket := floor(percentile * 100 / bucket_size_percent)]
  data[, bucket := (bucket * bucket_size_percent) / 100]

  result <- data[, .(analysis = list(gprofiler2::gost(
    gene,
    custom_bg = ranking$gene,
    domain_scope = "custom_annotated"
  ))), by = bucket]

  result[, result := lapply(analysis, function(a) a$result)]
  result <- result[, rbindlist(result), by = bucket]

  result[, .(count = .N), by = c("bucket", "source")]
}

sliding_gsea_plot <- function(
    gsea_data,
    sources = c("GO:MF", "GO:BP", "GO:CC", "KEGG", "REAC", "WP", "TF", "HP")) {
  data <- merge(
    gsea_data,
    gsea_source_colors[source %chin% sources],
    by = "source"
  )

  data[, source := factor(
    source,
    levels = gsea_source_colors[, source]
  )]

  setorder(data, source, bucket)

  plotly::plot_ly() |>
    plotly::add_bars(
      data = data,
      x = ~bucket,
      y = ~count,
      color = ~source,
      marker = ~ list(color = color)
    ) |>
    plotly::layout(
      xaxis = list(
        title = "Bucket of genes (Percentile)",
        tickformat = ".0%"
      ),
      yaxis = list(title = "Number of associated terms"),
      barmode = "stack",
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

data_gtex <- sliding_gsea(ubigen::rank_genes(ubigen::gtex_all))
data_cmap <- sliding_gsea(ubigen::rank_genes(ubigen::cmap))

fig_gtex <- sliding_gsea_plot(data_gtex)
fig_cmap <- sliding_gsea_plot(data_cmap)

# Plotly specifies all sizes in pixels, including font size. Because of
# that, we can actually think of these pixels as points. One point is defined as
# 1/72 inch and SVG uses 96 DPI as the standard resolution.
#
# 1 plotly pixel = 1 point = 1/72 inch = 1 1/3 actual pixels
#
# So, we specify width and height in points (= plotly pixels) and scale up the
# image by 96/72 to convert everything from points to pixels at 96 DPI.

plotly::save_image(
  fig_gtex |> plotly::hide_legend(),
  file = here("scripts/output/sliding_gsea_gtex.svg"),
  width = 6.27 * 72,
  height = 3.135 * 72,
  scale = 96 / 72
)

plotly::save_image(
  fig_cmap |> plotly::hide_legend(),
  file = here("scripts/output/sliding_gsea_cmap.svg"),
  width = 6.27 * 72,
  height = 3.135 * 72,
  scale = 96 / 72
)

plotly::save_image(
  fig_gtex,
  file = here("scripts/output/sliding_gsea_legend.svg"),
  width = 6.27 * 72,
  height = 3.135 * 72,
  scale = 96 / 72
)
