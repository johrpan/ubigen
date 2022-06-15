# This script performs a gene set enrichment analysis (GSEA) across the whole
# ranking of ubiquituos genes using g:Profiler.

# Size of each gene bucket. The GSEA is done once for each bucket within the
# ranking.
bucket_size <- 500

library(data.table)
library(here)

i_am("scripts/gsea.R")
file_path <- here("scripts", "output", "ubigen_gsea.Rds")
image_path <- here("scripts", "output", "ubigen_gsea.svg")

# The result will be saved in `file_path` to avoid unnecessary API calls.
result <- if (file.exists(file_path)) {
  readRDS(file_path)
} else {
  data <- copy(ubigen::genes)
  data[, bucket := ceiling(rank / bucket_size)]

  result <- data[, .(analysis = list(gprofiler2::gost(gene))), by = bucket]
  result[, count := nrow(analysis[[1]]$result), by = bucket]
  result[is.na(count), count := 0]

  saveRDS(result, file = file_path)

  result
}

fig <- plotly::plot_ly() |>
  plotly::add_bars(
    data = result,
    x = ~bucket,
    y = ~count
  ) |>
  plotly::layout(
    xaxis = list(title = "Bucket of genes (n = 500)"),
    yaxis = list(title = "Number of associated terms")
  )

plotly::save_image(fig, image_path, width = 1200, height = 800)
