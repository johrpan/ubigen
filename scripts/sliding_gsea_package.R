# This script performs a gene set enrichment analysis (GSEA) across the whole
# ranking of ubiquituos genes using g:Profiler.

# Size of each gene bucket. The GSEA is done once for each bucket within the
# ranking.
bucket_size <- 500

library(data.table)
library(here)

i_am("scripts/sliding_gsea_package.R")
file_path <- here("scripts", "output", "ubigen_gsea.Rds")
image_path <- here("scripts", "output", "ubigen_gsea.svg")

# The result will be saved in `file_path` to avoid unnecessary API calls.
result <- if (file.exists(file_path)) {
  readRDS(file_path)
} else {
  data <- copy(ubigen::genes)
  data[, bucket := ceiling(rank / bucket_size)]

  result <- data[, .(analysis = list(gprofiler2::gost(gene))), by = bucket]
  saveRDS(result, file = file_path)

  result
}

result[, result := lapply(analysis, function(a) a$result)]
result <- result[, rbindlist(result), by = bucket]

data <- result[, .(count = .N), by = c("bucket", "source")]
data[, total := sum(count), by = bucket]

smooth_model <- loess(total ~ bucket, data, span = 0.3)
bucket_smoothed <- seq(1, nrow(data), 0.1)
total_smoothed <- predict(smooth_model, bucket_smoothed)

fig <- plotly::plot_ly(data) |>
  plotly::add_bars(
    x = ~bucket,
    y = ~count,
    color = ~source
  ) |>
  plotly::add_lines(
    x = bucket_smoothed,
    y = total_smoothed,
    name = "All (smoothed)"
  ) |>
  plotly::layout(
    xaxis = list(title = glue::glue("Bucket of genes (n = {bucket_size})")),
    yaxis = list(title = "Number of associated terms"),
    barmode = "stack",
    legend = list(title = list(text = "<b>Source of term</b>"))
  )

plotly::save_image(fig, image_path, width = 1200, height = 800)

gsea_plot_ranking <- fig
usethis::use_data(gsea_plot_ranking, internal = TRUE, overwrite = TRUE)
