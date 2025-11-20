library(data.table)

data <- data.table(
  cross_sample_weight = double(),
  level_weight = double(),
  variation_weight = double(),
  genes = list(character())
)

cross_sample_weights <- seq(0.4, 0.6, 0.02)
level_weights <- seq(0.2, 0.3, 0.01)
variation_weights <- seq(-0.3, -0.2, 0.01)

n_total <- length(cross_sample_weights) *
  length(level_weights) *
  length(variation_weights)

progress <- txtProgressBar(
  min = 1,
  max = n_total,
  style = 3
)

n_done <- 0

for (cross_sample_weight in cross_sample_weights) {
  for (level_weight in level_weights) {
    for (variation_weight in variation_weights) {
      n_done <- n_done + 1

      ranking_gtex <- ubigen::rank_genes(
        cross_sample_weight = cross_sample_weight,
        level_weight = level_weight,
        variation_weight = variation_weight
      )

      ranking_cmap <- ubigen::rank_genes(
        data = ubigen::cmap,
        cross_sample_weight = cross_sample_weight,
        level_weight = level_weight,
        variation_weight = variation_weight
      )

      genes_11 <- intersect(
        ranking_gtex[percentile >= 0.95, gene],
        ranking_cmap[percentile >= 0.95, gene]
      )

      data <- rbind(data, data.table(
        cross_sample_weight = cross_sample_weight,
        level_weight = level_weight,
        variation_weight = variation_weight,
        genes = list(genes_11)
      ))

      setTxtProgressBar(progress, n_done)
    }
  }
}

close(progress)

# All gene lists have the same length
n_95 <- length(data$genes[[1]])
common <- Reduce(intersect, data$genes)
length(common) / n_95
