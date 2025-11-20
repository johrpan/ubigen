library(data.table)

data <- data.table(
  cross_sample_weight = double(),
  level_weight = double(),
  variation_weight = double(),
  above_50 = double()
)

cross_sample_weights <- seq(0.05, 1.0, 0.05)
level_weights <- seq(0.05, 1.0, 0.05)
variation_weights <- seq(-1.0, -0.05, 0.05)

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

      ranking <- ubigen::rank_genes(
        cross_sample_weight = cross_sample_weight,
        level_weight = level_weight,
        variation_weight = variation_weight
      )

      n_genes <- nrow(ranking)

      data <- rbind(data, data.table(
        cross_sample_weight = cross_sample_weight,
        level_weight = level_weight,
        variation_weight = variation_weight,
        above_50 = ranking[score >= 0.5, .N / n_genes]
      ))

      setTxtProgressBar(progress, n_done)
    }
  }
}

close(progress)

filtered <- data[above_50 >= 0.04 & above_50 <= 0.06]