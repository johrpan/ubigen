#' Rank genes based on how ubiquitous they are.
#'
#' This function will compute a weighted average across multiple metrics that
#' define how ubiquitous a gene is based on its expression across samples.
#'
#' @return A `data.table` with gene data as well as the scores, ranks and
#'   percentiles for each gene.
#'
#' @export
rank_genes <- function(cross_sample_metric = "above_95",
                       cross_sample_weight = 0.5,
                       level_metric = "median_expression_normalized",
                       level_weight = 0.25,
                       variation_metric = "qcv_expression_normalized",
                       variation_weight = -0.25) {
  total_weight <- abs(cross_sample_weight) +
    abs(level_weight) +
    abs(variation_weight)

  data <- copy(ubigen::genes)

  data[, score :=
    (cross_sample_weight * get(cross_sample_metric) +
      level_weight * get(level_metric) +
      variation_weight * get(variation_metric)) /
      total_weight]

  # Normalize scores to be between 0.0 and 1.0.
  data[, score := (score - min(score, na.rm = TRUE)) /
    (max(score, na.rm = TRUE) - min(score, na.rm = TRUE))]

  # These are genes that are not expressed at all.
  data[is.na(score), score := 0.0]

  setorder(data, -score)
  data[, rank := .I]
  data[, percentile := 1 - rank / max(rank)]

  data
}
