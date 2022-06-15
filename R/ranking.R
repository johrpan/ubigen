#' Rank genes based on how ubiquitous they are.
#'
#' This function will compute a weighted average across multiple metrics that
#' define how ubiquitous a gene is based on its expression across samples.
#'
#' @param cross_sample_metric Metric to use for calculating the number of
#'   samples a gene is expressed in. One of `above_95`, `above_median` or
#'   `above_zero`.
#' @param cross_sample_weight Weighting of the cross sample metric within the
#'   final score.
#' @param mean_expression_weight Weighting of the gene's mean expression within
#'   the final score.
#' @param sd_expression_weight Weighting of the standard deviation of the
#'   gene's expression within the final score.
#'
#' @return A `data.table` with gene data as well as the scores, ranks and
#'   percentiles for each gene.
#'
#' @export
rank_genes <- function(cross_sample_metric = "above_95",
                       cross_sample_weight = 0.5,
                       mean_expression_weight = 0.25,
                       sd_expression_weight = -0.25) {
  total_weight <- cross_sample_weight +
    mean_expression_weight +
    sd_expression_weight

  data <- copy(ubigen::genes)

  data[, score :=
    (cross_sample_weight * get(cross_sample_metric) +
      mean_expression_weight * mean_expression_normalized +
      sd_expression_weight * sd_expression_normalized) /
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
