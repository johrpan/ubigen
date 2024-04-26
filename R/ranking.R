#' Rank genes based on how ubiquitous they are.
#'
#' This function will compute a weighted average across multiple metrics that
#' define how ubiquitous a gene is based on its expression across samples.
#'
#' @param data The input data to use. This should either be the result of a
#'   previous call to this function or the return value of [analyze()].
#' @param cross_sample_metric Name of the column that should be used as the
#'   metric measuring the expression across samples.
#' @param cross_sample_weight Relative weight that should be assigned to the
#'   cross sample metric.
#' @param level_metric Name of the column that should be used to represent
#'   overall expression levels.
#' @param level_weight Relative weight that should be assigned to the level
#'   metric.
#' @param variation_metric Name of the column that should be used as the metric
#'   representing variation in expression.
#' @param variation_weight Relative weight that should be assigned to the
#'   variation metric.
#'
#' @return A `data.table` with gene data as well as the scores, ranks and
#'   percentiles for each gene.
#'
#' @export
rank_genes <- function(data = ubigen::gtex_all,
                       cross_sample_metric = "above_95",
                       cross_sample_weight = 0.5,
                       level_metric = "median_expression_normalized",
                       level_weight = 0.25,
                       variation_metric = "qcv_expression_normalized",
                       variation_weight = -0.25) {
  data <- copy(data)

  total_weight <- abs(cross_sample_weight) +
    abs(level_weight) +
    abs(variation_weight)

  data[, score :=
    (cross_sample_weight * get(cross_sample_metric) +
      level_weight * get(level_metric) +
      variation_weight * get(variation_metric)) /
      total_weight]

  # Normalize scores to be between 0.0 and 1.0.
  data[, score := (score - min(score, na.rm = TRUE)) /
    (max(score, na.rm = TRUE) - min(score, na.rm = TRUE))]

  # Exclude genes that are not expressed at all.
  data <- data[!is.na(score)]

  setorder(data, -score)
  data[, rank := .I]
  data[, percentile := 1 - rank / max(rank)]

  data
}
