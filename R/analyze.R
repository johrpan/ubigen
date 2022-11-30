#' Analyze the provided expression data for ubiquitously expressed genes.
#'
#' @param data A `data.table` in normalized, long format. There should be a
#'   `gene` column containing Ensembl gene IDs, a `sample` column containing
#'   abitrary sample identifiers that are unique per sample and an `expression`
#'   column containing the actual expression value for each given combination
#'   of gene and sample.
#'
#' @return A `data.table` containing all computed values per gene.
#'
#' @export
analyze <- function(data) {
    data[, `:=`(
        expression_median = median(expression),
        expression_95 = quantile(expression, probs = 0.95)
    ), by = sample]

    # Transform the expression logarithmically. The samples that don't express a
    # gene at all will be left out intentionally.
    data[expression > 0, expression_log := log2(expression)]

    results <- data[, .(
        median_expression = median(expression[expression > 0]),
        iqr_expression = IQR(expression[expression > 0]),
        mean_expression = mean(expression[expression > 0]),
        sd_expression = sd(expression[expression > 0]),
        median_expression_normalized = median(expression_log, na.rm = TRUE),
        iqr_expression_normalized = IQR(expression_log, na.rm = TRUE),
        mean_expression_normalized = mean(expression_log, na.rm = TRUE),
        sd_expression_normalized = sd(expression_log, na.rm = TRUE),
        above_zero = mean(expression > 0.0),
        above_threshold = mean(expression > 50.0),
        above_median = mean(expression > expression_median),
        above_95 = mean(expression > expression_95)
    ), by = "gene"]

    results[, `:=`(
        qcv_expression = iqr_expression / median_expression,
        qcv_expression_normalized =
            iqr_expression_normalized / median_expression_normalized,
        cv_expression = sd_expression / mean_expression,
        cv_expression_normalized =
            sd_expression_normalized / mean_expression_normalized
    )]

    # Normalize values to the range of 0.0 to 1.0.
    results[, `:=`(
        median_expression_normalized =
            (median_expression_normalized -
                min(median_expression_normalized, na.rm = TRUE)) /
                (max(median_expression_normalized, na.rm = TRUE) -
                    min(median_expression_normalized, na.rm = TRUE)),
        iqr_expression_normalized =
            (iqr_expression_normalized -
                min(iqr_expression_normalized, na.rm = TRUE)) /
                (max(iqr_expression_normalized, na.rm = TRUE) -
                    min(iqr_expression_normalized, na.rm = TRUE)),
        qcv_expression_normalized =
            (qcv_expression_normalized -
                min(qcv_expression_normalized, na.rm = TRUE)) /
                (max(qcv_expression_normalized, na.rm = TRUE) -
                    min(qcv_expression_normalized, na.rm = TRUE)),
        mean_expression_normalized =
            (mean_expression_normalized -
                min(mean_expression_normalized, na.rm = TRUE)) /
                (max(mean_expression_normalized, na.rm = TRUE) -
                    min(mean_expression_normalized, na.rm = TRUE)),
        sd_expression_normalized =
            (sd_expression_normalized -
                min(sd_expression_normalized, na.rm = TRUE)) /
                (max(sd_expression_normalized, na.rm = TRUE) -
                    min(sd_expression_normalized, na.rm = TRUE)),
        cv_expression_normalized =
            (cv_expression_normalized -
                min(cv_expression_normalized, na.rm = TRUE)) /
                (max(cv_expression_normalized, na.rm = TRUE) -
                    min(cv_expression_normalized, na.rm = TRUE))
    )]

    results
}
