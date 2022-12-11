# This file contains an API specification for Ubigen that can be served using
# the `plumber` package.

library(data.table)
library(plumber)

#' Retrieve a ranking of genes based on their ubiquity.
#'
#' Provide a whitespace separated list of gene IDs as the request content to
#' filter the results based on these genes.
#'
#' @param dataset The predefined dataset to use. This may be one of
#'   `gtex_all`, `gtex_tissues` or `hpa_tissues`.
#'
#' @get /ranking
#' @post /ranking
#' @parser text
#' @serializer csv
ranking <- function(req,
                    dataset = "gtex_all",
                    cross_sample_metric = "above_95",
                    cross_sample_weight = 0.5,
                    level_metric = "median_expression_normalized",
                    level_weight = 0.25,
                    variation_metric = "qcv_expression_normalized",
                    variation_weight = -0.25) {
    ranking <- ubigen::rank_genes(
        data = {
            analysis <- if (dataset == "gtex_tissues") {
                ubigen::gtex_tissues
            } else if (dataset == "hpa_tissues") {
                ubigen::hpa_tissues
            } else {
                ubigen::gtex_all
            }

            merge(analysis, ubigen::genes, by = "gene")
        },
        cross_sample_metric = cross_sample_metric,
        cross_sample_weight = as.numeric(cross_sample_weight),
        level_metric = level_metric,
        level_weight = as.numeric(level_weight),
        variation_metric = variation_metric,
        variation_weight = as.numeric(variation_weight)
    )

    if (length(req$body) >= 1) {
        inputs <- unique(strsplit(req$body, "\\s+")[[1]])
        inputs <- inputs[inputs != ""]
        ranking[gene %chin% inputs]
    } else {
        ranking
    }
}

#' Get a summary of the ubiquity of a given gene set.
#'
#' Provide a whitespace separated list of gene IDs as the request content to
#' analyze these genes.
#'
#' @param dataset The predefined dataset to use. This may be one of
#'   `gtex_all`, `gtex_tissues` or `hpa_tissues`.
#'
#' @post /summary
#' @parser text
#' @serializer json
summary <- function(req,
                    res,
                    dataset = "gtex_all",
                    cross_sample_metric = "above_95",
                    cross_sample_weight = 0.5,
                    level_metric = "median_expression_normalized",
                    level_weight = 0.25,
                    variation_metric = "qcv_expression_normalized",
                    variation_weight = -0.25) {
    ranking <- ubigen::rank_genes(
        data = {
            analysis <- if (dataset == "gtex_tissues") {
                ubigen::gtex_tissues
            } else if (dataset == "hpa_tissues") {
                ubigen::hpa_tissues
            } else {
                ubigen::gtex_all
            }

            merge(analysis, ubigen::genes, by = "gene")
        },
        cross_sample_metric = cross_sample_metric,
        cross_sample_weight = as.numeric(cross_sample_weight),
        level_metric = level_metric,
        level_weight = as.numeric(level_weight),
        variation_metric = variation_metric,
        variation_weight = as.numeric(variation_weight)
    )

    if (length(req$body) >= 1) {
        inputs <- unique(strsplit(req$body, "\\s+")[[1]])
        inputs <- inputs[inputs != ""]

        reference_scores <- ranking[!gene %chin% inputs, score]
        comparison_score <- ranking[gene %chin% inputs, score]
        comparison_percentile <- ranking[gene %chin% inputs, percentile]

        test_result <- stats::wilcox.test(
            x = comparison_score,
            y = reference_scores,
            conf.int = TRUE
        )

        list(
            "median_percentile" = stats::median(comparison_percentile) |>
                jsonlite::unbox(),
            "median_score" = stats::median(comparison_score) |>
                jsonlite::unbox(),
            "median_score_reference" = stats::median(reference_scores) |>
                jsonlite::unbox(),
            "p_value" = test_result$p.value |> jsonlite::unbox(),
            "change" = test_result$estimate |> jsonlite::unbox(),
            "conf_int_lower" = test_result$conf.int[1] |> jsonlite::unbox(),
            "conf_int_upper" = test_result$conf.int[2] |> jsonlite::unbox()
        )
    } else {
        res$status <- 400
        paste0(
            "Please provide a whitespace separated list of Ensembl gene IDs ",
            "in the request body."
        ) |> jsonlite::unbox()
    }
}
