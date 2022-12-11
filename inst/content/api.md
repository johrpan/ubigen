<style>
h2, h3 {
     margin-top: 64px;
}

table {
     margin-top: 32px;
     margin-bottom: 32px;
}

th, td {
     padding: 4px;
}
</style>

## Using the API for retrieving the data

You can use the API endpoint `/ranking` to download the dataset formatted in
CSV, either with the default parameters, or any combination of parameters. The
following optional query parameters are supported. They work for all queries
to the API.

| Parameter             | Value                                                                                                             | Meaning                                                                  |
| --------------------- | ----------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------ |
| `dataset`             | `gtex_all` (default), `gtex_tissues` or `hpa_tissues`                                                             | Which expression dataset to use as the base for all analyses.            |
| `cross_sample_metric` | `above_95` (default), `above_zero` or `above_median`                                                              | How to determine which samples to include for a genes sample proportion. |
| `level_metric`        | `median_expression_normalized ` (default), `mean_expression_normalized`, `median_expression` or `mean_expression` | Metric for assessing the overall expression level.                       |
| `variation_metric`    | `iqr_expression_normalized` (default), `sd_expression_normalized`, `iqr_expression` or `sd_expression`            | Metric for assessing the expression variation between samples.           |
| `cross_sample_weight` | numeric (default: `0.5`)                                                                                          | Weight of the cross-sample metric for the final score.                   |
| `level_weight`        | numeric (default: `0.25`)                                                                                         | Weight of the expression level metric for the final score.               |
| `variation_weight`    | numeric (default: `-0.25`)                                                                                        | Weight of the cross-sample metric for the final score.                   |

Example using cURL:

```bash
# Download the ranking based on default parameters.
curl "https://ubigen.uni-rostock.de/api/ranking" > ubigen_default.csv

# Use an alternative expression dataset.
curl "https://ubigen.uni-rostock.de/api/ranking?dataset=hpa_tissues" > ubigen_hpa.csv

# Ignore expression variation and focus on the mean expression level.
curl "https://ubigen.uni-rostock.de/api/ranking?level_metric=mean_expression_normalized?variation_weight=0" > ubigen_custom.csv
```

## Using the API to analyze your genes

Custom genes can be submitted to the API using a `POST` request. Include your
genes of interest as a whitespace separated list in the request body.

*Do not forget to add the correct `Content-Type: text/plain` header (see
examples)!*

### Limit ranking to selected genes

For downloading the ranking data for a user defined gene set, use a `POST`
request on the `/ranking` endpoint. This supports all query parameters to
customize the ranking as defined above.

Example using cURL:

```bash
# Download the data for five random genes using default parameters.
curl -X POST \
     -H "Content-Type: text/plain" \
     -d "ENSG00000168907 ENSG00000182872 ENSG00000188763 ENSG00000196531 ENSG00000161638" \
     "https://ubigen.uni-rostock.de/api/ranking" > results.csv
```

### Summarize information on your genes

If you are interested in a summary of the properties of your custom gene set in
relation to other genes within the ranking, use a `POST` request to the
`/summary` endpoint. This supports all query parameters to customize the ranking
as defined above.

The request returns a JSON map with the following values:

| Value                    | Meaning                                                                                                                             |
| ------------------------ | ----------------------------------------------------------------------------------------------------------------------------------- |
| `median_percentile`      | Median percentile of the selected genes within the ranking.                                                                         |
| `median_score`           | Median score of the selected genes.                                                                                                 |
| `median_score_reference` | Median score of all other genes.                                                                                                    |
| `p_value`                | p-value for the alternative hypothesis that the selected genes have different scores than the other genes (Wilcoxon rank sum test). |
| `change`                 | Estimate of the effect size (Wilcoxon rank sum test).                                                                               |
| `conf_int_lower`         | Lower bound of the estimated 95% confidence interval (Wilcoxon rank sum test).                                                      |
| `conf_int_upper`         | Upper bound of the estimated 95% confidence interval (Wilcoxon rank sum test).                                                      |

Example using cURL:

```bash
# Summarize the results for five random genes using default parameters.
curl -X POST \
     -H "Content-Type: text/plain" \
     -d "ENSG00000168907 ENSG00000182872 ENSG00000188763 ENSG00000196531 ENSG00000161638" \
     "https://ubigen.uni-rostock.de/api/summary"
```

The above command generates a JSON map like this:

```json
{
    "median_percentile": 0.8859,
    "median_score": 0.2567,
    "median_score_reference": 0.1934,
    "p_value": 0.0013,
    "change": 0.0875,
    "conf_int_lower": 0.0456,
    "conf_int_upper": 0.4808
}
```
