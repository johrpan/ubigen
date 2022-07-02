# This script uses the results (See results.csv) and computes a score for each
# gene. This is the data that will be used in the package.

library(data.table)
library(here)

i_am("scripts/input.R")

data <- fread(here("scripts", "output", "results.csv"))

# Keep only the actual Ensembl ID for each gene.
data[, gene := stringr::str_split(gene, "\\.") |> purrr::map_chr(1)]

data[, score := 0.5 * above_95 +
  0.25 * mean_expression_normalized +
  -0.25 * sd_expression_normalized]

# Normalize scores to be between 0.0 and 1.0.
data[, score := (score - min(score, na.rm = TRUE)) /
  (max(score, na.rm = TRUE) - min(score, na.rm = TRUE))]

# These are genes that are not expressed at all or expressed just once, in case
# the standard deviation is used in the score.
data[is.na(score), score := 0.0]

setorder(data, -score)

# Remove duplicates. This will keep the best row for each duplicated gene.
data <- unique(data, by = "gene")

data[, `:=`(
  hgnc_name = gprofiler2::gconvert(
    gene,
    target = "HGNC",
    mthreshold = 1,
    filter_na = FALSE
  )$target,
  rank = .I
)]

fwrite(data, file = here("scripts", "output", "genes.csv"))
