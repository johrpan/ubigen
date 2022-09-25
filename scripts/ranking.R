# This script uses the results (See results.csv) and computes a score for each
# gene. This is the data that will be used in the package.

library(data.table)
library(here)

i_am("scripts/input.R")

genes <- fread(here("scripts", "input", "genes.csv"))
data <- fread(here("scripts", "output", "results.csv"))

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

# Reintroduce gene IDs and HGNC symbols.

setnames(data, "gene", "id")

data <- merge(
  data,
  genes,
  by = "id",
  all.x = TRUE,
  sort = FALSE
)

setnames(data, "hgnc_symbol", "hgnc_name")
data[, id := NULL]

# Remove duplicates. This will keep the best row for each duplicated gene.
data <- unique(data, by = "gene")

data[, rank := .I]

fwrite(data, file = here("scripts", "output", "genes.csv"))
