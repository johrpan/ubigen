# This script uses the results (See results.csv) and computes a score for each
# gene. This is the data that will be used in the package.

library(data.table)
library(here)

i_am("scripts/input.R")

# To save memory, the data includes fake IDs for genes. The actual Ensembl IDs
# are part of the separate genes table.

genes <- fread(here("scripts", "input", "genes.csv"))
data <- fread(here("scripts", "output", "results.csv"))

# Rank the data using default parameters.
data <- ubigen::rank_genes(data = data)

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

# Reassign ranks, because duplicates may have been removed.
data[, rank := .I]

fwrite(data, file = here("scripts", "output", "genes.csv"))
