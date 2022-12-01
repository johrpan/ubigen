# This script uses the results (See results.csv) and computes a score for each
# gene. This is the data that will be used in the package.

library(data.table)
library(here)

i_am("scripts/input.R")

data <- fread(here("scripts", "input", "data_long.csv"))
gtex_all <- ubigen::analyze(data)

# To save memory, the data includes fake IDs for genes. The actual Ensembl IDs
# are part of the separate genes table.

genes <- fread(here("scripts", "input", "genes.csv"))

setnames(gtex_all, "gene", "id")

data <- merge(
  gtex_all,
  genes[, .(id, gene)],
  by = "id",
  all.x = TRUE,
  sort = FALSE
)

data[, id := NULL]

usethis::use_data(gtex_all, overwrite = TRUE)

genes[, id := NULL]
usethis::use_data(genes, overwrite = TRUE)
