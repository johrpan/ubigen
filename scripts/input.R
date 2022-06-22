# This script reads data from GTEx and transforms it into various formats for
# further analysis. Note that this requires very good computational resources
# and especially a lot of RAM because of the size of the data.

library(data.table)
library(here)

i_am("scripts/input.R")

# Source: https://storage.googleapis.com/gtex_analysis_v8/rna_seq_data/
# GTEx_Analysis_2017-06-05_v8_RNASeQCv1.1.9_gene_tpm.gct.gz
# The file has been edited removing the lines above the column headers.
data_wide_samples <- fread(here("scripts", "input", "gtex.tsv.gz"))

setnames(
  data_wide_samples,
  c("Name", "Description"),
  c("gene", "hgnc_symbol")
)

data_long <- melt(
  data_wide_samples,
  id.vars = c("gene", "hgnc_symbol"),
  variable.name = "sample",
  value.name = "expression",
  variable.factor = FALSE
)

fwrite(
  data_wide_samples,
  file = here(
    "scripts",
    "input",
    "data_wide_samples.csv"
  )
)

fwrite(data_long, file = here("scripts", "input", "data_long.csv"))
