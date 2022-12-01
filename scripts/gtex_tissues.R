library(data.table)
library(here)

i_am("scripts/gtex_tissues.R")

# Source: https://storage.googleapis.com/gtex_analysis_v8/rna_seq_data/GTEx_Analysis_2017-06-05_v8_RNASeQCv1.1.9_gene_median_tpm.gct.gz
data <- fread(here("scripts", "input", "gtex_tissues.tsv"))
data[, Description := NULL]
setnames(data, "Name", "gene")

# Remove Ensembl gene version numbers.
data[, gene := stringr::str_split(gene, "\\.") |> purrr::map_chr(1)]

data <- melt(
    data,
    id.vars = "gene",
    variable.name = "sample",
    value.name = "expression",
    variable.factor = FALSE
)

gtex_tissues <- ubigen::analyze(data)
usethis::use_data(gtex_tissues, overwrite = TRUE)
