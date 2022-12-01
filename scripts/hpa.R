library(data.table)
library(here)

i_am("scripts/hpa.R")

# Source: https://www.proteinatlas.org/download/rna_tissue_hpa.tsv.zip
data <- fread(here("scripts", "input", "rna_tissue_hpa.tsv"))
setnames(data, c("Gene", "Tissue", "nTPM"), c("gene", "sample", "expression"))
data[, `:=`("Gene name" = NULL, TPM = NULL, pTPM = NULL)]

hpa_tissues <- ubigen::analyze(data)
usethis::use_data(hpa_tissues, overwrite = TRUE)