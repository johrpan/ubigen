# This script reads data from GTEx and transforms it into various formats for
# further analysis. Note that this requires very good computational resources
# and especially a lot of RAM because of the size of the data.

library(biomaRt)
library(edgeR)
library(data.table)
library(here)

i_am("scripts/gtex_all_input.R")

# Source: https://storage.googleapis.com/gtex_analysis_v8/rna_seq_data/
# GTEx_Analysis_2017-06-05_v8_RNASeQCv1.1.9_gene_reads.gct.gz
read_counts <- fread(here("scripts", "input", "gtex_read_counts.tsv.gz"))

setnames(
  read_counts,
  c("Name", "Description"),
  c("gene", "hgnc_symbol")
)

# Remove Ensembl gene version numbers.
read_counts[, gene := stringr::str_split(gene, "\\.") |> purrr::map_chr(1)]

# Get gene lengths from Ensembl.

ensembl <- useEnsembl("ensembl", version = 107)
dataset <- useDataset("hsapiens_gene_ensembl", mart = ensembl)
gene_lengths <- data.table(getBM(
  attributes = c(
    "ensembl_gene_id",
    "start_position",
    "end_position"
  ),
  mart = dataset
))

setnames(gene_lengths, "ensembl_gene_id", "gene")

gene_lengths[, length := end_position - start_position]

read_counts <- merge(
  read_counts,
  gene_lengths,
  by = "gene",
  all.x = TRUE
)

read_counts <- read_counts[!is.na(length)]
hgnc_symbols <- read_counts$hgnc_symbol
gene_lengths <- read_counts$length

read_counts <- as.matrix(
  read_counts[, !c(
    "hgnc_symbol",
    "start_position",
    "end_position",
    "length"
  )],
  rownames = "gene"
)

# Normalize read counts for gene lengths
read_counts <- read_counts / (gene_lengths / 1000)

getpm <- DGEList(counts = read_counts) |>
  calcNormFactors() |>
  cpm()

data_wide_samples <- data.table(getpm, keep.rownames = "gene")
data_wide_samples[, hgnc_symbol := hgnc_symbols]

# Create lookup tables for genes and samples.

genes <- data_wide_samples[, .(id = .I, gene, hgnc_symbol)]
fwrite(genes, file = here("scripts", "input", "genes.csv"))

sample_names <- colnames(data_wide_samples[, !c("gene", "hgnc_symbol")])
samples <- data.table(id = seq_along(sample_names), sample = sample_names)
fwrite(samples, file = here("scripts", "input", "samples.csv"))

data_wide_samples[, `:=`(gene = .I, hgnc_symbol = NULL)]
colnames(data_wide_samples) <- c("gene", seq_along(sample_names))

data_long <- melt(
  data_wide_samples,
  id.vars = "gene",
  variable.name = "sample",
  value.name = "expression",
  variable.factor = FALSE
)

fwrite(data_long, file = here("scripts", "input", "data_long.csv"))
