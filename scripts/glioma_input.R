library(biomaRt)
library(data.table)
library(edgeR)
library(glue)
library(here)
library(jsonlite)
library(purrr)
library(stringr)

i_am("scripts/glioma_input.R")

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

# Read sample index.

metadata <- read_json(
  here("scripts", "input", "metadata.repository.2025-11-27.json")
)

samples <- map(metadata, function(x) {
  list(
    id = x$file_id,
    path = here("scripts", "input", "glioma", x$file_id, x$file_name)
  )
})

read_sample <- function(sample) {
  read_counts <- fread(sample$path)

  setnames(
    read_counts,
    c("gene_id", "gene_name"),
    c("gene", "hgnc_symbol")
  )

  # Only include actual genes.
  read_counts <- read_counts[
    str_starts(gene, "ENSG"),
    .(gene, hgnc_symbol, unstranded)
  ]

  # Remove Ensembl gene version numbers.
  read_counts[, gene := str_split(gene, "\\.") |> map_chr(1)]

  read_counts <- merge(
    read_counts,
    gene_lengths,
    by = "gene",
    all.x = TRUE
  )

  # Remove missing Ensembl genes.
  read_counts <- read_counts[!is.na(length)]

  gene_lengths_ordered <- read_counts$length

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
  read_counts <- read_counts / (gene_lengths_ordered / 1000)

  getpm <- DGEList(counts = read_counts) |>
    calcNormFactors() |>
    cpm()

  result <- data.table(getpm, keep.rownames = "gene")
  setnames(result, "unstranded", "expression")
  result[, sample := sample$id]

  result
}

result <- data.table(
  gene = character(),
  sample = character(),
  expression = double()
)

for (sample in samples) {
  sample_result <- read_sample(sample)

  rlog::log_info(glue(
    "Read sample {sample$id} with {nrow(sample_result)} rows."
  ))

  result <- rbind(result, sample_result)
}

fwrite(result, file = here("scripts", "input", "glioma_long.csv"))
