library(data.table)
library(glue)
library(here)

i_am("scripts/validate_glioma.R")

ranking_gtex <- ubigen::rank_genes(ubigen::gtex_all)
ranking_cmap <- ubigen::rank_genes(ubigen::cmap)
ranking_glioma <- ubigen::rank_genes(ubigen::glioma)

data <- merge(
  ranking_gtex[, .(gene, score, percentile)],
  ranking_cmap[, .(gene, score, percentile)],
  by = "gene",
  suffixes = c(x = "_gtex", y = "_cmap")
)

gtex_genes <- data[percentile_gtex >= 0.95, gene]

sector_11_genes <- data[
  percentile_gtex >= 0.95 & percentile_cmap >= 0.95,
  gene
]

glioma_gtex <-
  ranking_glioma[gene %chin% gtex_genes, sum(percentile >= 0.95) / .N]
glioma_sector_11 <-
  ranking_glioma[gene %chin% sector_11_genes, sum(percentile >= 0.95) / .N]

glue("“GTEx (all)” genes ubiquitous in glioma: {glioma_gtex}")
glue("Sector 11 genes ubiquitous in glioma: {glioma_sector_11}")
