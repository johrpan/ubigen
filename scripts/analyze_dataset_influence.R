library(data.table)
library(here)

i_am("scripts/analyze_dataset_sectors.R")

ranking_gtex_a <- ubigen::rank_genes(ubigen::gtex_all)
ranking_cmap_a <- ubigen::rank_genes(ubigen::cmap)

data_a <- merge(
  ranking_gtex_a[, .(gene, score, percentile)],
  ranking_cmap_a[, .(gene, score, percentile)],
  by = "gene",
  suffixes = c(x = "_gtex", y = "_cmap")
)

ranking_gtex_b <- ubigen::rank_genes(
  ubigen::gtex_all,
  level_weight = 0.0,
  variation_weight = 0.0
)

ranking_cmap_b <- ubigen::rank_genes(
  ubigen::cmap,
  level_weight = 0.0,
  variation_weight = 0.0
)

data_b <- merge(
  ranking_gtex_b[, .(gene, score, percentile)],
  ranking_cmap_b[, .(gene, score, percentile)],
  by = "gene",
  suffixes = c(x = "_gtex", y = "_cmap")
)

genes_11_a <- data_a[percentile_gtex >= 0.95 & percentile_cmap >= 0.95]
genes_11_b <- data_b[percentile_gtex >= 0.95 & percentile_cmap >= 0.95]

b <- length(intersect(genes_11_a$gene, genes_11_b$gene))
a <- nrow(genes_11_a)
c <- b / a
