library(data.table)
library(gprofiler2)
library(here)

i_am("scripts/cmap_drugs_input.R")

# Source: custom
load(here("scripts", "input", "CMap_20180808.RData"))

data <- CMap$"HT_HG-U133A"
rm(CMap)

transcripts <- dimnames(data)$transcripts
genes <- gconvert(
  transcripts,
  numeric_ns = "ENTREZGENE_ACC",
  mthreshold = 1,
  filter_na = FALSE
)$target
dimnames(data)[[1]] <- genes

data_drugs <- as.data.table(data)
data_drugs <- na.omit(data_drugs)
data_drugs <- data_drugs[data == "logFoldChange", .(transcripts, drugs, value)]

setnames(
  data_drugs,
  c("transcripts", "drugs", "value"),
  c("gene", "drug", "change")
)

genes_0_0 <- scan(here("scripts/output/genes_0_0.txt"), what = character())
genes_0_1 <- scan(here("scripts/output/genes_0_1.txt"), what = character())
genes_1_0 <- scan(here("scripts/output/genes_1_0.txt"), what = character())
genes_1_1 <- scan(here("scripts/output/genes_1_1.txt"), what = character())

data_drugs[gene %chin% genes_0_0, group := "genes_0_0"]
data_drugs[gene %chin% genes_0_1, group := "genes_0_1"]
data_drugs[gene %chin% genes_1_0, group := "genes_1_0"]
data_drugs[gene %chin% genes_1_1, group := "genes_1_1"]

data_drugs <- na.omit(data_drugs)

results <- data_drugs[, .(mean_change = mean(change)), by = .(drug, group)]
fwrite(results, file = here("scripts/output/cmap_drugs.csv"))

write(data_drugs[, unique(drug)], file = here("scripts/output/drugs.txt"))
