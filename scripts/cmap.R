library(data.table)
library(gprofiler2)
library(here)

i_am("scripts/cmap.R")

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

d <- as.data.table(data)
d <- na.omit(d)
d <- d[data == "logMean", .(transcripts, drugs, value)]

setnames(
  d,
  c("transcripts", "drugs", "value"),
  c("gene", "sample", "expression")
)

cmap <- ubigen::analyze(d)
usethis::use_data(cmap, overwrite = TRUE)