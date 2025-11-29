library(data.table)
library(here)

i_am("scripts/glioma.R")

data <- fread(here("scripts", "input", "glioma_long.csv"))
glioma <- ubigen::analyze(data)

usethis::use_data(glioma, overwrite = TRUE)
