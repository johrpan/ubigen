# This scripts reads the input data (See input.R) and performs various
# computations on it in order to later use the results for computating scores
# for ubuiquitously expressed genes.

library(data.table)
library(here)

i_am("scripts/input.R")

data <- fread(here("scripts", "input", "data_long.csv"))
results <- ubigen::analyze(data)
fwrite(results, file = here("scripts", "output", "results.csv"))
