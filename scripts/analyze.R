# This scripts reads the input data (See input.R) and performs various
# computations on it in order to later use the results for computating scores
# for ubuiquitously expressed genes.

library(data.table)
library(here)

i_am("scripts/input.R")

data <- fread(here("scripts", "input", "data_long.csv"))

data[, `:=`(
    expression_median = median(expression),
    expression_95 = quantile(expression, probs = 0.95)
), by = sample]

results <- data[, .(
    median_expression = median(expression),
    mean_expression = mean(expression),
    sd_expression = sd(expression),
    above_zero = mean(expression > 0.0),
    above_threshold = mean(expression > 50.0),
    above_median = mean(expression > expression_median),
    above_95 = mean(expression > expression_95)
), by = "gene"]

fwrite(results, file = here("scripts", "output", "results.csv"))
