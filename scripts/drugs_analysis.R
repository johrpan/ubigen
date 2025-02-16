library(data.table)
library(here)

i_am("scripts/drugs_analysis.R")

drugs_cmap <- fread(here("scripts/output/drugs_cmap.csv"))

# Only keep significant changes
drugs_cmap <- drugs_cmap[p_value <= 0.05]

# Keep one row per gene and drug, with the most significant change.
setkey(drugs_cmap, gene, drug, p_value)
drugs_cmap <- drugs_cmap[
  rowid(gene, drug) == 1,
  .(gene, drug, log_fold_change, p_value)
]

drugs_cmap[, negative_log_10_p := -log10(p_value)]

ranking_data <- fread(here("scripts/output/gsea_vs_cmap_groups.csv"))
n_ubiquitous <- ranking_data[percentile_gtex >= 0.95, .N]
n_non_ubiquitous <- ranking_data[percentile_gtex < 0.95, .N]
data <- merge(drugs_cmap, ranking_data, by = "gene")

drugs <- fread(here("scripts/output/drugs.csv"), na.strings = "")
data <- merge(data, drugs, by = "drug", all.x = TRUE, allow.cartesian = TRUE)

# Use CMap names as fallback (for drugs not present in drugs.csv above)
data[is.na(name), name := stringr::str_to_sentence(drug)]

# Figures for single drugs

results_drugs <- unique(data, by = c("drug", "gene"))
results_drugs[,
  `:=`(
    proportion_ubiquitous =
      .SD[percentile_gtex >= 0.95, .N / n_ubiquitous],
    proportion_non_ubiquitous =
      .SD[percentile_gtex < 0.95, .N / n_non_ubiquitous],
    drug_score_gtex = weighted.mean(score_gtex, abs(log_fold_change)),
    drug_score_cmap = weighted.mean(score_cmap, abs(log_fold_change))
  ),
  by = drug
]

results_drugs[, bias := proportion_ubiquitous / proportion_non_ubiquitous]
setorder(results_drugs, -bias)

results_drugs_unique <- unique(results_drugs, by = "drug")

# Exclude some exotic drugs
results_drugs_unique <- results_drugs_unique[!is.na(indication)]

n_drugs <- nrow(results_drugs_unique)
selected_drugs <- c(
  results_drugs_unique[1:10, drug],
  results_drugs_unique[(n_drugs - 9):n_drugs, drug]
)

fig_drug_scores_new <- plotly::plot_ly(results_drugs_unique) |>
  plotly::add_markers(
    x = ~drug_score_gtex,
    y = ~drug_score_cmap,
    text = ~name,
    marker = list(size = 4)
  ) |>
  plotly::layout(
    xaxis = list(
      title = "Score based on GTEx (all)"
    ),
    yaxis = list(
      title = "Score based on CMap"
    )
  )

# To not overwrite other data:
load(here("R/sysdata.rda"))
fig_drug_scores <- fig_drug_scores_new

usethis::use_data(
  fig_drug_scores,
  gsea_plot_ranking, # From R/sysdata.rda
  internal = TRUE,
  overwrite = TRUE
)

results_drugs_unique <- results_drugs_unique[drug %chin% selected_drugs]

fig_drugs <- plotly::plot_ly(results_drugs_unique) |>
  plotly::add_bars(
    x = ~proportion_ubiquitous,
    y = ~name
  ) |>
  plotly::add_bars(
    x = ~ -proportion_non_ubiquitous,
    y = ~name
  ) |>
  plotly::layout(
    xaxis = list(
      range = c(-0.8, 0.8),
      title = "Proportion of genes that are influenced significantly",
      tickformat = ".0%"
    ),
    yaxis = list(
      categoryarray = rev(results_drugs_unique[, name]),
      title = ""
    ),
    barmode = "relative",
    showlegend = FALSE,
    font = list(size = 8),
    margin = list(
      pad = 2,
      l = 0,
      r = 0,
      t = 0,
      b = 36
    )
  )

# Figure for mechanisms of action

results_moa <- unique(
  data[!is.na(mechanism_of_action) & mechanism_of_action != "Unknown"],
  by = c("drug", "gene", "mechanism_of_action")
)

results_moa <- results_moa[,
  .(
    percentile_gtex = percentile_gtex[1],
    log_fold_change = mean(log_fold_change),
    score_gtex = mean(score_gtex)
  ),
  by = c("mechanism_of_action", "gene")
]

results_moa[,
  `:=`(
    proportion_ubiquitous = .SD[percentile_gtex >= 0.95, .N / n_ubiquitous],
    proportion_non_ubiquitous =
      .SD[percentile_gtex < 0.95, .N / n_non_ubiquitous],
    moa_score = weighted.mean(score_gtex, abs(log_fold_change))
  ),
  by = mechanism_of_action
]

results_moa[, bias := proportion_ubiquitous / proportion_non_ubiquitous]
setorder(results_moa, -bias)

results_moa_unique <- unique(results_moa, by = "mechanism_of_action")
n_moa <- nrow(results_moa_unique)
selected_moas <- c(
  results_moa_unique[1:10, mechanism_of_action],
  results_moa_unique[(n_moa - 9):n_moa, mechanism_of_action]
)

results_moa_unique <-
  results_moa_unique[mechanism_of_action %chin% selected_moas]

fig_moas <- plotly::plot_ly(results_moa_unique) |>
  plotly::add_bars(
    x = ~proportion_ubiquitous,
    y = ~mechanism_of_action
  ) |>
  plotly::add_bars(
    x = ~ -proportion_non_ubiquitous,
    y = ~mechanism_of_action
  ) |>
  plotly::layout(
    xaxis = list(
      range = c(-0.8, 0.8),
      title = "Proportion of genes that are influenced significantly",
      tickformat = ".0%"
    ),
    yaxis = list(
      categoryarray = rev(results_moa_unique[, mechanism_of_action]),
      title = ""
    ),
    barmode = "relative",
    showlegend = FALSE,
    font = list(size = 8),
    margin = list(
      pad = 2,
      l = 0,
      r = 0,
      t = 0,
      b = 36
    )
  )

plotly::save_image(
  fig_drug_scores |> plotly::layout(
    font = list(size = 8),
    margin = list(
      pad = 2,
      l = 36,
      r = 0,
      t = 0,
      b = 36
    )
  ),
  file = here("scripts/output/drug_scores.svg"),
  width = 6.27 * 72,
  height = 6.27 * 72,
  scale = 96 / 72
)

plotly::save_image(
  fig_drugs,
  file = here("scripts/output/drugs_labels.svg"),
  width = 3.135 * 72,
  height = 6.27 * 72,
  scale = 96 / 72
)

plotly::save_image(
  fig_drugs |> plotly::layout(yaxis = list(showticklabels = FALSE)),
  file = here("scripts/output/drugs.svg"),
  width = 3.135 * 72,
  height = 6.27 * 72,
  scale = 96 / 72
)

plotly::save_image(
  fig_moas,
  file = here("scripts/output/moas_labels.svg"),
  width = 3.135 * 72,
  height = 6.27 * 72,
  scale = 96 / 72
)

plotly::save_image(
  fig_moas |> plotly::layout(yaxis = list(showticklabels = FALSE)),
  file = here("scripts/output/moas.svg"),
  width = 3.135 * 72,
  height = 6.27 * 72,
  scale = 96 / 72
)
