library(data.table)
library(here)

i_am("scripts/cmap_drugs_analysis.R")

data <- fread(here("scripts/output/cmap_drugs.csv"))

data[, c("drug", "concentration", "cell_line") :=
  tstrsplit(drug, "_", fixed = TRUE)]

data[, concentration := as.double(concentration)]

data <- data[,
  .(abs_mean_change = mean(abs(mean_change))),
  by = .(drug, group)
]

# Source: PubChem ID list upload based on identifiers converted from CMap
# drug names using the PubChem ID exchange.
pubchem_data <- fread(here("scripts/input/pubchem_data.csv"))

pubchem_data <- pubchem_data[, .(cid, cmpdname, annotation)]
pubchem_data <- unique(pubchem_data, by = "cid")
pubchem_data <- pubchem_data[,
  .(
    cmpdname,
    annotation = strsplit(annotation, "|", fixed = TRUE) |> unlist()
  ),
  by = cid
]

# Filter for WHO ATC annotations
pubchem_data <- pubchem_data[stringr::str_detect(annotation, "^[A-Z] - ")]

# Extract ATC levels

pubchem_data[, atc_1 := stringr::str_match(
  annotation,
  "^[A-Z] - ([^>]*)"
)[, 2] |> stringr::str_trim()]

pubchem_data[, atc_2 := stringr::str_match(
  annotation,
  "> [A-Z][0-9][0-9] - ([^>]*)"
)[, 2] |> stringr::str_trim()]

pubchem_data[, atc_3 := stringr::str_match(
  annotation,
  "> [A-Z][0-9][0-9][A-Z] - ([^>]*)"
)[, 2] |> stringr::str_trim()]

# Source: PubChem ID exchange
drugs_pubchem_mapping <- fread(here("scripts/input/drugs_pubchem.tsv")) |>
  na.omit()

data <- merge(data, drugs_pubchem_mapping, by = "drug", allow.cartesian = TRUE)
data <- merge(data, pubchem_data, by = "cid", allow.cartesian = TRUE)
data[, drug_category := atc_1]


# Select top drug categories

results_drug_categories <- data[,
  .(score = mean(abs_mean_change)),
  by = .(group, drug_category)
]

results_drug_categories <- results_drug_categories[,
  .(mean_score = mean(score)),
  by = drug_category
]

setorder(results_drug_categories, -mean_score)
top_drug_categories <- results_drug_categories[1:7, drug_category]
drug_categories <- c(top_drug_categories, "Other")

# Merge other drug categories

data[!(drug_category %chin% top_drug_categories), drug_category := "Other"]

# Recompute results with new categories

results <- data[,
  .(score = mean(abs_mean_change)),
  by = .(group, drug_category)
]

group_plots <- list()

for (group_value in results[, unique(group)]) {
  group_plot <- plotly::plot_ly() |>
    plotly::add_bars(
      data = results[group == group_value],
      x = ~drug_category,
      y = ~score,
      color = ~drug_category
    ) |>
    plotly::layout(
      xaxis = list(
        categoryarray = drug_categories,
        title = "",
        showticklabels = FALSE
      ),
      yaxis = list(
        range = c(0.0, 0.03),
        nticks = 4,
        title = ""
      ),
      font = list(size = 8),
      margin = list(
        pad = 2,
        l = 48,
        r = 0,
        t = 0,
        b = 36
      )
    )

  plotly::save_image(
    group_plot |> plotly::hide_legend(),
    file = here(glue::glue("scripts/output/drug_categories_{group_value}.svg")),
    width = 3 * 72,
    height = 4 * 72,
    scale = 96 / 72
  )

  group_plots <- c(group_plots, list(group_plot))
}

plotly::save_image(
  group_plot,
  file = here(glue::glue("scripts/output/drug_categories_legend.svg")),
  width = 6.27 * 72,
  height = 6.27 * 72,
  scale = 96 / 72
)
