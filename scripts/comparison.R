library(data.table)
library(here)

i_am("scripts/comparison.R")

w2000 <- scan(here("scripts/input/datasets/warrington_2000.txt"), character())
z2008 <- scan(here("scripts/input/datasets/zhu_2008.txt"), character())
e2013 <- scan(here("scripts/input/datasets/eisenberg_2013.txt"), character())
c2011 <- scan(here("scripts/input/datasets/chang_2011.txt"), character())
j2022 <- scan(here("scripts/input/datasets/joshi_2022.txt"), character())

datasets <- list(
  "Warrington 2000" = w2000,
  "Zhu 2008" = z2008,
  "Eisenberg 2013" = e2013,
  "Chang 2011" = c2011,
  "Joshi 2022" = j2022
)

VennDiagram::venn.diagram(datasets, filename = NULL, disable.logging = TRUE) |>
  ggplot2::ggsave(file = here("scripts/output/venn.svg"), device = "svg")

partitions <- VennDiagram::get.venn.partitions(datasets) |> data.table()
genes_venn <- partitions[1]$..values..[[1]]
write(genes_venn, file = here("scripts/output/genes_venn.txt"))

gene_sets <- fread(here("scripts/input/gene_sets.csv"))
genes_literature <- gene_sets[type == "literature", unique(gene)]
genes_recommended <- gene_sets[type == "expression", unique(gene)]
genes_literature_ids <- data.table(
  gene = genes_literature,
  literature_id = seq_along(genes_literature)
)

ranking_gtex <- ubigen::rank_genes(ubigen::gtex_all)
ranking_cmap <- ubigen::rank_genes(ubigen::cmap)

data <- fread(here("scripts/output/gsea_vs_cmap_groups.csv"))

genes_table <- gene_sets[type == "literature"]
genes_table[, hgnc_symbol := gprofiler2::gconvert(gene, target = "HGNC")$target]
genes_table <- genes_table[,
  .(
    gene = unique(gene),
    source = paste(label, collapse = ", ")
  ),
  by = hgnc_symbol
]
genes_table <- merge(genes_table, data, by = "gene", sort = FALSE)
fwrite(genes_table, file = here("scripts/output/genes_table.csv"))

datasets_data <- rbindlist(lapply(names(datasets), function(name) {
  data.table(
    dataset = name,
    gene = datasets[[name]]
  )
}))

datasets_data <- rbind(
  datasets_data,
  data.table(
    dataset = "Venn",
    gene = genes_venn
  )
)

datasets_data <- rbind(
  datasets_data,
  data.table(
    dataset = "Recommended",
    gene = genes_recommended
  )
)

datasets_data <- rbind(
  datasets_data,
  data.table(
    dataset = "Literature",
    gene = genes_literature
  )
)

datasets_data <- merge(datasets_data, data, by = "gene")

datasets_table <- datasets_data[, .(count = .N), by = c("dataset", "group")]
datasets_table[, total := sum(count), by = dataset]
datasets_table[, proportion := count / total]

group_plots <- list()

for (group_value in datasets_table[, unique(group)]) {
  group_plot <- plotly::plot_ly() |>
    plotly::add_bars(
      data = datasets_table[group == group_value],
      x = ~dataset,
      color = ~dataset,
      y = ~proportion
    ) |>
    plotly::layout(
      xaxis = list(
        categoryarray = datasets_table[, unique(dataset)],
        title = ""
      ),
      yaxis = list(
        range = c(0.0, 1.0),
        title = ""
      ),
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
    group_plot |> plotly::hide_legend(),
    file = here(glue::glue("scripts/output/gene_sets_{group_value}.svg")),
    width = 3 * 72,
    height = 4 * 72,
    scale = 96 / 72
  )

  group_plots <- c(group_plots, list(group_plot))
}

plotly::save_image(
  group_plot,
  file = here(glue::glue("scripts/output/gene_sets_legend.svg")),
  width = 6.27 * 72,
  height = 6.27 * 72,
  scale = 96 / 72
)

data[, count := 0]

for (dataset in datasets) {
  data[gene %chin% dataset, count := count + 1]
}

threshold_gtex <- data[percentile_gtex >= 0.95, min(score_gtex)]
threshold_cmap <- data[percentile_cmap >= 0.95, min(score_cmap)]

fig <- plotly::plot_ly() |>
  plotly::add_markers(
    data = data[count >= 1 & !(gene %chin% genes_literature)],
    x = ~score_gtex,
    y = ~score_cmap,
    color = ~count,
    colors = c("#7d19bf", "#ff7f2a"),
    marker = list(
      size = 4,
      opacity = 0.8
    ),
    cliponaxis = FALSE
  ) |>
  plotly::add_text(
    data = merge(
      data[gene %chin% genes_literature],
      genes_literature_ids
    ),
    x = ~score_gtex,
    y = ~score_cmap,
    text = ~ as.character(literature_id),
    textfont = list(
      size = 8,
      color = "green"
    )
  ) |>
  plotly::layout(
    xaxis = list(
      title = "Ranking based on GTEx",
      range = c(0, 1)
    ),
    yaxis = list(
      title = "Ranking based on CMap",
      range = c(0, 1)
    ),
    annotations = list(
      list(
        text = "95%",
        x = threshold_gtex,
        y = 1,
        xshift = 2,
        yshift = 3,
        yref = "paper",
        xanchor = "left",
        yanchor = "top",
        showarrow = FALSE
      ),
      list(
        text = "95%",
        x = 1,
        y = threshold_cmap,
        yshift = 2,
        xref = "paper",
        xanchor = "right",
        yanchor = "bottom",
        showarrow = FALSE
      )
    ),
    shapes = list(
      list(
        type = "line",
        y0 = 0,
        y1 = 1,
        yref = "paper",
        x0 = threshold_gtex,
        x1 = threshold_gtex,
        line = list(
          color = "#00000080",
          opacity = 0.5,
          width = 1,
          dash = "dot"
        )
      ),
      list(
        type = "line",
        y0 = threshold_cmap,
        y1 = threshold_cmap,
        x0 = 0,
        x1 = 1,
        xref = "paper",
        line = list(
          color = "#00000080",
          width = 1,
          opacity = 0.5,
          dash = "dot"
        )
      )
    ),
    font = list(size = 8),
    margin = list(
      pad = 2,
      l = 36,
      r = 0,
      t = 0,
      b = 36
    )
  ) |>
  plotly::hide_legend()

plotly::save_image(
  fig |> plotly::hide_colorbar(),
  file = here("scripts/output/comparison.svg"),
  width = 6.27 * 72,
  height = 6.27 * 72,
  scale = 96 / 72
)

plotly::save_image(
  fig,
  file = here("scripts/output/comparison_legend.svg"),
  width = 6.27 * 72,
  height = 6.27 * 72,
  scale = 96 / 72
)
