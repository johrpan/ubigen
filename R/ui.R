#' Function for creating the main user interface.
#' @noRd
ui <- function(custom_dataset = NULL, show_api_docs = FALSE) {
  div(
    custom_css(),
    rclipboard::rclipboardSetup(),
    navbarPage(
      theme = bslib::bs_theme(
        version = 5,
        bootswatch = "united",
        primary = "#7d19bf"
      ),
      title = "Ubigen",
      tabPanel(
        "Explore",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            h5("Your genes"),
            gene_selector_ui("custom_genes"),
            h5("Method"),
            selectInput(
              "dataset",
              label = strong("Expression dataset"),
              {
                choices <- list(
                  "GTEx (across tissues and conditions)" = "gtex_all",
                  "GTEx (across tissues)" = "gtex_tissues",
                  "Human Protein Atlas (across tissues)" = "hpa_tissues",
                  "CMap" = "cmap",
                  "Glioma (TCGA)" = "glioma"
                )

                if (!is.null(custom_dataset)) {
                  c(list("Custom dataset" = "custom"), choices)
                } else {
                  choices
                }
              }
            ),
            selectInput(
              "cross_sample_metric",
              verticalLayout(
                strong("Expression across samples"),
                paste0(
                  "Proportion of samples in which the gene is expressed above ",
                  "the selected threshold. Select a method and a weight for ",
                  "the final score."
                )
              ),
              list(
                "Above 95th percentile" = "above_95",
                "Above median" = "above_median",
                "Above zero" = "above_zero"
              )
            ),
            sliderInput(
              "cross_sample_weight",
              label = NULL,
              min = -1.0,
              max = 1.0,
              step = 0.01,
              value = 0.5
            ),
            selectInput(
              "level_metric",
              verticalLayout(
                strong("Expression level"),
                "Typical expression level of the gene across all samples."
              ),
              list(
                "Median expression" = "median_expression_normalized",
                "Mean expression" = "mean_expression_normalized"
              )
            ),
            sliderInput(
              "level_weight",
              label = NULL,
              min = -1.0,
              max = 1.0,
              step = 0.01,
              value = 0.25
            ),
            selectInput(
              "variation_metric",
              verticalLayout(
                strong("Expression variation"),
                paste0(
                  "Measure of the variation of the gene's expression between ",
                  "samples."
                )
              ),
              list(
                "Quantile based coefficient of variation" =
                  "qcv_expression_normalized",
                "Interquartile range" = "iqr_expression_normalized",
                "Coefficient of variation" = "cv_expression_normalized",
                "Standard deviation" = "sd_expression_normalized"
              )
            ),
            sliderInput(
              "variation_weight",
              label = NULL,
              min = -1.0,
              max = 1.0,
              step = 0.01,
              value = -0.25
            ),
            div(
              class = "footer",
              HTML(glue::glue(
                "Ubigen version {packageVersion(\"ubigen\")}<br>Source code: ",
                "<a href=\"https://github.com/johrpan/ubigen/\" ",
                "target=\"blank\">available online</a> (GNU AGPL v3)<br>",
                "Citation: Manuscript in preparation"
              ))
            )
          ),
          mainPanel(
            width = 9,
            plotly::plotlyOutput("overview_plot", height = "200px"),
            tabsetPanel(
              id = "results_panel",
              selected = "top_genes",
              header = div(class = "p-2"),
              tabPanel(
                "Your genes",
                value = "custom_genes",
                htmlOutput("custom_genes_synopsis"),
                plotly::plotlyOutput("custom_genes_boxplot"),
                div(class = "p-1"),
                genes_table_ui("custom_genes")
              ),
              tabPanel(
                "Top genes",
                value = "top_genes",
                div(paste0(
                  "Hover over the markers to see details on each gene. Click ",
                  "or drag within the figure to select genes of interest. ",
                  "Double-click removes the selection."
                )),
                plotly::plotlyOutput("scores_plot"),
                div(class = "p-1"),
                div(paste0(
                  "Click on gene names to view them using the GTEx website. ",
                  "There, you can see the tissue specific expression behavior ",
                  "derived from the samples that this analysis is also based ",
                  "on."
                )),
                div(class = "p-1"),
                genes_table_ui("selected_top_genes")
              ),
              tabPanel(
                "Compare rankings",
                value = "rankings",
                div(
                  class = "flow-layout",
                  selectInput(
                    "ranking_y",
                    label = NULL,
                    list(
                      "Custom" = "custom",
                      "GTEx (across tissues and conditions)" = "gtex_all",
                      "GTEx (across tissues)" = "gtex_tissues",
                      "Human Protein Atlas (across tissues)" = "hpa_tissues",
                      "CMap" = "cmap",
                      "Glioma (TCGA)" = "glioma"
                    ),
                    selected = "cmap"
                  ),
                  span(
                    style = paste0(
                      "display: inline-block;",
                      "margin-right: 12px;",
                      "padding: 0.375rem 0.75rem;"
                    ),
                    "~"
                  ),
                  selectInput(
                    "ranking_x",
                    label = NULL,
                    list(
                      "Custom" = "custom",
                      "GTEx (across tissues and conditions)" = "gtex_all",
                      "GTEx (across tissues)" = "gtex_tissues",
                      "Human Protein Atlas (across tissues)" = "hpa_tissues",
                      "CMap" = "cmap",
                      "Glioma (TCGA)" = "glioma"
                    )
                  ),
                  selectInput(
                    "rankings_comparison_mode",
                    label = NULL,
                    list(
                      "Compare scores" = "scores",
                      "Compare percentiles" = "percentiles"
                    )
                  )
                ),
                div(class = "p-1"),
                div(paste0(
                  "Hover over the markers to see the HGNC symbols for the ",
                  "genes. Click or draw within the figure to select genes of ",
                  "interest. Double-click removes the selection."
                )),
                div(class = "p-1"),
                plotly::plotlyOutput(
                  "rankings_plot",
                  width = "800px",
                  height = "800px"
                ),
                div(class = "p-1"),
                genes_table_ui("selected_comparison_genes")
              ),
              tabPanel(
                "GSEA",
                value = "gsea",
                div(
                  class = "flow-layout",
                  selectInput(
                    "gsea_set",
                    label = NULL,
                    list(
                      "Top genes" = "top",
                      "Selected genes" = "selected",
                      "Your genes" = "custom"
                    )
                  ),
                  conditionalPanel(
                    "input.gsea_set == 'top'",
                    sliderInput(
                      "gsea_ranks",
                      label = NULL,
                      min = 10,
                      max = 1000,
                      value = 100,
                      step = 10,
                      ticks = FALSE
                    )
                  ),
                  actionButton(
                    "gsea_run",
                    "Update analysis",
                    class = "btn-primary"
                  ),
                  a(
                    "Powered by g:Profiler",
                    href = "https://biit.cs.ut.ee/gprofiler/gost",
                    target = "_blank",
                    style = "margin-left: 16px"
                  )
                ),
                plotly::plotlyOutput("gsea_plot"),
                div(class = "p-2"),
                DT::dataTableOutput("gsea_details")
              )
            )
          )
        )
      ),
      tabPanel(
        title = "Additional information",
        div(
          class = "container",
          h2("Drug effects"),
          p(HTML(paste0(
            "Scores for drugs based on the genes that are significantly ",
            "influenced by them. To compute a score for each drug, the scores ",
            "of all influenced genes based on “GTEx (all)” (X-axis) and ",
            "“CMap” (Y-axis) are averaged with weights based on the fold ",
            "change of the interactions. The position of each drug in this ",
            "plot is therefore a result of how ubiquitous the genes that it ",
            "influences are."
          ))),
          p(HTML(paste0(
            "Note: Hover over the markers to see drug names."
          ))),
          plotly::plotlyOutput("fig_drug_scores", height = "1200px"),
          h2("Number of interesting genes along the ranking"),
          p(HTML(paste0(
            "The notion of ubiquitous genes or housekeeping genes implies ",
            "some kind of enrichment of important genes. Within groups of ",
            "genes with certain biological associations, these genes should ",
            "be overrepresented. We use GeneOntology terms as well as some ",
            "other gene set sources to represent this concept. The following ",
            "plot shows the number of associated terms for a non-overlapping ",
            "sliding window of 500 genes along the ranking of ubiquity using ",
            "our default parameters. The terms have been obtained using a ",
            "gene set enrichment analysis with the tool ",
            "<a href=\"https://biit.cs.ut.ee/gprofiler/gost\" ",
            "target=\"_blank\">g:Profiler</a>. We observe that the most ",
            "ubiquitous genes have many more known biological implications ",
            "than any other bucket of genes. The genes of average ubiquity ",
            "have almost no associations with GeneOntology terms. The number ",
            "of associations rises again for the least ubiquitous genes."
          ))),
          p(HTML(paste0(
            "Note: Click on the legend items to toggle single sources. A ",
            "double-click will isolate a single source of interest."
          ))),
          plotly::plotlyOutput("gsea_plot_ranking", height = "600px")
        )
      ),
      tabPanel(
        title = "Help",
        div(
          class = "container",
          includeMarkdown(system.file("content", "help.md", package = "ubigen"))
        )
      ),
      if (show_api_docs) {
        tabPanel(
          title = "API access",
          div(
            class = "container",
            includeMarkdown(
              system.file("content", "api.md", package = "ubigen")
            )
          )
        )
      }
    )
  )
}
