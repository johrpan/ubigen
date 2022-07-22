#' Function for creating the main user interface.
#' @noRd
ui <- function() {
  div(
    custom_css(),
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
            h3("My genes"),
            gene_selector_ui("custom_genes"),
            h3("Scoring"),
            selectInput(
              "cross_sample_metric",
              verticalLayout(
                strong("Expression across samples"),
                paste0(
                  "Proportion samples in which the gene is expressed above ",
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
            sliderInput(
              "mean_expression",
              verticalLayout(
                strong("Mean Expression"),
                "Mean expression of the gene across all samples."
              ),
              min = -1.0,
              max = 1.0,
              step = 0.01,
              value = 0.25
            ),
            sliderInput(
              "sd_expression",
              verticalLayout(
                strong("Standard deviation"),
                paste0(
                  "Standard deviation of the gene's expression across all ",
                  "samples."
                )
              ),
              min = -1.0,
              max = 1.0,
              step = 0.01,
              value = -0.25
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
                DT::dataTableOutput("custom_genes_details")
              ),
              tabPanel(
                "Top genes",
                value = "top_genes",
                div(paste0(
                  "Hover over the markers to see details on each gene. Click ",
                  "or drag within the figure to select genes of interest."
                )),
                plotly::plotlyOutput("scores_plot"),
                div(paste0(
                  "Click on gene names to view them using the GTEx website. ",
                  "There, you can see the tissue specific expression behavior ",
                  "derived from the samples that this analysis is also based ",
                  "on."
                )),
                div(class = "p-1"),
                DT::dataTableOutput("selected_genes")
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
      )
    )
  )
}
