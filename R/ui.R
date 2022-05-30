#' Function for creating the main user interface.
#' @noRd
ui <- function() {
  navbarPage(
    theme = bslib::bs_theme(
      version = 5,
      bootswatch = "united",
      primary = "#7d19bf"
    ),
    title = "Ubigen",
    header = custom_css(),
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
                "Proportion samples in which the gene is expressed above the ",
                "selected threshold. Select a method and a weight for the ",
                "final score."
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
            "sd_expression",
            verticalLayout(
              strong("Standard deviation"),
              "Standard deviation of the gene's expression across all samples."
            ),
            min = -1.0,
            max = 1.0,
            step = 0.01,
            value = -0.5
          )
        ),
        mainPanel(
          width = 9,
          h3("Overview"),
          plotly::plotlyOutput("overview_plot", height = "200px"),
          tabsetPanel(
            id = "custom_genes_panel",
            type = "hidden",
            tabPanelBody("hide"),
            tabPanelBody(
              "show",
              h3("Your genes"),
              htmlOutput("custom_genes_synopsis"),
              plotly::plotlyOutput("custom_genes_boxplot")
            )
          ),
          h3("Focus on top genes"),
          div(paste0(
            "Click or drag within the figure to select genes of ",
            "interest."
          )),
          plotly::plotlyOutput("scores_plot"),
          h3("Detailed ranking"),
          div(paste0(
            "Click on gene names to view them using the Ensembl ",
            "genome browser."
          )),
          div(class = "p-1"),
          DT::dataTableOutput("selected_genes")
        )
      )
    ),
    tabPanel(
      title = "Help"
    ),
    tabPanel(
      title = "Publication"
    )
  )
}
