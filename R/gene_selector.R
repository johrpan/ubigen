#' Create the UI for a gene selector.
#'
#' @param id ID for namespacing.
#'
#' @return The user interface
#' @noRd
gene_selector_ui <- function(id) {
  named_genes <- ubigen::genes[hgnc_name != ""]
  named_genes <- unique(named_genes, by = "hgnc_name")
  gene_choices <- named_genes$gene
  names(gene_choices) <- named_genes$hgnc_name

  verticalLayout(
    selectInput(
      NS(id, "identifier_type"),
      verticalLayout(
        strong("Gene identifiers"),
        paste0(
          "Select whether you want to pick or enter custom genes to assess ",
          "how ubiquitous they are."
        )
      ),
      choices = list(
        "Select from list" = "list",
        "Enter HGNC symbols" = "hgnc",
        "Enter Ensembl gene IDs" = "ensembl"
      )
    ),
    tabsetPanel(
      id = NS(id, "custom_input"),
      type = "hidden",
      tabPanelBody(
        "list",
        shinyvs::virtualSelectInput(
          NS(id, "selected_genes"),
          label = NULL,
          choices = gene_choices,
          multiple = TRUE,
          search = TRUE,
          selectAllOnlyVisible = TRUE
        ),
      ),
      tabPanelBody(
        "hgnc",
        textAreaInput(
          NS(id, "hgnc_names_raw"),
          label = NULL,
          height = "250px"
        )
      ),
      tabPanelBody(
        "ensembl",
        textAreaInput(
          NS(id, "gene_ids_raw"),
          label = NULL,
          height = "250px"
        )
      )
    )
  )
}

#' Application logic for the gene selector.
#'
#' @param id ID for namespacing.
#'
#' @return A reactive containing the selected gene IDs.
#' @noRd
gene_selector_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    observe({
      updateTabsetPanel(
        session,
        "custom_input",
        selected = input$identifier_type
      )
    })

    reactive({
      gene_ids <- if (input$identifier_type == "list") {
        input$selected_genes
      } else if (input$identifier_type == "hgnc") {
        inputs <- unique(strsplit(input$hgnc_names_raw, "\\s+")[[1]])
        inputs <- inputs[inputs != ""]
        ubigen::genes[hgnc_name %chin% inputs, gene]
      } else {
        inputs <- unique(strsplit(input$gene_ids_raw, "\\s+")[[1]])
        inputs <- inputs[inputs != ""]
        ubigen::genes[gene %chin% inputs, gene]
      }

      if (length(gene_ids > 100)) {
        gene_ids[seq_len(100)]
      } else {
        gene_ids
      }
    })
  })
}
