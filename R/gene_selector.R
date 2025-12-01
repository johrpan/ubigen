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
        "Enter Ensembl gene IDs" = "ensembl",
        "Sample data: Common reference genes" = "literature",
        "Sample data: Glycolysis" = "sample"
      )
    ),
    tabsetPanel(
      id = NS(id, "custom_input"),
      type = "hidden",
      tabPanelBody(
        "list",
        shinyWidgets::virtualSelectInput(
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
    ),
    tabsetPanel(
      id = NS(id, "max_genes_panel"),
      type = "hidden",
      tabPanelBody("hide"),
      tabPanelBody(
        "show",
        HTML(paste0(
          "You have entered more than 100 genes, which is the maximum number ",
          "of genes supported in order not to overload the available ",
          "resources. To assess your gene set anyway, a random sample of 100 ",
          "genes was taken automatically."
        ))
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
      } else if (input$identifier_type == "ensembl") {
        inputs <- unique(strsplit(input$gene_ids_raw, "\\s+")[[1]])
        inputs <- inputs[inputs != ""]
        ubigen::genes[gene %chin% inputs, gene]
      } else if (input$identifier_type == "literature") {
        # Commonly used reference genes based on literature (see paper).
        c(
          "ENSG00000075624",
          "ENSG00000166710",
          "ENSG00000111640",
          "ENSG00000256269",
          "ENSG00000133704",
          "ENSG00000116459",
          "ENSG00000169919",
          "ENSG00000165704",
          "ENSG00000102144",
          "ENSG00000196262",
          "ENSG00000231500",
          "ENSG00000112592",
          "ENSG00000072274",
          "ENSG00000164924",
          "ENSG00000134333",
          "ENSG00000171314",
          "ENSG00000149925",
          "ENSG00000026025",
          "ENSG00000067057",
          "ENSG00000160211"
        )
      } else {
        # Sample genes involved in glycolysis according to the KEGG pathways
        # database [KEGG:hsa00010+M00001].
        c(
          "ENSG00000111640",
          "ENSG00000111669",
          "ENSG00000149925",
          "ENSG00000074800",
          "ENSG00000105220",
          "ENSG00000067225",
          "ENSG00000102144",
          "ENSG00000141959",
          "ENSG00000156515",
          "ENSG00000171314",
          "ENSG00000067057",
          "ENSG00000111674",
          "ENSG00000159322",
          "ENSG00000152556",
          "ENSG00000109107",
          "ENSG00000159399",
          "ENSG00000108515",
          "ENSG00000160883",
          "ENSG00000226784",
          "ENSG00000188316",
          "ENSG00000106633",
          "ENSG00000136872",
          "ENSG00000156510",
          "ENSG00000143627",
          "ENSG00000170950"
        )
      }

      if (length(gene_ids) > 100) {
        updateTabsetPanel(
          session,
          "max_genes_panel",
          selected = "show"
        )

        sample(gene_ids, 100)
      } else {
        updateTabsetPanel(
          session,
          "max_genes_panel",
          selected = "hide"
        )

        gene_ids
      }
    })
  })
}
