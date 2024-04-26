#' Construct UI for the genes table.
#' @noRd
genes_table_ui <- function(id) {
  verticalLayout(
    div(
      style = "margin-top: 16px",
      splitLayout(
        cellWidths = "auto",
        uiOutput(NS(id, "copy")),
        downloadButton(
          NS(id, "download"),
          "Download CSV",
          class = "btn-outline-primary"
        )
      )
    ),
    div(
      style = "margin-top: 16px; margin-bottom: 8px;",
      DT::DTOutput(NS(id, "genes"))
    )
  )
}

#' Server for the genes table.
#'
#' @param data A reactive containing the results to be displayed.
#'
#' @noRd
genes_table_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$copy <- renderUI({
      data <- data()

      gene_ids <- data[, gene]
      names <- data[hgnc_name != "", hgnc_name]

      genes_text <- paste(gene_ids, collapse = "\n")
      names_text <- paste(names, collapse = "\n")

      splitLayout(
        cellWidths = "auto",
        rclipboard::rclipButton(
          NS(id, "copy_ids_button"),
          "Copy gene IDs",
          genes_text,
          icon = icon("clipboard"),
          class = "btn-outline-primary"
        ),
        rclipboard::rclipButton(
          NS(id, "copy_names_button"),
          "Copy HGNC symbols",
          names_text,
          icon = icon("clipboard"),
          class = "btn-outline-primary"
        )
      )
    })

    output$download <- downloadHandler(
      filename = "ubigen.csv",
      content = \(file) fwrite(data(), file = file),
      contentType = "text/csv"
    )

    output$genes <- DT::renderDT({
      DT::datatable(
        data()[, .(
          "Gene" = glue::glue_data(
            data(),
            "<a href=\"https://gtexportal.org/home/gene/{hgnc_name}\" ",
            "target=\"_blank\">{hgnc_name}</a>"
          ),
          "Rank" = rank,
          "%" = percentile,
          "Score" = score,
          "Median" = median_expression,
          "IQR" = iqr_expression,
          "QCV" = qcv_expression,
          "Mean" = mean_expression,
          "SD" = sd_expression,
          "CV" = cv_expression,
          "> 0" = above_zero,
          "> median" = above_median,
          "> 95%" = above_95
        )],
        options = list(
          dom = "frtip",
          pageLength = 100
        ),
        rownames = FALSE,
        escape = FALSE,
        selection = "none"
      ) |>
        DT::formatPercentage(
          c(
            "%",
            "> 0",
            "> median",
            "> 95%"
          ),
          digits = 2,
        ) |>
        DT::formatRound(c(
          "Score",
          "Median",
          "IQR",
          "QCV",
          "Mean",
          "SD",
          "CV"
        ))
    })
  })
}
