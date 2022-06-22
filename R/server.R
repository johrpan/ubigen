#' Server implementing the main user interface.
#' @noRd
server <- function(input, output, session) {
  ranked_data <- reactive({
    rank_genes(
      cross_sample_metric = input$cross_sample_metric,
      cross_sample_weight = input$cross_sample_weight,
      mean_expression_weight = input$mean_expression,
      sd_expression_weight = input$sd_expression
    )
  })

  custom_genes <- gene_selector_server("custom_genes") |> debounce(500)

  output$overview_plot <- plotly::renderPlotly(overview_plot(
    ranked_data(),
    highlighted_genes = custom_genes()
  ))

  observeEvent(custom_genes(),
    { # nolint
      if (length(custom_genes()) > 0) {
        updateTabsetPanel(session, "results_panel", selected = "custom_genes")
      } else if (input$results_panel == "custom_genes") {
        updateTabsetPanel(session, "results_panel", selected = "top_genes")
      }
    },
    ignoreNULL = FALSE
  )

  output$custom_genes_synopsis <- renderText({
    comparison_gene_ids <- custom_genes()

    if (length(comparison_gene_ids) > 1) {
      reference <- ranked_data()[!gene %chin% comparison_gene_ids, score]
      comparison <- ranked_data()[gene %chin% comparison_gene_ids, score]

      reference_median <- format(
        round(stats::median(reference), digits = 3),
        nsmall = 3
      )

      comparison_median <- format(
        round(stats::median(comparison), digits = 3),
        nsmall = 3
      )

      test_result <- stats::wilcox.test(
        x = comparison,
        y = reference,
        conf.int = TRUE
      )

      p_value <- format(
        round(test_result$p.value, digits = 4),
        nsmall = 4,
        scientific = FALSE
      )

      lower <- format(round(test_result$conf.int[1], digits = 3), nsmall = 3)
      upper <- format(round(test_result$conf.int[2], digits = 3), nsmall = 3)

      HTML(glue::glue(
        "The p-value for the alternative hypothesis that your genes have ",
        "different scores than other genes is <b>{p_value}</b>. This value ",
        "was computed using a Wilcoxon rank sum test. Based on a 95% ",
        "confidence, the difference in scores is between <b>{lower}</b> and ",
        "<b>{upper}</b>. The median score of your genes is ",
        "<b>{comparison_median}</b> compared to a median score of ",
        "<b>{reference_median}</b> of the other genes."
      ))
    }
  })

  output$custom_genes_boxplot <- plotly::renderPlotly(
    box_plot(ranked_data(), custom_genes())
  )

  output$custom_genes_details <- DT::renderDT({
    genes_table(ranked_data()[gene %chin% custom_genes()])
  })

  output$scores_plot <- plotly::renderPlotly(scores_plot(
    ranked_data(),
    highlighted_genes = custom_genes()
  ))

  selected_genes <- reactive({
    selected_points <- plotly::event_data("plotly_selected")
    ranked_data()[rank %in% selected_points$x]
  })

  output$selected_genes <- DT::renderDataTable({
    data <- if (nrow(selected_genes()) > 0) {
      selected_genes()
    } else {
      ranked_data()
    }

    genes_table(data)
  })

  gsea_genes <- reactive({
    sort(if (input$gsea_set == "top") {
      ranked_data()[rank >= input$gsea_ranks, gene]
    } else if (input$gsea_set == "selected") {
      selected_genes()[, gene]
    } else {
      custom_genes()
    })
  })

  gsea_result <- reactive({
    withProgress(
      message = "Querying g:Profiler",
      value = 0.0,
      { # nolint
        setProgress(0.2)
        gprofiler2::gost(gsea_genes())
      }
    )
  }) |>
    bindCache(gsea_genes()) |>
    bindEvent(input$gsea_run, ignoreNULL = FALSE)

  output$gsea_plot <- plotly::renderPlotly({
    gprofiler2::gostplot(gsea_result(), interactive = TRUE)
  })

  output$gsea_details <- DT::renderDT({
    data <- data.table(gsea_result()$result)
    setorder(data, p_value)

    data[, total_ratio := term_size / effective_domain_size]
    data[, query_ratio := intersection_size / query_size]

    data <- data[, .(
      source,
      term_name,
      total_ratio,
      query_ratio,
      p_value
    )]

    DT::datatable(
      data,
      rownames = FALSE,
      colnames = c(
        "Source",
        "Term",
        "Total ratio",
        "Query ratio",
        "p-value"
      ),
      options = list(
        pageLength = 25
      )
    ) |>
      DT::formatRound("p_value", digits = 4) |>
      DT::formatPercentage(c("total_ratio", "query_ratio"), digits = 1)
  })

  output$gsea_plot_ranking <- plotly::renderPlotly(gsea_plot_ranking)
}

#' Create a displayable data table from the gene results data.
#' @noRd
genes_table <- function(data) {
  data <- data[, .(
    "Gene" = glue::glue_data(
      data,
      "<a href=\"https://gtexportal.org/home/gene/{hgnc_name}\" ",
      "target=\"_blank\">{hgnc_name}</a>"
    ),
    "Rank" = rank,
    "Percentile" = percentile,
    "Score" = score,
    "Median" = median_expression,
    "Mean" = mean_expression,
    "Standard deviation" = sd_expression,
    "Expressed" = above_zero,
    "Above median" = above_median,
    "Above 95%" = above_95
  )]

  DT::datatable(
    data,
    options = list(
      buttons = list(
        list(
          extend = "copy",
          text = "Copy to clipboard"
        ),
        list(
          extend = "csv",
          text = "Download CSV"
        )
      ),
      dom = "fBrtip",
      pageLength = 100
    ),
    rownames = FALSE,
    escape = FALSE,
    selection = "none",
    extensions = "Buttons"
  ) |>
    DT::formatPercentage(
      c(
        "Percentile",
        "Score",
        "Expressed",
        "Above median",
        "Above 95%"
      ),
      digits = 2,
    ) |>
    DT::formatRound(c(
      "Median",
      "Mean",
      "Standard deviation"
    ))
}
