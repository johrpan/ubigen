#' Server implementing the main user interface.
#' @noRd
server <- function(custom_dataset = NULL) {
  function(input, output, session) {
    dataset <- reactive({
      analysis <- if (input$dataset == "gtex_tissues") {
        ubigen::gtex_tissues
      } else if (input$dataset == "hpa_tissues") {
        ubigen::hpa_tissues
      } else if (input$dataset == "gtex_all") {
        ubigen::gtex_all
      } else if (input$dataset == "cmap") {
        ubigen::cmap
      } else {
        custom_dataset
      }

      merge(analysis, ubigen::genes, by = "gene")
    })

    ranked_data <- reactive({
      rank_genes(
        data = dataset(),
        cross_sample_metric = input$cross_sample_metric,
        cross_sample_weight = input$cross_sample_weight,
        level_metric = input$level_metric,
        level_weight = input$level_weight,
        variation_metric = input$variation_metric,
        variation_weight = input$variation_weight
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
          "The p-value with the alternative hypothesis that your genes have ",
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

    genes_table_server("custom_genes", reactive({
      ranked_data()[gene %chin% custom_genes()]
    }))

    output$scores_plot <- plotly::renderPlotly(scores_plot(
      ranked_data(),
      highlighted_genes = custom_genes()
    ))

    selected_genes <- reactive({
      selected_points <- plotly::event_data("plotly_selected")
      ranked_data()[rank %in% selected_points$x]
    })

    genes_table_server("selected_genes", reactive({
      if (nrow(selected_genes()) > 0) {
        selected_genes()
      } else {
        ranked_data()
      }
    }))

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
      data[, increase := (query_ratio - total_ratio) / total_ratio]

      data <- data[, .(
        source,
        term_name,
        total_ratio,
        query_ratio,
        increase,
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
          "Increase",
          "p-value"
        ),
        options = list(
          pageLength = 25
        )
      ) |>
        DT::formatRound("p_value", digits = 4) |>
        DT::formatPercentage(
          c("total_ratio", "query_ratio", "increase"),
          digits = 2
        )
    })

    output$gsea_plot_ranking <- plotly::renderPlotly(gsea_plot_ranking)
  }
}
