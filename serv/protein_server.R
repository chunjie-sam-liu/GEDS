# source by server.R
# saved as protein_server.R


# Clear input -------------------------------------------------------------

observeEvent(input$input_protein_set_reset, {
  names(selected_analysis) %>% purrr::walk(.f = function(.x) { selected_analysis[[.x]] <- FALSE })
  shinyjs::reset("input_protein_set")
  closeAlert(session = session, alertId = "guide-alert")
  status$gene_set <- FALSE
  status$protein_set <- FALSE
  status$miRNA_set <- FALSE
})


# Monitor search ----------------------------------------------------------

validate_input_protein_set <- eventReactive(
  eventExpr = input$input_protein_set_search,
  ignoreNULL = TRUE,
  valueExpr = {
    status$protein_set <- TRUE
    if (is.null(input$input_protein_set) || input$input_protein_set == "") {
      error$protein_set <- "Error: Please input protein symbol."
      status$protein_trigger <- if (status$protein_trigger == TRUE) FALSE else TRUE
      status$protein_set <- FALSE
      return()
    }
    # check gene
    .v_igs <- check_gene_set(.s = input$input_protein_set)
    # validate genes
    validate_input_set(.v = .v_igs, .total_symbol = total_protein_symbol, input_list_check = input_list_check)
    
  }
)


# ObserveEvent ------------------------------------------------------------
observeEvent(input$select_protein_TCGA,{
  TCGA_protein %>% dplyr::filter(cancer_types %in% input$select_protein_TCGA) %>%
    dplyr::mutate(
      expr = purrr::map(
        .x = expr,
        .f = function(.x) {
          .x %>%
            dplyr::filter(symbol %in% input_list_check$match)
        }
      )
    ) -> expr_clean
  tibble_change_to_plot(.expr_clean = expr_clean)->>plot_result
  tibble_format_change(.expr_clean = expr_clean)->>table_result
  output$expr_bubble_plot <- renderPlot({plot_result %>% expr_buble_plot()})
  output$expr_dt_comparison <- DT::renderDataTable({expr_clean_datatable(table_result)})
})
observeEvent(status$protein_trigger, {
  if (error$protein_set != "" && !is.null(error$protein_set)) {
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Error...",
      text = error$protein_set,
      type = "error"
    )
  }
})

# ovserve -----------------------------------------------------------------
observe(validate_input_protein_set())


