# source by server.R
# saved as miRNA_server.R


# Clear input -------------------------------------------------------------

observeEvent(input$input_miRNA_set_reset, {
  names(selected_analysis) %>% purrr::walk(.f = function(.x) { selected_analysis[[.x]] <- FALSE })
  shinyjs::reset("input_miRNA_set")
  closeAlert(session = session, alertId = "guide-alert")
  status$gene_set <- FALSE
  status$protein_set <- FALSE
  status$miRNA_set <- FALSE
})


# Monitor search ----------------------------------------------------------

validate_input_miRNA_set <- eventReactive(
  eventExpr = input$input_miRNA_set_search,
  ignoreNULL = TRUE,
  valueExpr = {
    status$miRNA_set <- TRUE
    if (is.null(input$input_miRNA_set) || input$input_miRNA_set == "") {
      error$miRNA_set <- "Error: Please input miRNA symbol."
      status$miRNA_trigger <- if (status$miRNA_trigger == TRUE) FALSE else TRUE
      status$miRNA_set <- FALSE
      return()
    }
    # check gene
    .v_igs <- check_gene_set(.s = input$input_miRNA_set)
    # validate genes
    validate_input_set(.v = .v_igs, .total_symbol = total_miRNA_symbol, input_list_check = input_list_check)
  }
)


# ObserveEvent ------------------------------------------------------------
observeEvent(input$select_miRNA_TCGA,{
  TCGA_miRNA %>% dplyr::filter(cancer_types %in% input$select_miRNA_TCGA) %>%
    dplyr::mutate(
      mirna = purrr::map(
        .x = mirna,
        .f = function(.x) {
          .x %>%
            dplyr::filter(symbol %in% input_list_check$match)
        }
      )
    ) ->> expr_clean
  tibble_format_change_mirna(.expr_clean = expr_clean)->>table_result
  output$expr_dt_comparison <- DT::renderDataTable({expr_clean_datatable_mirna(table_result)})
})
observeEvent(status$miRNA_trigger, {
  if (error$miRNA_set != "" && !is.null(error$miRNA_set)) {
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Error...",
      text = error$miRNA_set,
      type = "error"
    )
  }
})

# observe -----------------------------------------------------------------
observe(validate_input_miRNA_set())

