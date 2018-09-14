# source by server.R
# saved as mRNA_server.R


# Clear input -------------------------------------------------------------

observeEvent(input$input_gene_set_reset, {
  names(selected_analysis) %>% purrr::walk(.f = function(.x) { selected_analysis[[.x]] <- FALSE })
  shinyjs::reset("input_gene_set")
  closeAlert(session = session, alertId = "guide-alert")
  status$gene_set <- FALSE
  status$result <- FALSE
})


# Monitor search ----------------------------------------------------------

validate_input_gene_set <- eventReactive(
  eventExpr = input$input_gene_set_search,
  ignoreNULL = TRUE,
  valueExpr = {
    status$gene_set <- TRUE
    if (is.null(input$input_gene_set) || input$input_gene_set == "") {
      error$gene_set <- "Error: Please input gene symbol."
      status$gene_trigger <- if (status$gene_trigger == TRUE) FALSE else TRUE
      status$gene_set <- FALSE
      return()
    }
    # check gene
    .v_igs <- check_gene_set(.s = input$input_gene_set)
    # validate genes
    validate_input_set(.v = .v_igs, .total_symbol = total_gene_symbol, input_list_check = input_list_check)
    
  }
)


# ObserveEvent ------------------------------------------------------------
observeEvent(status$gene_trigger, {
  if (error$gene_set != "" && !is.null(error$gene_set)) {
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Error...",
      text = error$gene_set,
      type = "error"
    )
  }
})


# observe -----------------------------------------------------------------
observe(validate_input_gene_set())

