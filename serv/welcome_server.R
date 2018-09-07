# sourced by "server.R"
# Clear input -------------------------------------------------------------
observeEvent(input$input_gene_set_reset, {
  names(selected_analysis) %>% purrr::walk(.f = function(.x) { selected_analysis[[.x]] <- FALSE })
  shinyjs::reset("input_gene_set")
  closeAlert(session = session, alertId = "guide-alert")
  status$gene_set <- FALSE
  status$protein_set <- FALSE
  status$miRNA_set <- FALSE
})
observeEvent(input$input_protein_set_reset, {
  names(selected_analysis) %>% purrr::walk(.f = function(.x) { selected_analysis[[.x]] <- FALSE })
  shinyjs::reset("input_protein_set")
  closeAlert(session = session, alertId = "guide-alert")
  status$gene_set <- FALSE
  status$protein_set <- FALSE
  status$miRNA_set <- FALSE
})
observeEvent(input$input_miRNA_set_reset, {
  names(selected_analysis) %>% purrr::walk(.f = function(.x) { selected_analysis[[.x]] <- FALSE })
  shinyjs::reset("input_miRNA_set")
  closeAlert(session = session, alertId = "guide-alert")
  status$gene_set <- FALSE
  status$protein_set <- FALSE
  status$miRNA_set <- FALSE
})

# Source the function -----------------------------------------------------

source(file.path(config$func, "welcome_func.R"))

# welcome message
output$ui_welcome_msg <- renderUI({fn_welcome_msg()})
output$ui_analysis <- renderUI({fn_analysis()})
# cancer types selection --------------------------------------------------
output$ui_multi_cancer_input <- renderUI({
  if (status$gene_set) {fn_gene_select()} 
  else if (status$protein_set) {
    fn_protein_select(.protein = protein_TCGA)} 
  else if (status$miRNA_set) {
    fn_miRNA_select(.miRNA = miRNA_TCGA)} 
  else {NULL}})
output$ui_mRNA_TCGA_select <- renderUI({fn_mRNA_TCGA_select(.TCGA = mRNA_TCGA)})
output$ui_mRNA_GTEX_select <- renderUI({fn_mRNA_GTEX_select(.GTEX= mRNA_GTEX)})
output$ui_mRNA_CCLE_select <- renderUI({fn_mRNA_CCLE_select(.CCLE = mRNA_CCLE)})
output$ui_mRNA_HPA_tissue_select <- renderUI({fn_mRNA_HPA_tissue_select(.HPA_tissue = mRNA_HPA_tissue)})
output$ui_mRNA_HPA_cellline_select <- renderUI({fn_mRNA_HPA_cellline_select(.HPA_cellline= mRNA_HPA_cellline)})
# Statistics of input gene list -------------------------------------------
output$ui_gene_set_stat <- renderUI({
  if (status$gene_set) {fn_gene_set_stat(input_list_check)} 
  else if (status$protein_set) {
    fn_gene_set_stat(input_list_check)} 
  else if (status$miRNA_set) {
    fn_gene_set_stat(input_list_check)} 
  else {NULL}})
# Start analysis ----------------------------------------------------------
output$ui_start_analysis <- renderUI({if (status$gene_set) {fn_start_analysis()} else {NULL}})
output$ui_result <- renderUI({if(status$result) {fn_result("expr")} else {NULL}})

# introduction ------------------------------------------------------------

output$ui_introduction <- renderUI({fn_introduction()})

# figure ------------------------------------------------------------------


output$ui_feature_figure <- renderUI({fn_feature_figure()})



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
    validate_gene_set(.v = .v_igs, total_gene_symbol = total_gene_symbol, input_list_check = input_list_check)
    
  }
)
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
    validate_protein_set(.v = .v_igs, total_protein_symbol = total_protein_symbol, input_list_check = input_list_check)

  }
)
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
    #.v_igs <- check_miRNA_set(.s = input$input_miRNA_set, status = status, error = error)
  }
)

observeEvent(input$select_protein_TCGA,{
    status$result <- TRUE
    TCGA_protein %>% dplyr::filter(cancer_types %in% input$select_protein_TCGA) %>%
      dplyr::mutate(
        expr = purrr::map(
          .x = expr,
          .f = function(.x) {
            .x %>%
              dplyr::filter(symbol %in% input_list_check$match)
          }
        )
      ) ->> expr_clean
    tibble_format_change(.expr_clean = expr_clean)
    output$expr_dt_comparison <- DT::renderDataTable({expr_clean_datatable(table_result)})
})
# match_result_output -----------------------------------------------------

observeEvent(input$invalid_gene,{
  shinyWidgets::sendSweetAlert(
    session = session,
    title = "invalid_gene",
    text = input_list_check$non_match,
    type = "error"
  )
})

# observeEvent ------------------------------------------------------------
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

observe(validate_input_gene_set())
observe(validate_input_protein_set())
observe(validate_input_miRNA_set())
