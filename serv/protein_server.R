# source by server.R
# saved as protein_server.R

# Check input gene set ----------------------------------------------------
check_protein_set <- function(.s) {
  .s %>%stringr::str_split(pattern = "[ ,;]+", simplify = TRUE) %>%.[1, ] -> .ss
  .ss
}

# Validate gene with TCGA gene symbol -------------------------------------

validate_protein_set <- function(.v,  .total_symbol, input_list_check = input_list_check) {
  
  .vvv <- .v[.v != ""] %>% unique() %>% sapply(FUN = toupper, USE.NAMES = FALSE)
  tibble::tibble(symbol=.vvv) %>%
    dplyr::mutate(
      expression = purrr::map(
        .x = symbol,
        .f = function(.x) {
          grep(pattern = .x, .total_symbol$symbol, value = TRUE ) ->a
          if(length(a)>0){a}
        }
      )
    ) -> .v_dedup
  .v_dedup %>% tidyr::drop_na() -> protein_match
  input_list_check$match <-  protein_match$symbol
  match$protein <- tidyr::separate_rows(protein_match,sep="\t") %>% dplyr::distinct() %>% .$expression
  .vvv %in% input_list_check$match   -> .inter
  input_list_check$non_match <- .vvv[!.inter]
  input_list_check$n_non_match <- length(.vvv[!.inter])
  input_list_check$n_match <- length(protein_match$symbol)
  input_list_check$n_total <- length(protein_match$symbol) + length(.vvv[!.inter])

  if(input_list_check$n_match > 0) {
    status$protein_result <- TRUE
    status$valid <- TRUE } 
  else {
    status$protein_result <- FALSE
    status$valid <- FALSE}
}


# Clear input -------------------------------------------------------------

observeEvent(input$input_protein_set_reset, {
  names(selected_analysis) %>% purrr::walk(.f = function(.x) { selected_analysis[[.x]] <- FALSE })
  shinyjs::reset("input_protein_set")
  closeAlert(session = session, alertId = "guide-alert")
  status$protein_set <- FALSE
  status$protein_result <- FALSE
  status$valid <- FALSE
  status$protein_trigger <- FALSE
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
    .v_igs <- check_protein_set(.s = input$input_protein_set)
    # validate genes
    validate_protein_set(.v = .v_igs, .total_symbol = total_protein_symbol, input_list_check = input_list_check)
  }
)
# protein table_print -------------------------------------------------------------
tibble_change_to_plot <- function(.expr_clean){
  .expr_clean %>%
    dplyr::mutate(
      mean = purrr::map(
        .x = expr,
        .f = function(.x){
          .x %>% 
            tidyr::gather(key = barcode, value = expr, -c(symbol, protein)) %>%
            tidyr::drop_na(expr) %>%
            dplyr::group_by(symbol, protein) %>%
            dplyr::ungroup() 
        }
      )
    ) %>%
    dplyr::select(-expr) %>% 
    tidyr::unnest()
}
tibble_format_change <- function(.expr_clean){
  .expr_clean %>%
    dplyr::mutate(
      mean = purrr::map(
        .x = expr,
        .f = function(.x){
          .x %>% 
            tidyr::gather(key = barcode, value = expr, -c(symbol, protein)) %>%
            tidyr::drop_na(expr) %>%
            dplyr::group_by(symbol, protein) %>%
            dplyr::summarise(mean = mean(expr)) %>%
            dplyr::ungroup() 
        }
      )
    ) %>%
    dplyr::select(-expr) %>% 
    tidyr::unnest() 
}
expr_buble_plot <-  function(.expr){
  .expr %>%
    ggplot(mapping=aes(x=cancer_types,y=expr,color=cancer_types)) +
    geom_boxplot() +
    facet_grid(~protein) +
    theme(
      axis.line = element_line(color = "black"),
      panel.background  = element_rect(fill = "white", color = "grey"),
      panel.grid = element_line(colour = "grey"),
      axis.title.x = element_blank(),
      legend.title = element_blank(),
      text = element_text(size = 20)
    )
}
expr_clean_datatable <- function(.expr_clean) {
  DT::datatable(
    data = .expr_clean,
    rownames = FALSE,
    colnames = c("Cancer Types", "Symbol", "Protein", "Mean expr."),
    filter = "top",
    extensions = "Buttons",
    style = "bootstrap",
    class = "table-bordered table-condensed"
  ) 
}

# ObserveEvent ------------------------------------------------------------
observeEvent(input$select_protein_TCGA,{
  TCGA_protein %>% dplyr::filter(cancer_types %in% input$select_protein_TCGA) %>%
    dplyr::mutate(
      expr = purrr::map(
        .x = expr,
        .f = function(.x) {
          .x %>%
            dplyr::filter(symbol %in% match$protein)
        }
      )
    ) -> expr_clean
  tibble_change_to_plot(.expr_clean = expr_clean)->>plot_result
  tibble_format_change(.expr_clean = expr_clean)->>table_result
  output$expr_bubble_plot <- renderPlot({expr_buble_plot(plot_result)})
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


