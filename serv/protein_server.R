# source by server.R
# saved as protein_server.R


# Clear input -------------------------------------------------------------

observeEvent(input$input_protein_set_reset, {
  names(selected_analysis) %>% purrr::walk(.f = function(.x) { selected_analysis[[.x]] <- FALSE })
  shinyjs::reset("input_protein_set")
  closeAlert(session = session, alertId = "guide-alert")
  status$protein_set <- FALSE
  status$result <- FALSE
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
# protein table_print -------------------------------------------------------------
protein_filter <- function(){
  TCGA_protein %>% dplyr::filter(cancer_types %in% input$select_protein_TCGA) %>%
    dplyr::mutate(
      expr = purrr::map(
        .x = expr,
        .f = function(.x) {
          .x %>%
            dplyr::filter(symbol %in% input_list_check$match)
        }
      )
    )
}
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
    facet_grid(~symbol) +
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
            dplyr::filter(symbol %in% input_list_check$match)
        }
      )
    ) -> expr_clean
  tibble_change_to_plot(.expr_clean = expr_clean)->>plot_result
  print(plot_result)
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


