# source by server.R
# saved as miRNA_server.R

# Check input gene set ----------------------------------------------------
check_mirna_set <- function(.s) {
  .s %>%stringr::str_split(pattern = "[ ,;]+", simplify = TRUE) %>%.[1, ]  -> .ss
  .ss
}

# Validate gene with TCGA gene symbol -------------------------------------

validate_miRNA_set <- function(.v,  .total_symbol, input_miRNA_check = input_miRNA_check) {
  .v[.v != ""] %>% unique()  -> .vv
  gsub(pattern = "-",replacement = "", .vv) %>% sapply(FUN = tolower, USE.NAMES = FALSE) %>% 
    grep(pattern="mir|let",.,value=TRUE)-> .vvv
  tibble::tibble(symbol=.vvv) %>%
    dplyr::mutate(
      expression = purrr::map(
        .x = symbol,
        .f = function(.x) {
          paste(.x,"-") %>% stringr::str_replace(" ",'') %>% grep(pattern = ., .total_symbol$match, value = TRUE)->a
          .total_symbol %>% dplyr::filter(match %in% a) %>% .$symbol->b
          if(length(a)<1){
            paste(.x,",") %>% stringr::str_replace(" ",'') %>% grep(pattern = ., .total_symbol$match, value = TRUE)->a
            .total_symbol %>% dplyr::filter(match %in% a) %>% .$symbol->b
            if(length(a)<1){
              grep(pattern = .x, .total_symbol$match2, value = TRUE) ->a
              .total_symbol %>% dplyr::filter(match2 %in% a) %>% .$symbol->b
            }
          }
          b
        }
      )
    ) -> .v_dedup
  .v_dedup %>% tidyr::drop_na() -> miRNA_match
  input_miRNA_check$match <-  miRNA_match$symbol
  match$miRNA <- tidyr::separate_rows(miRNA_match,sep="\t") %>% dplyr::distinct() %>% .$expression
  .vvv %in% input_miRNA_check$match   -> .inter
  input_miRNA_check$non_match <- .vvv[!.inter]
  input_miRNA_check$n_match <- length(miRNA_match$symbol)
  input_miRNA_check$n_non_match <- length(.vvv[!.inter])
  input_miRNA_check$n_total <- length(miRNA_match$symbol) + length(.vvv[!.inter])
  if(input_miRNA_check$n_match > 0) {
    status$miRNA_result <- TRUE
    status$miRNA_valid <- TRUE } 
  else {
    status$miRNA_result <- FALSE
    status$miRNA_valid <- FALSE}
}

# Example -----------------------------------------------------------------

observeEvent(input$miRNA_example, {
  status$miRNA_set <- FALSE
  status$miRNA_result <- FALSE
  status$miRNA_trigger <- FALSE
  closeAlert(session = session, alertId = "guide-alert")
  shinyjs::js$example_miRNA_set(id = "seinput_miRNA_set")
  shinyjs::enable(id = "input_miRNA_set")
})

# Clear input -------------------------------------------------------------

observeEvent(input$input_miRNA_set_reset, {
  shinyjs::reset("input_miRNA_set")
  closeAlert(session = session, alertId = "guide-alert")
  status$miRNA_set <- FALSE
  status$miRNA_result <- FALSE
  status$miRNA_valid <- TRUE
  status$miRNA_trigger <- FALSE
  output$expr_bubble_plot_mirna <- NULL
  output$expr_dt_comparison_mirna <-  NULL
})


# Monitor search ----------------------------------------------------------

validate_input_miRNA_set <- eventReactive(
  eventExpr = input$input_miRNA_set_search,
  ignoreNULL = TRUE,
  valueExpr = {
    status$miRNA_set <- TRUE
    if(reset$miRNA){reset$miRNA <- FALSE} else{reset$miRNA <- TRUE}
    if (is.null(input$input_miRNA_set) || input$input_miRNA_set == "") {
      error$miRNA_set <- "Error: Please input miRNA symbol."
      status$miRNA_trigger <- if (status$miRNA_trigger == TRUE) FALSE else TRUE
      status$miRNA_set <- FALSE
      return()
    }
    # check gene
    .v_igs <- check_mirna_set(.s = input$input_miRNA_set)
    # validate genes
    validate_miRNA_set(.v = .v_igs, .total_symbol = total_miRNA_symbol, input_miRNA_check = input_miRNA_check)
  }
)

# miRNA table print -------------------------------------------------------
expr_buble_plot_mirna <-  function(.expr){
  .expr %>% dplyr::rename(TPM = expr) %>%
    ggplot(mapping=aes(x=cancer_types,y=TPM,color=cancer_types)) +
    geom_boxplot(width=0.5) +
    facet_wrap(~name,ncol = 1,scales = "free") +
    theme(
      axis.line = element_line(color = "black"),
      panel.background  = element_rect(fill = "white", color = "grey"),
      axis.title.x = element_blank(),
      legend.position = "bottom",
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      legend.title = element_blank(),
      text = element_text(size = 20)
    )
}
expr_clean_datatable_mirna <- function(.expr_clean) {
  DT::datatable(
    data = .expr_clean,
    rownames = FALSE,
    colnames = c("Cancer Types", "Symbol","Name", "Mean Rppa expr."),
    filter = "top",
    extensions = "Buttons",
    style = "bootstrap",
    class = "table-bordered table-condensed"
  ) %>% 
    DT::formatSignif(columns = c("expr"), digits = 2) %>%
    DT::formatRound(columns = c("expr"), 2)
}


# ObserveEvent ------------------------------------------------------------
observeEvent(c(input$select_miRNA_TCGA,reset$miRNA),{
  if(length(input$select_miRNA_TCGA)>0 && status$miRNA_valid){
    if(status$miRNA_trigger){status$miRNA_trigger <- FALSE} else{status$miRNA_trigger <- TRUE}
    TCGA_miRNA %>% dplyr::filter(cancer_types %in% input$select_miRNA_TCGA) %>%
    dplyr::mutate(
      mirna = purrr::map(
        .x = summary,
        .f = function(.x) {
          .x %>%
            dplyr::filter(name %in% match$miRNA) %>% 
            tidyr::unnest()
        }
      )
    ) %>% dplyr::select(-summary) %>% tidyr::unnest() %>% dplyr::rename(expr=summary) ->> expr_clean
    expr_clean %>% dplyr::group_by(cancer_types,gene,name) %>% dplyr::slice(1:5) %>% tidyr::drop_na() %>% dplyr::ungroup() ->> mirna_plot_result
    print(mirna_plot_result)
    expr_clean %>% dplyr::group_by(cancer_types,gene,name) %>% dplyr::slice(6) %>% tidyr::drop_na() %>% dplyr::ungroup() ->>mirna_table_result
    mirna_plot_result %>% dplyr::select(name) %>% dplyr::distinct() %>% .$name -> plot_number$miRNA
    choice$miRNA <- mirna_plot_result %>% dplyr::filter(name %in% plot_number$miRNA[1]) %>% dplyr::select(gene) %>% dplyr::distinct() %>% .$gene 
    number <- length(plot_number$miRNA)
    dataset_number$miRNA <-  length(input$select_miRNA_TCGA)
    if(number < 5){
      if(dataset_number$miRNA == 1 ){
        output$expr_bubble_plot_mirna <- renderPlot({mirna_plot_result %>% expr_buble_plot_mirna()}, height = number*200, width = 300)}
      else if(dataset_number$miRNA <5 ){
        output$expr_bubble_plot_mirna <- renderPlot({mirna_plot_result %>% 
            expr_buble_plot_mirna()},height = number*200, width = dataset_number$miRNA*200)
      }
      else{
        output$expr_bubble_plot_mirna <- renderPlot({mirna_plot_result %>% expr_buble_plot_mirna()},height = 6*dataset_number$miRNA+number*200)
      }
      multiple$miRNA <- FALSE
    }
    else{
      multiple$miRNA <- TRUE
    }
    output$expr_dt_comparison_mirna <- DT::renderDataTable({expr_clean_datatable_mirna(mirna_table_result)})
    return(mirna_plot_result)
}})
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

observeEvent(status$miRNA_valid, {
  if (status$miRNA_valid == FALSE) {
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Error...",
      text = "No matched symbol, please check",
      type = "error"
    )
  }
})
# observe -----------------------------------------------------------------
observe(validate_input_miRNA_set())
observeEvent(c(input$select_miRNA_result,status$miRNA_trigger), {
  if(length(input$select_miRNA_result)>0 && status$miRNA_valid){
    choice$miRNA <- total_miRNA_symbol %>% dplyr::filter(symbol %in% input$select_miRNA_result)  %>% .$gene
    mirna_plot_result %>% dplyr::filter(gene %in% choice$miRNA) -> one_plot
    if(dataset_number$miRNA == 1 ){
      output[[choice$miRNA]] <- renderPlot({one_plot %>% expr_buble_plot_mirna()}, height = 200, width = 300)}
    else if(dataset_number$miRNA<5){
      output[[choice$miRNA]] <- renderPlot({one_plot %>% expr_buble_plot_mirna()}, height = 200, width = dataset_number$miRNA*200)
    }
    else{
      output[[choice$miRNA]] <- renderPlot({one_plot %>% expr_buble_plot_mirna()}, height = 200+6*dataset_number$miRNA)
    }
  }
})

