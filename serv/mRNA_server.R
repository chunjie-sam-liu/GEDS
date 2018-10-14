# source by server.R
# saved as mRNA_server.R

# Check input gene set ----------------------------------------------------
check_mRNA_set <- function(.s) {
  .s %>%stringr::str_split(pattern = "[ ,;]+", simplify = TRUE) %>%.[1, ] -> .ss
  .ss
}

# Validate gene with TCGA gene symbol -------------------------------------

validate_mRNA_set <- function(.v,  .total_symbol, input_mRNA_check = input_mRNA_check) {
  
  .vvv <- .v[.v != ""] %>% unique() %>% sapply(FUN = toupper, USE.NAMES = FALSE)
  tibble::tibble(symbol=.vvv) %>%
    dplyr::mutate(
      expression = purrr::map(
        .x = symbol,
        .f = function(.x) {
          grep(pattern = (paste(",",.x,",") %>% 
            stringr::str_replace_all(' ','')), .total_symbol$alias_match, value = TRUE ) ->a
          .total_symbol %>% dplyr::filter(alias_match %in% a) %>% .$symbol->b
          grep(pattern = (paste(",",.x,",") %>% 
            stringr::str_replace_all(' ','')), .total_symbol$symbol_match, value = TRUE ) ->c
          .total_symbol %>% dplyr::filter(symbol_match %in% c) %>% .$symbol->d
          e <- c(b,d)
          if(length(e)>0){e}
        }
      )
    ) -> .v_dedup
  .v_dedup %>% tidyr::drop_na() -> mRNA_match
  input_mRNA_check$match <-  mRNA_match$symbol
  match$mRNA <- tidyr::separate_rows(mRNA_match,sep="\t") %>% dplyr::distinct() %>% .$expression
  .vvv %in% input_mRNA_check$match   -> .inter
  input_mRNA_check$non_match <- .vvv[!.inter]
  input_mRNA_check$n_non_match <- length(.vvv[!.inter])
  input_mRNA_check$n_match <- length(mRNA_match$symbol)
  input_mRNA_check$n_total <- length(mRNA_match$symbol) + length(.vvv[!.inter])
  if(input_mRNA_check$n_match > 0) {
    status$mRNA_valid <- TRUE } 
  else {
    status$mRNA_valid <- FALSE}
}

# Example -----------------------------------------------------------------

observeEvent(input$mRNA_example, {
  status$mRNA_set <- FALSE
  status$mRNA_result <- FALSE
  closeAlert(session = session, alertId = "guide-alert")
  shinyjs::js$example_mRNA_set(id = "seinput_mRNA_set")
  shinyjs::enable(id = "input_mRNA_set")
})

# Clear input -------------------------------------------------------------

observeEvent(input$input_mRNA_set_reset, {
  shinyjs::reset("input_mRNA_set")
  closeAlert(session = session, alertId = "guide-alert")
  status$mRNA_set <- FALSE
  status$mRNA_result <- FALSE
  status$mRNA_valid <- TRUE
  status$mRNA_trigger <- FALSE
  status$analysis <- FALSE
  output$expr_bubble_plot_mRNA <- NULL
  output$expr_dt_comparison_mRNA <- NULL
})

# Monitor search ----------------------------------------------------------

validate_input_mRNA_set <- eventReactive(
  eventExpr = input$input_mRNA_set_search,
  ignoreNULL = TRUE,
  valueExpr = {
    status$mRNA_set <- TRUE
    if (is.null(input$input_mRNA_set) || input$input_mRNA_set == "") {
      error$mRNA_set <- "Error: Please input gene symbol."
      status$mRNA_trigger <- if (status$mRNA_trigger == TRUE) FALSE else TRUE
      status$mRNA_set <- FALSE
      return()
    }
    # check gene
    .v_igs <- check_mRNA_set(.s = input$input_mRNA_set)
    # validate genes
    validate_mRNA_set(.v = .v_igs, .total_symbol = total_mRNA_symbol, input_mRNA_check = input_mRNA_check)
    
  }
)

validate_analysis <- eventReactive(
  eventExpr = input$analysis,
  ignoreNULL = TRUE,
  valueExpr = {
    if(status$analysis){status$analysis <- FALSE} else{status$analysis <-  TRUE}
    status$mRNA_result <- TRUE
    }
  )

# mRNA table_print -------------------------------------------------------------
expr_buble_plot_mRNA <-  function(.expr){
  .expr %>% dplyr::rename(FPKM = expr) %>%
    ggplot(mapping=aes(x=cancer_types,y=FPKM,color=cancer_types)) +
    geom_boxplot(width=0.5) +
    facet_wrap(~symbol, ncol = 1,scales = "free") +
    theme(
      axis.line = element_line(color = "black"),
      panel.background  = element_rect(fill = "white", color = "grey"),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      legend.title = element_blank(),
      legend.position = "bottom",
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      text = element_text(size = 20)
    )
}
expr_clean_datatable_mRNA <- function(.expr_clean) {
  DT::datatable(
    data = .expr_clean,
    rownames = FALSE,
    colnames = c("Cancer types/tissues", "Symbol", "Alias", "Mean expr."),
    filter = "top",
    extensions = "Buttons",
    style = "bootstrap",
    class = "table-bordered table-condensed"
  ) %>% 
    DT::formatSignif(columns = c("expr"), digits = 2) %>%
    DT::formatRound(columns = c("expr"), 2)
}


# ObserveEvent ------------------------------------------------------------
observeEvent(c(status$analysis,reset$mRNA),{
  if(length(input$select_mRNA)>0){
    if(status$mRNA_trigger){status$mRNA_trigger <- FALSE} else{status$mRNA_trigger <- TRUE}
    if(input$select_mRNA == "Cancer Types"){
      dataset_number$mRNA <-  length(input$select_mRNA_TCGA)
      TCGA_mRNA %>% dplyr::filter(cancer_types %in% input$select_mRNA_TCGA) %>%
        dplyr::mutate(
          expr = purrr::map(
            .x = summary,
            .f = function(.x) {
              .x %>%
                dplyr::filter(symbol %in% match$mRNA) %>% dplyr::select(-entrez_id) %>%
                tidyr::gather(key = barcode, value = expr, -c(symbol)) %>% tidyr::unnest()
            }
          )
        ) %>% dplyr::select(-summary) %>% tidyr::unnest() %>% 
        dplyr::mutate(tmp = paste(cancer_types,barcode)) %>% 
        dplyr::select(cancer_types=tmp,symbol,expr) -> expr_clean }
    else if(input$select_mRNA == "Tissues"){
      dataset_number$mRNA <-  length(input$select_mRNA_GTEX)
      GTEX_mRNA %>% dplyr::filter(SMTS %in% input$select_mRNA_GTEX) %>%
        dplyr::mutate(
          expr = purrr::map(
            .x = summary,
            .f = function(.x) {
              .x %>%
                dplyr::filter(symbol %in% match$mRNA) %>% dplyr::select(-ensembl_gene_id) %>%
                tidyr::unnest()
            }
          )
        ) %>% dplyr::select(-summary) %>% tidyr::unnest() %>% dplyr::rename(cancer_types = SMTS,expr=summary) -> expr_clean }
    else if(input$select_mRNA == "Celllines"){
      dataset_number$mRNA <-  length(input$select_mRNA_CCLE)
      CCLE_mRNA %>% dplyr::filter(tissue %in% input$select_mRNA_CCLE) %>%
        dplyr::mutate(
          expr = purrr::map(
            .x = summary,
            .f = function(.x) {
              .x %>%
                dplyr::filter(symbol %in% match$mRNA) %>% tidyr::unnest()
            }
          )
        ) %>% dplyr::select(-summary) %>% tidyr::unnest() %>% dplyr::rename(cancer_types = tissue,expr=summary)-> expr_clean }
    expr_clean %>% dplyr::group_by(cancer_types,symbol) %>% dplyr::slice(1:5) %>% tidyr::drop_na() %>% dplyr::ungroup() ->> mRNA_plot_result
    expr_clean %>% dplyr::group_by(cancer_types,symbol) %>% dplyr::slice(6) %>% tidyr::drop_na() %>% dplyr::ungroup() %>%
      dplyr::left_join(.,total_mRNA_symbol,by = "symbol") %>% dplyr::select(cancer_types,symbol,alias,expr)->>mRNA_table_result
    mRNA_plot_result %>% dplyr::select(symbol) %>% dplyr::distinct() %>% .$symbol -> plot_number$mRNA
    choice$mRNA <- mRNA_plot_result %>% dplyr::filter(symbol %in% plot_number$symbol[1]) %>% 
      dplyr::select(symbol) %>% dplyr::distinct() %>% .$symbol
    number <- length(plot_number$mRNA)
    if(number < 5){
      if(dataset_number$mRNA == 1 ){
        output$expr_bubble_plot_mRNA <- renderPlot({expr_buble_plot_mRNA(mRNA_plot_result)},height = number*200, width = 300)}
      else if(dataset_number$mRNA <5 ){
        output$expr_bubble_plot_mRNA <- renderPlot({expr_buble_plot_mRNA(mRNA_plot_result)},height = number*200, width = dataset_number$mRNA*200)
      }
      else{
        output$expr_bubble_plot_mRNA <- renderPlot({expr_buble_plot_mRNA(mRNA_plot_result)},height = 6*dataset_number$mRNA+number*200)
      }
      multiple$mRNA <- FALSE
    }
    else{multiple$mRNA <- TRUE}
    output$expr_dt_comparison_mRNA <- DT::renderDataTable({expr_clean_datatable_mRNA(mRNA_table_result)})
    return(mRNA_plot_result)
  }})

observeEvent(status$mRNA_trigger, {
  if (error$mRNA_set != "" && !is.null(error$mRNA_set)) {
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Error...",
      text = error$mRNA_set,
      type = "error"
    )
  }
})

observeEvent(status$mRNA_valid, {
  if (status$mRNA_valid == FALSE) {
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Error...",
      text = "No matched symbol, please check",
      type = "error"
    )
  }
})

# observe -----------------------------------------------------------------
observe(validate_input_mRNA_set())
observe(validate_analysis())
observeEvent(c(input$select_mRNA_result,status$mRNA_trigger), {
  if(length(input$select_mRNA_result)>0){
    choice$mRNA <- paste(input$select_mRNA,input$select_mRNA_result,status$mRNA_trigger) %>% stringr::str_replace_all(' ','')
    mRNA_plot_result %>% dplyr::filter(symbol %in% input$select_mRNA_result) -> one_plot
    if(dataset_number$mRNA == 1 ){
      output[[choice$mRNA]] <- renderPlot({one_plot %>% expr_buble_plot_mRNA()}, height = 200, width = 300)}
    else if(dataset_number$mRNA<5 && dataset_number$mRNA>1){
      output[[choice$mRNA]] <- renderPlot({one_plot %>% expr_buble_plot_mRNA()},height = 200, width = dataset_number$mRNA*200)
    }
    else{
      output[[choice$mRNA]] <- renderPlot({one_plot %>% expr_buble_plot_mRNA()},height = 200+6*dataset_number$mRNA)
    }
  }
})
