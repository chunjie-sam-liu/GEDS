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
  gsub(pattern = "-",replacement = "", .vv) %>% sapply(FUN = tolower, USE.NAMES = FALSE) -> .vvv
  tibble::tibble(l=.vvv) %>%
    dplyr::mutate(
      expression = purrr::map(
        .x = l,
        .f = function(.x) {
          grep(pattern = .x, .total_symbol$match, value = TRUE)
        }
      )
    ) -> .v_dedup
  as.character(.v_dedup$expression) %in% .total_symbol$match -> .inter
  .total_symbol %>% dplyr::filter(match %in% as.character(.v_dedup$expression)[.inter]) ->.vvv
  input_miRNA_check$match <- .vvv$symbol
  input_miRNA_check$non_match <- .v_dedup$l[!.inter]
  input_miRNA_check$n_match <- length(as.character(.v_dedup$expression)[.inter])
  input_miRNA_check$n_non_match <- length(.v_dedup$l[!.inter])
  input_miRNA_check$n_total <- length(as.character(.v_dedup$expression)[.inter]) + length(.v_dedup$l[!.inter])
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
tibble_change_to_plot_mirna <- function(.expr_clean){
  .expr_clean %>%
    dplyr::mutate(
      mean = purrr::map(
        .x = mirna,
        .f = function(.x){
          .x %>% 
            tidyr::gather(key = barcode, value = mirna, -c(gene, name)) %>%
            tidyr::drop_na(mirna) %>%
            dplyr::group_by(gene, name) %>%
            dplyr::ungroup() 
        }
      )
    ) %>%
    dplyr::select(-mirna) %>% 
    tidyr::unnest()
}
tibble_format_change_mirna <- function(.expr_clean){
  .expr_clean %>%
    dplyr::mutate(
      mean = purrr::map(
        .x = mirna,
        .f = function(.x){
          .x %>% 
            tidyr::gather(key = barcode, value = mirna, -c(gene, name)) %>%
            tidyr::drop_na(mirna) %>%
            dplyr::group_by(gene,name) %>%
            dplyr::summarise(mean = mean(mirna)) %>%
            dplyr::ungroup() 
        }
      )
    ) %>%
    dplyr::select(-mirna) %>% 
    tidyr::unnest() 
}
expr_buble_plot_mirna <-  function(.expr){
  .expr %>% dplyr::rename(TPM = mirna) %>%
    ggplot(mapping=aes(x=cancer_types,y=TPM,color=cancer_types)) +
    geom_boxplot(width=0.5) +
    facet_wrap(~name,ncol = 1,scales = "free") +
    theme(
      axis.line = element_line(color = "black"),
      panel.background  = element_rect(fill = "white", color = "grey"),
      axis.title.x = element_blank(),
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
    DT::formatSignif(columns = c("mean"), digits = 2) %>%
    DT::formatRound(columns = c("mean"), 2)
}


# ObserveEvent ------------------------------------------------------------
observeEvent(c(input$select_miRNA_TCGA,reset$miRNA),{
  if(length(input$select_miRNA_TCGA)>0){
    if(status$miRNA_trigger){status$miRNA_trigger <- FALSE} else{status$miRNA_trigger <- TRUE}
    TCGA_miRNA %>% dplyr::filter(cancer_types %in% input$select_miRNA_TCGA) %>%
    dplyr::mutate(
      mirna = purrr::map(
        .x = mirna,
        .f = function(.x) {
          .x %>%
            dplyr::filter(name %in% input_miRNA_check$match)
        }
      )
    ) ->> expr_clean
    tibble_change_to_plot_mirna(.expr_clean = expr_clean)->>mirna_plot_result
    tibble_format_change_mirna(.expr_clean = expr_clean) ->>mirna_table_result
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
        output$expr_bubble_plot_mirna <- renderPlot({mirna_plot_result %>% expr_buble_plot_mirna()},height = number*200)
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
  if(length(input$select_miRNA_result)>0){
    choice$miRNA <- total_miRNA_symbol %>% dplyr::filter(symbol %in% input$select_miRNA_result)  %>% .$gene
    mirna_plot_result %>% dplyr::filter(gene %in% choice$miRNA) -> one_plot
    if(dataset_number$miRNA == 1 ){
      output[[choice$miRNA]] <- renderPlot({one_plot %>% expr_buble_plot_mirna()}, height = 200, width = 300)}
    else if(dataset_number$miRNA<5){
      output[[choice$miRNA]] <- renderPlot({one_plot %>% expr_buble_plot_mirna()}, height = 200, width = dataset_number$miRNA*200)
    }
    else{
      output[[choice$miRNA]] <- renderPlot({one_plot %>% expr_buble_plot_mirna()}, height = 200)
    }
  }
})

