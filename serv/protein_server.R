# source by server.R
# saved as protein_server.R

# Check input gene set ----------------------------------------------------
check_protein_set <- function(.s) {
  .s %>%stringr::str_split(pattern = "[ ,;]+", simplify = TRUE) %>%.[1, ] -> .ss
  .ss
}

# Validate gene with TCGA gene symbol -------------------------------------

validate_protein_set <- function(.v,  .total_symbol, input_protein_check = input_protein_check) {
  
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
  input_protein_check$match <-  protein_match$symbol
  match$protein <- tidyr::separate_rows(protein_match,sep="\t") %>% dplyr::distinct() %>% .$expression
  .vvv %in% input_protein_check$match   -> .inter
  input_protein_check$non_match <- .vvv[!.inter]
  input_protein_check$n_non_match <- length(.vvv[!.inter])
  input_protein_check$n_match <- length(protein_match$symbol)
  input_protein_check$n_total <- length(protein_match$symbol) + length(.vvv[!.inter])
  if(input_protein_check$n_match > 0) {
    status$protein_result <- TRUE
    status$protein_valid <- TRUE } 
  else {
    status$protein_result <- FALSE
    status$protein_valid <- FALSE}
}

# Example -----------------------------------------------------------------

observeEvent(input$protein_example, {
  status$protein_set <- FALSE
  status$protein_result <- FALSE
  status$protein_trigger <- FALSE
  closeAlert(session = session, alertId = "guide-alert")
  shinyjs::js$example_protein_set(id = "seinput_protein_set")
  shinyjs::enable(id = "input_protein_set")
})

# Clear input -------------------------------------------------------------

observeEvent(input$input_protein_set_reset, {
  shinyjs::reset("input_protein_set")
  closeAlert(session = session, alertId = "guide-alert")
  status$protein_set <- FALSE
  status$protein_result <- FALSE
  status$protein_valid <- TRUE
  status$protein_trigger <- FALSE
  output$expr_bubble_plot <- NULL
  output$expr_dt_comparison <- NULL
})


# Monitor search ----------------------------------------------------------

validate_input_protein_set <- eventReactive(
  eventExpr = input$input_protein_set_search,
  ignoreNULL = TRUE,
  valueExpr = {
    status$protein_set <- TRUE
    if(reset$protein){reset$protein <- FALSE} else{reset$protein <- TRUE}
    if (is.null(input$input_protein_set) || input$input_protein_set == "") {
      error$protein_set <- "Error: Please input protein symbol."
      status$protein_trigger <- if (status$protein_trigger == TRUE) FALSE else TRUE
      status$protein_set <- FALSE
      return()
    }
    # check gene
    .v_igs <- check_protein_set(.s = input$input_protein_set)
    # validate genes
    validate_protein_set(.v = .v_igs, .total_symbol = total_protein_symbol, input_protein_check = input_protein_check)
  }
)

# protein table_print -------------------------------------------------------------
tibble_change_to_plot_protein <- function(.expr_clean){
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
tibble_format_change_protein <- function(.expr_clean){
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
expr_buble_plot_protein <-  function(.expr){
  .expr %>% dplyr::rename(FPKM = expr) %>%
    ggplot(mapping=aes(x=cancer_types,y=FPKM,color=cancer_types)) +
    geom_boxplot(width=0.5) +
    facet_wrap(~protein, ncol = 1,scales = "free") +
    theme(
      axis.line = element_line(color = "black"),
      panel.background  = element_rect(fill = "white", color = "grey"),
      axis.title.x = element_blank(),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      legend.title = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      text = element_text(size = 20)
    )
}
expr_clean_datatable_protein <- function(.expr_clean) {
  DT::datatable(
    data = .expr_clean,
    rownames = FALSE,
    colnames = c("Cancer Types/Tissues", "Symbol", "Protein", "Mean expr."),
    filter = "top",
    extensions = "Buttons",
    style = "bootstrap",
    class = "table-bordered table-condensed"
  ) 
}

# ObserveEvent ------------------------------------------------------------
observeEvent(c(input$select_protein,input$select_protein_TCGA,input$select_protein_MCLP,reset$protein),{
  if(length(input$select_protein)>0){
    if(input$select_protein == "Cancer Types"){
    dataset_number$protein <-  length(input$select_protein_TCGA)
    TCGA_protein %>% dplyr::filter(cancer_types %in% input$select_protein_TCGA) %>%
      dplyr::mutate(
        expr = purrr::map(
          .x = expr,
          .f = function(.x) {
            .x %>%
              dplyr::filter(symbol %in% match$protein)
          }
        )
      ) -> expr_clean }
    else if(input$select_protein == "Tissues"){
      dataset_number$protein <-  length(input$select_protein_MCLP)
      MCLP_protein %>% dplyr::filter(tis %in% input$select_protein_MCLP) %>%
        dplyr::mutate(
          expr = purrr::map(
            .x = expression,
            .f = function(.x) {
              .x %>%
                dplyr::filter(symbol %in% match$protein)
            }
          )
        ) %>% dplyr::select(-expression) %>%　dplyr::rename(cancer_types = tis)-> expr_clean }
  tibble_change_to_plot_protein(.expr_clean = expr_clean)->> protein_plot_result
  tibble_format_change_protein(.expr_clean = expr_clean)->>protein_table_result
  protein_plot_result %>% dplyr::select(protein) %>% dplyr::distinct() %>% .$protein -> plot_number$protein
  choice$protein <- protein_plot_result %>% dplyr::filter(protein %in% plot_number$protein[1]) %>% dplyr::select(protein) %>% dplyr::distinct() %>% .$protein
  number <- length(plot_number$protein)
  if(number < 5){
    if(dataset_number$protein <5 ){
      output$expr_bubble_plot_protein <- renderPlot({expr_buble_plot_protein(protein_plot_result)},height = number*200, width = dataset_number$protein*200)
    }
    else{
      output$expr_bubble_plot_protein <- renderPlot({expr_buble_plot_protein(protein_plot_result)},height = number*200)
    }
    multiple$protein <- FALSE
  }
  else{multiple$protein <- TRUE}
  output$expr_dt_comparison_protein <- DT::renderDataTable({expr_clean_datatable_protein(protein_table_result)})
  return(protein_plot_result)
}})
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
observeEvent(status$protein_valid, {
  if (status$protein_valid == FALSE) {
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Error...",
      text = "No matched symbol, please check",
      type = "error"
    )
  }
})
# observe -----------------------------------------------------------------
observe(validate_input_protein_set())
observeEvent(c(input$select_protein,input$select_protein_result), {
  if(length(input$select_protein_result)>0){
    choice$protein <- paste(input$select_protein,input$select_protein_result) %>% stringr::str_replace_all(' ','')
    protein_plot_result %>% dplyr::filter(protein %in% input$select_protein_result) -> one_plot
    if(dataset_number$protein<5){
      output[[choice$protein]] <- renderPlot({one_plot %>% expr_buble_plot_protein()},height = 200, width = dataset_number$protein*200)
    }
    else{
      output[[choice$protein]] <- renderPlot({one_plot %>% expr_buble_plot_protein()},height = 200)
    }
  }
})

