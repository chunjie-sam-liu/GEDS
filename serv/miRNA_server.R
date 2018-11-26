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
    grep(pattern="mir|let",.,value=TRUE) -> .vvv
  input_miRNA_check$non_match <- gsub(pattern = "-",replacement = "", .vv) %>% sapply(FUN = tolower, USE.NAMES = FALSE) %>% grep(pattern="mir|let",.,value=TRUE, invert = TRUE)
  tibble::tibble(symbol=.vvv) %>%
    dplyr::mutate(
      expression = purrr::map(
        .x = symbol,
        .f = function(.x) {
          paste(.x,"-") %>% stringr::str_replace(" ",'') %>% grep(pattern = ., .total_symbol$match, value = TRUE) -> a
          .total_symbol %>% dplyr::filter(match %in% a) %>% .$symbol->b
          if(length(a) < 1){
            paste(.x,",") %>% stringr::str_replace(" ",'') %>% grep(pattern = ., .total_symbol$match, value = TRUE) -> a
            .total_symbol %>% dplyr::filter(match %in% a) %>% .$symbol->b
            if(length(a)<1){
              grep(pattern = .x, .total_symbol$match2, value = TRUE) ->a
              .total_symbol %>% dplyr::filter(match2 %in% a) %>% .$symbol->b
            }
          }
          if(length(b)>0){b} else{"drop"}
        }
      )
    ) -> .v_dedup
  ddd <- .v_dedup %>% dplyr::filter(expression %in% "drop") %>% .$symbol
  .vvv %in% ddd -> .inter
  input_miRNA_check$match <-  .vvv[!.inter]
  input_miRNA_check$n_match <- length(.vvv[!.inter])
  input_miRNA_check$total <- c(input_miRNA_check$match,input_miRNA_check$non_match)
  input_miRNA_check$n_non_match <- length(input_miRNA_check$non_match)
  input_miRNA_check$n_total <- length(input_miRNA_check$non_match) + length(.vvv[!.inter])
  if(input_miRNA_check$n_match > 0) {
    status$miRNA_set <- TRUE
    status$miRNA_valid <- TRUE 
    match$miRNA <- .v_dedup %>% dplyr::filter(symbol %in% .vvv[!.inter]) %>% .$expression %>% unlist() %>% tibble::tibble(x = .) %>% dplyr::distinct() %>% .$x
    if(length(input_miRNA_check$non_match) > 0){
      status$miRNA_invalid <- TRUE
      output$miRNA_invalid <- renderText({paste("The list below is invalid:", input_miRNA_check$non_match %>% toString())})
    }
    else{
      status$miRNA_invalid <- FALSE
    }
  } 
  else {
    status$miRNA_set <- FALSE
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
    if(reset$miRNA){reset$miRNA <- FALSE} else{reset$miRNA <- TRUE}
    if (is.null(input$input_miRNA_set) || input$input_miRNA_set == "") {
      error$miRNA_set <- "Error: Please input miRNA symbol."
      status$miRNA_trigger <- if (status$miRNA_trigger == TRUE) FALSE else TRUE
      return()
    }
    # check gene
    .v_igs <- check_mirna_set(.s = input$input_miRNA_set)
    # validate genes
    validate_miRNA_set(.v = .v_igs, .total_symbol = total_miRNA_symbol, input_miRNA_check = input_miRNA_check)
  }
)

###add new
observeEvent(c(reset$miRNA),{
  if(status$miRNA_set){
    dataset_number$miRNA <- 30
    TCGA_miRNA_result()
    plot_number$miRNA <- TCGA_miRNA_plot_result %>% dplyr::select(name) %>% dplyr::distinct() %>% .$name
    if(status$miRNA_trigger){status$miRNA_trigger <- FALSE} else{status$miRNA_trigger <- TRUE}
    status$miRNA_result <- TRUE
    return(TCGA_miRNA_plot_result)
  }}
)

TCGA_miRNA_result <- function(){
  TCGA_miRNA %>%
    dplyr::mutate(
      mirna = purrr::map(
        .x = summary,
        .f = function(.x) {
          .x %>%
            dplyr::filter(name %in% match$miRNA) %>% 
            tidyr::gather(key = barcode, value = expr, -c(gene,name)) %>% tidyr::unnest()
        }
      )
    ) %>% dplyr::select(-summary) %>% tidyr::unnest() %>% 
    dplyr::mutate(tmp = paste(cancer_types,barcode)) %>% 
    dplyr::select(cancer_types=tmp,site,gene,name,expr) ->> expr_clean
  expr_clean %>% dplyr::group_by(cancer_types,gene,name) %>% dplyr::slice(1:5) %>% tidyr::drop_na() %>% dplyr::ungroup() %>% 
    dplyr::mutate(tmp = log2(expr+1)) %>% dplyr::select(cancer_types,site,gene,name,expr = tmp) ->> TCGA_miRNA_plot_result
  expr_clean %>% dplyr::group_by(cancer_types,gene,name) %>% dplyr::slice(6) %>% tidyr::drop_na() %>% dplyr::ungroup() %>% 
    dplyr::mutate(tmp = log2(expr+1)) %>% dplyr::select(cancer_types,site,gene,name,expr = tmp) ->> TCGA_miRNA_table_result
  output$expr_dt_comparison_TCGA_mirna <- DT::renderDataTable({expr_clean_datatable_mirna(TCGA_miRNA_table_result)})
  return(TCGA_miRNA_plot_result)
}
###add new

# miRNA table print -------------------------------------------------------
expr_box_plot_mirna <-  function(.expr){
  quantile_names <- c("lower.whisker", "lower.hinge", "median", "upper.hinge", "upper.whisker")
  .expr %>% dplyr::rename(TPM = expr,symbol = name) %>%
    tidyr::separate(col = cancer_types, into = c("cancer_types", "types")) %>% 
    dplyr::mutate(tmp = paste(site,"(",cancer_types,")")) %>%
    dplyr::select(cancer_types=tmp,types,gene,symbol,TPM) %>%
    dplyr::mutate(types = stringr::str_to_title(types)) %>% 
    dplyr::mutate(name = purrr::rep_along(cancer_types, quantile_names)) %>% 
    tidyr::spread(key = name, value = TPM) %>% 
    dplyr::group_by(symbol) %>% dplyr::arrange(symbol,desc(median)) %>% dplyr::ungroup() -> t
    t %>% dplyr::filter(types %in% "Tumor") %>% .$cancer_types -> order
    t %>%
    ggplot(mapping = aes(x = cancer_types, middle = median,
                         ymin = lower.whisker, ymax = upper.whisker,
                         lower = lower.hinge, upper = upper.hinge, color = types)) +
    scale_x_discrete(limits= order) +
    scale_color_manual(values = c("midnightblue", "red3")) +
    geom_errorbar(width = 0.3, position = position_dodge(0.75, preserve = 'single')) +
    geom_boxplot(stat = 'identity', width = 0.6, position = position_dodge(0.75, preserve = 'single')) +
    facet_wrap(~symbol, ncol = 1,scales = "free_y", strip.position = 'right') +
    theme(
      text = element_text(colour = 'black', size = 18),
      
      axis.line = element_line(color = "black", size = 0.1),
      axis.title.x = element_blank(),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, colour = 'black'),
      axis.text.y = element_text(color = 'black', size = 14),
      
      strip.background = element_rect(fill = NA, color = "white"),
      
      panel.background = element_rect(fill = "white", color = "black", size = 0.5),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      
      legend.position = 'right',
      legend.box = "vertical",
      legend.key = element_rect(fill = 'white'),
      plot.title = element_text(hjust = 0.5,size = 30)
    ) +
    labs(
      title = "Cancer Types (TCGA)",
      x = 'Cancer Types',
      y = 'TPM(log2)'
    ) +
    guides(
      color = guide_legend(
        # legend title
        title = "",
        title.position = "left",
        
        # legend label
        label.position = "bottom",
        label.theme = element_text(angle = 270, hjust = 0.5, vjust = 0.5),
        nrow = 2,
        reverse = TRUE
      )
    )
}
expr_clean_datatable_mirna <- function(.expr_clean) {
  DT::datatable(
    data = .expr_clean,
    options = list(
      pageLength = 10,
      autoWidth = TRUE,
      dom = "Bfrtip",
      buttons = c("copy", "csv", "print")
    ),
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
  if(length(input$select_miRNA_result) > 0 && status$miRNA_valid){
    choice$miRNA <- total_miRNA_symbol %>% dplyr::filter(symbol %in% input$select_miRNA_result)  %>% .$gene
    TCGA_miRNA_plot_result %>% dplyr::filter(gene %in% choice$miRNA) -> TCGA_one_plot
    if(length(TCGA_one_plot$cancer_types) ){
      output[[choice$miRNA]] <- renderPlot({TCGA_one_plot %>% expr_box_plot_mirna()}, height = 500)
      output$`miRNA-picdownload` <- downloadHandler(
        filename = function() {
          paste("Differential_Expression", ".", input$`miRNA-pictype`, sep = "")
        },
        content = function(file){
          ggsave(file,expr_box_plot_mirna(one_plot),device = input$`miRNA-pictype`,width = input$`miRNA-d_width`,height = input$`miRNA-d_height`  )}
      )
    }
  }
})

# observeEvent of selectall -----------------------------------------------

observeEvent(input$select_all_miRNA_TCGA, {
  shinyjs::js$TCGAmiRNAselectall()
})

observeEvent(input$unselect_all_miRNA_TCGA, {
  shinyjs::js$TCGAmiRNAunselectall()
  status$miRNA_result <- FALSE
})