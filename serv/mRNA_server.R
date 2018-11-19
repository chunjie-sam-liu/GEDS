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
          if(length(e)>0){e} else{"drop"}
        }
      )
    ) -> .v_dedup
  input_mRNA_check$non_match <- .v_dedup %>% dplyr::filter(expression %in% "drop") %>% .$symbol
  .vvv %in% input_mRNA_check$non_match ->.inter
  input_mRNA_check$match <-  .vvv[!.inter]
  match$mRNA <- .v_dedup %>% dplyr::filter(symbol %in% .vvv[!.inter]) %>% .$expression %>% unlist() %>% 
    tibble::tibble(x=.) %>% dplyr::distinct() %>% .$x
  input_mRNA_check$total <- c(input_mRNA_check$match,input_mRNA_check$non_match)
  input_mRNA_check$n_non_match <- length(input_mRNA_check$non_match)
  input_mRNA_check$n_match <- length(.vvv[!.inter])
  input_mRNA_check$n_total <- length(input_mRNA_check$non_match) + length(.vvv[!.inter])
  output$download_total_mRNA_set <- fn_gs_download(data = input_mRNA_check$total,txt = "total_gene_set.txt")
  output$download_valid_mRNA_set <- fn_gs_download(data = input_mRNA_check$match,txt = "valid_gene_set.txt")
  output$download_unmatched_mRNA_set <- fn_gs_download(data = input_mRNA_check$n_non_match,txt = "error_gene_set.txt")
  if(input_mRNA_check$n_match > 0) {
    status$mRNA_set <- TRUE
    status$mRNA_valid <- TRUE } 
  else {
    status$mRNA_set <- FALSE
    status$mRNA_result <- FALSE
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
  output$expr_bubble_plot_mRNA <- NULL
  output$expr_dt_comparison_mRNA <- NULL
})

# Monitor search ----------------------------------------------------------

validate_input_mRNA_set <- eventReactive(
  eventExpr = input$input_mRNA_set_search,
  ignoreNULL = TRUE,
  valueExpr = {
    if(reset$mRNA){reset$mRNA <- FALSE} else{reset$mRNA <- TRUE}
    if (is.null(input$input_mRNA_set) || input$input_mRNA_set == "") {
      error$mRNA_set <- "Error: Please input gene symbol."
      status$mRNA_trigger <- if (status$mRNA_trigger == TRUE) FALSE else TRUE
      return()
    }
    # check gene
    .v_igs <- check_mRNA_set(.s = input$input_mRNA_set)
    # validate genes
    validate_mRNA_set(.v = .v_igs, .total_symbol = total_mRNA_symbol, input_mRNA_check = input_mRNA_check)
    
  }
)

# mRNA table_print -------------------------------------------------------------
expr_box_plot_mRNA <-  function(.expr){
  quantile_names <- c("lower.whisker", "lower.hinge", "median", "upper.hinge", "upper.whisker")
  if(input$select_mRNA == "Cancer types"){
  .expr %>% dplyr::rename(FPKM = expr) %>%
    tidyr::separate(col = cancer_types, into = c("cancer_types", "types")) %>% 
    dplyr::mutate(types = stringr::str_to_title(types)) %>% 
    dplyr::mutate(name = purrr::rep_along(cancer_types, quantile_names)) %>% 
    tidyr::spread(key = name, value = FPKM) -> dd
    dd %>% 
      dplyr::filter(types == 'Tumor') %>% 
      dplyr::arrange(symbol, median) %>% 
      dplyr::mutate(.r = dplyr::row_number() %>% rev()) %>% 
      dplyr::select(cancer_types, symbol, .r) %>% 
      dplyr::right_join(dd, by = c('cancer_types', 'symbol')) %>% 
      dplyr::mutate(.r = 0.3 * .r) %>%
      dplyr::arrange(-.r) -> ddd
    ddd %>% 
    ggplot(mapping = aes(x = .r, middle = median,
                         ymin = lower.whisker, ymax = upper.whisker,
                         lower = lower.hinge, upper = upper.hinge, color = types)) +
    scale_color_manual(values = c("midnightblue", "red3")) -> p
  }
  else{
    nu <- length(.expr$cancer_types)
    TCGA_color %>% head(n = nu) %>% dplyr::select(color) %>% dplyr::pull(color) -> .color
    .expr %>% dplyr::rename(FPKM = expr) %>%
      dplyr::mutate(name = purrr::rep_along(cancer_types, quantile_names)) %>%
      tidyr::spread(key = name, value = FPKM) -> dd 
    dd %>% 
      dplyr::arrange(symbol, median) %>% 
      dplyr::mutate(.r = dplyr::row_number() %>% rev()) %>% 
      dplyr::select(cancer_types, symbol, .r) %>% 
      dplyr::right_join(dd, by = c('cancer_types', 'symbol')) %>% 
      dplyr::mutate(.r = 0.4 * .r) %>%
      dplyr::arrange(-.r) -> ddd
    ddd %>% 
      ggplot(mapping = aes(x = .r, middle = median,
                           ymin = lower.whisker, ymax = upper.whisker,
                           lower = lower.hinge, upper = upper.hinge, color = cancer_types)) +
    scale_color_manual(values = .color) -> p
  }
    p +
    geom_errorbar(width = 0.1, position = position_dodge(0.25, preserve = 'single')) +
    geom_boxplot(stat = 'identity', width = 0.2, position = position_dodge(0.25, preserve = 'single')) +
    facet_wrap(~symbol, ncol = 1, scales = "free", strip.position = 'right') +
    scale_x_continuous(breaks = ddd$.r,labels = ddd$cancer_types) +
    # facet_wrap(~symbol, ncol = 1, scales = "free") +
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
      
      legend.position = 'top',
      legend.key = element_rect(fill = 'white')
    ) +
    labs(
      x = 'Cancer Types',
      y = 'FPKM(log2)'
    ) +
    guides(
      color = guide_legend(
        # legend title
        title = "Cancer Types",
        title.position = "left",
        
        # legend label
        label.position = "right",
        # label.theme = element_text(size = 14),
        nrow = 2,
        reverse = TRUE
      )
    )
}
expr_clean_datatable_mRNA <- function(.expr_clean) {
  DT::datatable(
    data = .expr_clean,
    options = list(
      pageLength = 10,
      autoWidth = TRUE,
      dom = "Bfrtip",
      buttons = c("copy", "csv", "print")
    ),
    rownames = FALSE,
    colnames = c("Cancer types/tissues", "Symbol", "Alias", "Mean expr(log2)"),
    filter = "top",
    extensions = "Buttons",
    style = "bootstrap",
    class = "table-bordered table-condensed"
  ) %>% 
    DT::formatSignif(columns = c("expr"), digits = 2) %>%
    DT::formatRound(columns = c("expr"), 2)
}


# ObserveEvent ------------------------------------------------------------
observeEvent(c(input$select_mRNA,input$select_mRNA_TCGA,input$select_mRNA_GTEX,input$select_mRNA_CCLE,reset$mRNA),{
  re <- "0"
  if(length(input$select_mRNA)>0 && status$mRNA_set){
    if(input$select_mRNA == "Cancer types" && length(input$select_mRNA_TCGA)>0){
      re <- "1"
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
    else if(input$select_mRNA == "Normal tissues" && length(input$select_mRNA_GTEX)>0){
      re <- "1"
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
    else if(input$select_mRNA == "Cell lines" && length(input$select_mRNA_CCLE)>0){
      re <- "1"
        dataset_number$mRNA <-  length(input$select_mRNA_CCLE)
        CCLE_mRNA  %>% dplyr::filter(tissue %in% input$select_mRNA_CCLE) %>%
        dplyr::mutate(
        expr = purrr::map(
          .x = summary,
          .f = function(.x) {
            .x %>%
              dplyr::filter(symbol %in% match$mRNA) %>% tidyr::unnest()
          }
        )
        ) %>% dplyr::select(-summary) %>% tidyr::unnest() %>% dplyr::rename(cancer_types = tissue,expr=summary)-> expr_clean }
    if(re == "1"){
      if(status$mRNA_trigger){status$mRNA_trigger <- FALSE} else{status$mRNA_trigger <- TRUE}
      status$mRNA_result <- TRUE
    expr_clean %>% dplyr::group_by(cancer_types,symbol) %>% dplyr::slice(1:5) %>% tidyr::drop_na() %>% dplyr::ungroup() %>% 
      dplyr::mutate(tmp = log2(expr+1)) %>% dplyr::select(cancer_types,symbol,expr = tmp) ->> mRNA_plot_result
    expr_clean %>% dplyr::group_by(cancer_types,symbol) %>% dplyr::slice(6) %>% tidyr::drop_na() %>% dplyr::ungroup() %>% 
      dplyr::mutate(tmp = log2(expr+1)) %>% dplyr::select(cancer_types,symbol,expr = tmp) %>%
      dplyr::left_join(.,total_mRNA_symbol,by = "symbol") %>% dplyr::select(cancer_types,symbol,alias,expr)->>mRNA_table_result
    mRNA_plot_result %>% dplyr::select(symbol) %>% dplyr::distinct() %>% .$symbol -> plot_number$mRNA
    choice$mRNA <- mRNA_plot_result %>% dplyr::filter(symbol %in% plot_number$symbol[1]) %>% 
      dplyr::select(symbol) %>% dplyr::distinct() %>% .$symbol
    number <- length(plot_number$mRNA)
    if(number < 5){
      if(dataset_number$mRNA == 1 ){
        output$expr_bubble_plot_mRNA <- renderPlot({expr_box_plot_mRNA(mRNA_plot_result)},height = number*300, width = 260)
        output$`mRNA-picdownload` <- downloadHandler(
          filename = function() {
            paste("Differential_Expression", ".", input$`mRNA-pictype`, sep = "")
          },
          content = function(file){
            ggsave(file,expr_box_plot_mRNA(mRNA_plot_result),device = input$`mRNA-pictype`,width = input$`mRNA-d_width`,height = input$`mRNA-d_height`  )}
        )}
      else if(dataset_number$mRNA <5 ){
        output$expr_bubble_plot_mRNA <- renderPlot({expr_box_plot_mRNA(mRNA_plot_result)},height = number*300, width = dataset_number$mRNA*260)
        output$`mRNA-picdownload` <- downloadHandler(
          filename = function() {
            paste("Differential_Expression", ".", input$`mRNA-pictype`, sep = "")
          },
          content = function(file){
            ggsave(file,expr_box_plot_mRNA(mRNA_plot_result),device = input$`mRNA-pictype`,width = input$`mRNA-d_width`,height = input$`mRNA-d_height`  )}
        )
      }
      else{
        output$expr_bubble_plot_mRNA <- renderPlot({expr_box_plot_mRNA(mRNA_plot_result)},height = 6*dataset_number$mRNA+number*300)
        output$`mRNA-picdownload` <- downloadHandler(
          filename = function() {
            paste("Differential_Expression", ".", input$`mRNA-pictype`, sep = "")
          },
          content = function(file){
            ggsave(file,expr_box_plot_mRNA(mRNA_plot_result),device = input$`mRNA-pictype`,width = input$`mRNA-d_width`,height = input$`mRNA-d_height`  )}
        )
      }
      multiple$mRNA <- FALSE
    }
    else{multiple$mRNA <- TRUE}
    output$expr_dt_comparison_mRNA <- DT::renderDataTable({expr_clean_datatable_mRNA(mRNA_table_result)})
    return(mRNA_plot_result)}
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
observeEvent(c(input$select_mRNA,input$select_mRNA_result,status$mRNA_trigger), {
  if(length(input$select_mRNA_result)>0 && status$mRNA_set){
    choice$mRNA <- paste(input$select_mRNA,input$select_mRNA_result,status$mRNA_trigger) %>% stringr::str_replace_all(' ','')
    mRNA_plot_result %>% dplyr::filter(symbol %in% input$select_mRNA_result) -> one_plot
    if(dataset_number$mRNA == 1 ){
      output[[choice$mRNA]] <- renderPlot({one_plot %>% expr_box_plot_mRNA()}, height = 300, width = 260)
      output$`mRNA-picdownload` <- downloadHandler(
        filename = function() {
          paste("Differential_Expression", ".", input$`mRNA-pictype`, sep = "")
        },
        content = function(file){
          ggsave(file,expr_box_plot_mRNA(one_plot),device = input$`mRNA-pictype`,width = input$`mRNA-d_width`,height = input$`mRNA-d_height`  )}
      )
    }
    else if(dataset_number$mRNA<5 && dataset_number$mRNA>1){
      output[[choice$mRNA]] <- renderPlot({one_plot %>% expr_box_plot_mRNA()},height = 300, width = dataset_number$mRNA*260)
      output$`mRNA-picdownload` <- downloadHandler(
        filename = function() {
          paste("Differential_Expression", ".", input$`mRNA-pictype`, sep = "")
        },
        content = function(file){
          ggsave(file,expr_box_plot_mRNA(one_plot),device = input$`mRNA-pictype`,width = input$`mRNA-d_width`,height = input$`mRNA-d_height`  )}
      )
    }
    else{
      output[[choice$mRNA]] <- renderPlot({one_plot %>% expr_box_plot_mRNA()},height = 300+6*dataset_number$mRNA)
      output$`mRNA-picdownload` <- downloadHandler(
        filename = function() {
          paste("Differential_Expression", ".", input$`mRNA-pictype`, sep = "")
        },
        content = function(file){
          ggsave(file,expr_box_plot_mRNA(one_plot),device = input$`mRNA-pictype`,width = input$`mRNA-d_width`,height = input$`mRNA-d_height`  )}
      )
    }
  }
})

# observeEvent of selectall -----------------------------------------------

observeEvent(input$select_all_mRNA_TCGA, {
  shinyjs::js$TCGAmRNAselectall()
})

observeEvent(input$unselect_all_mRNA_TCGA, {
  shinyjs::js$TCGAmRNAunselectall()
  status$mRNA_result <- FALSE
})
observeEvent(input$select_all_mRNA_GTEX, {
  shinyjs::js$GTEXmRNAselectall()
})

observeEvent(input$unselect_all_mRNA_GTEX, {
  shinyjs::js$GTEXmRNAunselectall()
  status$mRNA_result <- FALSE
})
observeEvent(input$select_all_mRNA_CCLE, {
  shinyjs::js$CCLEmRNAselectall()
})
observeEvent(input$unselect_all_mRNA_CCLE, {
  shinyjs::js$CCLEmRNAunselectall()
  status$mRNA_result <- FALSE
})

