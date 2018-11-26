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
  input_mRNA_check$total <- c(input_mRNA_check$match,input_mRNA_check$non_match)
  input_mRNA_check$n_non_match <- length(input_mRNA_check$non_match)
  input_mRNA_check$n_match <- length(.vvv[!.inter])
  input_mRNA_check$n_total <- length(input_mRNA_check$non_match) + length(.vvv[!.inter])
  if(input_mRNA_check$n_match > 0) {
    status$mRNA_set <- TRUE
    status$mRNA_valid <- TRUE 
    match$mRNA <- .v_dedup %>% dplyr::filter(symbol %in% .vvv[!.inter]) %>% .$expression %>% unlist() %>% tibble::tibble(x = .) %>% dplyr::distinct() %>% .$x
    if(length(input_mRNA_check$non_match) > 0){
      status$mRNA_invalid <- TRUE
      output$mRNA_invalid <- renderText({paste("The list below is invalid:", input_mRNA_check$non_match %>% toString())})
    }
    else{
      status$mRNA_invalid <- FALSE
    }
    } 
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
  error$mRNA_set <- ""
})

# Monitor search ----------------------------------------------------------

validate_input_mRNA_set <- eventReactive(
  eventExpr = input$input_mRNA_set_search,
  ignoreNULL = TRUE,
  valueExpr = {
    if(reset$mRNA){reset$mRNA <- FALSE} else{reset$mRNA <- TRUE}
    error$mRNA_set <- ""
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

#####add new
observeEvent(c(reset$mRNA),{
  if(status$mRNA_set){
  dataset_number$mRNA <- 30
  TCGA_mRNA_result()
  GTEX_mRNA_result()
  CCLE_mRNA_result()
  a <- TCGA_mRNA_plot_result %>% dplyr::select(symbol) %>% dplyr::distinct() %>% .$symbol
  b <- GTEX_mRNA_table_result %>% dplyr::select(symbol) %>% dplyr::distinct() %>% .$symbol
  c <- CCLE_mRNA_table_result %>% dplyr::select(symbol) %>% dplyr::distinct() %>% .$symbol
  plot_number$mRNA <- c(a,b,c) %>% tibble::tibble(x = .) %>% dplyr::distinct() %>% .$x
  status$mRNA_result <- TRUE
  if(status$mRNA_trigger){status$mRNA_trigger <- FALSE} else{status$mRNA_trigger <- TRUE}
  return(TCGA_mRNA_plot_result)
  return(GTEX_mRNA_table_result)
  return(CCLE_mRNA_table_result)
}}
)
TCGA_mRNA_result <- function(){
  TCGA_mRNA %>% 
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
    dplyr::select(cancer_types = tmp,site,symbol,expr) -> expr_clean 
  expr_clean %>% dplyr::group_by(cancer_types,site,symbol) %>% dplyr::slice(1:5) %>% tidyr::drop_na() %>% dplyr::ungroup() %>% 
    dplyr::mutate(tmp = log2(expr+1)) %>% dplyr::select(cancer_types,site,symbol,expr = tmp) ->> TCGA_mRNA_plot_result
  expr_clean %>% dplyr::group_by(cancer_types,site,symbol) %>% dplyr::slice(6) %>% tidyr::drop_na() %>% dplyr::ungroup() %>% 
    dplyr::mutate(tmp = log2(expr+1)) %>% dplyr::select(cancer_types,symbol,expr = tmp) %>%
    dplyr::left_join(.,total_mRNA_symbol,by = "symbol") %>% dplyr::select(cancer_types,symbol,alias,expr) ->> TCGA_mRNA_table_result
    output$expr_dt_comparison_TCGA_mRNA <- DT::renderDataTable({expr_clean_datatable_mRNA(TCGA_mRNA_table_result)})
    return(TCGA_mRNA_plot_result)
}

GTEX_mRNA_result <- function(){
  GTEX_mRNA %>%
    dplyr::mutate(
      expr = purrr::map(
        .x = summary,
        .f = function(.x) {
          .x %>%
            dplyr::filter(symbol %in% match$mRNA) %>% dplyr::select(-ensembl_gene_id) %>%
            tidyr::unnest()
        }
      )
    ) %>% dplyr::select(-summary) %>% tidyr::unnest() %>% dplyr::rename(cancer_types = SMTS,expr=summary) -> expr_clean 
  expr_clean %>% dplyr::group_by(cancer_types,symbol) %>% dplyr::slice(1:5) %>% tidyr::drop_na() %>% dplyr::ungroup() ->> GTEX_mRNA_plot_result
  expr_clean %>% dplyr::group_by(cancer_types,symbol) %>% dplyr::slice(6) %>% tidyr::drop_na() %>% dplyr::ungroup() %>%
    dplyr::left_join(.,total_mRNA_symbol,by = "symbol") %>% dplyr::select(cancer_types,symbol,alias,expr) ->> GTEX_mRNA_table_result
  output$expr_dt_comparison_GTEX_mRNA <- DT::renderDataTable({expr_clean_datatable_mRNA(GTEX_mRNA_table_result)})
  return(GTEX_mRNA_table_result)
}

CCLE_mRNA_result <- function(){
  CCLE_mRNA  %>% 
    dplyr::mutate(
      expr = purrr::map(
        .x = summary,
        .f = function(.x) {
          .x %>%
            dplyr::filter(symbol %in% match$mRNA) %>% tidyr::unnest()
        }
      )
    ) %>% dplyr::select(-summary) %>% tidyr::unnest() %>% dplyr::rename(cancer_types = tissue,expr=summary) -> expr_clean
  expr_clean %>% dplyr::group_by(cancer_types,symbol) %>% dplyr::slice(1:5) %>% tidyr::drop_na() %>% dplyr::ungroup()  ->> CCLE_mRNA_plot_result
  expr_clean %>% dplyr::group_by(cancer_types,symbol) %>% dplyr::slice(6) %>% tidyr::drop_na() %>% dplyr::ungroup() %>%
    dplyr::left_join(.,total_mRNA_symbol,by = "symbol") %>% dplyr::select(cancer_types,symbol,alias,expr) ->> CCLE_mRNA_table_result
  output$expr_dt_comparison_CCLE_mRNA <- DT::renderDataTable({expr_clean_datatable_mRNA(CCLE_mRNA_table_result)})
  return(CCLE_mRNA_table_result)
}
#####add new

# mRNA table_print -------------------------------------------------------------
expr_box_plot_mRNA <-  function(.expr,.type){
  quantile_names <- c("lower.whisker", "lower.hinge", "median", "upper.hinge", "upper.whisker")
  ###add new
  if(.type == "TCGA"){
  ### add new
    .expr %>% dplyr::rename(FPKM = expr) %>%
      tidyr::separate(col = cancer_types, into = c("cancer_types", "types")) %>%
      dplyr::mutate(tmp = paste(site,"(",cancer_types,")")) %>%
      dplyr::select(cancer_types=tmp,types,symbol,FPKM) %>%
      dplyr::mutate(types = stringr::str_to_title(types)) %>% 
      dplyr::mutate(name = purrr::rep_along(cancer_types, quantile_names)) %>% 
    ###add new
    tidyr::spread(key = name, value = FPKM) %>% 
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
      facet_wrap(~symbol, ncol = 1, scales = "free", strip.position = 'right') +
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
        
        legend.position = 'right',
        legend.box = "vertical",
        legend.key = element_rect(fill = 'white'),
        plot.title = element_text(hjust = 0.5,size = 30)
      ) +
      labs(
        title = "Cancer Types (TCGA)",
        x = 'Cancer Types',
        y = 'RSEM(log2)'
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
    ###add new
  }
  else{
    nu <- length(.expr$cancer_types)
    .expr %>% dplyr::rename(FPKM = expr) %>%
      dplyr::group_by(symbol) %>% dplyr::arrange(symbol,desc(FPKM)) %>% dplyr::ungroup() -> t
    t %>%  .$cancer_types -> order
    t %>%
      ggplot(mapping = aes(x = cancer_types, y = FPKM , color = cancer_types)) +
      scale_x_discrete(limits = order) +
      geom_bar(stat = "identity",colour = "black",width = 0.6, fill = "#2cdbf9") +
      facet_wrap(~symbol, ncol = 1, scales = "free", strip.position = 'right') +
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
        
        legend.position = 'none',
        legend.key = element_rect(fill = 'white'),
        plot.title = element_text(hjust = 0.5,size = 30)
      ) -> p 
    if(.type == "GTEX"){
      p +   
        labs(
          title = "Tissues (GTEx)",
          x = 'Cancer Types',
          y = 'FPKM'
        ) -> q} 
    else{
      p +   
        labs(
          title = "Cell lines (CCLE)",
          x = 'Cancer Types',
          y = 'FPKM'
        ) -> q}
    q +
      guides(
        color = guide_legend(
          # legend title
          title = "Cancer Types",
          title.position = "left",
          
          # legend label
          label.position = "right",
          # label.theme = element_text(size = 14),
          reverse = TRUE
        )
      )
    ###add new
  }
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
observeEvent(c(input$select_mRNA_result,status$mRNA_trigger), {
  if(length(input$select_mRNA_result)>0 && status$mRNA_set){
    ###add new
    TCGA_mRNA_plot_result %>% dplyr::filter(symbol %in% input$select_mRNA_result) -> TCGA_one_plot
    GTEX_mRNA_table_result %>% dplyr::filter(symbol %in% input$select_mRNA_result) -> GTEX_one_plot
    CCLE_mRNA_table_result %>% dplyr::filter(symbol %in% input$select_mRNA_result) -> CCLE_one_plot
    if(length(TCGA_one_plot$cancer_types)*length(GTEX_one_plot$cancer_types)*length(CCLE_one_plot$cancer_types) > 0 ){
      choice$mRNA <- paste(input$select_mRNA_result,status$mRNA_trigger) %>% stringr::str_replace_all(' ','')
      TCGA_plot <- expr_box_plot_mRNA(TCGA_one_plot,"TCGA")
      GTEX_plot <- expr_box_plot_mRNA(GTEX_one_plot,"GTEX")
      CCLE_plot <- expr_box_plot_mRNA(CCLE_one_plot,"CCLE")
      ggpubr::ggarrange(
        TCGA_plot,CCLE_plot,GTEX_plot,
        ncol = 1,nrow = 3, heights = c(1.5,1.2,1)
      ) -> plot_result
      output[[choice$mRNA]] <- renderPlot({
        plot_result
      }, height = 1200 )
      output$`mRNA-picdownload` <- downloadHandler(
        filename = function() {
          paste("Differential_Expression", ".", input$`mRNA-pictype`, sep = "")
        },
        content = function(file){
          ggsave(file,plot_result,device = input$`mRNA-pictype`,width = input$`mRNA-d_width`,height = input$`mRNA-d_height`  )}
      )
      ###add new
    }
  }
})

