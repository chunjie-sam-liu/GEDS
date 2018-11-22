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

#####add new
observeEvent(c(reset$mRNA),{
  if(status$mRNA_set){
  dataset_number$mRNA <- 30
  TCGA_result()
  GTEX_result()
  CCLE_result()
  a <- TCGA_mRNA_plot_result %>% dplyr::select(symbol) %>% dplyr::distinct() %>% .$symbol
  b <- GTEX_mRNA_plot_result %>% dplyr::select(symbol) %>% dplyr::distinct() %>% .$symbol
  c <- CCLE_mRNA_plot_result %>% dplyr::select(symbol) %>% dplyr::distinct() %>% .$symbol
  plot_number$mRNA <- c(a,b,c) %>% tibble::tibble(x = .) %>% dplyr::distinct() %>% .$x
  if(status$mRNA_trigger){status$mRNA_trigger <- FALSE} else{status$mRNA_trigger <- TRUE}
  status$mRNA_result <- TRUE
  return(TCGA_mRNA_plot_result)
  return(GTEX_mRNA_plot_result)
  return(CCLE_mRNA_plot_result)
}}
)
TCGA_result <- function(){
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

GTEX_result <- function(){
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
  expr_clean %>% dplyr::group_by(cancer_types,symbol) %>% dplyr::slice(1:5) %>% tidyr::drop_na() %>% dplyr::ungroup() %>% 
    dplyr::mutate(tmp = log2(expr+1)) %>% dplyr::select(cancer_types,symbol,expr = tmp) ->> GTEX_mRNA_plot_result
  expr_clean %>% dplyr::group_by(cancer_types,symbol) %>% dplyr::slice(6) %>% tidyr::drop_na() %>% dplyr::ungroup() %>% 
    dplyr::mutate(tmp = log2(expr+1)) %>% dplyr::select(cancer_types,symbol,expr = tmp) %>%
    dplyr::left_join(.,total_mRNA_symbol,by = "symbol") %>% dplyr::select(cancer_types,symbol,alias,expr) ->> GTEX_mRNA_table_result
  output$expr_dt_comparison_GTEX_mRNA <- DT::renderDataTable({expr_clean_datatable_mRNA(GTEX_mRNA_table_result)})
  return(GTEX_mRNA_plot_result)
}

CCLE_result <- function(){
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
  expr_clean %>% dplyr::group_by(cancer_types,symbol) %>% dplyr::slice(1:5) %>% tidyr::drop_na() %>% dplyr::ungroup() %>% 
    dplyr::mutate(tmp = log2(expr+1)) %>% dplyr::select(cancer_types,symbol,expr = tmp) ->> CCLE_mRNA_plot_result
  expr_clean %>% dplyr::group_by(cancer_types,symbol) %>% dplyr::slice(6) %>% tidyr::drop_na() %>% dplyr::ungroup() %>% 
    dplyr::mutate(tmp = log2(expr+1)) %>% dplyr::select(cancer_types,symbol,expr = tmp) %>%
    dplyr::left_join(.,total_mRNA_symbol,by = "symbol") %>% dplyr::select(cancer_types,symbol,alias,expr) ->> CCLE_mRNA_table_result
  output$expr_dt_comparison_CCLE_mRNA <- DT::renderDataTable({expr_clean_datatable_mRNA(CCLE_mRNA_table_result)})
  return(CCLE_mRNA_plot_result)
}
#####add new

# mRNA table_print -------------------------------------------------------------
expr_box_plot_mRNA <-  function(.expr,.type){
  quantile_names <- c("lower.whisker", "lower.hinge", "median", "upper.hinge", "upper.whisker")
  ###before
  #if(input$select_mRNA == "Cancer types"){
    #nu <- length(input$select_mRNA_TCGA)
  ###before
  ###add new
  if(.type == "TCGA"){
    nu = 33
  ### add new
    .expr %>% dplyr::rename(FPKM = expr) %>%
      tidyr::separate(col = cancer_types, into = c("cancer_types", "types")) %>%
      dplyr::mutate(tmp = paste(site,"(",cancer_types,")")) %>%
      dplyr::select(cancer_types=tmp,types,symbol,FPKM) %>%
      dplyr::mutate(types = stringr::str_to_title(types)) %>% 
      dplyr::mutate(name = purrr::rep_along(cancer_types, quantile_names)) %>% 
    ###before
      #tidyr::spread(key = name, value = FPKM) -> dd
    #if(nu > 1){
    #dd %>% 
      #dplyr::filter(types == 'Tumor') %>% 
      #dplyr::arrange(symbol, median) %>% 
      #dplyr::mutate(.r = dplyr::row_number() %>% rev()) %>% 
      #dplyr::select(cancer_types, symbol, .r) %>% 
      #dplyr::right_join(dd, by = c('cancer_types', 'symbol')) -> s
      #if(nu < 5){
        #s %>%
        #dplyr::mutate(.r = 0.5 * .r) %>%
        #dplyr::arrange(-.r) -> ddd
      #}
      #else{
        #s %>% 
        #dplyr::mutate(.r = 0.3 * .r-0.3) %>%
        #dplyr::arrange(-.r) -> ddd
      #}
    #ddd %>% 
    #ggplot(mapping = aes(x = .r, middle = median,
     #                    ymin = lower.whisker, ymax = upper.whisker,
      #                   lower = lower.hinge, upper = upper.hinge, color = types)) +
      #scale_x_continuous(breaks = ddd$.r,labels = ddd$cancer_types) +
      #scale_color_manual(values = c("midnightblue", "red3")) -> p
  #}
  #else{
    #dd %>% 
      #ggplot(mapping = aes(x = cancer_types, middle = median,
       #                    ymin = lower.whisker, ymax = upper.whisker,
        #                   lower = lower.hinge, upper = upper.hinge, color = types)) +
      #scale_color_manual(values = c("midnightblue", "red3")) -> p
  #}
    ###before
    ###add new
    tidyr::spread(key = name, value = FPKM) %>% 
    dplyr::group_by(symbol) %>% dplyr::arrange(symbol,desc(median)) %>% dplyr::ungroup() -> t
    t %>% dplyr::filter(types %in% "Tumor") %>% .$cancer_types -> order
    t %>%
    ggplot(mapping = aes(x = cancer_types, middle = median,
                         ymin = lower.whisker, ymax = upper.whisker,
                         lower = lower.hinge, upper = upper.hinge, color = types)) +
    scale_x_discrete(limits= order) +
    scale_color_manual(values = c("midnightblue", "red3")) -> p
    p +
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
        
        legend.position = 'top',
        legend.key = element_rect(fill = 'white'),
        plot.title = element_text(hjust = 0.5,size = 18)
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
    ###add new
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
    ###add new
    p +
      geom_errorbar(width = 0.1, position = position_dodge(0.25, preserve = 'single')) +
      geom_boxplot(stat = 'identity', width = 0.2, position = position_dodge(0.25, preserve = 'single')) +
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
        
        legend.position = 'top',
        legend.key = element_rect(fill = 'white'),
        plot.title = element_text(hjust = 0.5,size = 18)
      ) +
      labs(
        #title = "GTeX",
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
          reverse = TRUE
        )
      )
    ###add new
  }
  ###before
    #p +
    #geom_errorbar(width = 0.1, position = position_dodge(0.25, preserve = 'single')) +
    #geom_boxplot(stat = 'identity', width = 0.2, position = position_dodge(0.25, preserve = 'single')) +
    #facet_wrap(~symbol, ncol = 1, scales = "free", strip.position = 'right') +
    
    #theme(
     #text = element_text(colour = 'black', size = 18),
      
      #axis.line = element_line(color = "black", size = 0.1),
      #axis.title.x = element_blank(),
      #axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, colour = 'black'),
      #axis.text.y = element_text(color = 'black', size = 14),
      
      #strip.background = element_rect(fill = NA, color = "white"),
      
      #panel.background = element_rect(fill = "white", color = "black", size = 0.5),
      #panel.grid.major.y = element_blank(),
      #panel.grid.minor.y = element_blank(),
      
      #legend.position = 'top',
      #legend.key = element_rect(fill = 'white'),
      #plot.title = element_text(hjust = 0.5,size = 18)
    #) +
    #labs(
      #x = 'Cancer Types',
      #y = 'FPKM(log2)'
    #) +
    #guides(
      #color = guide_legend(
        ## legend title
        #title = "Cancer Types",
        #title.position = "left",
        
        ## legend label
        #label.position = "right",
        
        #nrow = 2,
        #reverse = TRUE
      #)
    #)
    ###before
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
        TCGA_mRNA %>% dplyr::filter(site %in% input$select_mRNA_TCGA) -> p
        dataset_number$mRNA <- p %>% .$cancer_types %>% length()
        p %>%
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
          dplyr::mutate(tmp = log2(expr+1)) %>% dplyr::select(cancer_types,site,symbol,expr = tmp) ->> mRNA_plot_result
        expr_clean %>% dplyr::group_by(cancer_types,site,symbol) %>% dplyr::slice(6) %>% tidyr::drop_na() %>% dplyr::ungroup() %>% 
          dplyr::mutate(tmp = log2(expr+1)) %>% dplyr::select(cancer_types,symbol,expr = tmp) %>%
          dplyr::left_join(.,total_mRNA_symbol,by = "symbol") %>% dplyr::select(cancer_types,symbol,alias,expr) ->> mRNA_table_result
        }
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
        ) %>% dplyr::select(-summary) %>% tidyr::unnest() %>% dplyr::rename(cancer_types = SMTS,expr=summary) -> expr_clean 
        expr_clean %>% dplyr::group_by(cancer_types,symbol) %>% dplyr::slice(1:5) %>% tidyr::drop_na() %>% dplyr::ungroup() %>% 
          dplyr::mutate(tmp = log2(expr+1)) %>% dplyr::select(cancer_types,symbol,expr = tmp) ->> mRNA_plot_result
        expr_clean %>% dplyr::group_by(cancer_types,symbol) %>% dplyr::slice(6) %>% tidyr::drop_na() %>% dplyr::ungroup() %>% 
          dplyr::mutate(tmp = log2(expr+1)) %>% dplyr::select(cancer_types,symbol,expr = tmp) %>%
          dplyr::left_join(.,total_mRNA_symbol,by = "symbol") %>% dplyr::select(cancer_types,symbol,alias,expr) ->> mRNA_table_result
        }
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
        ) %>% dplyr::select(-summary) %>% tidyr::unnest() %>% dplyr::rename(cancer_types = tissue,expr=summary) -> expr_clean
        expr_clean %>% dplyr::group_by(cancer_types,symbol) %>% dplyr::slice(1:5) %>% tidyr::drop_na() %>% dplyr::ungroup() %>% 
          dplyr::mutate(tmp = log2(expr+1)) %>% dplyr::select(cancer_types,symbol,expr = tmp) ->> mRNA_plot_result
        expr_clean %>% dplyr::group_by(cancer_types,symbol) %>% dplyr::slice(6) %>% tidyr::drop_na() %>% dplyr::ungroup() %>% 
          dplyr::mutate(tmp = log2(expr+1)) %>% dplyr::select(cancer_types,symbol,expr = tmp) %>%
          dplyr::left_join(.,total_mRNA_symbol,by = "symbol") %>% dplyr::select(cancer_types,symbol,alias,expr) ->> mRNA_table_result
        }
    if(re == "1"){
      if(status$mRNA_trigger){status$mRNA_trigger <- FALSE} else{status$mRNA_trigger <- TRUE}
    status$mRNA_result <- TRUE
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
    ### before
    #choice$mRNA <- paste(input$select_mRNA,input$select_mRNA_result,status$mRNA_trigger) %>% stringr::str_replace_all(' ','')
    #mRNA_plot_result %>% dplyr::filter(symbol %in% input$select_mRNA_result) -> one_plot
    ### before
    ### add new
    choice$mRNA <- paste(input$select_mRNA_result,status$mRNA_trigger) %>% stringr::str_replace_all(' ','')
    ### add new
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
      ###before
      #output[[choice$mRNA]] <- renderPlot({one_plot %>% expr_box_plot_mRNA()},height = 300+6*dataset_number$mRNA)
      #output$`mRNA-picdownload` <- downloadHandler(
        #filename = function() {
          #paste("Differential_Expression", ".", input$`mRNA-pictype`, sep = "")
        #},
        #content = function(file){
          #ggsave(file,expr_box_plot_mRNA(one_plot),device = input$`mRNA-pictype`,width = input$`mRNA-d_width`,height = input$`mRNA-d_height`  )}
      #)
      ###before
      ###add new
      TCGA_mRNA_plot_result %>% dplyr::filter(symbol %in% input$select_mRNA_result) -> TCGA_one_plot
      GTEX_mRNA_plot_result %>% dplyr::filter(symbol %in% input$select_mRNA_result) -> GTEX_one_plot
      CCLE_mRNA_plot_result %>% dplyr::filter(symbol %in% input$select_mRNA_result) -> CCLE_one_plot
      TCGA_plot <- expr_box_plot_mRNA(TCGA_one_plot,"TCGA")
      GTEX_plot <- expr_box_plot_mRNA(GTEX_one_plot,"GTEX")
      CCLE_plot <- expr_box_plot_mRNA(CCLE_one_plot,"CCLE")
      ggpubr::ggarrange(
        TCGA_plot,GTEX_plot,CCLE_plot,
        labels = c("TCGA","GTEX","CCLE"),
        ncol = 1,nrow = 3
      ) -> plot_result
      output[[choice$mRNA]] <- renderPlot({
        plot_result
      }, height = 1500 )
      output$`mRNA-picdownload` <- downloadHandler(
        filename = function() {
          paste("Differential_Expression", ".", input$`mRNA-pictype`, sep = "")
        },
        content = function(file){
          ggsave(file,expr_box_plot_mRNA(one_plot),device = input$`mRNA-pictype`,width = input$`mRNA-d_width`,height = input$`mRNA-d_height`  )}
      )
      ###add new
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

