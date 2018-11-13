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
          if(length(b)>0){b} else{"drop"}
        }
      )
    ) -> .v_dedup
  input_miRNA_check$non_match <- .v_dedup %>% dplyr::filter(expression %in% "drop") %>% .$symbol
  .vvv %in% input_miRNA_check$non_match ->.inter
  input_miRNA_check$match <-  .vvv[!.inter]
  match$miRNA <- .v_dedup %>% dplyr::filter(symbol %in% .vvv[!.inter]) %>% .$expression %>% unlist() %>% 
    tibble::tibble(x=.) %>% dplyr::distinct() %>% .$x
  input_miRNA_check$total <- c(input_miRNA_check$match,input_miRNA_check$non_match)
  input_miRNA_check$n_non_match <- length(input_miRNA_check$non_match)
  input_miRNA_check$n_match <- length(.vvv[!.inter])
  input_miRNA_check$n_total <- length(input_miRNA_check$non_match) + length(.vvv[!.inter])
  output$download_total_miRNA_set <- fn_gs_download(data = input_miRNA_check$total,txt = "total_miRNA_set.txt")
  output$download_valid_miRNA_set <- fn_gs_download(data = input_miRNA_check$match,txt = "valid_miRNA_set.txt")
  output$download_miRNA_input_logs <- fn_gs_download(data = input_miRNA_check$n_non_match,txt = "error_miRNA_set.txt")
  if(input_miRNA_check$n_match > 0) {
    status$miRNA_set <- TRUE
    status$miRNA_valid <- TRUE } 
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

# miRNA table print -------------------------------------------------------
expr_box_plot_mirna <-  function(.expr){
  quantile_names <- c("lower.whisker", "lower.hinge", "median", "upper.hinge", "upper.whisker")
  .expr %>% dplyr::rename(TPM = expr,symbol = name) %>%
    tidyr::separate(col = cancer_types, into = c("cancer_types", "types")) %>% 
    dplyr::mutate(types = stringr::str_to_title(types)) %>% 
    dplyr::mutate(name = purrr::rep_along(cancer_types, quantile_names)) %>% 
    tidyr::spread(key = name, value = TPM) %>% 
    ggplot(mapping = aes(x = cancer_types, middle = median,
                         ymin = lower.whisker, ymax = upper.whisker,
                         lower = lower.hinge, upper = upper.hinge, color = types)) +
    scale_color_manual(values = c("midnightblue", "red3")) +
    geom_errorbar(width = 0.1, position = position_dodge(0.25)) +
    geom_boxplot(stat = 'identity', width = 0.2, position = position_dodge(0.25)) +
    facet_wrap(~symbol, ncol = 1,scales = "free_y", strip.position = 'right') +
    theme(
      text = element_text(colour = 'black'),
      
      axis.line = element_line(color = "black", size = 0.1),
      axis.title.x = element_blank(),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, colour = 'black'),
      
      strip.background = element_rect(fill = "white", color = "white"),
      
      panel.background = element_rect(fill = "white", color = "black", size = 0.5),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      
      legend.position = 'top',
      legend.key = element_rect(fill = 'white')
    ) +
    labs(
      x = 'Cancer Types',
      y = 'TPM(log2)'
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
observeEvent(c(input$select_miRNA_TCGA,reset$miRNA),{
  if(length(input$select_miRNA_TCGA)>0 && status$miRNA_valid){
      dataset_number$miRNA <-  length(input$select_miRNA_TCGA)  
      TCGA_miRNA %>% dplyr::filter(cancer_types %in% input$select_miRNA_TCGA) %>%
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
        dplyr::select(cancer_types=tmp,gene,name,expr) ->> expr_clean
    status$miRNA_result <- TRUE
    if(status$miRNA_trigger){status$miRNA_trigger <- FALSE} else{status$miRNA_trigger <- TRUE}
    expr_clean %>% dplyr::group_by(cancer_types,gene,name) %>% dplyr::slice(1:5) %>% tidyr::drop_na() %>% dplyr::ungroup() %>% 
      dplyr::mutate(tmp = log2(expr+1)) %>% dplyr::select(cancer_types,gene,name,expr = tmp) ->> mirna_plot_result
    expr_clean %>% dplyr::group_by(cancer_types,gene,name) %>% dplyr::slice(6) %>% tidyr::drop_na() %>% dplyr::ungroup() %>% 
      dplyr::mutate(tmp = log2(expr+1)) %>% dplyr::select(cancer_types,gene,name,expr = tmp) ->>mirna_table_result
    mirna_plot_result %>% dplyr::select(name) %>% dplyr::distinct() %>% .$name -> plot_number$miRNA
    choice$miRNA <- mirna_plot_result %>% dplyr::filter(name %in% plot_number$miRNA[1]) %>% dplyr::select(gene) %>% dplyr::distinct() %>% .$gene 
    number <- length(plot_number$miRNA)
    if(number < 5){
      if(dataset_number$miRNA == 1 ){
        output$expr_bubble_plot_mirna <- renderPlot({mirna_plot_result %>% expr_box_plot_mirna()}, height = number*300, width = 260)
        output$`miRNA-picdownload` <- downloadHandler(
          filename = function() {
            paste("Differential_Expression", ".", input$`miRNA-pictype`, sep = "")
          },
          content = function(file){
            ggsave(file,expr_box_plot_mirna(mirna_plot_result),device = input$`miRNA-pictype`,width = input$`miRNA-d_width`,height = input$`miRNA-d_height`  )}
        )
        }
      else if(dataset_number$miRNA <5 ){
        output$expr_bubble_plot_mirna <- renderPlot({mirna_plot_result %>% 
            expr_box_plot_mirna()},height = number*300, width = dataset_number$miRNA*260)
        output$`miRNA-picdownload` <- downloadHandler(
          filename = function() {
            paste("Differential_Expression", ".", input$`miRNA-pictype`, sep = "")
          },
          content = function(file){
            ggsave(file,expr_box_plot_mirna(mirna_plot_result),device = input$`miRNA-pictype`,width = input$`miRNA-d_width`,height = input$`miRNA-d_height`  )}
        )
      }
      else{
        output$expr_bubble_plot_mirna <- renderPlot({mirna_plot_result %>% expr_box_plot_mirna()},height = 6*dataset_number$miRNA+number*300)
        output$`miRNA-picdownload` <- downloadHandler(
          filename = function() {
            paste("Differential_Expression", ".", input$`miRNA-pictype`, sep = "")
          },
          content = function(file){
            ggsave(file,expr_box_plot_mirna(mirna_plot_result),device = input$`miRNA-pictype`,width = input$`miRNA-d_width`,height = input$`miRNA-d_height`  )}
        )
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
      output[[choice$miRNA]] <- renderPlot({one_plot %>% expr_box_plot_mirna()}, height = 300, width = 260)
      output$`miRNA-picdownload` <- downloadHandler(
        filename = function() {
          paste("Differential_Expression", ".", input$`miRNA-pictype`, sep = "")
        },
        content = function(file){
          ggsave(file,expr_box_plot_mirna(one_plot),device = input$`miRNA-pictype`,width = input$`miRNA-d_width`,height = input$`miRNA-d_height`  )}
      )
      }
    else if(dataset_number$miRNA<5){
      output[[choice$miRNA]] <- renderPlot({one_plot %>% expr_box_plot_mirna()}, height = 300, width = dataset_number$miRNA*260)
      output$`miRNA-picdownload` <- downloadHandler(
        filename = function() {
          paste("Differential_Expression", ".", input$`miRNA-pictype`, sep = "")
        },
        content = function(file){
          ggsave(file,expr_box_plot_mirna(one_plot),device = input$`miRNA-pictype`,width = input$`miRNA-d_width`,height = input$`miRNA-d_height`  )}
      )
    }
    else{
      output[[choice$miRNA]] <- renderPlot({one_plot %>% expr_box_plot_mirna()}, height = 300+6*dataset_number$miRNA)
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