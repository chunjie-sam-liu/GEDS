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
  input_protein_check$non_match <- .v_dedup %>% dplyr::filter(expression %in% "drop") %>% .$symbol
  .vvv %in% input_protein_check$non_match ->.inter
  input_protein_check$match <-  .vvv[!.inter]
  input_protein_check$total <- c(input_protein_check$match,input_protein_check$non_match)
  input_protein_check$n_non_match <- length(input_protein_check$non_match)
  input_protein_check$n_match <- length(.vvv[!.inter])
  input_protein_check$n_total <- length(input_protein_check$non_match) + length(.vvv[!.inter])
  if(input_protein_check$n_match > 0) {
    status$protein_set <- TRUE
    status$protein_valid <- TRUE 
    match$protein <- .v_dedup %>% dplyr::filter(symbol %in% .vvv[!.inter]) %>% .$expression %>% unlist() %>% 
      tibble::tibble(x=.) %>% dplyr::distinct() %>% .$x
    if(length(input_protein_check$non_match) > 0){
      status$protein_invalid <- TRUE
      output$protein_invalid <- renderText({paste("The list below is invalid:", input_protein_check$non_match %>% toString(), sep = "\n")})
    }
    else{
      status$protein_invalid <- FALSE
    }
    } 
  else {
    status$protein_set <- FALSE
    status$protein_result <- FALSE
    status$protein_valid <- FALSE}
}

# Example -----------------------------------------------------------------

observeEvent(input$protein_example, {
  status$protein_set <- FALSE
  status$protein_result <- FALSE
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
  output$expr_bubble_plot_protein <- NULL
  output$expr_dt_comparison_protein <- NULL
})

# Monitor search ----------------------------------------------------------

validate_input_protein_set <- eventReactive(
  eventExpr = input$input_protein_set_search,
  ignoreNULL = TRUE,
  valueExpr = {
    if(reset$protein){reset$protein <- FALSE} else{reset$protein <- TRUE}
    if (is.null(input$input_protein_set) || input$input_protein_set == "") {
      error$protein_set <- "Error: Please input protein symbol."
      status$protein_trigger <- if (status$protein_trigger == TRUE) FALSE else TRUE
      return()
    }
    # check gene
    .v_igs <- check_protein_set(.s = input$input_protein_set)
    # validate genes
    validate_protein_set(.v = .v_igs, .total_symbol = total_protein_symbol, input_protein_check = input_protein_check)
  }
)

###add new
observeEvent(c(reset$protein),{
  if(status$protein_set){
    dataset_number$protein <- 30
    TCGA_protein_result()
    MCLP_protein_result()
    a <- TCGA_protein_plot_result %>% dplyr::select(protein) %>% dplyr::distinct() %>% .$protein
    b <- MCLP_protein_table_result %>% dplyr::select(protein) %>% dplyr::distinct() %>% .$protein
    plot_number$protein <- c(a,b) %>% tibble::tibble(x = .) %>% dplyr::distinct() %>% .$x
    if(status$protein_trigger){status$protein_trigger <- FALSE} else{status$protein_trigger <- TRUE}
    status$protein_result <- TRUE
    return(TCGA_protein_plot_result)
    return(MCLP_protein_table_result)
  }}
)

TCGA_protein_result <- function(){
  TCGA_protein %>%
    dplyr::mutate(
      expr = purrr::map(
        .x = summary,
        .f = function(.x) {
          .x %>%
            dplyr::filter(symbol %in% match$protein)  %>% tidyr::unnest()
        }
      )
    ) %>% dplyr::select(-summary) %>% tidyr::unnest() %>% dplyr::rename(expr=tumor) -> expr_clean
  expr_clean %>% dplyr::group_by(cancer_types,symbol,protein) %>% dplyr::slice(1:5) %>% tidyr::drop_na() %>% dplyr::ungroup()  ->> TCGA_protein_plot_result
  expr_clean %>% dplyr::group_by(cancer_types,symbol,protein) %>% dplyr::slice(6) %>% tidyr::drop_na() %>% dplyr::ungroup() ->> TCGA_protein_table_result
  output$expr_dt_comparison_TCGA_protein <- DT::renderDataTable({expr_clean_datatable_protein(TCGA_protein_table_result)})
  return(TCGA_protein_plot_result)
  
}

MCLP_protein_result <- function(){
  MCLP_protein %>%
    dplyr::mutate(
      expr = purrr::map(
        .x = summary,
        .f = function(.x) {
          .x %>%
            dplyr::filter(symbol %in% match$protein) %>% tidyr::unnest()
        }
      )
    ) %>% dplyr::select(-summary) %>% tidyr::unnest() %>%　dplyr::rename(cancer_types = tis,expr=summary) -> expr_clean
  expr_clean %>% dplyr::group_by(cancer_types,symbol,protein) %>% dplyr::slice(6) %>% tidyr::drop_na() %>% dplyr::ungroup() ->> MCLP_protein_table_result
  output$expr_dt_comparison_MCLP_protein <- DT::renderDataTable({expr_clean_datatable_protein(MCLP_protein_table_result)})
  return(MCLP_protein_table_result)
}

###add new

# protein table_print -------------------------------------------------------------
expr_buble_plot_protein <-  function(.expr,.type){
  quantile_names <- c("lower.whisker", "lower.hinge", "median", "upper.hinge", "upper.whisker")
  if(.type == "TCGA"){
  nu <- .expr$cancer_types %>% length()
  .expr %>% dplyr::rename(FPKM = expr) %>%
    dplyr::mutate(tmp = paste(site,"(",cancer_types,")")) %>%
    dplyr::select(cancer_types=tmp,symbol,protein,FPKM) %>%
    dplyr::mutate(name = purrr::rep_along(cancer_types, quantile_names))%>%
    tidyr::spread(key = name, value = FPKM) %>% 
      dplyr::group_by(protein) %>% dplyr::arrange(symbol,desc(median)) %>% dplyr::ungroup() -> t
    t %>% .$cancer_types -> order
    t %>%
    ggplot(mapping = aes(x = cancer_types, middle = median,
                         ymin = lower.whisker, ymax = upper.whisker,
                         lower = lower.hinge, upper = upper.hinge, color = cancer_types)) -> p
    TCGA_color %>% head(n = nu) %>% dplyr::select(color) %>% dplyr::pull(color) -> .color
    p +
    scale_color_manual(values = .color) +
    scale_x_discrete(limits= order) +
    geom_errorbar(width = 0.3, position = position_dodge(0.75)) +
    geom_boxplot(stat = 'identity', width = 0.6, position = position_dodge(0.75)) +
    facet_wrap(~protein, ncol = 1,scales = "free_y", strip.position = 'right') +
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
    ) +
    labs(
      title = 'Cancer Types (The Cancer Genome Atlas (TCGA))',
      x = 'Cancer Types',
      y = 'Protein expression'
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
  else{
    .expr %>% dplyr::rename(FPKM = expr) %>%
      dplyr::group_by(protein) %>% 
      dplyr::arrange(symbol,desc(FPKM)) %>% 
      dplyr::ungroup() -> t
    t %>%  .$cancer_types -> order
    t %>%
      ggplot(mapping = aes(x = cancer_types, y = FPKM , color = cancer_types)) +
      scale_x_discrete(limits = order) +
      geom_bar(stat = "identity",colour = "black",width = 0.6, fill = "#2cdbf9") +
      facet_wrap(~protein, ncol = 1, scales = "free", strip.position = 'right') +
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
      ) +   
      labs(
        title = "Cell lines (MD Anderson Cell Lines Project (MCLP))",
        x = 'Cancer Types',
        y = 'Protein expression'
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
  }
}
expr_clean_datatable_protein <- function(.expr_clean) {
  DT::datatable(
    data = .expr_clean,
    options = list(
      pageLength = 10,
      autoWidth = TRUE,
      dom = "Bfrtip",
      buttons = c("copy", "csv", "print")
    ),
    rownames = FALSE,
    colnames = c("Cancer Types/Tissues", "Symbol", "Protein", "Mean expr."),
    filter = "top",
    extensions = "Buttons",
    style = "bootstrap",
    class = "table-bordered table-condensed"
  ) %>% 
    DT::formatSignif(columns = c("expr"), digits = 2) %>%
    DT::formatRound(columns = c("expr"), 2)
}

# ObserveEvent ------------------------------------------------------------

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
observeEvent(c(input$select_protein_result,status$protein_trigger), {
  if(length(input$select_protein_result)>0 && status$protein_set){
    choice$protein <- paste(input$select_protein_result,status$protein_trigger) %>% stringr::str_replace_all(' ','')
    TCGA_protein_plot_result %>% dplyr::filter(protein %in% input$select_protein_result) -> TCGA_one_plot
    MCLP_protein_table_result %>% dplyr::filter(protein %in% input$select_protein_result) -> MCLP_one_plot
    if(length(TCGA_one_plot$cancer_types)*length(MCLP_one_plot$cancer_types) > 0){
      TCGA_protein_plot_result %>% dplyr::filter(protein %in% input$select_protein_result) -> TCGA_one_plot
      MCLP_protein_table_result %>% dplyr::filter(protein %in% input$select_protein_result) -> MCLP_one_plot
      TCGA_plot <- expr_buble_plot_protein(TCGA_one_plot,"TCGA")
      MCLP_plot <- expr_buble_plot_protein(MCLP_one_plot,"MCLP")
      ggpubr::ggarrange(
        TCGA_plot,MCLP_plot,
        ncol = 1,nrow = 3, heights = c(1.2,1)
      ) -> plot_result
      output[[choice$protein]] <- renderPlot({
        plot_result
      },height = 1000)
      output$`protein-picdownload` <- downloadHandler(
        filename = function() {
          paste("Differential_Expression", ".", input$`protein-pictype`, sep = "")
        },
        content = function(file){
          ggsave(file,plot_result,device = input$`protein-pictype`,width = input$`protein-d_width`,height = input$`protein-d_height`  )}
      )
    }
  }
})
