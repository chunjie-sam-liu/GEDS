# source by server.R
# saved as protein_server.R

# Clear input -------------------------------------------------------------
###add new
observeEvent(c(input$input_protein_set),{
    status$protein_result <- FALSE
    dataset_number$protein <- 30
    match$protein <- input$input_protein_set
    if(match$protein != ""){
    status$protein_plot <- FALSE
    TCGA_protein_result()
    MCLP_protein_result()
    CCLE_protein_result()
    protein$TCGA <- FALSE
    protein$MCLP <- FALSE
    protein$CCLE <- FALSE
    t <- 0
    m <- 0
    c <- 0
    if(length(TCGA_protein_plot_result) > 1){
      TCGA_protein_plot_result -> TCGA_one_plot
      TCGA_plot <- expr_buble_plot_protein(TCGA_one_plot,"TCGA")
      protein$TCGA <- TRUE
      t <- 1
      status$protein_result <- TRUE
    }
    if(length(MCLP_protein_table_result) > 1){
      MCLP_protein_table_result -> MCLP_one_plot
      MCLP_plot <- expr_buble_plot_protein(MCLP_one_plot,"MCLP")
      protein$MCLP <- TRUE
      m <- 1
      status$protein_result <- TRUE
    }
    if(length(CCLE_protein_table_result) > 1){
      CCLE_protein_table_result -> CCLE_one_plot
      CCLE_plot <- expr_buble_plot_protein(CCLE_one_plot,"CCLE")
      protein$CCLE <- TRUE
      c <- 1
      status$protein_result <- TRUE
    }
    if(t == 1 && m == 1 && c == 1){
      ggpubr::ggarrange(
        TCGA_plot,MCLP_plot,CCLE_plot,
        ncol = 1,nrow = 3, heights = c(1.4,1.2,1.5)
      ) -> plot_result
      output[[match$protein]] <- renderPlot({plot_result},height = 1200)
    }
    else if(t == 1 && m == 1){
      ggpubr::ggarrange(
        TCGA_plot,MCLP_plot,
        ncol = 1,nrow = 2, heights = c(1.3,1)
      ) -> plot_result
      output[[match$protein]] <- renderPlot({plot_result},height = 850)
    }else if(t == 1 && c == 1){
      ggpubr::ggarrange(
        TCGA_plot,CCLE_plot,
        ncol = 1,nrow = 2, heights = c(1.3,1)
      ) -> plot_result
      output[[match$protein]] <- renderPlot({plot_result},height = 900)
    }
    else if(m == 1 && c == 1){
      ggpubr::ggarrange(
        MCLP_plot,CCLE_plot,
        ncol = 1,nrow = 2, heights = c(1,1.3)
      ) -> plot_result
      output[[match$protein]] <- renderPlot({plot_result},height = 850)
    }
    else if(t == 1){
      plot_result <- TCGA_plot
      output[[match$protein]] <- renderPlot({
        plot_result
      },height = 440)
    }
    else if(m == 1){
      plot_result <- MCLP_plot
      output[[match$protein]] <- renderPlot({
        plot_result
      },height = 400)
    }
    else if(c == 1){
      plot_result <- CCLE_plot
      output[[match$protein]] <- renderPlot({
        plot_result
      },height = 440)
    }
    if(m == 1 || t == 1 || c == 1){
      output$`protein-picdownload` <- downloadHandler(
        filename = function() {
          paste("Differential_Expression", ".", input$`protein-pictype`, sep = "")
         },
         content = function(file){
           ggsave(file,plot_result,device = input$`protein-pictype`,width = input$`protein-d_width`,height = input$`protein-d_height`  )}
      )
    }
  }}
)

TCGA_protein_result <- function(){
  TCGA_protein %>%
    dplyr::mutate(
      expr = purrr::map(
        .x = summary,
        .f = function(.x) {
          .x %>% dplyr::mutate(protein = stringr::str_replace_all(pattern = "_",replacement = "",protein) %>% toupper()) %>%
            dplyr::filter(protein %in% match$protein)  %>% tidyr::unnest()
        }
      )
    ) %>% dplyr::select(-summary) %>% tidyr::unnest() %>% dplyr::rename(expr = tumor) -> expr_clean
  if(length(expr_clean$cancer_types) > 0){
    expr_clean %>% dplyr::group_by(cancer_types,symbol,protein) %>% dplyr::slice(1:5) %>% tidyr::drop_na() %>% dplyr::ungroup()  ->> TCGA_protein_plot_result
    expr_clean %>% dplyr::group_by(cancer_types,symbol,protein) %>% dplyr::slice(6) %>% tidyr::drop_na() %>% dplyr::ungroup() ->> TCGA_protein_table_result
    TCGA_protein_table_result %>% dplyr::left_join(protein_TCGA,by = "cancer_types") %>% dplyr::select(cancer_types = Disease_Type,symbol,protein,cancer,expr) -> TCGA_protein_table_result
    output$expr_dt_comparison_TCGA_protein <- DT::renderDataTable({expr_clean_datatable_protein(TCGA_protein_table_result,"Cancer Types (TCGA)")})
    output$expr_dt_download_TCGA_protein <- downloadHandler(
      filename = function() {
        paste(Sys.Date(),"TCGA_selected_protein.csv",sep = "_")
      },
      content = function(file) {
        write.csv(TCGA_protein_table_result, file, row.names = TRUE)
      }
    )
  }
  else{
    TCGA_protein_plot_result <<- "blank"
  }
  return(TCGA_protein_plot_result)
}

MCLP_protein_result <- function(){
  MCLP_protein %>%
    dplyr::mutate(
      expr = purrr::map(
        .x = summary,
        .f = function(.x) {
          .x %>% dplyr::mutate(protein = stringr::str_replace_all(pattern = "_",replacement = "",protein) %>% toupper()) %>%
            dplyr::filter(protein %in% match$protein) %>% tidyr::unnest()
        }
      )
    ) %>% dplyr::select(-summary) %>% tidyr::unnest() %>%　dplyr::rename(cancer_types = tis,expr=summary) -> expr_clean
  if(length(expr_clean$cancer_types) > 0){
    expr_clean %>% dplyr::group_by(cancer_types,symbol,protein) %>% dplyr::slice(6) %>% tidyr::drop_na() %>% dplyr::ungroup() %>% dplyr::left_join(protein_MCLP,by="cancer_types") %>% dplyr::select(cancer_types,symbol,protein,cellline_num,expr) ->> MCLP_protein_table_result
    output$expr_dt_comparison_MCLP_protein <- DT::renderDataTable({expr_clean_datatable_protein(MCLP_protein_table_result,"Cell lines (MCLP)")})
    output$expr_dt_download_MCLP_protein <- downloadHandler(
      filename = function() {
        paste(Sys.Date(),"MCLP_selected_protein.csv",sep = "_")
      },
      content = function(file) {
        write.csv(MCLP_protein_table_result, file, row.names = TRUE)
      }
    )
  }
  else{
    MCLP_protein_table_result <<- "blank"
  }
  return(MCLP_protein_table_result)
}

CCLE_protein_result <- function(){
  CCLE_protein %>%
    dplyr::mutate(
      expr = purrr::map(
        .x = summary,
        .f = function(.x) {
          .x %>% dplyr::mutate(protein = stringr::str_replace_all(pattern = "_",replacement = "",protein) %>% toupper()) %>% dplyr::filter(protein %in% match$protein) %>% tidyr::unnest()
        }
      )
    ) %>% dplyr::select(-summary) %>% tidyr::unnest() %>% dplyr::rename(cancer_types = tissue) -> expr_clean
  if(length(expr_clean$cancer_types)>0){
    expr_clean %>% 
      dplyr::mutate(cancer_types = stringr::str_replace_all(pattern = "_",replacement = " ",cancer_types)) %>% 
      dplyr::group_by(cancer_types,symbol,protein) %>% dplyr::slice(6) %>% tidyr::drop_na() %>% 
      dplyr::ungroup() %>% dplyr::left_join(protein_CCLE,by="cancer_types") %>% 
      dplyr::select(cancer_types,symbol,protein,cellline_num,expr) ->> CCLE_protein_table_result
    output$expr_dt_comparison_CCLE_protein <- DT::renderDataTable({expr_clean_datatable_protein(CCLE_protein_table_result,"Cell lines (CCLE)")})
    output$expr_dt_download_CCLE_protein <- downloadHandler(
      filename = function() {
        paste(Sys.Date(),"CCLE_selected_protein.csv",sep = "_")
      },
      content = function(file) {
        write.csv(CCLE_protein_table_result, file, row.names = TRUE)
      }
    )
  }
  else{
    CCLE_protein_table_result <<- "blank"
  }
  return(CCLE_protein_table_result)
}

###add new

# protein table_print -------------------------------------------------------------
expr_buble_plot_protein <-  function(.expr,.type){
  quantile_names <- c("lower.whisker", "lower.hinge", "median", "upper.hinge", "upper.whisker")
  if(.type == "TCGA"){
  nu <- .expr$cancer_types %>% length()
  .expr %>% dplyr::rename(FPKM = expr) %>%
    dplyr::mutate(tmp = paste(site,"(",cancer_types,")")) %>%
    dplyr::select(cancer_types = tmp,symbol,protein,FPKM) %>%
    dplyr::mutate(name = purrr::rep_along(cancer_types, quantile_names)) %>%
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
      axis.text.x = element_text(angle = 75, vjust = 1, hjust = 1, colour = 'black'),
      axis.text.y = element_text(color = 'black', size = 14),
      
      strip.background = element_rect(fill = NA, color = "white"),
      strip.text = element_text(size = 20),
      
      panel.background = element_rect(fill = "white", color = "black", size = 0.5),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      
      legend.position = 'none',
      legend.key = element_rect(fill = 'white'),
      plot.title = element_text(hjust = 0.5,size = 30)
    ) +
    labs(
      title = 'Cancer Types (TCGA)',
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
      dplyr::ungroup() %>%
      dplyr::mutate(tmp = stringr::str_to_title(cancer_types)) %>% dplyr::select(cancer_types=tmp,symbol,protein,FPKM) -> t
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
        axis.text.x = element_text(angle = 75, vjust = 1, hjust = 1, colour = 'black'),
        axis.text.y = element_text(color = 'black', size = 14),
        
        strip.background = element_rect(fill = NA, color = "white"),
        strip.text = element_text(size = 20),
        
        panel.background = element_rect(fill = "white", color = "black", size = 0.5),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        
        legend.position = 'none',
        legend.key = element_rect(fill = 'white'),
        plot.title = element_text(hjust = 0.5,size = 30)
      ) -> p
    if(.type == "MCLP"){
      p +   
        labs(
          title = "Cell lines (MCLP)",
          x = 'Cell lines',
          y = 'Protein expression'
        ) -> q} 
    else{
      p +   
        labs(
          title = "Cell lines (CCLE)",
          x = 'Cell lines',
          y = 'Protein expression'
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
  }
}
expr_clean_datatable_protein <- function(.expr_clean,.title) {
  if(.title == "Cancer Types (TCGA)"){
    site <- "Cancer Type"
  }
  else{
    site <- "Cell line Lineage"
  }
  DT::datatable(
    data = .expr_clean,
    options = list(
      pageLength = 10,
      autoWidth = TRUE,
      searching = TRUE,
      dom = "Bfrtip",
      columnDefs = list(list(className = 'dt-center',targets="_all"))
    ),
    caption = htmltools::tags$caption(
      .title,
      style = 'font-size: 25px; color: black'
    ),
    rownames = FALSE,
    colnames = c(site, "Symbol", "Protein", "Sample Statistics", "Mean expr."),
    style = "bootstrap",
    class = "table-bordered table-condensed"
  ) %>% 
    #DT::formatSignif(columns = c("expr"), digits = 2) %>%
    DT::formatRound(columns = c("expr"), 2)
}

# observe -----------------------------------------------------------------
