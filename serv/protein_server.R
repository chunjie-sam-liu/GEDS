# source by server.R
# saved as protein_server.R

# Clear input -------------------------------------------------------------
###add new
observeEvent(c(input$input_protein_set),{
    status$protein_result <- FALSE
    dataset_number$protein <- 30
    match$protein <- input$input_protein_set
    if(match$protein != ""){
    TCGA_protein_result()
    MCLP_protein_result()
    TCGA_protein_plot_result -> TCGA_one_plot
    MCLP_protein_table_result -> MCLP_one_plot
    t <- 0
    m <- 0
    if(length(TCGA_one_plot$cancer_types) > 0){
      TCGA_plot <- expr_buble_plot_protein(TCGA_one_plot,"TCGA")
      t <- 1
      status$protein_result <- TRUE
    }
    if(length(MCLP_one_plot$cancer_types) > 0){
      MCLP_plot <- expr_buble_plot_protein(MCLP_one_plot,"MCLP")
      m <- 1
      status$protein_result <- TRUE
    }
    if(t == 1 && m == 1){
      ggpubr::ggarrange(
        TCGA_plot,MCLP_plot,
        ncol = 1,nrow = 2, heights = c(1.2,1)
      ) -> plot_result
      output[[match$protein]] <- renderPlot({
        plot_result
      },height = 800)
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
    if(m == 1 || t == 1){
      
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
          .x %>%
            dplyr::filter(protein %in% match$protein)  %>% tidyr::unnest()
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
            dplyr::filter(protein %in% match$protein) %>% tidyr::unnest()
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
        title = "Cell lines (MCLP)",
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

# observe -----------------------------------------------------------------