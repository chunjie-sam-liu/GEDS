TCGA_protein %>% 
  dplyr::filter(cancer_types %in% c("ESCA", "LIHC","CESC","COAD")) %>%
  dplyr::mutate(
      expr = purrr::map(
          .x = expr,
          .f = function(.x) {
              .x %>%
                dplyr::filter(symbol %in% c("AKT", "SCD1"))
          }
      )
  ) ->e
  


#mean
e %>%
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
    tidyr::unnest() ->
    ee

#point plot
CPCOLS <- c("#000080", "#F8F8FF", "#CD0000")
  ee %>%
    ggplot(aes(x = cancer_types, y = protein)) +
    geom_point(aes(size = mean)) +
    scale_size_continuous(
      limit = c(-log10(0.05), 15),
      range = c(1, 6),
      breaks = c(-log10(0.05), 5, 10, 15),
      labels = c("1", "0.5", "-0.5", "1")),
      name = "expr"
    ) +
    theme(
      panel.background = element_rect(colour = "black", fill = "white"),
      panel.grid = element_line(colour = "grey", linetype = "dashed"),
      panel.grid.major = element_line(
        colour = "grey",
        linetype = "dashed",
        size = 0.2
      ),
      axis.title = element_blank(),
      axis.ticks = element_line(color = "black"),
      # axis.text.y = element_text(color = gene_rank$color),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),

      legend.text = element_text(size = 12),
      legend.title = element_text(size = 14),
      legend.key = element_rect(fill = "white", colour = "black")
    )

#violin plot
ee%>%
  ggplot(mapping=aes(x=cancer_types,y=expr,color=cancer_types)) +
  geom_boxplot() +
  geom_point(aes(x=cancer_types,y=expr,color=cancer_types), position = "jitter") +
  facet_grid(~symbol) +
  theme(
    axis.line = element_line(color = "black"),
    panel.background  = element_rect(fill = "white", color = "grey"),
    panel.grid = element_line(colour = "grey"),
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    text = element_text(size = 20)
  )


observeEvent(input$select_protein_TCGA,{
  protein_filter() ->> expr_clean
  #tibble_change_to_plot(.expr_clean = expr_clean)->>plot_result
  tibble_format_change(.expr_clean = expr_clean)->>table_result
  output$expr_dt_comparison <- DT::renderDataTable({expr_clean_datatable(table_result)})
  #output$expr_bubble_plot <- renderPlot({plot_result %>% expr_buble_plot()})
})