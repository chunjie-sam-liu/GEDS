TCGA %>% 
  dplyr::mutate(
      expr = purrr::map(
          .x = summary,
          .f = function(.x) {
              .x %>%
                dplyr::mutate(protein=ifelse(protein=="X1433BETA","1433BETA",protein)) %>%
                dplyr::mutate(protein=ifelse(protein=="X1433EPSILON","1433EPSILON",protein)) %>%
                dplyr::mutate(protein=ifelse(protein=="X1433ZETA","1433ZETA",protein)) %>%
                dplyr::mutate(protein=ifelse(protein=="X4EBP1","4EBP1",protein)) %>%
                dplyr::mutate(protein=ifelse(protein=="X4EBP1_pS65","4EBP1_pS65",protein)) %>%
                dplyr::mutate(protein=ifelse(protein=="X4EBP1_pT37T46","4EBP1_pT37T46",protein)) %>%
                dplyr::mutate(protein=ifelse(protein=="X4EBP1_pT70","4EBP1_pT70",protein)) %>%
                dplyr::mutate(protein=ifelse(protein=="X53BP1","53BP1",protein)) %>%
                dplyr::mutate(symbol=ifelse(symbol=="X1433BETA","1433BETA",symbol)) %>%
                dplyr::mutate(symbol=ifelse(symbol=="X1433EPSILON","1433EPSILON",symbol)) %>%
                dplyr::mutate(symbol=ifelse(symbol=="X1433ZETA","1433ZETA",symbol)) %>%
                dplyr::mutate(symbol=ifelse(symbol=="X4EBP1","4EBP1",symbol)) %>%
                dplyr::mutate(symbol=ifelse(symbol=="X53BP1","53BP1",symbol))
          }
      )
  ) ->e
  
grep(pattern = "PS[0-9]", symbol$protein, value = TRUE ) ->ps
grep(pattern = "PT[0-9]", symbol$protein, value = TRUE ) ->pt
grep(pattern = "PY[0-9]", symbol$protein, value = TRUE ) ->py

symbol %>% 
dplyr::mutate(protein = ifelse(protein %in% ps,stringr::str_replace(pattern="PS",replacement=" pS",protein),protein)) %>%
dplyr::mutate(protein = ifelse(protein %in% pt,stringr::str_replace(pattern="PT",replacement=" pT",protein),protein)) %>% 
dplyr::mutate(protein = ifelse(protein %in% py,stringr::str_replace(pattern="PY",replacement=" pY",protein),protein)) -> a

grep(pattern = "pS[0-9]", symbol$protein, value = TRUE ) ->ps
grep(pattern = "pT[0-9]", symbol$protein, value = TRUE ) ->pt
grep(pattern = "pY[0-9]", symbol$protein, value = TRUE ) ->py

symbol %>% 
dplyr::mutate(tmp = ifelse(protein %in% ps,"pS = phospho Serine",NA)) %>%
dplyr::mutate(tmp = ifelse(protein %in% pt,"pT = phospho Threonine",tmp)) %>% 
dplyr::mutate(tmp = ifelse(protein %in% py,"pY = phospho TYrosine",tmp)) -> a

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


a %>% dplyr::select(protein,anno = tmp) %>%
  dplyr::mutate(tmp = ifelse(protein=="1433BETA","14-3-3-BETA",protein)) %>%
  dplyr::mutate(tmp = ifelse(protein=="1433EPSILON","14-3-3-EPSILON",tmp)) %>%
  dplyr::mutate(tmp = ifelse(protein=="1433SIGMA","14-3-3-SIGMA",tmp)) %>%
  dplyr::mutate(tmp = ifelse(protein=="1433ZETA","14-3-3-ZETA",tmp)) -> b