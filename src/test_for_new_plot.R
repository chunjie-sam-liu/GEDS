library(shiny)
library(plotly)
library(shinyBS)

example <- readr::read_rds("/home/xiamx/file_for_GEDS_test/TCGA_example.rds.gz")
CCLE_example <- readr::read_rds("/home/xiamx/file_for_GEDS_test/CCLE_example.rds.gz")

quantile_names <- c("lower.whisker", "lower.hinge", "median", "upper.hinge", "upper.whisker")
example %>% dplyr::rename(FPKM = expr) %>%
    tidyr::separate(col = cancer_types, into = c("cancer_types", "types")) %>%
    dplyr::mutate(tmp = paste(site,"(",cancer_types,")")) %>%
    dplyr::select(cancer_types=tmp,types,symbol,FPKM) %>%
    dplyr::mutate(types = stringr::str_to_title(types)) %>% 
    dplyr::mutate(name = purrr::rep_along(cancer_types, quantile_names)) %>% 
    ###add new
    tidyr::spread(key = name, value = FPKM) %>% 
    dplyr::group_by(symbol) %>% dplyr::arrange(symbol,desc(median)) %>% dplyr::ungroup() -> t
  t %>% dplyr::filter(types %in% "Tumor") %>% .$cancer_types -> order
  t %>% dplyr::filter(types %in% "Tumor") %>% dplyr::select(cancer_types) %>% 
    dplyr::left_join(t %>% dplyr::arrange(desc(types)),by="cancer_types") %>% 
    dplyr::select(types) %>% 
    dplyr::mutate(tmp = ifelse(types=="Tumor","#DC7B7B","#8888B1")) -> fill_color
  t %>%
    ggplot(mapping = aes(x = cancer_types, middle = median,
                         ymin = lower.whisker, ymax = upper.whisker,
                         lower = lower.hinge, upper = upper.hinge, color = types)) +
    scale_color_manual(values = c("midnightblue", "red3")) +
    geom_errorbar(width = 0.3, position = position_dodge(0.75, preserve = 'single')) +
    geom_boxplot(stat = 'identity', width = 0.6, aes(fill = types), fill = fill_color$tmp,
                 position = position_dodge(0.75, preserve = 'single')) +
    scale_x_discrete(limits= order) +
    theme(
      text = element_text(colour = 'black', size = 12),
      legend.position = "none",
      axis.text.x = element_text(angle = 75, vjust = 1, hjust = 1, colour = 'black'),
      axis.text.y = element_text(color = 'black', size = 12),
      axis.line = element_line(colour = "black", 
                               size = 0.5, linetype = "solid"),
      panel.background = element_rect(fill = "white",  size = 0.5),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.ticks = element_blank(),
      plot.title = element_text(hjust = 0.5,size = 12)
    ) +
    labs(
      title = "CDK1",
      x = 'Cancer Types (TCGA)',
      y = 'RSEM(log2)'
    )
  
CCLE_example %>% dplyr::mutate(FPKM = log2(expr+1)) %>% dplyr::select(-expr) %>%
    dplyr::mutate(cancer_types = stringr::str_to_title(cancer_types)) %>% 
    dplyr::mutate(name = purrr::rep_along(cancer_types, quantile_names)) %>% 
    tidyr::spread(key = name, value = FPKM) %>% 
    dplyr::group_by(symbol) %>% dplyr::arrange(symbol,desc(median)) %>% dplyr::ungroup() -> t2
t2 %>% .$cancer_types -> order
t2 %>%
  ggplot(mapping = aes(x = cancer_types, middle = median,
                       ymin = lower.whisker, ymax = upper.whisker,
                       lower = lower.hinge, upper = upper.hinge, color = symbol)) +
  scale_color_manual(values = "#ffc0cb") +
  geom_errorbar(width = 0.3, position = position_dodge(0.75, preserve = 'single')) +
  geom_boxplot(stat = 'identity', width = 0.6, aes(fill = symbol), fill = "#FBDDE2",
               position = position_dodge(0.75, preserve = 'single')) +
  scale_x_discrete(limits= order) +
  scale_y_discrete(limits= c(2,4,6,8)) +
  theme(
    text = element_text(colour = 'black', size = 12),
    legend.position = "none",
    axis.text.x = element_text(angle = 75, vjust = 1, hjust = 1, colour = 'black'),
    axis.text.y = element_text(color = 'black', size = 12),
    axis.line = element_line(colour = "black", 
                             size = 0.5, linetype = "solid"),
    panel.background = element_rect(fill = "white",  size = 0.5),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5,size = 12)
  ) +
  labs(
    title = "CDK1",
    x = 'Cancer Types (TCGA)',
    y = 'RSEM(log2)'
  )