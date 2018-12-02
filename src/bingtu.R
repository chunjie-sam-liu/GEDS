library(magrittr)
library(ggplot2)

# load data ---------------------------------------------------------------

all <- readr::read_tsv("/home/xiamx/for_bingtu_all") %>% dplyr::select(-mRNA,-protein,-miRNA)

tcga <- all %>% dplyr::filter(dataset %in% c("TCGA_tumor","TCGA_normal","TCGA")) %>%
  dplyr::mutate(tmp = paste(source,dataset,sep = "_")) %>% 
  dplyr::select(-dataset,-source) %>% dplyr::rename(Source = tmp)

other <- all %>% dplyr::filter(dataset %in% c("CCLE","MCLP","black")) %>% 
  dplyr::mutate(tmp = paste(source,dataset,sep = "_")) %>% 
  dplyr::select(-dataset,-source) %>% dplyr::rename(Source = tmp)

# plot --------------------------------------------------------------------
tcga %>% 
  dplyr::mutate(percent = 0.2) %>%
  dplyr::mutate(ypos=ifelse(Source == "mRNA_TCGA_tumor",1.2,0.1)) %>%
  dplyr::mutate(ypos=ifelse(Source == "miRNA_TCGA_tumor",1,ypos)) %>%
  dplyr::mutate(ypos=ifelse(Source == "protein_TCGA",0.8,ypos)) %>%
  dplyr::mutate(ypos=ifelse(Source == "mRNA_TCGA_normal",0.6,ypos)) %>%
  dplyr::mutate(ypos=ifelse(Source == "miRNA_TCGA_normal",0.4,ypos))  -> plot_ready

other %>%
  dplyr::mutate(percent = 0.2) %>%
  dplyr::mutate(ypos=ifelse(Source == "mRNA_CCLE",0.1,0.3)) %>%
  dplyr::mutate(ypos=ifelse(Source == "protein_MCLP",0.3,ypos)) -> other_plot_ready

data.frame(Project=as.character(plot_ready$Project[1:33]),
           Cases = NA,
           Source = as.character("SS"),
           percent = 0.3,
           ypos = 0.1) %>%
  dplyr::as_tibble() -> white.plot

rbind(plot_ready, white.plot) -> circle_plot_ready

circle_plot_ready %>% dplyr::mutate(Source = factor(circle_plot_ready$Source,levels = c("mRNA_TCGA_tumor","miRNA_TCGA_tumor","protein_TCGA","mRNA_TCGA_normal","miRNA_TCGA_normal","SS"))) -> circle_plot_ready

other_plot_ready %>% dplyr::mutate(Source = factor(other_plot_ready$Source,levels = c("protein_black", "protein_MCLP", "mRNA_black", "mRNA_CCLE"))) -> other_plot_ready

# circle plot -------------------------------------------------------------

circle_plot_ready %>%
  ggplot(mapping= aes(x=Project,y=percent,fill = Source)) +
  geom_col(position = "stack")  +
  scale_fill_manual(values = c("#bcb1f9", "#d9fcff", "#ffff88","#ffc0cb", "#2ac940", "white")) +
  geom_text(aes(label = Cases), y = circle_plot_ready$ypos, size = 4) +
  theme(
    plot.background = element_blank(),
    text = element_text(colour = 'black', size = 20),
    axis.text.x = element_text(colour = 'black', size = 18),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  coord_polar("x")

other_plot_ready %>%
ggplot(mapping= aes(x=Project,y=percent,fill=Source)) +
  geom_col(position = "stack")  +
  scale_fill_manual(values = c("#f0f8ff", "#75a3e7", "#f0f8ff", "#75a3e7")) +
  geom_text(aes(label = Cases), y = other_plot_ready$ypos, size = 4 , angle = 90) +
  theme(
    plot.background = element_blank(),
    text = element_text(colour = 'black', size = 20),
    axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 1, colour = 'black'),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "white", color = NA)
  )