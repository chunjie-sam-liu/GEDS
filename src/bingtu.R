library(magrittr)
library(ggplot2)

# load data ---------------------------------------------------------------

all <- readr::read_tsv("/home/xiamx/for_bingtu_all")

tcga <- all %>% dplyr::filter(dataset %in% c("TCGA_tumor","TCGA_normal","TCGA")) %>%
  dplyr::mutate(tmp = paste(source,dataset,sep = "_")) %>% 
  dplyr::select(-dataset,-source) %>% dplyr::rename(Source = tmp)

other <- all %>% dplyr::filter(dataset %in% c("CCLE","MCLP","black")) %>% 
  dplyr::mutate(tmp = paste(source,dataset,sep = "_")) %>% 
  dplyr::select(-dataset,-source) %>% dplyr::rename(Source = tmp)

# TCGA plot --------------------------------------------------------------------
tcga %>% 
  dplyr::mutate(percent = 0.2) %>%
  dplyr::mutate(ypos=ifelse(Source == "mRNA_TCGA_tumor",1.4,0.1)) %>%
  dplyr::mutate(ypos=ifelse(Source == "miRNA_TCGA_tumor",1.2,ypos)) %>%
  dplyr::mutate(ypos=ifelse(Source == "protein_TCGA",1.0,ypos)) %>%
  dplyr::mutate(ypos=ifelse(Source == "mRNA_TCGA_normal",0.8,ypos)) %>%
  dplyr::mutate(ypos=ifelse(Source == "miRNA_TCGA_normal",0.6,ypos))  -> plot_ready

data.frame(Project=as.character(plot_ready$Project[1:33]),
           Cases = NA,
           Source = as.character("SS"),
           percent = 0.5,
           ypos = 0.1) %>%
  dplyr::as_tibble() -> white.plot

rbind(plot_ready, white.plot) -> circle_plot_ready

circle_plot_ready %>% dplyr::mutate(Source = factor(circle_plot_ready$Source,levels = c("mRNA_TCGA_tumor","miRNA_TCGA_tumor","protein_TCGA","mRNA_TCGA_normal","miRNA_TCGA_normal","black_TCGA","SS"))) -> circle_plot_ready


circle_plot_ready %>%
  ggplot(mapping= aes(x=Project,y=percent,fill = Source)) +
  geom_col(position = "stack")  +
  scale_fill_manual(values = c("#bcb1f9", "#d9fcff", "#ffff88","#ffc0cb", "#2ac940", "#f0f8ff", "white")) +
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


#cell line

other %>%
  dplyr::mutate(percent = 0.1) %>%
  dplyr::mutate(ypos=ifelse(Source == "mRNA_CCLE",0.75,0.5)) %>%
  dplyr::mutate(ypos=ifelse(Source == "protein_CCLE",0.85,ypos)) %>%
  dplyr::mutate(ypos=ifelse(Source == "protein_MCLP",0.95,ypos)) -> other_plot_ready

other_plot_ready %>% dplyr::select(Project) %>% dplyr::distinct() %>% dplyr::arrange(Project) %>% .$Project -> project

data.frame(Project=project,
           Cases = project,
           Source = as.character("Name"),
           percent = 0.7,
           ypos = 0.65-nchar(project)/90) %>%
  dplyr::as_tibble() -> name.plot

rbind(other_plot_ready,name.plot) -> other_plot_ready

other_plot_ready %>% dplyr::mutate(Source = factor(other_plot_ready$Source,levels = c("protein_black", "protein_MCLP", "protein_CCLE", "mRNA_black", "mRNA_CCLE", "Name"))) -> other_plot_ready

# circle plot -------------------------------------------------------------
-85 - 360 / length(project) * seq_along(project) -> ang
ang <- c(rep(0,111),ang)

other_plot_ready %>%
ggplot(mapping= aes(x=Project,y=percent,fill=Source)) +
  geom_col(position = "stack")  +
  scale_fill_manual(values = c("#f0f8ff", "#75a3e7", "#cbb255", "#f0f8ff", "#f59563","white")) +
  geom_text(aes(label = Cases), y = other_plot_ready$ypos, size = 3.5 , angle = ang) +
  theme(
    plot.background = element_blank(),
    text = element_text(colour = 'black', size = 19),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  coord_polar("x")