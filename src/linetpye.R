TCGA_mRNA <- readr::read_rds(file.path(config$database, "mRNA","TCGA_mRNA_summary.rds.gz"))
GTEX_mRNA <- readr::read_rds(file.path(config$database, "mRNA","GTEX_mRNA_summary.rds.gz"))
CCLE_mRNA <- readr::read_rds(file.path(config$database, "mRNA","CCLE_mRNA_summary.rds.gz"))

TCGA_protein <- readr::read_rds(file.path(config$database, "protein","TCGA_protein_summary.rds.gz"))
MCLP_protein <- readr::read_rds(file.path(config$database, "protein","MCLP_protein_summary.rds.gz"))
CCLE_protein <- readr::read_rds(file.path(config$database, "protein","CCLE_protein_summary.rds.gz"))

TCGA_protein %>% tidyr::unnest() %>% tidyr::unnest() %>% 
  dplyr::filter(protein %in% c("4EBP1","4EBP1_pS65","4EBP1_pT37T46","4EBP1_pT70")) %>% 
  dplyr::group_by(cancer_types,symbol,protein) %>% dplyr::slice(6) %>% tidyr::drop_na() ->a

ggplot(a, aes(x=cancer_types, y=tumor, colour=protein,group=protein)) + 
  geom_line(size=2) + 
  theme(axis.text.x = element_text(angle = 75, vjust = 1, hjust = 1, colour = 'black'))

MCLP_protein %>% tidyr::unnest() %>% tidyr::unnest() %>% 
  dplyr::filter(protein %in% c("4EBP1","4EBP1_pS65","4EBP1_pT37T46","4EBP1_pT70")) %>% 
  dplyr::group_by(tis,symbol,protein) %>% dplyr::slice(6) %>% tidyr::drop_na() ->b

ggplot(b, aes(x=tis, y=summary, colour=protein,group=protein)) + 
  geom_line(size=2) + 
  theme(axis.text.x = element_text(angle = 75, vjust = 1, hjust = 1, colour = 'black'))

CCLE_protein %>% tidyr::unnest() %>% tidyr::unnest() %>% 
  dplyr::filter(protein %in% c("4EBP1","4EBP1PS65","4EBP1PT37T46","4EBP1PT70")) %>% 
  dplyr::group_by(tissue,symbol,protein) %>% dplyr::slice(6) %>% tidyr::drop_na() ->c

ggplot(c, aes(x=tissue, y=expr, colour=protein,group=protein)) + 
  geom_line(size=2) + 
  theme(axis.text.x = element_text(angle = 75, vjust = 1, hjust = 1, colour = 'black'))

EIF4EBP1 