
# library -----------------------------------------------------------------

library(magrittr)

# path --------------------------------------------------------------------

path_data <- '/data/shiny-data/GEDS'


# load data ---------------------------------------------------------------

mrna_tcga <- readr::read_rds(path = file.path(path_data, 'mRNA', 'TCGA_mRNA_cancertype_summary.rds.gz')) %>% 
  dplyr::rename(mrna_tumor = cancer, mrna_normal = normal) 

mirna_tcga <- readr::read_rds(path = file.path(path_data, 'miRNA', 'TCGA_miRNA_cancertype_summary.rds.gz')) %>% 
  dplyr::rename(mirna_tumor = cancer, mirna_normal = normal)

protein_tcga <- readr::read_rds(path = file.path(path_data, 'protein', 'TCGA_protein_cancertype_summary.rds.gz')) %>% 
  dplyr::rename(protein_tumor = cancer)

pcc <- readr::read_rds(path = file.path(path_data, 'TCGA_color.rds.gz')) 

# merge data --------------------------------------------------------------


# tcga sample stat --------------------------------------------------------

mrna_tcga %>% 
  dplyr::left_join(mirna_tcga, by = 'cancer_types') %>% 
  dplyr::left_join(protein_tcga, by = 'cancer_types') %>% 
  dplyr::left_join(pcc, by = 'cancer_types') %>% 
  dplyr::mutate(study_name = stringr::str_to_title(study_name)) %>% 
  dplyr::mutate(cancer_types = glue::glue('{study_name} ({cancer_types})')) %>% 
  dplyr::select(-color, -study_name) %>% 
  dplyr::arrange(-mrna_tumor) ->
  tcga_sample_stat

names(tcga_sample_stat) <- c('Cancer types', 'mRNA Tumor', 'mRNA Normal', 'miRNA Tumor', 'miRNA Normal', 'Protein Tumor')
  



# combine gtex and cell line data classify b tissue -----------------------


gtex_mrna <- readr::read_rds(path = file.path(path_data, 'mRNA', 'GTEX_mRNA_tissue_summary.rds.gz')) %>% 
  dplyr::rename(tissue = SMTS, mrna_normal_tissue = tissue_num) %>% 
  dplyr::mutate(tissue = stringr::str_replace_all(string = tissue, pattern = '[^[:alnum:]]+', replacement = ' ')) %>% 
  dplyr::mutate(tissue = stringr::str_to_title(string = tissue)) %>% 
  dplyr::arrange(-mrna_normal_tissue)

names(gtex_mrna) %>% 
  stringr::str_replace_all(pattern = '[^[:alnum:]]+', replacement = ' ') %>% 
  stringr::str_to_title() ->
  names(gtex_mrna)

ccle_mrna <- readr::read_rds(path = file.path(path_data, 'mRNA', 'CCLE_mRNA_cellline_summary.rds.gz')) %>% 
  dplyr::mutate(tissue = stringr::str_replace_all(string = tissue, pattern = '[^[:alnum:]]+', replacement = ' ')) %>% 
  dplyr::mutate(tissue = stringr::str_to_title(string = tissue)) %>% 
  dplyr::rename(mrna_cell_line = cellline_num) %>% 
  dplyr::arrange(-mrna_cell_line)

names(ccle_mrna) %>% 
  stringr::str_replace_all(pattern = '[^[:alnum:]]+', replacement = ' ') %>% 
  stringr::str_to_title() ->
  names(ccle_mrna)
  

mclp_protein <- readr::read_rds(path = file.path(path_data, 'protein', 'MCLP_protein_cellline_summary.rds.gz')) %>% 
  dplyr::rename(tissue = tis, protein_cell_line = cellline_num) %>% 
  dplyr::mutate(tissue = stringr::str_replace_all(string = tissue, pattern = '[^[:alnum:]]+', replacement = ' ')) %>% 
  dplyr::mutate(tissue = stringr::str_to_title(string = tissue)) %>% 
  dplyr::arrange(-protein_cell_line)

names(mclp_protein) %>% 
  stringr::str_replace_all(pattern = '[^[:alnum:]]+', replacement = ' ') %>% 
  stringr::str_to_title() ->
  names(mclp_protein)


# combine stat data -------------------------------------------------------

geds_data_stat <- list(
  tcga_sample_stat = tcga_sample_stat,
  gtex_mrna_stat = gtex_mrna,
  ccle_mrna_stat = ccle_mrna,
  mclp_protein_stat = mclp_protein
)

readr::write_rds(x = geds_data_stat, path = file.path(path_data, 'geds-data-stat.rds.gz'), compress = 'gz')

  


