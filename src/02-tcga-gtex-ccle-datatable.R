
# library -----------------------------------------------------------------

library(magrittr)

# path --------------------------------------------------------------------

path_data <- '/data/shiny-data/GEDS'


# load data ---------------------------------------------------------------

mrna_tcga <- readr::read_rds(path = file.path(path_data, 'mRNA', 'TCGA_mRNA_summary.rds.gz'))
