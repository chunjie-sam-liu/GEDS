
# load library ------------------------------------------------------------

library(magrittr)


# data path ---------------------------------------------------------------

path_data <- "/data/shiny-data/GSEXPR/mRNA/CCLE/result"


# loading data ------------------------------------------------------------

ccle <- readr::read_rds(path = file.path(path_data, "CCLE_expr.rds.gz"))


# data process ------------------------------------------------------------

ccle %>% 
  dplyr::mutate()
  
  
  
  
  
  
  



