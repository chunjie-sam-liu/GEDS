CCLE <- readr::read_csv("CCLE_RPPA_20180123.csv")
CCLE_info <- readr::read_csv("CCLE_RPPA_Ab_info_20180123.csv")

CCLE %>% colnames() %>% .[-1] -> barcode
CCLE %>% dplyr::mutate(tmp = stringr::str_split_fixed(X1,pattern="_",n=2) %>% .[,1]) %>% 
dplyr::select(Cell_line = tmp,X1,barcode) %>% 
dplyr::mutate(tmp = stringr::str_split_fixed(X1,pattern="_",n=2) %>% .[,2]) %>%
dplyr::select(Cell_line , tissue = tmp, barcode) -> CCLE_new

CCLE_new %>% dplyr::select(tissue) %>% dplyr::distinct() -> tissue

symbol <- readr::read_rds("protein_symbol_alias.rds.gz")
CCLE_info %>% dplyr::select(Antibody_Name,Target_Genes) %>% 
dplyr::mutate(tmp = stringr::str_replace(pattern="_Caution",replacement="",Antibody_Name)) %>% 
dplyr::mutate(tmp = stringr::str_replace_all(pattern="_",replacement="",tmp)) %>% 
dplyr::mutate(tmp = stringr::str_replace_all(pattern="-",replacement="",tmp)) %>% 
dplyr::mutate(tmp = stringr::str_replace_all(pattern=" ",replacement="",tmp)) %>% 
dplyr::mutate(tmp = stringr::str_replace_all(pattern='\\(',replacement="",tmp)) %>% 
dplyr::mutate(tmp = stringr::str_replace_all(pattern='\\)',replacement="",tmp) %>% toupper ) %>% 
dplyr::rename(protein = tmp) %>% dplyr::left_join(symbol,by="protein") %>% 
dplyr::select(Antibody_Name,Target_Genes,protein,symbol,alias) -> new_info

new_info %>% .[c(36,37,39,40,47,48,114,132,137,177,206),] %>% .$Antibody_Name -> a
new_info %>% .[c(36,37,39,40,47,48,114,132,137,177,206),] %>% .$Target_Genes -> b
new_info %>% .[c(36,37,39,40,47,48,114,132,137,177,206),] %>% .$protein -> c

data.frame(Antibody_Name=a,
           Target_Genes=b,
           protein=c,
           symbol=b,
           alias=NA
           ) %>%
  dplyr::as_tibble() -> rep

rbind(new_info, rep) -> new_info_fixed
new_info_fixed %>% dplyr::select(-alias) %>% tidyr::drop_na() %>% dplyr::arrange(Antibody_Name) %>% 
dplyr::mutate(tmp = stringr::str_replace_all(pattern=" ",replacement=",",symbol)) %>% 
dplyr::select(antibody = Antibody_Name,protein,symbol=tmp) -> new_info_final

tissue %>% dplyr::mutate(
      expr = purrr::map(
        .x = tissue,
        .f = function(.x){
            CCLE_new %>% dplyr::filter(tissue %in% .x) %>% dplyr::select(-tissue) %>% 
            tidyr::gather(key=protein,value=expr,-Cell_line) %>% dplyr::select(-Cell_line) -> a
            a %>% dplyr::select(protein) %>% dplyr::distinct() %>% dplyr::rename(antibody=protein) -> antibody
            antibody %>% dplyr::mutate(
                    expr = purrr::map(
                        .x = antibody,
                        .f = function(.x){
                            a %>% dplyr::filter(protein %in% .x) %>% .$expr -> .xv
                            c(quantile(.xv),mean(.xv)) %>% unname()
                        }
                    )
            ) %>% dplyr::left_join(new_info_final,by="antibody") %>% dplyr::select(symbol,protein,expr)
        }
      )
    ) %>% dplyr::rename(summary=expr) %>% dplyr::mutate(tissue = tolower(tissue)) -> new
new %>% readr::write_rds("CCLE_protein_summary.rds.gz",compress="gz")

tissue %>% dplyr::mutate(
    cellline_num = purrr::map(
        .x = tissue,
        .f = function(.x){
            CCLE_new %>% dplyr::filter(tissue %in% .x) %>% .$tissue %>% length()
        }
    )
) %>% tidyr::unnest() %>% dplyr::mutate(tissue = tolower(tissue)) -> tissue_count
tissue_count %>% readr::write_rds("CCLE_protein_cellline_summary.rds.gz",compress="gz")

TCGA