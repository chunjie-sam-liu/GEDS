
# load library ------------------------------------------------------------

library(magrittr)


# data path ---------------------------------------------------------------

path_data <- "/data/shiny-data/GSEXPR/mRNA/CCLE/result"


# loading data ------------------------------------------------------------

ccle <- readr::read_rds(path = file.path(path_data, "CCLE_expr.rds.gz"))


# data process ------------------------------------------------------------

ccle %>%
  dplyr::mutate(
    expression = purrr::map(
      .x = expression,
      .f = function(.x) {
        .x[-c(16196,18848,19790,7667,24823,24841,24868,24860,24833,16510),]
      }
    )
  ) ->
  ccle_symbol



# save data ---------------------------------------------------------------

ccle_symbol %>% readr::write_rds(path = file.path(path_data, "CCLE_expr.rds.gz"), compress = "gz")

TCGA_protein %>% dplyr::mutate(
    expr = purrr::map(
      .x = expr,
      .f = function(.x) {
        .x$protein %>% gsub(pattern = "_",replacement = "") %>% 
        sapply(FUN = toupper, USE.NAMES = FALSE) ->protein_name
        .total_symbol %>% dplyr::filter(protein %in% protein_name) %>% .$symbol ->symbol_name
        .x %>% tibble::add_column(symbol_name = symbol_name, .before =1) %>% dplyr::select(-symbol)
      }
    )
  ) ->
  TCGA_protein_new
 
paf <- readr::read_tsv('protein_annotation_full')
TCGA_protein %>% dplyr::mutate(
    expr = purrr::map(
      .x = expression,
      .f = function(.x) {
        .x %>% dplyr::mutate(tmp = stringr::str_replace(protein, '_', '') %>% toupper()) -> d
        paf %>% dplyr::rename(tmp = protein) ->dd
        d %>% dplyr::left_join(dd, by = 'tmp')-> ddd
        .x %>% names %>% .[c(-1)] -> .barcode
        ddd %>% dplyr::select(symbol = symbol, protein, .barcode)
      }
  )
) %>% .[-10,]->TCGA_protein_new

MCLP_protein %>% dplyr::mutate(
    expression = purrr::map(
      .x = expression,
      .f = function(.x) {
        .x %>% dplyr::mutate(tmp = stringr::str_replace(protein, '_', '') %>% toupper()) -> d
        paf %>% dplyr::rename(tmp = protein) ->dd
        d %>% dplyr::left_join(dd, by = 'tmp')-> ddd
        .x %>% names %>% .[c(-1,-2)] -> .barcode
        ddd %>% dplyr::select(symbol = symbol, protein, .barcode)
      }
  )
) ->MCLP_protein_new

.s <- "AKT1 aaa FOXM1"
.s %>%stringr::str_split(pattern = "[ ,;]+", simplify = TRUE) %>%.[1, ] ->.v
.vvv <- .v[.v != ""] %>% unique() %>% sapply(FUN = toupper, USE.NAMES = FALSE)
tibble::tibble(symbol=.vvv) %>%
    dplyr::mutate(
      expression = purrr::map(
        .x = symbol,
        .f = function(.x) {
          grep(pattern = (paste(",",.x,",") %>% 
          stringr::str_replace_all(' ','')), mRNA_symbol_alias_new$alias, value = TRUE ) ->a
          mRNA_symbol_alias_new %>% dplyr::filter(alias %in% a) %>% .$symbol->b
          grep(pattern = (paste(",",.x,",") %>% 
          stringr::str_replace_all(' ','')), mRNA_symbol_alias_new$match, value = TRUE ) ->c
          mRNA_symbol_alias_new %>% dplyr::filter(match %in% c) %>% .$symbol->d
          e <- c(b,d)
          if(length(e)>0){e}
        }
      )
    ) -> .v_dedup

a %>% 
  dplyr::mutate(
    symbol = purrr::map(
      .x = mirna,
      .f = function(.x){
        .x %>% dplyr::select("gene","name")
      }
    )
  ) -> c

TCGA %>%
    dplyr::mutate(
      mean = purrr::map(
        .x = mirna,
        .f = function(.x){
          .x %>% 
            tidyr::gather(key = barcode, value = expr, -c(gene, name)) %>%
            tidyr::drop_na(expr) %>%
            dplyr::group_by(gene, name) %>%
            dplyr::summarise(mean = mean(expr)) %>%
            dplyr::ungroup() 
        }
      )
    ) %>%
    dplyr::select(-mirna) %>% 
    tidyr::unnest() 
MCLP    %>%
    dplyr::mutate(
      mean = purrr::map(
        .x = expression,
        .f = function(.x){
          .x %>% 
            tidyr::gather(key = barcode, value = expr, -c(symbol)) %>%
            tidyr::drop_na(expr) %>%
            dplyr::group_by(symbol) %>%
            dplyr::summarise(mean = mean(expr)) %>%
            dplyr::ungroup() 
        }
      )
    ) %>%
    dplyr::select(-expression) %>% 
    tidyr::unnest() 

dataset_number$mRNA <-  length(input$select_mRNA_CCLE)
CCLE %>% dplyr::filter(tissue %in% tissue) %>%
        dplyr::mutate(
          expr = purrr::map(
            .x = expr,
            .f = function(.x) {
              .x %>%
                dplyr::filter(symbol %in% .vvv)
            }
          )
        ) %>% dplyr::select(-expression) %>%　dplyr::rename(cancer_types = tissue)-> expr_clean     
expr_clean %>%
    dplyr::mutate(
      mean = purrr::map(
        .x = expr,
        .f = function(.x){
          .x %>% 
            tidyr::gather(key = barcode, value = expr, -c(symbol)) %>%
            tidyr::drop_na(expr) %>%
            dplyr::group_by(symbol) %>%
            dplyr::ungroup() 
        }
      )
    ) %>%
    dplyr::select(-expr) %>% 
    tidyr::unnest()    

#new data trans
TCGA %>% dplyr::filter(cancer_types %in% c("TGCT","GBM")) %>%
        dplyr::mutate(
          expr = purrr::map(
            .x = mean,
            .f = function(.x) {
              .x %>%
                dplyr::filter(symbol %in% c("A1BG","A1CF")) %>% dplyr::select(-entrez_id) %>%
                tidyr::gather(key = barcode, value = expr, -c(symbol)) %>% tidyr::unnest()
            }
          )
        ) %>% dplyr::select(-mean) %>% tidyr::unnest() %>% 
        dplyr::mutate(tmp = paste(cancer_types,barcode)) %>% 
        dplyr::select(cancer_types=tmp,symbol,expr)
         -> expr_clean

GTEX %>% dplyr::filter(SMTS %in% c("testis","bladder")) %>%
        dplyr::mutate(
          expr = purrr::map(
            .x = mean,
            .f = function(.x) {
              .x %>%
                dplyr::filter(symbol %in% c("A1BG","A1CF")) %>% dplyr::select(-ensembl_gene_id) %>%
                tidyr::unnest()
            }
          )
        ) %>% dplyr::select(-mean) %>% tidyr::unnest() %>% dplyr::rename(cancer_types = SMTS,expr=summary)
         -> expr_clean

CCLE %>% dplyr::filter(tissue %in% c("liver","pancreas")) %>%
        dplyr::mutate(
          expr = purrr::map(
            .x = mean,
            .f = function(.x) {
              .x %>%
                dplyr::filter(symbol %in% c("A1BG","A1CF")) %>% 
                tidyr::unnest()
            }
          )
        ) %>% dplyr::select(-mean) %>% tidyr::unnest() %>% dplyr::rename(cancer_types = tissue,expr=summary)
         -> expr_clean

#miRNA
TCGA %>% dplyr::filter(cancer_types %in% c("OV","KICH")) %>%
        dplyr::mutate(
          expr = purrr::map(
            .x = mean,
            .f = function(.x) {
              .x %>%
                dplyr::filter(name %in% c("hsa-miR-1298-3p","hsa-miR-3921")) %>% 
                tidyr::unnest()
            }
          )
        ) %>% dplyr::select(-mean) %>% tidyr::unnest() %>% dplyr::rename(expr=summary)
         -> expr_clean