
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
        .x %>% dplyr::rename(symbol = `Symbol name`)
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
          grep(pattern = .x, .total_symbol$symbol, value = TRUE ) 
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
            tidyr::gather(key = barcode, value = expr, -c(symbol, protein)) %>%
            tidyr::drop_na(expr) %>%
            dplyr::group_by(symbol, protein) %>%
            dplyr::summarise(mean = mean(expr)) %>%
            dplyr::ungroup() 
        }
      )
    ) %>%
    dplyr::select(-expression) %>% 
    tidyr::unnest() 