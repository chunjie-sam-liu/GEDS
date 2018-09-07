# source by server.R
# saved as functions_server.R

# Check input gene set ----------------------------------------------------
check_gene_set <- function(.s) {
  .s %>%stringr::str_split(pattern = "[^[:alnum:]]+", simplify = TRUE) %>%.[1, ] %>%stringr::str_trim(side = "both") -> .ss
  .ss
}


# Validate gene with TCGA gene symbol -------------------------------------
validate_gene_set <- function(.v, total_gene_symbol = total_gene_symbol,  input_list_check = input_list_check) {

  .v_dedup <- .v[.v != ""] %>% unique() %>% sapply(FUN = toupper, USE.NAMES = FALSE)
  .v_dedup %in% total_gene_symbol -> .inter
  
  input_list_check$match <- total_gene_symbol[.v_dedup[.inter]]
  input_list_check$non_match <- .v_dedup[!.inter]
  input_list_check$n_match <- length(total_gene_symbol[.v_dedup[.inter]])
  input_list_check$n_non_match <- length(.v_dedup[!.inter])
  input_list_check$n_total <- length(total_gene_symbol[.v_dedup[.inter]]) + length(.v_dedup[!.inter])

}
validate_protein_set <- function(.v,  total_protein_symbol = total_protein_symbol, input_list_check = input_list_check) {
  
  .v_dedup <- .v[.v != ""] %>% unique() %>% sapply(FUN = toupper, USE.NAMES = FALSE)
  .v_dedup %in% total_protein_symbol -> .inter
  input_list_check$match <- .v_dedup[.inter]
  input_list_check$non_match <- .v_dedup[!.inter]
  input_list_check$n_match <- length(total_protein_symbol[.v_dedup[.inter]])
  input_list_check$n_non_match <- length(.v_dedup[!.inter])
  input_list_check$n_total <- length(total_protein_symbol[.v_dedup[.inter]]) + length(.v_dedup[!.inter])

}
validate_miRNA_set <- function(.v, total_miRNA_symbol = total_miRNA_symbol, input_list_check = input_list_check) {

  .v_dedup <- .v[.v != ""] %>% unique() %>% sapply(FUN = tolower, USE.NAMES = FALSE)
  .v_dedup %in% names(total_miRNA_symbol) -> .inter
  
  input_list_check$match <- total_miRNA_symbol[.v_dedup[.inter]]
  input_list_check$non_match <- .v_dedup[!.inter]
  input_list_check$n_match <- length(total_miRNA_symbol[.v_dedup[.inter]])
  input_list_check$n_non_match <- length(.v_dedup[!.inter])
  input_list_check$n_total <- length(total_miRNA_symbol[.v_dedup[.inter]]) + length(.v_dedup[!.inter])

}



# table_print -------------------------------------------------------------
tibble_format_change <- function(.expr_clean){
  .expr_clean %>%
    dplyr::mutate(
      mean = purrr::map(
        .x = expr,
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
    dplyr::select(-expr) %>% 
    tidyr::unnest() ->table_result
}
expr_clean_datatable <- function(.expr_clean) {
  DT::datatable(
    data = .expr_clean,
    rownames = FALSE,
    colnames = c("Cancer Types", "Symbol", "Protein", "Mean expr."),
    filter = "top",
    extensions = "Buttons",
    style = "bootstrap",
    class = "table-bordered table-condensed"
  ) 
}

