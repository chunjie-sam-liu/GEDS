# source by server.R
# saved as functions_server.R

# Check input gene set ----------------------------------------------------
check_gene_set <- function(.s) {
  .s %>%stringr::str_split(pattern = "[ ,;]+", simplify = TRUE) %>%.[1, ] %>%stringr::str_trim(side = "both") -> .ss
  .ss
}


# Validate gene with TCGA gene symbol -------------------------------------

validate_input_set <- function(.v,  .total_symbol, input_list_check = input_list_check) {
  
  .v_dedup <- .v[.v != ""] %>% unique()
  .v_dedup %in% .total_symbol -> .inter
  input_list_check$match <- .v_dedup[.inter]
  input_list_check$non_match <- .v_dedup[!.inter]
  input_list_check$n_match <- length(.total_symbol[.v_dedup[.inter]])
  input_list_check$n_non_match <- length(.v_dedup[!.inter])
  input_list_check$n_total <- length(.total_symbol[.v_dedup[.inter]]) + length(.v_dedup[!.inter])
  if(.inter) {
    status$result <- TRUE
    status$valid <- TRUE } 
  else {
    status$result <- FALSE
    status$valid <- FALSE}
}





