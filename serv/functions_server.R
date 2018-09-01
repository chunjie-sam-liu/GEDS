# source by server.R
# saved as functions_server.R

# Check input gene set ----------------------------------------------------
check_gene_set <- function(.s, status = status, error = error) {
  .s %>%
    stringr::str_split(pattern = "[^[:alnum:]]+", simplify = TRUE) %>%
    .[1, ] %>%
    stringr::str_trim(side = "both") -> .ss

  if (!dplyr::between(length(.ss), 1, 100000)) {
    error$gene_set <- "Please enter the gene or gene set."
    status$trigger <- if (status$trigger == TRUE) FALSE else TRUE
    status$gene_set <- FALSE
  }

  .ss
}


# Validate gene with TCGA gene symbol -------------------------------------
validate_gene_set <- function(.v, user_dir = user_dir, user_logs = user_logs, total_gene_symbol = total_gene_symbol, status = status, error = error, gene_set = gene_set) {



  .v_dedup <- .v[.v != ""] %>% unique() %>% sapply(FUN = tolower, USE.NAMES = FALSE)
  .v_dedup %in% names(total_gene_symbol) -> .inter


  gene_set$match <- total_gene_symbol[.v_dedup[.inter]]
  gene_set$non_match <- .v_dedup[!.inter]
  gene_set$n_match <- length(total_gene_symbol[.v_dedup[.inter]])
  gene_set$n_non_match <- length(.v_dedup[!.inter])
  gene_set$n_total <- length(total_gene_symbol[.v_dedup[.inter]]) + length(.v_dedup[!.inter])

  .log <- c(
    glue::glue("{paste0(rep('-', 10), collapse = '')} Notice: Input total gene set number is {length(gene_set$n_total)} {paste0(rep('-', 10), collapse = '')}"),
    glue::glue("{paste0(rep('-', 10), collapse = '')} Notice: Unique gene set number is {length(.v_dedup)} {paste0(rep('-', 10), collapse = '')}"),
    glue::glue("#Total input gene set: {paste0(.v, collapse = ', ')}"),
    glue::glue("#Validated genes: {paste0(gene_set$match, collapse = ', ')}"),
    glue::glue("#Invalidated genes: {paste0(gene_set$non_match, collapse = ', ')}")
  )
  write(x = .log, file = .log_file, append = TRUE)
}
validate_protein_set <- function(.v, user_dir = user_dir, user_logs = user_logs, total_protein_symbol = total_protein_symbol, status = status, error = error, protein_set = protein_set) {
  
  
  
  .v_dedup <- .v[.v != ""] %>% unique() %>% sapply(FUN = tolower, USE.NAMES = FALSE)
  .v_dedup %in% names(total_protein_symbol) -> .inter
  
  
  protein_set$match <- total_protein_symbol[.v_dedup[.inter]]
  protein_set$non_match <- .v_dedup[!.inter]
  protein_set$n_match <- length(total_protein_symbol[.v_dedup[.inter]])
  protein_set$n_non_match <- length(.v_dedup[!.inter])
  protein_set$n_total <- length(total_protein_symbol[.v_dedup[.inter]]) + length(.v_dedup[!.inter])
  
  .log <- c(
    glue::glue("{paste0(rep('-', 10), collapse = '')} Notice: Input total protein set number is {length(protein_set$n_total)} {paste0(rep('-', 10), collapse = '')}"),
    glue::glue("{paste0(rep('-', 10), collapse = '')} Notice: Unique protein set number is {length(.v_dedup)} {paste0(rep('-', 10), collapse = '')}"),
    glue::glue("#Total input protein set: {paste0(.v, collapse = ', ')}"),
    glue::glue("#Validated proteins: {paste0(protein_set$match, collapse = ', ')}"),
    glue::glue("#Invalidated proteins: {paste0(protein_set$non_match, collapse = ', ')}")
  )
  write(x = .log, file = .log_file, append = TRUE)
}
validate_miRNA_set <- function(.v, user_dir = user_dir, user_logs = user_logs, total_miRNA_symbol = total_miRNA_symbol, status = status, error = error, miRNA_set = miRNA_set) {
  
  
  
  .v_dedup <- .v[.v != ""] %>% unique() %>% sapply(FUN = tolower, USE.NAMES = FALSE)
  .v_dedup %in% names(total_miRNA_symbol) -> .inter
  
  
  miRNA_set$match <- total_miRNA_symbol[.v_dedup[.inter]]
  miRNA_set$non_match <- .v_dedup[!.inter]
  miRNA_set$n_match <- length(total_miRNA_symbol[.v_dedup[.inter]])
  miRNA_set$n_non_match <- length(.v_dedup[!.inter])
  miRNA_set$n_total <- length(total_miRNA_symbol[.v_dedup[.inter]]) + length(.v_dedup[!.inter])
  
  .log <- c(
    glue::glue("{paste0(rep('-', 10), collapse = '')} Notice: Input total miRNA set number is {length(miRNA_set$n_total)} {paste0(rep('-', 10), collapse = '')}"),
    glue::glue("{paste0(rep('-', 10), collapse = '')} Notice: Unique miRNA set number is {length(.v_dedup)} {paste0(rep('-', 10), collapse = '')}"),
    glue::glue("#Total input miRNA set: {paste0(.v, collapse = ', ')}"),
    glue::glue("#Validated miRNAs: {paste0(miRNA_set$match, collapse = ', ')}"),
    glue::glue("#Invalidated miRNAs: {paste0(miRNA_set$non_match, collapse = ', ')}")
  )
  write(x = .log, file = .log_file, append = TRUE)
}


# extract gene set from TCGA data -----------------------------------------


filter_gene_list <- function(.x, gene_list) {
  .x %>%
    dplyr::filter(symbol %in% gene_list)
  # gene_list %>%
  #   dplyr::select(symbol) %>%
  #   dplyr::left_join(.x, by = "symbol")
}
