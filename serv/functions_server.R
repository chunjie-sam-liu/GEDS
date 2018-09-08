# source by server.R
# saved as functions_server.R

# Check input gene set ----------------------------------------------------
check_gene_set <- function(.s) {
  .s %>%stringr::str_split(pattern = "[^[:alnum:]]+", simplify = TRUE) %>%.[1, ] %>%stringr::str_trim(side = "both") -> .ss
  .ss
}


# Validate gene with TCGA gene symbol -------------------------------------

validate_input_set <- function(.v,  .total_symbol, input_list_check = input_list_check) {
  
  .v_dedup <- .v[.v != ""] %>% unique() %>% sapply(FUN = toupper, USE.NAMES = FALSE)
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




# protein table_print -------------------------------------------------------------
protein_filter <- function(){
  TCGA_protein %>% dplyr::filter(cancer_types %in% input$select_protein_TCGA) %>%
    dplyr::mutate(
      mirna = purrr::map(
        .x = expr,
        .f = function(.x) {
          .x %>%
            dplyr::filter(symbol %in% input_list_check$match)
        }
      )
    )
}
tibble_change_to_plot <- function(.expr_clean){
  .expr_clean %>%
  dplyr::mutate(
    mean = purrr::map(
      .x = expr,
      .f = function(.x){
        .x %>% 
          tidyr::gather(key = barcode, value = expr, -c(symbol, protein)) %>%
          tidyr::drop_na(expr) %>%
          dplyr::group_by(symbol, protein) %>%
          dplyr::ungroup() 
      }
    )
  ) %>%
    dplyr::select(-expr) %>% 
    tidyr::unnest()
}
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
    tidyr::unnest() 
}
expr_buble_plot <-  function(.expr){
  .expr %>%
    ggplot(mapping=aes(x=cancer_types,y=expr,color=cancer_types)) +
    geom_boxplot() +
    facet_grid(~symbol) +
    theme(
      axis.line = element_line(color = "black"),
      panel.background  = element_rect(fill = "white", color = "grey"),
      panel.grid = element_line(colour = "grey"),
      axis.title.x = element_blank(),
      legend.title = element_blank(),
      text = element_text(size = 20)
    )
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

# miRNA table print -------------------------------------------------------
tibble_format_change_mirna <- function(.expr_clean){
  .expr_clean %>%
    dplyr::mutate(
      mean = purrr::map(
        .x = mirna,
        .f = function(.x){
          .x %>% 
            tidyr::gather(key = barcode, value = mirna, -c(gene, name)) %>%
            tidyr::drop_na(mirna) %>%
            dplyr::group_by(gene,name) %>%
            dplyr::summarise(mean = mean(mirna)) %>%
            dplyr::ungroup() 
        }
      )
    ) %>%
    dplyr::select(-mirna) %>% 
    tidyr::unnest() 
}
expr_clean_datatable_mirna <- function(.expr_clean) {
  DT::datatable(
    data = .expr_clean,
    rownames = FALSE,
    colnames = c("Cancer Types", "Symbol","Name", "Mean Rppa expr."),
    filter = "top",
    extensions = "Buttons",
    style = "bootstrap",
    class = "table-bordered table-condensed"
  ) 
}


