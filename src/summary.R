TCGA %>% dplyr::mutate(
      cancer = purrr::map(
        .x = expr,
        .f = function(.x){
            .x %>% names %>% .[c(-1,-2)] %>% tibble::tibble(barcode = .) %>% 
            dplyr::mutate(type = stringr::str_sub(string = barcode, start = 14, end = 15))%>%
            dplyr::mutate(type = ifelse(type == "11", "Normal", "Tumor")) ->cancertype
            cancertype %>% dplyr::filter(type %in% "Tumor") %>% .$type %>% length() 
        } 
      )
    ) %>% dplyr::mutate(
      normal = purrr::map(
        .x = expr,
        .f = function(.x){
            .x %>% names %>% .[c(-1,-2)] %>% tibble::tibble(barcode = .) %>% 
            dplyr::mutate(type = stringr::str_sub(string = barcode, start = 14, end = 15))%>%
            dplyr::mutate(type = ifelse(type == "11", "Normal", "Tumor")) ->cancertype
            cancertype %>% dplyr::filter(type %in% "Normal") %>% .$type %>% length() 
        } 
      )
    ) ->test
GTEX %>% dplyr::mutate(
      normal = purrr::map(
        .x = expr,
        .f = function(.x){
            .x %>% names %>% .[c(-1,-2)]  %>% length() 
        } 
      )
    ) ->test

CCLE %>% dplyr::mutate(
      cellline_num = purrr::map(
        .x = expression,
        .f = function(.x){
            .x %>% names %>% .[c(-1,-2)]  %>% length() 
        } 
      )
    ) ->test

TCGA %>% dplyr::mutate(
      cancer = purrr::map(
        .x = mirna,
        .f = function(.x){
            .x %>% names %>% .[c(-1,-313)] %>% tibble::tibble(barcode = .) %>% 
            dplyr::mutate(type = stringr::str_sub(string = barcode, start = 14, end = 15))%>%
            dplyr::mutate(type = ifelse(type == "11", "Normal", "Tumor")) ->cancertype
            cancertype %>% dplyr::filter(type %in% "Tumor") %>% .$type %>% length() 
        } 
      )
    ) %>% dplyr::mutate(
      normal = purrr::map(
        .x = mirna,
        .f = function(.x){
            .x %>% names %>% .[c(-1,-313)]  %>% tibble::tibble(barcode = .) %>% 
            dplyr::mutate(type = stringr::str_sub(string = barcode, start = 14, end = 15))%>%
            dplyr::mutate(type = ifelse(type == "11", "Normal", "Tumor")) ->cancertype
            cancertype %>% dplyr::filter(type %in% "Normal") %>% .$type %>% length() 
        } 
      )
    ) ->test