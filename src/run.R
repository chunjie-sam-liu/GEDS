test$symbol -> .v
.vvv <- .v[.v != ""] %>% unique() %>% sapply(FUN = toupper, USE.NAMES = FALSE)
tibble::tibble(symbol = .vvv) %>%
    dplyr::mutate(
      expression = purrr::map(
        .x = symbol,
        .f = function(.x) {
          grep(pattern = (paste(",",.x,",") %>% 
              stringr::str_replace_all(' ','')), .total_symbol$alias_match, value = TRUE ) -> a
          .total_symbol %>% dplyr::filter(alias_match %in% a) %>% .$symbol-> b
          grep(pattern = (paste(",",.x,",") %>% 
              stringr::str_replace_all(' ','')), .total_symbol$symbol_match, value = TRUE ) -> c
          .total_symbol %>% dplyr::filter(symbol_match %in% c) %>% .$symbol-> d
          e <- c(b,d)
          if(length(e)>0){e} else{"drop"}
        }
      )
    ) -> .v_dedup