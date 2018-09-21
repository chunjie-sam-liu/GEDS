rppa_json_url <- "http://tcpaportal.org/mclp/annotation-antibody"

jsonlite::fromJSON(txt = rppa_json_url) %>%
  .$antibodies %>% 
  dplyr::as_tibble() %>%
  dplyr::select(`0`:`2`) %>%
  dplyr::rename(
    protein = `0`,
    symbol = `1`,
    status = `2`,
    origin = `3`,
    source = `4`,
    catalog = `5`
  ) %>%
  dplyr::mutate_all(.funs = dplyr::funs(stringr::str_trim)) %>%
  dplyr::mutate(
    origin = stringr::str_sub(origin, end = 1),
    status = dplyr::recode(
      status,
      "Validated as ELISA" = "V",
      "Use with Caution" = "C",
      .default = "E"
    ),
    protein = stringr::str_replace(protein, pattern = "^x", "")
  ) %>%
  dplyr::mutate(
    protein = stringr::str_c(protein, origin, status, sep = "-")
  ) %>%
  dplyr::select(protein, symbol) %>%
  dplyr::mutate(name = stringr::str_replace(protein, pattern = "-\\w-\\w$", "")) %>%
  dplyr::mutate(name = stringr::str_to_upper(name)) %>%
  dplyr::mutate(name = stringr::str_replace_all(name, pattern = "-|_", "")) -> tcpa_name
