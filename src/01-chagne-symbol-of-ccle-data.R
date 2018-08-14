
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


