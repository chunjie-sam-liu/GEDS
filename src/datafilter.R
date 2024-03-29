TCGA %>% dplyr::mutate(
    expr = purrr::map(
      .x = expr,
      .f = function(.x) {
        .x %>% dplyr::filter(symbol != "?") -> .xx
        .xx %>% names %>% .[c(-1,-2)] -> .barcode
        .xx %>% dplyr::mutate(tmp = mRNA_symbol$symbol) %>% dplyr::select(symbol = tmp ,entrez_id, .barcode)
      }
  )
) -> TCGA_new

TCGA_new$expr[[2]] %>% names %>% .[c(-1,-2)] %>% tibble::tibble(barcode = .) %>% dplyr::mutate(
      type = stringr::str_sub(string = barcode, start = 14, end = 15)
    ) %>% 

cluster <- multidplyr::create_cluster(33)
TCGA_new %>% multidplyr::partition(cluster = cluster) %>%
    multidplyr::cluster_library("magrittr") %>%
    dplyr::mutate(
      mean = purrr::map(
        .x = expr,
        .f = function(.x){
            .x %>% names %>% .[c(-1,-2)] %>% tibble::tibble(barcode = .) %>% 
            dplyr::mutate(type = stringr::str_sub(string = barcode, start = 14, end = 15)) %>% 
            dplyr::mutate(type = ifelse(type == "11", "Normal", "Tumor")) ->cancertype
            cancertype %>% dplyr::filter(type %in% "Tumor") %>% .$barcode->tumor_barcode
            cancertype %>% dplyr::filter(type %in% "Normal") %>% .$barcode->normal_barcode
            .x %>% 
                purrrlyr::by_row(
                    ..f = function(.y) {
                        .y %>% dplyr::select(tumor_barcode) %>% unlist() -> .xv
                        c(quantile(.xv),mean(.xv)) %>% unname()
                    }
                ) %>% dplyr::rename(tumor = .out) %>% 
                purrrlyr::by_row(
                    ..f = function(.y) {
                        .y %>% dplyr::select(normal_barcode) %>% unlist() -> .xv
                        c(quantile(.xv),mean(.xv)) %>% unname()
                    }
                ) %>% dplyr::rename(normal = .out) %>%
            dplyr::select(symbol,entrez_id,tumor,normal)
        }
      )
    ) %>% 
    dplyr::collect() %>%
    dplyr::as_tibble() %>%
    dplyr::ungroup() %>%
    dplyr::select(-PARTITION_ID) ->TCGA_summary
    parallel::stopCluster(cluster)


GTEX %>% dplyr::mutate(
    expr = purrr::map(
      .x = expr,
      .f = function(.x) { 
        .x %>% names %>% .[c(-1,-2)] -> .barcode
        .x %>%  dplyr::mutate(tmp = stringr::str_replace(symbol,'SLC35E2','SLC35E2A')) %>% 
        dplyr::mutate(tmp2 = stringr::str_replace(tmp,'SLC35E2AB','SLC35E2B')) %>% 
        dplyr::select(ensembl_gene_id,symbol=tmp2,.barcode) %>%
        dplyr::filter(symbol %in% mRNA_symbol$symbol)
      }
  )
) ->GTEX_filter

cluster <- multidplyr::create_cluster(30)
GTEX_filter %>% multidplyr::partition(cluster = cluster) %>%
    multidplyr::cluster_library("magrittr") %>%
    dplyr::mutate(
      mean = purrr::map(
        .x = expr,
        .f = function(.x){
            .x %>% 
                purrrlyr::by_row(
                    ..f = function(.y) {
                        .y %>% dplyr::select(-ensembl_gene_id,-symbol) %>% unlist() -> .xv
                        c(quantile(.xv),mean(.xv)) %>% unname()
                    }
                ) %>% dplyr::rename(summary = .out) %>% 
            dplyr::select(ensembl_gene_id,symbol,summary)
        }
      )
    ) %>% 
    dplyr::collect() %>%
    dplyr::as_tibble() %>%
    dplyr::ungroup() %>%
    dplyr::select(-PARTITION_ID) ->GTEX_summary
    parallel::stopCluster(cluster)
    GTEX_summary %>% dplyr::select(-phenotype,-expr) %>% readr::write_rds("GTEX_summary.rds.gz",compress="gz")

CCLE %>% dplyr::mutate(
    expr = purrr::map(
      .x = expression,
      .f = function(.x) { 
        .x %>% names %>% .[c(-1)] -> .barcode
        .x %>%  dplyr::mutate(tmp = stringr::str_replace(symbol,'SLC35E2','SLC35E2A')) %>% 
        dplyr::mutate(tmp2 = stringr::str_replace(tmp,'SLC35E2AB','SLC35E2B')) %>% 
        dplyr::select(symbol=tmp2,.barcode) %>%
        dplyr::filter(symbol %in% mRNA_symbol$symbol)
      }
  )
) %>% dplyr::select(-expression) ->CCLE_filter

cluster <- multidplyr::create_cluster(30)
CCLE_filter %>% multidplyr::partition(cluster = cluster) %>%
    multidplyr::cluster_library("magrittr") %>%
    dplyr::mutate(
      mean = purrr::map(
        .x = expr,
        .f = function(.x){
            .x %>% 
                purrrlyr::by_row(
                    ..f = function(.y) {
                        .y %>% dplyr::select(-symbol,-protein) %>% unlist() -> .xv
                        c(quantile(.xv),mean(.xv)) %>% unname()
                    }
                ) %>% dplyr::rename(summary = .out) %>% 
            dplyr::select(symbol,protein,summary) %>%tidyr::unnest()
        }
      )
    ) %>% 
    dplyr::collect() %>%
    dplyr::as_tibble() %>%
    dplyr::ungroup() %>%
    dplyr::select(-PARTITION_ID) ->CCLE_summary
    parallel::stopCluster(cluster)
    CCLE_summary %>% dplyr::select(-expr) %>% readr::write_rds("CCLE_summary.rds.gz",compress="gz")

#protein
cluster <- multidplyr::create_cluster(19)
MCLP %>% multidplyr::partition(cluster = cluster) %>%
    multidplyr::cluster_library("magrittr") %>%
    dplyr::mutate(
      mean = purrr::map(
        .x = expression,
        .f = function(.x){
            .x %>% 
                purrrlyr::by_row(
                    ..f = function(.y) {
                        .y %>% tidyr::gather(key = barcode, value = expr, -c(symbol,protein)) %>% tidyr::drop_na() %>% .$expr -> .xv
                        c(quantile(.xv),mean(.xv)) %>% unname()
                    }
                ) %>% dplyr::rename(summary = .out) %>%
            dplyr::select(symbol,protein,summary)
        }
      )
    ) %>% 
    dplyr::collect() %>%
    dplyr::as_tibble() %>%
    dplyr::ungroup() %>%
    dplyr::select(-PARTITION_ID) ->MCLP_summary
    parallel::stopCluster(cluster)

cluster <- multidplyr::create_cluster(32)
TCGA %>% multidplyr::partition(cluster = cluster) %>%
    multidplyr::cluster_library("magrittr") %>%
    dplyr::mutate(
      mean = purrr::map2(
        .x = expr,
        .y = cancer_types,
        .f = function(.x, .y){
            print(.y)
            .x %>% names %>% .[c(-1,-2)] %>% tibble::tibble(barcode = .) %>% 
            dplyr::mutate(type = stringr::str_sub(string = barcode, start = 14, end = 15)) %>% 
            dplyr::mutate(type = ifelse(type == "11", "Normal", "Tumor")) ->cancertype
            cancertype %>% dplyr::filter(type %in% "Tumor") %>% .$barcode->tumor_barcode
            .x %>% 
                purrrlyr::by_row(
                    ..f = function(.y) {
                        .y %>% dplyr::select(symbol,protein,tumor_barcode) %>% tidyr::gather(key = barcode, value = expr, -c(symbol,protein)) %>% tidyr::drop_na() %>% .$expr -> .xv
                        c(quantile(.xv),mean(.xv)) %>% unname()
                    }
                ) %>% dplyr::rename(tumor = .out)  %>%
            dplyr::select(symbol,protein,tumor)
        }
      )
    ) %>% 
    dplyr::collect() %>%
    dplyr::as_tibble() %>%
    dplyr::ungroup() %>%
    dplyr::select(-PARTITION_ID) ->TCGA_summary
    parallel::stopCluster(cluster)

#miRNA
cluster <- multidplyr::create_cluster(33)
TCGA %>% multidplyr::partition(cluster = cluster) %>%
    multidplyr::cluster_library("magrittr") %>%
    dplyr::mutate(
      mean = purrr::map(
        .x = mirna,
        .f = function(.x){
            .x %>% names %>% .[c(-1,-313)] %>% tibble::tibble(barcode = .) %>% 
            dplyr::mutate(type = stringr::str_sub(string = barcode, start = 14, end = 15)) %>% 
            dplyr::mutate(type = ifelse(type == "11", "Normal", "Tumor")) ->cancertype
            cancertype %>% dplyr::filter(type %in% "Tumor") %>% .$barcode->tumor_barcode
            cancertype %>% dplyr::filter(type %in% "Normal") %>% .$barcode->normal_barcode
            .x %>% 
                purrrlyr::by_row(
                    ..f = function(.y) {
                        .y %>% dplyr::select(gene,name,tumor_barcode) %>% tidyr::gather(key = barcode, value = expr, -c(gene,name)) %>% tidyr::drop_na() %>% .$expr -> .xv
                        c(quantile(.xv),mean(.xv)) %>% unname()
                    }
                ) %>% dplyr::rename(tumor = .out) %>% 
                purrrlyr::by_row(
                    ..f = function(.y) {
                        .y %>% dplyr::select(gene,name,tumor,normal_barcode) %>% tidyr::gather(key = barcode, value = expr, -c(gene,name,tumor)) %>% tidyr::drop_na() %>% .$expr -> .xv
                        c(quantile(.xv),mean(.xv)) %>% unname()
                    }
                ) %>% dplyr::rename(normal = .out) %>%
            dplyr::select(gene,name,tumor,normal)
        }
      )
    ) %>% 
    dplyr::collect() %>%
    dplyr::as_tibble() %>%
    dplyr::ungroup() %>%
    dplyr::select(-PARTITION_ID) ->TCGA_summary
    parallel::stopCluster(cluster)

dplyr::mutate(
      mirna = purrr::map(
        .x = summary,
        .f = function(.x) {
          .x %>%
            dplyr::filter(name %in% c("hsa-miR-1290","hsa-miR-519d-3p")) %>% 
            tidyr::gather(key = barcode, value = expr, -c(gene,name)) %>% tidyr::unnest()
        }
      )
    ) %>% dplyr::select(-summary) %>% tidyr::unnest() %>% 
    dplyr::mutate(tmp = paste(cancer_types,barcode)) %>% 
    dplyr::select(cancer_types=tmp,name,expr=mirna) %>%
    dplyr::rename(expr=summary)