####mRNA
GTEX <- readr::read_rds("/home/liucj/shiny-data/GSCALite/GTEx/gtex_gene_tmp_annotation_phenotype_v7.rds.gz")
mRNA_symbol <- readr::read_rds("/home/liucj/shiny-data/GEDS/mRNA/mRNA_symbol_alias.rds.gz")
TCGA <- readr::read_rds("tcga_pancan33-expr.rds.gz")

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

GTEX_filter %>% 
dplyr::mutate(tmp= stringr::str_replace_all(SMTS,pattern="Adipose Tissue",replacement="Adipose")) %>% dplyr::arrange(tmp) %>% 
dplyr::select(SMTS=tmp,expr) -> GTEX_filter2

### for sd file make
cluster <- multidplyr::create_cluster(30)
GTEX_filter2 %>% multidplyr::partition(cluster = cluster) %>%
    multidplyr::cluster_library("magrittr") %>%
    dplyr::mutate(
      mean = purrr::map(
        .x = expr,
        .f = function(.x){
            .x %>% 
                purrrlyr::by_row(
                    ..f = function(.y) {
                        .y %>% dplyr::select(-ensembl_gene_id,-symbol) %>% unlist() -> .xv
                        sd(log2(.xv+1)) %>% unname()
                    }
                ) %>% dplyr::rename(sd = .out) %>% 
            dplyr::select(ensembl_gene_id,symbol,sd) %>% tidyr::unnest()
        }
      )
    ) %>% 
    dplyr::collect() %>%
    dplyr::as_tibble() %>%
    dplyr::ungroup() %>%
    dplyr::select(-PARTITION_ID) ->GTEX_sd
    parallel::stopCluster(cluster)
    GTEX_sd %>% dplyr::select(-expr) %>% tidyr::unnest() %>% 
      readr::write_rds("GTEX_sd.rds.gz",compress="gz")
###

GTEX_filter2 %>% dplyr::mutate(tmp = stringr::str_replace_all(SMTS,pattern=" ",replacement="")) %>% 
    dplyr::mutate(tmp2 = paste("/data/xiamx/GEDS/mRNA/split_file/GTEX/",tmp,".rds.gz",sep="")) %>% 
    dplyr::select(tmp2) %>% print(n=Inf)

CCLE <- readr::read_rds("/home/xiamx/file_for_GEDS_test/CCLE_expr.rds.gz")

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

CCLE_filter %>% .[-28,] %>%
dplyr::mutate(tmp= stringr::str_replace_all(tissue,pattern="haematopoietic_and_lymphoid_tissue",replacement="haematopoietic_and_lymphoid")) %>% dplyr::arrange(tmp) %>% 
dplyr::select(tissue=tmp,expr) -> CCLE_filter2
### for sd
cluster <- multidplyr::create_cluster(30)
CCLE_filter2 %>% multidplyr::partition(cluster = cluster) %>%
    multidplyr::cluster_library("magrittr") %>%
    dplyr::mutate(
      mean = purrr::map(
        .x = expr,
        .f = function(.x){
            .x %>% 
                purrrlyr::by_row(
                    ..f = function(.y) {
                        .y %>% dplyr::select(-symbol) %>% unlist() -> .xv
                        sd(log2(.xv+1)) %>% unname()
                    }
                ) %>% dplyr::rename(sd = .out) %>% 
            dplyr::select(symbol,sd) %>% tidyr::unnest()
        }
      )
    ) %>% 
    dplyr::collect() %>%
    dplyr::as_tibble() %>%
    dplyr::ungroup() %>%
    dplyr::select(-PARTITION_ID) ->CCLE_sd
    parallel::stopCluster(cluster)
    CCLE_sd %>% dplyr::select(-expr) %>% tidyr::unnest() %>%
      readr::write_rds("CCLE_sd.rds.gz",compress="gz")
###

CCLE_filter2 %>% dplyr::mutate(tmp = stringr::str_replace_all(tissue,pattern="_",replacement=" ") %>% 
    stringr::str_to_title() %>% stringr::str_replace_all(pattern=" ",replacement="")) %>% 
    dplyr::mutate(tmp2 = paste("/data/xiamx/GEDS/mRNA/split_file/CCLE/",tmp,".rds.gz",sep="")) %>% 
    dplyr::select(tmp2) %>% print(n=Inf)

TCGA %>% dplyr::mutate(
    expr = purrr::map(
      .x = expr,
      .f = function(.x) {
        .x %>% dplyr::filter(symbol != "?") -> .xx
        .xx %>% names %>% .[c(-1,-2)] -> .barcode
        .xx %>% dplyr::mutate(tmp = ifelse(entrez_id == "728661","SLC35E2B",symbol)) %>% 
        dplyr::mutate(tmp2 = ifelse(entrez_id == "9906","SLC35E2A",tmp)) %>% 
        dplyr::select(symbol=tmp2,entrez_id,.barcode)
      }
  )
) -> TCGA_new

TCGA_new %>% dplyr::mutate(tmp2 = paste("/data/xiamx/GEDS/mRNA/split_file/TCGA/",cancer_types,".rds.gz",sep="")) %>% 
    dplyr::select(tmp2) %>% print(n=Inf)

#### mRNA
#### miRNA
TCGA <- readr::read_rds("tcga_pancan33-expr.rds.gz")
TCGA %>% dplyr::mutate(tmp2 = paste("/data/xiamx/GEDS/miRNA/split_file/TCGA/",cancer_types,".rds.gz",sep="")) %>% 
    dplyr::select(tmp2) %>% print(n=Inf)
#### miRNA

#### protein
TCGA <- readr::read_rds("TCGA_protein_new.rds.gz")
TCGA %>% 
  dplyr::mutate(
      new = purrr::map(
          .x = expr,
          .f = function(.x) {
              .x %>%
                dplyr::mutate(protein=ifelse(protein=="X1433BETA","1433BETA",protein)) %>%
                dplyr::mutate(protein=ifelse(protein=="X1433EPSILON","1433EPSILON",protein)) %>%
                dplyr::mutate(protein=ifelse(protein=="X1433ZETA","1433ZETA",protein)) %>%
                dplyr::mutate(protein=ifelse(protein=="X4EBP1","4EBP1",protein)) %>%
                dplyr::mutate(protein=ifelse(protein=="X4EBP1_pS65","4EBP1_pS65",protein)) %>%
                dplyr::mutate(protein=ifelse(protein=="X4EBP1_pT37T46","4EBP1_pT37T46",protein)) %>%
                dplyr::mutate(protein=ifelse(protein=="X4EBP1_pT70","4EBP1_pT70",protein)) %>%
                dplyr::mutate(protein=ifelse(protein=="X53BP1","53BP1",protein)) %>%
                dplyr::mutate(symbol=ifelse(symbol=="X1433BETA","1433BETA",symbol)) %>%
                dplyr::mutate(symbol=ifelse(symbol=="X1433EPSILON","1433EPSILON",symbol)) %>%
                dplyr::mutate(symbol=ifelse(symbol=="X1433ZETA","1433ZETA",symbol)) %>%
                dplyr::mutate(symbol=ifelse(symbol=="X4EBP1","4EBP1",symbol)) %>%
                dplyr::mutate(symbol=ifelse(symbol=="X53BP1","53BP1",symbol))
          }
      )
  ) ->TCGA_new

TCGA_new %>% dplyr::mutate(tmp2 = paste("/data/xiamx/GEDS/protein/split_file/TCGA/",cancer_types,".rds.gz",sep="")) %>% 
    dplyr::select(tmp2) %>% print(n=Inf)

MCLP <- readr::read_rds("/home/xiamx/file_for_GEDS_test/MCLP_expr_new.rds.gz")

### for sd
cluster <- multidplyr::create_cluster(19)
MCLP %>% .[-19,] %>% multidplyr::partition(cluster = cluster) %>%
    multidplyr::cluster_library("magrittr") %>%
    dplyr::mutate(
      mean = purrr::map(
        .x = expression,
        .f = function(.x){
            .x %>% 
                purrrlyr::by_row(
                    ..f = function(.y) {
                        .y %>% tidyr::gather(key = barcode, value = expr, -c(symbol,protein)) %>% tidyr::drop_na() %>% .$expr -> .xv
                        sd(log2(.xv+1)) %>% unname()
                    }
                ) %>% dplyr::rename(sd = .out) %>%
            dplyr::select(symbol,protein,sd)
        }
      )
    ) %>% 
    dplyr::collect() %>%
    dplyr::as_tibble() %>%
    dplyr::ungroup() %>%
    dplyr::select(-PARTITION_ID) ->MCLP_sd
    parallel::stopCluster(cluster)
    MCLP_sd %>% dplyr::select(-expression) %>% tidyr::unnest() %>% tidyr::unnest() %>%
      readr::write_rds("MCLP_sd.rds.gz",compress="gz")
###
MCLP %>% dplyr::mutate(tmp = stringr::str_to_title(tis)) %>% 
    dplyr::mutate(tmp2 = paste("/data/xiamx/GEDS/protein/split_file/MCLP/",tmp,".rds.gz",sep="")) %>% 
    dplyr::select(tmp2) %>% print(n=Inf)

### for sd
CCLE <- readr::read_csv("CCLE_RPPA_20180123.csv")
CCLE_info <- readr::read_csv("CCLE_RPPA_Ab_info_20180123.csv")

CCLE %>% colnames() %>% .[-1] -> barcode
CCLE %>% dplyr::mutate(tmp = stringr::str_split_fixed(X1,pattern="_",n=2) %>% .[,1]) %>% 
dplyr::select(Cell_line = tmp,X1,barcode) %>% 
dplyr::mutate(tmp = stringr::str_split_fixed(X1,pattern="_",n=2) %>% .[,2]) %>%
dplyr::select(Cell_line , tissue = tmp, barcode) -> CCLE_new

CCLE_new %>% dplyr::select(tissue) %>% dplyr::distinct() -> tissue
symbol <- readr::read_rds("protein_symbol_alias.rds.gz")
CCLE_info %>% dplyr::select(Antibody_Name,Target_Genes) %>% 
dplyr::mutate(tmp = stringr::str_replace(pattern="_Caution",replacement="",Antibody_Name)) %>% 
dplyr::mutate(tmp = stringr::str_replace_all(pattern="_",replacement="",tmp)) %>% 
dplyr::mutate(tmp = stringr::str_replace_all(pattern="-",replacement="",tmp)) %>% 
dplyr::mutate(tmp = stringr::str_replace_all(pattern=" ",replacement="",tmp)) %>% 
dplyr::mutate(tmp = stringr::str_replace_all(pattern='\\(',replacement="",tmp)) %>% 
dplyr::mutate(tmp = stringr::str_replace_all(pattern='\\)',replacement="",tmp) %>% toupper ) %>% 
dplyr::rename(protein = tmp) %>% dplyr::left_join(symbol,by="protein") %>% 
dplyr::select(Antibody_Name,Target_Genes,protein,symbol,alias) -> new_info

new_info %>% .[c(36,37,39,40,47,48,114,132,137,177,206),] %>% .$Antibody_Name -> a
new_info %>% .[c(36,37,39,40,47,48,114,132,137,177,206),] %>% .$Target_Genes -> b
new_info %>% .[c(36,37,39,40,47,48,114,132,137,177,206),] %>% .$protein -> c

data.frame(Antibody_Name=a,
           Target_Genes=b,
           protein=c,
           symbol=b,
           alias=NA
           ) %>%
  dplyr::as_tibble() -> rep

rbind(new_info, rep) -> new_info_fixed
new_info_fixed %>% dplyr::select(-alias) %>% tidyr::drop_na() %>% dplyr::arrange(Antibody_Name) %>% 
dplyr::mutate(tmp = stringr::str_replace_all(pattern=" ",replacement=",",symbol)) %>% 
dplyr::select(antibody = Antibody_Name,protein,symbol=tmp) -> new_info_final

tissue %>% dplyr::mutate(
      expr = purrr::map(
        .x = tissue,
        .f = function(.x){
            CCLE_new %>% dplyr::filter(tissue %in% .x) %>% dplyr::select(-tissue) %>% 
            tidyr::gather(key=protein,value=expr,-Cell_line) %>% dplyr::select(-Cell_line) -> a
            a %>% dplyr::select(protein) %>% dplyr::distinct() %>% dplyr::rename(antibody=protein) -> antibody
            antibody %>% dplyr::mutate(
                    expr = purrr::map(
                        .x = antibody,
                        .f = function(.x){
                            a %>% dplyr::filter(protein %in% .x) %>% .$expr -> .xv
                            sd(log2(.xv+1)) %>% unname()
                        }
                    )
            ) %>% dplyr::left_join(new_info_final,by="antibody") %>% dplyr::select(symbol,protein,expr)
        }
      )
    ) %>% dplyr::rename(summary=expr) %>% dplyr::mutate(tissue = tolower(tissue)) -> new
new %>% dplyr::mutate(tmp= stringr::str_replace_all(tissue,pattern="haematopoietic_and_lymphoid_tissue",replacement="haematopoietic_and_lymphoid")) %>% dplyr::arrange(tmp) %>% 
dplyr::select(tissue=tmp,summary) %>% tidyr::unnest() %>% tidyr::unnest() %>% 
readr::write_rds("CCLE_protein_sd.rds.gz",compress="gz")
###

CCLE <- readr::read_rds("CCLE_protein_expr.rds.gz")
CCLE %>%
dplyr::mutate(tmp= stringr::str_replace_all(tissue,pattern="haematopoietic_and_lymphoid_tissue",replacement="haematopoietic_and_lymphoid")) %>% dplyr::arrange(tmp) %>% 
dplyr::select(tissue=tmp,expr) -> CCLE_filter2

CCLE_filter2 %>% dplyr::mutate(tmp = stringr::str_replace_all(tissue,pattern="_",replacement=" ") %>% 
    stringr::str_to_title() %>% stringr::str_replace_all(pattern=" ",replacement="")) %>% 
    dplyr::mutate(tmp2 = paste("/data/xiamx/GEDS/protein/split_file/CCLE/",tmp,".rds.gz",sep="")) %>% 
    dplyr::select(tmp2) %>% print(n=Inf)
#### protein