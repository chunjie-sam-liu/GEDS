library(tidyverse)
library(readr)
library(tibble)
library(magrittr)
# CCLE_path <- "/data/shiny-data/GSEXPR/mRNA/CCLE"
# adrenal_cortex <- read_tsv(file.path(CCLE_path, "result", "adrenal_cortex"), col_names = TRUE)
# autonomic_ganglia <- read_tsv(file.path(CCLE_path, "result", "autonomic_ganglia"), col_names = TRUE)
# biliary_tract <- read_tsv(file.path(CCLE_path, "result", "biliary_tract"), col_names = TRUE)
# bone <- read_tsv(file.path(CCLE_path, "result", "bone"), col_names = TRUE)
# brain <- read_tsv(file.path(CCLE_path, "result", "brain"), col_names = TRUE)
# breast <- read_tsv(file.path(CCLE_path, "result", "breast"), col_names = TRUE)
# central_nervous_system <- read_tsv(file.path(CCLE_path, "result", "central_nervous_system"), col_names = TRUE)
# cervix <- read_tsv(file.path(CCLE_path, "result", "cervix"), col_names = TRUE)
# endometrium <- read_tsv(file.path(CCLE_path, "result", "endometrium"), col_names = TRUE)
# eye <- read_tsv(file.path(CCLE_path, "result", "eye"), col_names = TRUE)
# haematopoietic_and_lymphoid_tissue <- read_tsv(file.path(CCLE_path, "result", "haematopoietic_and_lymphoid_tissue"), col_names = TRUE)
# intestine <- read_tsv(file.path(CCLE_path, "result", "intestine"), col_names = TRUE)
# kidney <- read_tsv(file.path(CCLE_path, "result", "kidney"), col_names = TRUE)
# liver <- read_tsv(file.path(CCLE_path, "result", "liver"), col_names = TRUE)
# lung <- read_tsv(file.path(CCLE_path, "result", "lung"), col_names = TRUE)
# oesophagus <- read_tsv(file.path(CCLE_path, "result", "oesophagus"), col_names = TRUE)
# ovary <- read_tsv(file.path(CCLE_path, "result", "ovary"), col_names = TRUE)
# pancreas <- read_tsv(file.path(CCLE_path, "result", "pancreas"), col_names = TRUE)
# placenta <- read_tsv(file.path(CCLE_path, "result", "placenta"), col_names = TRUE)
# pleura <- read_tsv(file.path(CCLE_path, "result", "pleura"), col_names = TRUE)
# prostate <- read_tsv(file.path(CCLE_path, "result", "prostate"), col_names = TRUE)
# salivary_gland <- read_tsv(file.path(CCLE_path, "result", "salivary_gland"), col_names = TRUE)
# skin <- read_tsv(file.path(CCLE_path, "result", "skin"), col_names = TRUE)
# soft_tissue <- read_tsv(file.path(CCLE_path, "result", "soft_tissue"), col_names = TRUE)
# stomach <- read_tsv(file.path(CCLE_path, "result", "stomach"), col_names = TRUE)
# synovium <- read_tsv(file.path(CCLE_path, "result", "synovium"), col_names = TRUE)
# thyroid <- read_tsv(file.path(CCLE_path, "result", "thyroid"), col_names = TRUE)
# unknown <- read_tsv(file.path(CCLE_path, "result", "unknown"), col_names = TRUE)
# upper_aerodigestive_tract <- read_tsv(file.path(CCLE_path, "result", "upper_aerodigestive_tract"), col_names = TRUE)
# urinary_tract <- read_tsv(file.path(CCLE_path, "result", "urinary_tract"), col_names = TRUE)
# CCLE_tibble <- list(
#   adrenal_cortex = adrenal_cortex,
#   autonomic_ganglia = autonomic_ganglia,
#   biliary_tract = biliary_tract,
#   bone = bone,
#   brain = brain,
#   breast = breast,
#   central_nervous_system = central_nervous_system,
#   cervix = cervix,
#   endometrium = endometrium,
#   eye = eye,
#   haematopoietic_and_lymphoid_tissue = haematopoietic_and_lymphoid_tissue,
#   intestine = intestine,
#   kidney = kidney,
#   liver = liver,
#   lung = lung,
#   oesophagus = oesophagus,
#   ovary = ovary,
#   pancreas = pancreas,
#   placenta = placenta,
#   pleura = pleura,
#   prostate = prostate,
#   salivary_gland = salivary_gland,
#   skin = skin,
#   soft_tissue = soft_tissue,
#   stomach = stomach,
#   synovium = synovium,
#   thyroid = thyroid,
#   unknown = unknown,
#   upper_aerodigestive_tract = upper_aerodigestive_tract,
#   urinary_tract = urinary_tract
# ) %>% tibble::enframe(name = "tissue", value = "expression")
# CCLE_tibble %>% readr::write_rds(path = file.path(CCLE_path, "result", "CCLE_expr.rds.gz"), compress = "gz")



# change name -------------------------------------------------------------
library(magrittr)
ccle_path <- "/data/shiny-data/GEDS/protein/test"

tibble::tibble(
  tis = list.files(path = ccle_path )
) %>% 
  dplyr::mutate(
    expression = purrr::map(
      .x = tis,
      .f = function(.x) {
        readr::read_tsv(file = file.path(ccle_path, .x))
        print(.x)
      }
    )
  ) ->
  ccle_data

# save cell line data to tibble.


protein_path <- "/data/shiny-data/GSEXPR/protein/drop/result"


tibble::tibble(
  tis = list.files(path = protein_path )
) %>%
  dplyr::mutate(
    expression = purrr::map(
      .x = tis,
      .f = function(.x) {
        .d <- readr::read_tsv(file = file.path(protein_path, .x))

        .d %>%
          tidyr::gather(key = 'protein', value = 'expr', -Sample_Name) %>%
          # tidyr::replace_na(replace = list(expr = 0)) %>%
          dplyr::mutate(expr = as.numeric(expr)) %>%
          tidyr::spread(key = Sample_Name, value = expr)
      }
    )
  ) ->
  d


a <- "LET7a-3p let7b-3p let-7c3p let-7D-3p aaaa bbbbb ccccc"
a %>%stringr::str_split(pattern = "[ ,;]+", simplify = TRUE) %>% .[1,] %>% tibble::tibble() %>%
dplyr::mutate(
    expression = purrr::map(
      .x = .,
      .f = function(.x) {
          grep(pattern = .x, mirna$symbol, value = TRUE) %>% as.character
      }
    )
  ) -> h
as.character(h$expression) %in% mirna$symbol -> .inter
as.character(h$expression)[.inter]



  .s %>%stringr::str_split(pattern = "[ ,;]+", simplify = TRUE) %>%.[1, ] -> .v
  .vvv <- .v[.v != ""] %>% unique() %>% sapply(FUN = toupper, USE.NAMES = FALSE)
  tibble::tibble(symbol=.vvv) %>%
    dplyr::mutate(
      expression = purrr::map(
        .x = symbol,
        .f = function(.x) {
          grep(pattern = .x, .total_symbol$symbol, value = TRUE ) ->a
          if(length(a)>0){a}
        }
      )
    ) -> .v_dedup
  .v_dedup %>% tidyr::drop_na() -> protein_match
  match_protein <- tidyr::separate_rows(protein_match,sep="\t") %>% dplyr::distinct() %>% .$expression
  TCGA_protein %>% dplyr::filter(cancer_types %in% select_protein_TCGA) %>%
    dplyr::mutate(
      expr = purrr::map(
        .x = expr,
        .f = function(.x) {
          .x %>%
            dplyr::filter(symbol %in% match_protein)
        }
      )
    ) -> expr_clean

  expr_clean %>%
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
    tidyr::unnest()  ->>plot_result

    test %>% tibble::tibble(x=.) %>%
      dplyr::mutate(
      plot = purrr::map(
        .x = x,
        .f = function(.x){
          plot_result %>% 
            dplyr::filter(protein %in% .x) %>% print()
        }
      )
    )  -> test2

    expr_clean %>%
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
    tidyr::unnest() ->>table_result

  plot_result$protein %>% 
    tibble::tibble(x=.) %>% 
    dplyr::distinct() %>%
    dplyr::mutate(
      plot = purrr::map(
        .x = x,
        .f = function(.x){
          plot_result  %>% 
            ggplot(mapping=aes(x=cancer_types,y=expr,color=cancer_types)) +
            geom_boxplot(outlier.colour = NA) +
            facet_wrap(~protein, ncol = 1) +
            theme(
            axis.line = element_line(color = "black"),
            panel.background  = element_rect(fill = "white", color = "grey"),
            panel.grid = element_line(colour = "grey"),
            axis.title.x = element_blank(),
            legend.title = element_blank(),
            text = element_text(size = 20)
            ) -> gg_result
          
          .filename <- file.path(config$wd,"plot",paste(.x,".png",sep = ""))
          
          print(.filename)
          ggsave(filename = file.path(config$wd,"plot",paste(.x,".png",sep = "")), plot = gg_result)
          
          paste(config$wd,"/plot/",.x,".png",sep = "") %>% c(plot$protein,.) ->plot$protein 
        }
      )
    )
  print(plot$protein)
  output$expr_bubble_plot <- renderImage({
    filename <- plot$protein 
    list(src = filename,alt= "plot result")
  }, deleteFile = F)