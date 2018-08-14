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
ccle_path <- "/data/shiny-data/GSEXPR/mRNA/CCLE/result"

tibble::tibble(
  tis = list.files(path = ccle_path )
) %>% 
  dplyr::mutate(
    expression = purrr::map(
      .x = tis,
      .f = function(.x) {
        .d <- readr::read_tsv(file = file.path(ccle_path, .x)) %>% 
          dplyr::rename(symbol = `Symbol name`)
      }
    )
  ) ->
  ccle_data







