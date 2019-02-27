TCGA <- readr::read_rds("TCGA_mRNA_summary.rds.gz")
new_anno <- readr::read_tsv("/home/xiamx/web/Homo_sapiens.gene_info")
new_anno %>% dplyr::select(GeneID,Symbol,Synonyms,dbXrefs) ->cut_new_anno

cut_new_anno %>% 
dplyr::mutate(tmp = stringr::str_replace_all(pattern="\\|",replacement=",",Synonyms)) %>% 
dplyr::mutate(tmp2 = paste(",",tmp,",") %>% stringr::str_replace_all(pattern=" ",replacement="")) %>% 
dplyr::mutate(tmp = paste(",",Symbol,",") %>% stringr::str_replace_all(pattern=" ",replacement="")) %>% 
dplyr::select(GeneID,Symbol,alias_match=tmp2,symbol_match=tmp,dbXrefs) %>% 
dplyr::mutate(GeneID=as.character(GeneID))->anno_to_match

###TCGA handle in .4
TCGA$summary[[1]] %>% dplyr::select(symbol,GeneID=entrez_id) %>% 
  dplyr::filter(GeneID %in% anno_to_match$GeneID) -> paired

paired %>% dplyr::left_join(by="GeneID",anno_to_match) ->pared_anno_result

TCGA$summary[[1]] %>% dplyr::select(symbol,GeneID=entrez_id) %>% 
dplyr::mutate(tmp = ifelse(GeneID %in% anno_to_match$GeneID,"yes","no")) %>% 
dplyr::filter(tmp %in% "no") %>% .$symbol ->not_id

tibble::tibble(symbol = not_id) %>%
    dplyr::mutate(
      expression = purrr::map(
        .x = symbol,
        .f = function(.x) {
          grep(pattern = (paste(",",.x,",") %>% 
            stringr::str_replace_all(' ','')), anno_to_match$symbol_match, value = TRUE ) -> a
          anno_to_match %>% dplyr::filter(symbol_match %in% a) %>% .$Symbol-> b
          grep(pattern = (paste(",",.x,",") %>% 
            stringr::str_replace_all(' ','')), anno_to_match$alias_match, value = TRUE ) -> c
          anno_to_match %>% dplyr::filter(alias_match %in% c) %>% .$Symbol-> d
          e <- c(b,d)
          if(length(e)>0){e} else{"drop"}
        }
      )
    ) -> .v_dedup

.v_dedup %>% dplyr::filter(expression %in% "drop") -> actually_not_in_new_anno
actually_not_in_new_anno %>% dplyr::mutate(Symbol=symbol)  %>% 
dplyr::left_join(by="symbol",TCGA$summary[[1]]) %>% dplyr::mutate(alias_match=",-,") %>% 
dplyr::mutate(dbXrefs="-") %>% 
dplyr::mutate(symbol_match = paste(",",Symbol,",") %>% stringr::str_replace_all(pattern=" ",replacement="")) %>% 
dplyr::select(symbol,GeneID=entrez_id,Symbol,alias_match,symbol_match,dbXrefs) -> actually_not_in_new_anno_result

not_id %in% actually_not_in_new_anno$symbol -> .int
not_id[!.int] -> entrez_id_changed

.v_dedup %>% dplyr::filter(symbol %in% entrez_id_changed)  %>% 
tidyr::unnest() %>% dplyr::select(symbol) %>% 
dplyr::filter(symbol %in% unique(symbol[duplicated(symbol)])) %>% 
dplyr::distinct() -> duplicated_list

##remove duplicated according to the NCBI RefSeq status = VALIDATED and symbol = Symbol
.v_dedup %>% dplyr::filter(symbol %in% duplicated_list$symbol) %>% 
  tidyr::unnest() %>% .[c(-2,-4,-6,-8,-10,-11,-12,-13,-15),] %>%
  dplyr::select(symbol,Symbol=expression) %>% 
  dplyr::left_join(by="Symbol",anno_to_match) %>%
  dplyr::select(symbol,GeneID,Symbol,alias_match,symbol_match,dbXrefs) ->duplicated_result

entrez_id_changed %in% duplicated_list$symbol ->ee
entrez_id_changed[!ee] -> not_duplicated

.v_dedup %>% dplyr::filter(symbol %in% not_duplicated) %>% tidyr::unnest() %>%
  dplyr::select(symbol,Symbol=expression) %>% 
  dplyr::left_join(by="Symbol",anno_to_match) %>%
  dplyr::select(symbol,GeneID,Symbol,alias_match,symbol_match,dbXrefs) ->not_duplicated_result

TCGA_anno <- rbind(pared_anno_result,not_duplicated_result,duplicated_result,actually_not_in_new_anno_result)

###GTEX handled in .3 /data/xiamx/GEDS/mRNA
GTEX <- readr::read_rds("gtex_gene_tmp_annotation_phenotype_v7.rds.gz")
anno_to_match <- readr::read_rds("anno_to_match.rds.gz")
GTEX$expr[[1]] %>% dplyr::select(ensembl_gene_id,symbol) %>% 
  dplyr::mutate(tmp = stringr::str_split_fixed(ensembl_gene_id,pattern="\\.",n=2) %>% 
  .[,1]) %>% dplyr::select(symbol,dbXrefs=tmp) -> GTEX_cut_list
anno_to_match %>% dplyr::mutate(dbXrefs = stringr::str_split_fixed(dbXrefs,pattern="Ensembl:",n=2) %>% 
.[,2]) %>% dplyr::filter(dbXrefs %in% GTEX_cut_list$dbXrefs) %>% 
dplyr::left_join(GTEX_cut_list,by="dbXrefs") %>% 
dplyr::select(symbol,GeneID,Symbol,alias_match,symbol_match,dbXrefs) ->GTEX_anno
## if continue follow datafilter.R GTEX process 
### and change mRNA_symbol to GTEX_anno 
### and continue next step
## give up to build new GTEX filter for the reason that new GTEX expression file contained 1W+ duplicated gene
### continue to use old GTEX file in .4
GTEX <- readr::read_rds("GTEX_mRNA_summary.rds.gz")
GTEX$summary[[1]] %>% dplyr::select(ensembl_gene_id,symbol) %>% 
  dplyr::mutate(tmp = stringr::str_split_fixed(ensembl_gene_id,pattern="\\.",n=2) %>% 
  .[,1]) %>% dplyr::select(symbol,dbXrefs=tmp) -> GTEX_cut_list
anno_to_match %>% dplyr::mutate(dbXrefs = stringr::str_split_fixed(dbXrefs,pattern="Ensembl:",n=2) %>% 
.[,2]) %>% dplyr::filter(dbXrefs %in% GTEX_cut_list$dbXrefs) %>% 
dplyr::left_join(GTEX_cut_list,by="dbXrefs") %>% 
dplyr::select(symbol,Symbol,alias_match,symbol_match,dbXrefs) ->GTEX_paired_result
GTEX_cut_list$symbol -> GTEX_gene_list
GTEX_paired_result$symbol -> GTEX_paired
GTEX_gene_list %in% GTEX_paired ->.int
GTEX_gene_list[!.int] ->GTEX_unpaired

tibble::tibble(symbol = GTEX_unpaired) %>% dplyr::distinct() %>%
    dplyr::mutate(
      Symbol = purrr::map(
        .x = symbol,
        .f = function(.x) {
          grep(pattern = (paste(",",.x,",") %>% 
            stringr::str_replace_all(' ','')), anno_to_match$symbol_match, value = TRUE ) -> a
          anno_to_match %>% dplyr::filter(symbol_match %in% a) %>% .$Symbol-> b
          grep(pattern = (paste(",",.x,",") %>% 
            stringr::str_replace_all(' ','')), anno_to_match$alias_match, value = TRUE ) -> c
          anno_to_match %>% dplyr::filter(alias_match %in% c) %>% .$Symbol-> d
          e <- c(b,d)
          if(length(e)>0){e} else{"drop"}
        }
      )
    ) -> .GTEX

.GTEX %>% dplyr::filter(Symbol %in% "drop") -> actually_not_in_new_anno
actually_not_in_new_anno %>% dplyr::mutate(Symbol=symbol)  %>% 
dplyr::left_join(by="symbol",GTEX$summary[[1]]) %>% dplyr::mutate(alias_match=",-,") %>% 
dplyr::mutate(dbXrefs="-") %>% 
dplyr::mutate(symbol_match = paste(",",Symbol,",") %>% stringr::str_replace_all(pattern=" ",replacement="")) %>% 
dplyr::select(symbol,Symbol,alias_match,symbol_match,dbXrefs) -> actually_not_in_new_anno_result

GTEX_unpaired %in% actually_not_in_new_anno$symbol -> .int
GTEX_unpaired[!.int] -> ensembl_id_changed

.GTEX %>% dplyr::filter(symbol %in% ensembl_id_changed)  %>% 
tidyr::unnest() %>% dplyr::select(symbol) %>% 
dplyr::filter(symbol %in% unique(symbol[duplicated(symbol)])) %>% 
dplyr::distinct() -> duplicated_list

##remove duplicated according to the NCBI RefSeq status = VALIDATED and symbol = Symbol
.GTEX %>% dplyr::filter(symbol %in% duplicated_list$symbol) %>% 
  tidyr::unnest() %>%
  dplyr::left_join(by="Symbol",anno_to_match) %>%
  dplyr::select(symbol,Symbol,alias_match,symbol_match,dbXrefs) ->duplicated_result

ensembl_id_changed %in% duplicated_list$symbol ->ee
ensembl_id_changed[!ee] -> not_duplicated

.GTEX %>% dplyr::filter(symbol %in% not_duplicated) %>% tidyr::unnest() %>% 
  dplyr::left_join(by="Symbol",anno_to_match) %>%
  dplyr::select(symbol,Symbol,alias_match,symbol_match,dbXrefs) ->not_duplicated_result

GTEX_anno <- rbind(GTEX_paired_result,not_duplicated_result,duplicated_result,actually_not_in_new_anno_result)

##merge result
TCGA_anno %>% dplyr::select(symbol,Symbol,alias_match,symbol_match) -> a
GTEX_anno %>% dplyr::select(symbol,Symbol,alias_match,symbol_match) -> b
rbind(a,b) %>% dplyr::distinct() %>% readr::write_rds("/home/xiamx/web/mRNA_symbol_alias.rds.gz")