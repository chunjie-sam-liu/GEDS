# sourced by "server.R"
# save as "init_server.R"

# User logs and infos -----------------------------------------------------

user_analysis <- c(
  "gene_set", "protein_set","miRNA_set",
  "tcga_expr", "tcga_protein", "tcga_mirna",
  "ccle_expr", "hpa_expr", "gtex_expr")

# Status and error --------------------------------------------------------

progress <- reactiveValues(
  "expr_loading" = FALSE,
  "expr_calc" = FALSE,
  "progress_end" = FALSE
)
processing <- reactiveValues(
  "expr_loading_start" = FALSE,
  "expr_loading_end" = FALSE,
  "expr_calc_start" = FALSE,
  "expr_calc_end" = FALSE
)

status <- reactiveValues(
  "gene_set" = FALSE,
  "protein_set" = FALSE,
  "miRNA_set" = FALSE,
  "analysis" = FALSE,
  "tcga_expr" = FALSE,
  "gene_trigger" = FALSE,
  "protein_trigger" = FALSE,
  "miRNA_trigger" = FALSE,
  "progressbar" = FALSE
)

error <- reactiveValues(
  "gene_set" = "",
  "protein_set" = "",
  "miRNA_set" = "",
  "tcga_expr" = "",
  "ccle_expr" = "",
  "hpa_expr" = "",
  "gtex_expr" = ""
)


# analysis ----------------------------------------------------------------

selected_analysis <- reactiveValues(
  'mRNA' = FALSE,
  'protein' = FALSE,
  'mirna' = FALSE
)

selected_ctyps <- reactiveVal()

# Gene sets ---------------------------------------------------------------
gene_set <- reactiveValues(
  match = "",
  non_match = "",
  n_match = "",
  n_non_match = "",
  n_total = ""
)

# Load gene list ----------------------------------------------------------

mRNA_TCGA <- readr::read_tsv(file.path(config$database,"mRNA","datalist","TCGA_cancertypes"), col_names = FALSE) %>% .$X1
mRNA_GTEX <- readr::read_tsv(file.path(config$database,"mRNA","datalist","GTEX_tissues"), col_names = FALSE) %>% .$X1
mRNA_CCLE <- readr::read_tsv(file.path(config$database,"mRNA","datalist","CCLE_tissues"), col_names = FALSE) %>% .$X1
mRNA_HPA_tissue <- readr::read_tsv(file.path(config$database,"mRNA","datalist","hpa_tissue_file_list"), col_names = FALSE) %>% .$X1
mRNA_HPA_cellline <- readr::read_tsv(file.path(config$database,"mRNA","datalist","hpa_cellline_file_list"), col_names = FALSE) %>% .$X1
protein_TCGA <- "TCGA"
miRNA_TCGA <- "TCGA"
