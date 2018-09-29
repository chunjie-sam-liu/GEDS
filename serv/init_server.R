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
reset <- reactiveValues(
  "protein" = FALSE,
  "mRNA" = FALSE,
  "miRNA" = FALSE
)
status <- reactiveValues(
  "gene_set" = FALSE,
  "protein_set" = FALSE,
  "miRNA_set" = FALSE,
  "analysis" = FALSE,
  "valid" = TRUE,
  "protein_result" = FALSE,
  "miRNA_result" = FALSE,
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
input_list_check <- reactiveValues(
  match = "",
  non_match = "",
  n_match = "",
  n_non_match = "",
  n_total = ""
)


# match -------------------------------------------------------------------
match <- reactiveValues(
  protein = "",
  mRNA = "",
  miRNA = ""
)


# plot --------------------------------------------------------------------
plot <- reactiveValues(
  protein = NULL,
  mRNA = NULL,
  miRNA = NULL
)


# Load data ---------------------------------------------------------------

TCGA_protein <- readr::read_rds(file.path(config$database, "protein","TCGA_protein_new.rds.gz"))
TCGA_miRNA <- readr::read_rds(file.path(config$database, "miRNA","tcga_pancan33-mirna-expr.rds.gz"))
# Load gene list ----------------------------------------------------------

mRNA_TCGA <- readr::read_rds(file.path(config$database,"mRNA","TCGA_sort_cancertype.rds.gz"))
mRNA_GTEX <- readr::read_rds(file.path(config$database,"mRNA","GTEX_new_tissues.rds.gz"))
mRNA_CCLE <- readr::read_rds(file.path(config$database,"mRNA","CCLE_tissues.rds.gz"))
protein_TCGA <- readr::read_rds(file.path(config$database,"protein","TCGA_protein_sort_cancertype.rds.gz"))
protein_MCLP <- readr::read_rds(file.path(config$database,"protein","MCLP_tissues_new.rds.gz"))
miRNA_TCGA <- readr::read_rds(file.path(config$database,"miRNA","TCGA_sort_miRNA_cancertype.rds.gz"))
total_gene_symbol <- "BRAF"
total_protein_symbol <- readr::read_rds(file.path(config$database,"protein","protein_symbol.rds.gz"))
total_miRNA_symbol <- readr::read_rds(file.path(config$database,"miRNA","TCGA_miRNA_symbol.rds.gz"))
