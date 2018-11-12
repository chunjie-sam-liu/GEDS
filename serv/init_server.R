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
  "mRNA_set" = FALSE,
  "protein_set" = FALSE,
  "miRNA_set" = FALSE,
  "analysis" = FALSE,
  "mRNA_valid" = TRUE,
  "protein_valid" = TRUE,
  "miRNA_valid" = TRUE,
  "protein_result" = FALSE,
  "miRNA_result" = FALSE,
  "mRNA_result" = FALSE,
  "mRNA_trigger" = FALSE,
  "protein_trigger" = FALSE,
  "miRNA_trigger" = FALSE,
  "progressbar" = FALSE
)

error <- reactiveValues(
  "mRNA_set" = "",
  "protein_set" = "",
  "miRNA_set" = "",
  "tcga_expr" = "",
  "ccle_expr" = "",
  "hpa_expr" = "",
  "gtex_expr" = ""
)


# analysis ----------------------------------------------------------------

selected_all <- reactiveValues(
  'mRNA' = FALSE,
  'protein' = FALSE,
  'mirna' = FALSE
)


# Gene sets ---------------------------------------------------------------
input_mRNA_check <- reactiveValues(
  match = "",
  non_match = "",
  total = "",
  n_match = "",
  n_non_match = "",
  n_total = ""
)
input_protein_check <- reactiveValues(
  match = "",
  non_match = "",
  total = "",
  n_match = "",
  n_non_match = "",
  n_total = ""
)
input_miRNA_check <- reactiveValues(
  match = "",
  non_match = "",
  total = "",
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
plot_number <- reactiveValues(
  protein = FALSE,
  mRNA = FALSE,
  miRNA = FALSE
)
dataset_number <- reactiveValues(
  protein = NULL,
  mRNA = NULL,
  miRNA = NULL
)
multiple <- reactiveValues(
  protein = FALSE,
  mRNA = FALSE,
  miRNA = FALSE
)
choice <- reactiveValues(
  protein = NULL,
  mRNA = NULL,
  miRNA = NULL
)

# Load data ---------------------------------------------------------------
TCGA_mRNA <- readr::read_rds(file.path(config$database, "mRNA","TCGA_mRNA_summary.rds.gz"))
GTEX_mRNA <- readr::read_rds(file.path(config$database, "mRNA","GTEX_mRNA_summary.rds.gz"))
CCLE_mRNA <- readr::read_rds(file.path(config$database, "mRNA","CCLE_mRNA_summary.rds.gz"))


#TCGA_protein <- readr::read_rds(file.path(config$database, "protein","TCGA_protein_summary.rds.gz"))
# MCLP_protein <- readr::read_rds(file.path(config$database, "protein","MCLP_protein_summary.rds.gz"))
#TCGA_miRNA <- readr::read_rds(file.path(config$database, "miRNA","TCGA_miRNA_summary.rds.gz"))

TCGA_color <- readr::read_rds(file.path(config$database, "TCGA_color.rds.gz"))
# Load gene list ----------------------------------------------------------
mRNA_TCGA <- readr::read_rds(file.path(config$database,"mRNA","TCGA_sort_cancertype.rds.gz"))
mRNA_GTEX <- readr::read_rds(file.path(config$database,"mRNA","GTEX_new_tissues_without.rds.gz"))
mRNA_CCLE <- readr::read_rds(file.path(config$database,"mRNA","CCLE_tissues_without.rds.gz"))
protein_TCGA <- readr::read_rds(file.path(config$database,"protein","TCGA_protein_sort_cancertype.rds.gz"))
protein_MCLP <- readr::read_rds(file.path(config$database,"protein","MCLP_tissues_filter_new.rds.gz"))
miRNA_TCGA <- readr::read_rds(file.path(config$database,"miRNA","TCGA_sort_miRNA_cancertype.rds.gz"))
total_mRNA_symbol <- readr::read_rds(file.path(config$database,"mRNA","mRNA_symbol_alias.rds.gz"))
total_protein_symbol <- readr::read_rds(file.path(config$database,"protein","protein_symbol_alias.rds.gz"))
total_miRNA_symbol <- readr::read_rds(file.path(config$database,"miRNA","TCGA_miRNA_symbol_new.rds.gz"))
