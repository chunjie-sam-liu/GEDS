# sourced by "server.R"
# save as "init_server.R"

# Status and error --------------------------------------------------------
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
  "mRNA_invalid" = FALSE,
  "protein_valid" = TRUE,
  "protein_invalid" = FALSE,
  "miRNA_valid" = TRUE,
  "miRNA_invalid" = FALSE,
  "protein_result" = FALSE,
  "miRNA_result" = FALSE,
  "mRNA_result" = FALSE,
  "mRNA_trigger" = FALSE,
  "protein_trigger" = FALSE,
  "miRNA_trigger" = FALSE,
  "mRNA_plot" = TRUE,
  "protein_plot" = TRUE,
  "miRNA_plot" = TRUE
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
alias <- reactiveValues(
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

miRNA <- reactiveValues(
  TCGA_table = NULL,
  TCGA_download = NULL
)

mRNA <- reactiveValues(
  TCGA_table = NULL,
  TCGA_download = NULL,
  GTEX_table = NULL,
  GTEX_download = NULL,
  CCLE_table = NULL,
  CCLE_download = NULL
)

protein <- reactiveValues(
  TCGA = FALSE,
  MCLP = FALSE,
  CCLE = FALSE
)



# Load data ---------------------------------------------------------------
TCGA_mRNA <- readr::read_rds(file.path(config$database, "mRNA","TCGA_mRNA_summary.rds.gz"))
GTEX_mRNA <- readr::read_rds(file.path(config$database, "mRNA","GTEX_mRNA_summary.rds.gz"))
CCLE_mRNA <- readr::read_rds(file.path(config$database, "mRNA","CCLE_mRNA_summary.rds.gz"))


TCGA_protein <- readr::read_rds(file.path(config$database, "protein","TCGA_protein_summary.rds.gz"))
MCLP_protein <- readr::read_rds(file.path(config$database, "protein","MCLP_protein_summary.rds.gz"))
CCLE_protein <- readr::read_rds(file.path(config$database, "protein","CCLE_protein_summary.rds.gz"))

TCGA_miRNA <- readr::read_rds(file.path(config$database, "miRNA","TCGA_miRNA_summary.rds.gz"))

TCGA_color <- readr::read_rds(file.path(config$database, "TCGA_color.rds.gz"))
# Load gene list ----------------------------------------------------------
mRNA_TCGA <- readr::read_rds(file.path(config$database,"mRNA","TCGA_mRNA_cancertype_summary.rds.gz"))
mRNA_GTEX <- readr::read_rds(file.path(config$database,"mRNA","GTEX_mRNA_tissue_summary.rds.gz"))
mRNA_CCLE <- readr::read_rds(file.path(config$database,"mRNA","CCLE_mRNA_cellline_summary.rds.gz"))

protein_TCGA <- readr::read_rds(file.path(config$database,"protein","TCGA_protein_cancertype_summary.rds.gz"))
protein_MCLP <- readr::read_rds(file.path(config$database,"protein","MCLP_protein_cellline_summary.rds.gz"))
protein_CCLE <- readr::read_rds(file.path(config$database,"protein","CCLE_protein_cellline_summary.rds.gz"))

miRNA_TCGA <- readr::read_rds(file.path(config$database,"miRNA","TCGA_miRNA_cancertype_summary.rds.gz"))
total_mRNA_symbol <- readr::read_rds(file.path(config$database,"mRNA","mRNA_symbol_alias.rds.gz"))
total_protein_symbol <- readr::read_rds(file.path(config$database,"protein","protein_symbol_alias.rds.gz"))
total_miRNA_symbol <- readr::read_rds(file.path(config$database,"miRNA","TCGA_miRNA_symbol_new.rds.gz"))

protein_symbol_choices <- readr::read_rds(file.path(config$database,"protein","protein_choice.rds.gz"))
