# sourced by "server.R"

# Source the function -----------------------------------------------------

source(file.path(config$func, "welcome_func.R"))
source(file.path(config$func, "mRNA_func.R"))
source(file.path(config$func, "miRNA_func.R"))
source(file.path(config$func, "protein_func.R"))

# welcome message

output$ui_welcome_msg <- renderUI({fn_welcome_msg()})
output$ui_analysis <- renderUI({fn_analysis()})

# search panel ------------------------------------------------------------

output$ui_panel_mRNA <- renderUI({fn_panel_mRNA()})
output$ui_panel_protein <- renderUI({fn_panel_protein()})
output$ui_panel_miRNA <- renderUI({fn_panel_miRNA()})

# cancer types selection --------------------------------------------------

output$ui_multi_cancer_input <- renderUI({if (status$gene_set) {fn_gene_select()}  else {NULL}})
output$ui_mRNA_TCGA_select <- renderUI({fn_mRNA_TCGA_select(.TCGA = mRNA_TCGA)})
output$ui_mRNA_GTEX_select <- renderUI({fn_mRNA_GTEX_select(.GTEX= mRNA_GTEX)})
output$ui_mRNA_CCLE_select <- renderUI({fn_mRNA_CCLE_select(.CCLE = mRNA_CCLE)})
output$ui_protein_select <- renderUI({if (status$protein_set) {
  fn_protein_select(.tcga = protein_TCGA$cancer_types, .mclp = protein_MCLP$tissue)} else {NULL}})
output$ui_miRNA_select <- renderUI({if (status$miRNA_set) {fn_miRNA_select(miRNA_TCGA$cancer_types)} else {NULL}})

# Statistics of input gene list -------------------------------------------

output$ui_mRNA_stat <- renderUI({if (status$gene_set) {fn_gene_set_stat(input_list_check)} else {NULL}})
output$ui_protein_stat <- renderUI({if (status$protein_set) {fn_protein_set_stat(input_protein_check)} else {NULL}})
output$ui_miRNA_stat <- renderUI({if (status$miRNA_set) {fn_miRNA_set_stat(input_miRNA_check)} else {NULL}})

# Start analysis ----------------------------------------------------------

output$ui_start_analysis <- renderUI({if (status$gene_set) {fn_start_analysis()} else {NULL}})
output$ui_protein_result <- renderUI({
  if(status$protein_result) {
    if(multiple$protein){
      fn_protein_multi_result(list = plot_number$protein)
    }
    else{
      fn_protein_single_result()
    }}
    else {NULL}
    })
output$plot_multiple_protein <- renderUI({
  if(status$protein_result){
    if(multiple$protein){
      fn_plot_multiple_protein(choice$protein)}
  }})
output$ui_miRNA_result <- renderUI({
  if(status$miRNA_result) {
    if(multiple$miRNA){
      fn_mirna_multi_result(list = plot_number$miRNA)
    }
    else{
      fn_mirna_single_result()
    }
  } 
  else {NULL}
  })
output$plot_multiple_miRNA <- renderUI({
  if(status$miRNA_result){
    if(multiple$miRNA){
      fn_plot_multiple_miRNA(choice$miRNA)}
    }})

