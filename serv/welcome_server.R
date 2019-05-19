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
output$ui_panel_protein <- renderUI({fn_panel_protein(.choice = protein_symbol_choices)})
output$ui_panel_miRNA <- renderUI({fn_panel_miRNA()})

# cancer types selection --------------------------------------------------

output$ui_protein_start <- renderUI({fn_protein_start() })

output$ui_mRNA_select <- renderUI({if (status$mRNA_set) {
  fn_mRNA_select(.tcga = mRNA_TCGA %>% dplyr::arrange(site) %>% dplyr::select(site) %>% dplyr::distinct() %>% .$site, .gtex = mRNA_GTEX$tissue, .ccle = mRNA_CCLE$tissue)}  else {NULL}})
output$ui_protein_select <- renderUI({if (status$protein_set) {
  fn_protein_select(.tcga = protein_TCGA$cancer_types, .mclp = protein_MCLP$tissue)} else {NULL}})
output$ui_miRNA_select <- renderUI({if (status$miRNA_set) {fn_miRNA_select(miRNA_TCGA$cancer_types)} else {NULL}})

# Statistics of input gene list -------------------------------------------

output$ui_mRNA_stat <- renderUI({if (status$mRNA_invalid) {fn_mRNA_set_stat()} else {NULL}})
output$ui_miRNA_stat <- renderUI({if (status$miRNA_invalid) {fn_miRNA_set_stat()} else {NULL}})

# Start analysis ----------------------------------------------------------
output$ui_mRNA_result <- renderUI({
  if(status$mRNA_result) {
    #if(multiple$mRNA){
      fn_mRNA_multi_result(list = plot_number$mRNA)
    #}
    #else{
      #fn_mRNA_single_result()
    #}
  }
  else {NULL}
})
output$plot_multiple_mRNA <- renderUI({
  if(status$mRNA_result){
    #if(multiple$mRNA){
      fn_plot_multiple_mRNA(choice$mRNA)
      #}
  }})

output$table_multiple_mRNA <- renderUI({
  if(status$mRNA_result){
    fn_table_multiple_mRNA(mRNA$TCGA_table,mRNA$GTEX_table,mRNA$CCLE_table,mRNA$TCGA_download,mRNA$GTEX_download,mRNA$CCLE_download)
  }
})

output$plot_result_mRNA <- renderUI({
  if(plotmode$mRNA == 1){
    fn_plot_result_mRNA1()
  }
  else if(plotmode$mRNA == 2){
    fn_plot_result_mRNA2()
  }
  else if(plotmode$mRNA == 3){
    fn_plot_result_mRNA3()
  }
  else if(plotmode$mRNA == 4){
    fn_plot_result_mRNA4()
  }
  else if(plotmode$mRNA == 5){
    fn_plot_result_mRNA5()
  }
  else if(plotmode$mRNA == 6){
    fn_plot_result_mRNA6()
  }
  else if(plotmode$mRNA == 7){
    fn_plot_result_mRNA7()
  }
})

output$ui_protein_result <- renderUI({
  if(status$protein_result) {
    #if(multiple$protein){
      fn_protein_multi_result(list = plot_number$protein)
    #}
    #else{
     # fn_protein_single_result()
    #}
    }
    else {NULL}
    })
output$plot_multiple_protein <- renderUI({
  if(status$protein_result){
    #if(multiple$protein){
      fn_plot_multiple_protein(match$protein)
    #}
  }})

output$plot_result_protein <- renderUI({
  if(plotmode$protein == 1){
    fn_plot_result_protein1()
  }
  else if(plotmode$protein == 2){
    fn_plot_result_protein2()
  }
  else if(plotmode$protein == 3){
    fn_plot_result_protein3()
  }
  else if(plotmode$protein == 4){
    fn_plot_result_protein4()
  }
  else if(plotmode$protein == 5){
    fn_plot_result_protein5()
  }
  else if(plotmode$protein == 6){
    fn_plot_result_protein6()
  }
  else if(plotmode$protein == 7){
    fn_plot_result_protein7()
  }
})

output$ui_miRNA_result <- renderUI({
  if(status$miRNA_result) {
    #if(multiple$miRNA){
      fn_mirna_multi_result(list = plot_number$miRNA)
    #}
    #else{
      #fn_mirna_single_result()
    #}
  } 
  else {NULL}
  })
output$plot_multiple_miRNA <- renderUI({
  if(status$miRNA_result){
      fn_plot_multiple_miRNA(choice$miRNA)
    }
  })

output$table_multiple_miRNA <- renderUI({
  if(status$miRNA_result){
    fn_table_multiple_miRNA(miRNA$TCGA_table,miRNA$TCGA_download)
  }
})

output$TCGA_protein <- renderUI({if(protein$TCGA){fn_protein_TCGA()}})
output$MCLP_protein <- renderUI({if(protein$MCLP){fn_protein_MCLP()}})
output$CCLE_protein <- renderUI({if(protein$CCLE){fn_protein_CCLE()}})

shinyjs::onclick(id = "detail", expr = shinyjs::js$openTab(id = "Document"))
shinyjs::onclick(id = "detail2", expr = shinyjs::js$openTab(id = "Document"))
shinyjs::onclick(id = "detail3", expr = shinyjs::js$openTab(id = "Document"))