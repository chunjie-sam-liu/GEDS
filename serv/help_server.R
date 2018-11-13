# sourced by "server.R"

# Source the function -----------------------------------------------------

source(file.path(config$func, "help_func.R"))


# ui help content ---------------------------------------------------------

output$ui_help_content <- shiny::renderUI({fn_help_content()})


# data table -------------------------------------------------------------

output$tcga_data_table <- DT::renderDataTable({help_data_table(source = 'tcga_sample_stat')})
output$gtex_data_table <- DT::renderDataTable({help_data_table(source = 'gtex_mrna_stat')})
output$ccle_data_table <- DT::renderDataTable({help_data_table(source = 'ccle_mrna_stat')})
output$mclp_data_table <- DT::renderDataTable({help_data_table(source = 'mclp_protein_stat')})
