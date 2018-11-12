# sourced by "server.R"

# Source the function -----------------------------------------------------

source(file.path(config$func, "help_func.R"))


# ui help content ---------------------------------------------------------

output$ui_help_content <- shiny::renderUI({fn_help_content()})


# data table -------------------------------------------------------------

output$tcga_data_table <- shiny::renderDataTable({help_data_table(source = 'tcga')})

