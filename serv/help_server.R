# sourced by "server.R"

# Source the function -----------------------------------------------------

source(file.path(config$func, "help_func.R"))



# introduction ------------------------------------------------------------

# output$ui_introduction <- renderUI({fn_introduction()})

# figure ------------------------------------------------------------------
# output$ui_feature_figure <- renderUI({fn_feature_figure()})

# ui help content ---------------------------------------------------------

output$ui_help_content <- shiny::renderUI({fn_help_content()})
