# sourced by "server.R"


# Source the function -----------------------------------------------------

source(file.path(config$func, "welcome_func.R"))


# welcome message
output$ui_welcome_msg <- renderUI({fn_welcome_msg()})
output$ui_introduction <- renderUI({fn_introduction()})
output$ui_analysis <- renderUI({fn_analysis()})
output$ui_feature_figure <- renderUI({fn_feature_figure()})