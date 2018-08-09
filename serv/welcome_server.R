# sourced by "server.R"


# Source the function -----------------------------------------------------

source(file.path(config$func, "welcome_func.R"))

# welcome message
output$ui_welcome_msg <- renderUI({fn_welcome_msg()})