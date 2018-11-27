# sourced by "server.R"

source(file.path(config$func, "contact_func.R"))

# Source the function -----------------------------------------------------

output$ui_contact_info <- shiny::renderUI({fn_contact_info()})

output$ay <- shiny::renderPlot({email('guoay@hust.edu.cn')})
output$cj <- shiny::renderPlot({email('chunjie-sam-liu@foxmail.com')})
output$mx <- shiny::renderPlot(email('xiamx@hust.edu.cn'))