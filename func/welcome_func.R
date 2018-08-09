# sourced by "welcome_server.R"

fn_welcome_msg <- function() {
  column(
    width = 12, offset = 0,
    shiny::tags$h1("GSEXPR offers you a web-based platform for gene set expression")
  )
}