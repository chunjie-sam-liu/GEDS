# shiny server

# Options -----------------------------------------------------------------

options(shiny.reactlog = FALSE)
options(shiny.sanitize.errors = FALSE)


# Server function ---------------------------------------------------------

server <- function(input, output, session) {
  # welcome
  #source(file = file.path(config$serv, "welcome_server.R"), local = TRUE)
  source(file = file.path(config$serv, "detail_search_server.R"), local = TRUE)
  source(file = file.path(config$serv, "help_server.R"), local = TRUE)
  source(file = file.path(config$serv, "mRNA_server.R"), local = TRUE)
  source(file = file.path(config$serv, "protein_server.R"), local = TRUE)
  source(file = file.path(config$serv, "miRNA_server.R"), local = TRUE)
  source(file = file.path(config$serv, "init_server.R"), local = TRUE)
  source(file = file.path(config$serv, "functions_server.R"), local = TRUE)
}



# shiny server ------------------------------------------------------------

shinyServer(func = server)

