# shiny server

# Options -----------------------------------------------------------------

options(shiny.reactlog = FALSE)
options(shiny.sanitize.errors = FALSE)


# Server function ---------------------------------------------------------

server <- function(input, output, session) {
  
  # welcome
  source(file = file.path(config$serv, "welcome_server.R"), local = TRUE)
}


# shiny server ------------------------------------------------------------

shinyServer(func = server)

