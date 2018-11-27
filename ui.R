# shiny ui

# Load library ------------------------------------------------------------
library(magrittr)

# For shiny ---------------------------------------------------------------

library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)
library(shinycssloaders)
library(shinydashboard)
library(shinythemes)
# For frontend ------------------------------------------------------------

library(DT)
library(grid)


# For plot ----------------------------------------------------------------

library(ggplot2)

# page -----------------------------------------------------------

page <- fluidPage(
          theme = shinytheme("paper"),
          navbarPage(
            windowTitle = 'GEDS - Gene Expression Display Server',
            title = HTML(paste("",
            img(
              src = "./img/logo.png",
              align = "middle",
              class = "img-responsvie",
              style = "height:55px !important; margin-top: -15px"
            )
          )),
                   tabPanel(
                     "Welcome",icon = icon("home"),
                     source(file = file.path(config$wd, "ui", "welcome_ui.R"), local = TRUE)$value
                   ),
                   tabPanel(
                     "Help", icon = icon("question"),
                     source(file = file.path(config$wd, "ui", "help_ui.R"), local = TRUE)$value
                    ),
                   tabPanel(
                     "Contact", icon= icon("envelope"),
                     source(file = file.path(config$wd, "ui", "contact_ui.R"), local = TRUE)$value
                   ),
            fluid = TRUE,
          collapsible = TRUE
          )
)

# shiny UI ----------------------------------------------------------------

ui <- tagList(
  div(id = "loading-content", span(id = "loading-text","GEDS"),div(class="lds-facebook",div(), div(), div(), div(), div())),
  shinyjs::hidden(div(id = "app-content", page)),
  shiny::tags$head(
    shinyWidgets::useSweetAlert(),
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(script = file.path(config$wd, "www", "js", "geds.js")),
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "css/main.css"),
    shiny::tags$link(rel = "shortcut icon", href = "img/logo.ico"),
    shiny::tags$script(type = "text/javascript", src = "js/main.js")
  )
)


shinyUI(ui = ui)


