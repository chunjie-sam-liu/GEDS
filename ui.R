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

# Header ------------------------------------------------------------------

header <- dashboardHeader(
  title = "Gene Set Expression"
)

# Sidebar -----------------------------------------------------------------

sidebar <- dashboardSidebar(
  sidebarMenu(

# Welcome -----------------------------------------------------------------
    menuItem("Welcome", tabName = "welcome", icon = icon("home")),

# Help --------------------------------------------------------------------
    menuItem("Help", tabName = "help", icon = icon("question")),

# Contact -----------------------------------------------------------------
    menuItem("Contact", tabName = "contact", icon= icon("envelope"))

    
  )
)


# Body --------------------------------------------------------------------

body <- dashboardBody(
  shiny::tags$head(
    shinyWidgets::useSweetAlert(),
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(script = file.path(config$wd, "www", "js", "geds.js")),
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "css/main.css"),
    shiny::tags$script(type = "text/javascript", src = "js/main.js")
  ),
  
  # main body
  tabItems(

  # Welcome -----------------------------------------------------------------
    source(file = file.path(config$wd, "ui", "welcome_ui.R"), local = TRUE)$value,
  
  # Help --------------------------------------------------------------------
    source(file = file.path(config$wd, "ui", "help_ui.R"), local = TRUE)$value,
  # Contact -----------------------------------------------------------------
    source(file = file.path(config$wd, "ui", "contact_ui.R"), local = TRUE)$value
  )
)


# Dashboardpage -----------------------------------------------------------

page1 <- dashboardPage(
  title = "GSEXPR - Gene Expression",
  header = header,
  sidebar = sidebar,
  body = body
)

page <- fluidPage(
          theme = shinytheme("yeti"),
          navbarPage("GEDS",
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
                   )
          )
)

# shiny UI ----------------------------------------------------------------

ui <- tagList(
  div(id = "loading-content",class="lds-facebook", span(id = "loading-text","GEDS"),div(), div(), div()),
  shinyjs::hidden(div(id = "app-content", page)),
  shiny::tags$head(
    shinyWidgets::useSweetAlert(),
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(script = file.path(config$wd, "www", "js", "geds.js")),
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "css/main.css"),
    shiny::tags$script(type = "text/javascript", src = "js/main.js")
  )
)


shinyUI(ui = ui)


