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

# For frontend ------------------------------------------------------------

library(DT)
library(grid)


# Header ------------------------------------------------------------------

header <- dashboardHeader(
  title = "Gene Set Expression"
)

# Sidebar -----------------------------------------------------------------

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Welcome", tabName = "welcome", icon = icon("home"))
  )
)


# Body --------------------------------------------------------------------

body <- dashboardBody(
  shiny::tags$head(
    shinyWidgets::useSweetAlert(),
    shinyjs::useShinyjs()
    #shinyjs::extendShinyjs()
    #shiny::tags$link()
    #shiny::tags$script()
  ),
  
  # main body
  tabItems(
    source(file = file.path(config$wd, "ui", "welcome_ui.R"), local = TRUE)$value
  )
)


# Dashboardpage -----------------------------------------------------------

page <- dashboardPage(
  title = "GSEXPR - Gene Set Expression",
  header = header,
  sidebar = sidebar,
  body = body
)


# shiny UI ----------------------------------------------------------------

ui <- page

shinyUI(ui = ui)


