# sourced by 'ui.R'
# ui elements for welcome

tabItem(
  tabName = "welcome", align = "center",
  
  # welcome
  fluidRow(shiny::uiOutput(outputId = "ui_welcome_msg"))
)