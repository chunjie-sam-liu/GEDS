# sourced by 'ui.R'
# ui elements for welcome

tabItem(
  tabName = "detail_search", align = "center",
  
  # welcome
  fluidRow(shiny::uiOutput(outputId = "ui_welcome_msg")),
  fluidRow(shiny::uiOutput(outputId = "ui_analysis")),
  #fluidRow(shiny::uiOutput(outputId = "ui_introduction")),
  #fluidRow(shiny::uiOutput(outputId = "ui_feature_figure")),
  
  

# footer ------------------------------------------------------------------
  source(file.path(config$wd, "ui", "footer.R"), echo = FALSE, verbose = FALSE)$value
)