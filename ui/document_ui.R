# sourced by 'ui.R'
# ui elements for contact

tabItem(
  tabName = "document", align = "center",
  
  # welcome
  
  # fluidRow(shiny::uiOutput(outputId = "ui_introduction")),
  # fluidRow(shiny::uiOutput(outputId = "ui_feature_figure")),
  fluidRow(shiny::uiOutput(outputId = 'ui_document')),
  
  
  
  # footer ------------------------------------------------------------------
  source(file.path(config$wd, "ui", "footer.R"), echo = FALSE, verbose = FALSE)$value
)