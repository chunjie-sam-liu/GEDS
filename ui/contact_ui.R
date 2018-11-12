# sourced by 'ui.R'
# save as 'about_ui.R'
# ui elements for about


tabItem(
  tabName = "contact", align = "center",
  
  # Welcome message ----
  fluidRow(shiny::uiOutput(outputId = "ui_contact_info")),
  # Load footer ----
  source(file.path(config$wd, "ui", "footer.R"), echo = FALSE, verbose = FALSE)$value
) # End of tabItem