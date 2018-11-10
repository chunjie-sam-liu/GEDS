# sourced by 'ui.R'
# save as 'about_ui.R'
# ui elements for about


tabItem(
  tabName = "contact", align = "center",
  
  # Welcome message ----
  fluidRow(
    style = "width:80%;", 
    
    column(
      width = 12, offset = 0, align = "left",
      
      shiny::tags$h1(
        class = "text-success text-left",
        shiny::icon(name = "angle-double-right", class = "fa-fw"),
        "Contact"
      ),
      shiny::hr(),
      
      shiny::tags$address(
        shiny::tags$p(shiny::tags$strong("An-Yuan Guo, Ph.D. Professor of Bioinformatics")),
        shiny::fluidRow(
          shiny::column(width = 1, offset = 0, shiny::div(shiny::strong("Email: "))),
          shiny::column(width = 3, offset = 0, shiny::plotOutput(outputId = "ay", height = "20px", width = "200px"))
        )
      ),
      
      shiny::tags$address(
        shiny::tags$p(shiny::tags$strong("Chun-Jie Liu, Ph.D. Postdoc fellow")),
        shiny::fluidRow(
          shiny::column(width = 1, offset = 0, shiny::div(shiny::strong("Email: "))),
          shiny::column(width = 3, offset = 0, shiny::plotOutput(outputId = "cj", height = "20px", width = "200px"))
        )
      ),
      shiny::tags$address(
        shiny::tags$p(shiny::tags$strong("Meng-xuan Xia, Ph.D. Candidate")),
        shiny::fluidRow(
          shiny::column(width = 1, offset = 0, shiny::div(shiny::strong("Email: "))),
          shiny::column(width = 3, offset = 0, shiny::plotOutput(outputId = "mx", height = "20px", width = "200px"))
        )
      )
    )

  ),
  # Load footer ----
  source(file.path(config$wd, "ui", "footer.R"), echo = FALSE, verbose = FALSE)$value
) # End of tabItem