# sourced by contact_server.R


# plot email --------------------------------------------------------------

email <- function(.txt) {
  grid::grid.newpage()
  grid::grid.text(
    label = .txt, hjust = 0, vjust = 1, x = 0, y = 1, 
    gp = grid::gpar(col = "#2196f3", fontsize = 18, fontface = 'bold', fontfamily = 'times')
  )
}

# address -----------------------------------------------------------------

address <- function(title, id) {
  shiny::tags$address(
    shiny::tags$p(shiny::tags$strong(title)),
    shiny::fluidRow(
      shiny::column(width = 1, offset = 0, shiny::div(shiny::strong("Email: "))),
      shiny::column(width = 3, offset = 0, shiny::plotOutput(outputId = id, height = "20px", width = "300px"))
    )
  )
} 

# contact info ------------------------------------------------------------

fn_contact_info <- function() {
  column(
    width = 10, offset = 1, align = 'left',
    
    shiny::tags$h3(
      class = "text-left",
      shiny::icon(name = "angle-double-right", class = "fa-fw"),
      "Contact"
    ),
    
    shiny::hr(),
    
    address(title = "An-Yuan Guo, Ph.D. Professor of Bioinformatics.", id = 'ay'),
    address(title = "Chun-Jie Liu, Ph.D. Postdoc fellow.", id = 'cj'),
    address(title = "Meng-xuan Xia, Ph.D. Candidate.", id = 'mx')
  )
}