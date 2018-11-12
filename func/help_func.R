# sourced by "help_server.R"

# tutorial ----------------------------------------------------------------

fn_tutorial <- function() {
  shiny::tagList(
    shiny::tags$h1(shiny::icon(name = "hand-o-right"), "Tutorial")
    
    
  )
}


# document ----------------------------------------------------------------

fn_document <- function() {
  shiny::tagList(
    shiny::tags$h1(shiny::icon(name = "hand-o-right"), "Tutorial")
  )
}

# help content ------------------------------------------------------------

fn_help_content <- function(){
  column(
    width = 12, offset = 0,
    
    shiny::tags$h1(
      class = "text-success text-left",
      shiny::icon(name = "angle-double-right", class = "fa-fw"),
      "Tutorial and Documentation"
    ),
    
    shiny::tags$div(
      
      # nav tabs
      shiny::tags$ul(
        class = "nav nav-tabs", role = "tablist",
        shiny::tags$li(
          role = "presentation", class = "active",
          shiny::tags$a(
            href = "#ui_tutorial", "aria-controls" = "ui_tutorial", 
            role = "tab", "data-toggle" = "tab", "Tutorial"
          )
        ),
        shiny::tags$li(
          role = "presentation",
          shiny::tags$a(
            href = "#ui_document", "aria-controls" = "ui_document", 
            role = "tab", "data-toggle" = "tab", "Document"
          )
        )
      ),
      
      # tab panes
      shiny::tags$div(
        class = "tab-content",
        shiny::tags$div(
          role = "tabpanel", class = "tab-pane active", id = "ui_tutorial",
          fn_tutorial()
        ),
        shiny::tags$div(
          role = "tabpanel", class = "tab-pane", id = "ui_document",
          fn_document()
        )
      )
    )
  )
}
