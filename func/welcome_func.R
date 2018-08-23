# sourced by "welcome_server.R"

fn_welcome_msg <- function() {
  column(
    width = 12, offset = 0,
    shiny::tags$h1("GSEXPR offers you a web-based platform for gene set expression")
  )
}

fn_introduction <- function() {
  column(
    width = 12, offset = 0,
    shinydashboard::box(
      width = 12,
      status = "primary",
      solidHeader = TRUE,
      title="This is introduction of GSEXPR"
    )
  )
}

fn_analysis <-  function(){
  column(
    width =12, offset = 0,
    dashboardBody(
        tabBox(
          title = "",
          id = "tabset1", height = "140px",
          tabPanel("mRNA", 
                   column(
                     width = 12, offset = 0,
                     shinyWidgets::searchInput(
                       inputId = "input_gene_set",
                       label = "",
                       placeholder = 'Please input HGNC symbol gene set separated by space or " , "or " ; "',
                       btnSearch = icon("search"),
                       btnReset = icon("remove"),
                       width = "100%"
                     )
                   )
                  ),
          tabPanel("protein", 
                   column(
                     width = 12, offset = 0,
                     shinyWidgets::searchInput(
                       inputId = "input_protin_set",
                       label = "",
                       placeholder = 'Please input protein set separated by space or " , "or " ; "',
                       btnSearch = icon("search"),
                       btnReset = icon("remove"),
                       width = "100%"
                     )
                   )
          ),
          tabPanel("miRNA", 
                   column(
                     width = 12, offset = 0,
                     shinyWidgets::searchInput(
                       inputId = "input_miRNA_set",
                       label = "",
                       placeholder = 'Please input miRNA set separated by space or " , "or " ; "',
                       btnSearch = icon("search"),
                       btnReset = icon("remove"),
                       width = "100%"
                     )
                   )
          )
        )
    )
  )
}

fn_feature_figure <- function(){
  column(
    width=12,offset=0,
    shinydashboard::box(
      title = "This is figure",
      width = 12,
      solidHeader = TRUE,
      status = "primary"
    )
  )
}