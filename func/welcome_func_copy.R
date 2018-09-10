# sourced by "welcome_server.R"

fn_welcome_msg <- function() {
  column(
    width = 12, offset = 0,
    shiny::tags$h1("GSEXPR offers you a web-based platform for gene set expression")
  )
}

# introduction ------------------------------------------------------------

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


# analysis ----------------------------------------------------------------

fn_analysis <-  function(){
  column(
    width =12, offset = 0,
        shinydashboard::tabBox(
          title = "",id = "tabset1", height = "140px",width=12,
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
}

# multi cancer types input ------------------------------------------------

fn_multi_cancer_input <- function(){
  shiny::fluidRow(
    column(
      width = 4,
      shinydashboard::box(
        title = "Select Analysis",width=4,solidHeader = TRUE,status = "primary",height = "240px",
      radioGroupButtons(
        inputId = "select_analysis",
        label = "", 
        choices = c("mRNA", "protein", "miRNA"),selected = "mRNA",
        direction = "vertical",
         size = "lg",
        status = "primary"
      ),
      shinyjs::hide(switchInput(
        inputId = "ana_switch", label = "Analysis", value = FALSE,
        onLabel = "All", offLabel = "None", size = "large", offStatus = "danger"
      ))
    )
    ),
    column(
      width = 8,
      multiInput(
        inputId = "select_analysis", label = "Select Analysis (Selected in right)",
        choices = "a"
      ),
      shinyjs::hide(switchInput(
        inputId = "select_dataset", label = "Dataset", value = FALSE,
        onLabel = "All", offLabel = "None", size = "large", offStatus = "danger"
      ))
    )
  )
}
# figure ------------------------------------------------------------------

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