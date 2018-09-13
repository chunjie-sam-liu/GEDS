# sourced by "welcome_server.R"

# panel -------------------------------------------------------------------

fn_panel_miRNA <- function(){
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
}

# dataset seletct ---------------------------------------------------------

fn_miRNA_select <- function(.miRNA){
  shiny::fluidRow(
    column(
      width = 12, offset = 1,
      shinydashboard::box(
        width = 10,
        status = "primary",
        solidHeader = TRUE,
        title="Select TCGA Cancer Types",
        checkboxGroupButtons(
          inputId = "select_miRNA_TCGA", label = "", status = "primary", size = "lg",selected = c('CESC','LIHC','LGG','UCS'),
          choices = .miRNA
        ),
        shinyjs::hide(switchInput(
          inputId = "select_dataset7", label = "Dataset", value = FALSE,
          onLabel = "All", offLabel = "None", size = "large", offStatus = "danger"
        )))
    )
  )
}
