# sourced by "welcome_server.R"

# panel -------------------------------------------------------------------

fn_panel_mRNA <- function(){
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
}


# dataset seletct ---------------------------------------------------------

fn_gene_select <- function(){
  shiny::fluidRow(
    shiny::uiOutput(outputId = "ui_mRNA_TCGA_select")#,
    #shiny::uiOutput(outputId = "ui_mRNA_GTEX_select"),
    #shiny::uiOutput(outputId = "ui_mRNA_CCLE_select"),
    #shiny::uiOutput(outputId = "ui_mRNA_HPA_tissue_select"),
    #shiny::uiOutput(outputId = "ui_mRNA_HPA_cellline_select")
  )
}
fn_mRNA_TCGA_select <- function(.TCGA){
  shiny::fluidRow(
    column(
      width = 12, offset=1,
      shinydashboard::box(
        width = 10,
        status = "primary",
        solidHeader = TRUE,
        title="Select TCGA Cancer Types",
        checkboxGroupButtons(
          inputId = "select_mRNA_TCGA",label = "",status = "primary", size = "lg",
          choices = .TCGA,
          checkIcon = list(yes = icon("ok", lib = "glyphicon" ),no = icon("remove",lib = "glyphicon"))
        ),
        shinyjs::hide(switchInput(
          inputId = "select_dataset1", label = "Dataset", value = FALSE,
          onLabel = "All", offLabel = "None", size = "large", offStatus = "danger"
        )))
    )
  )
}
fn_mRNA_GTEX_select <- function(.GTEX){
  shiny::fluidRow(
    column(
      width = 12, offset = 1,
      shinydashboard::box(
        width = 10,
        status = "danger",
        solidHeader = TRUE,
        title="Select GTEX Tissues",
        checkboxGroupButtons(
          inputId = "select_mRNA_GTEX",label = "",status = "danger", size = "lg",
          choices = .GTEX,
          checkIcon = list(yes = icon("ok", lib = "glyphicon" ),no = icon("remove",lib = "glyphicon"))
        ),
        shinyjs::hide(switchInput(
          inputId = "select_dataset2", label = "Dataset", value = FALSE,
          onLabel = "All", offLabel = "None", size = "large", offStatus = "danger"
        )))
    )
  )
}
fn_mRNA_CCLE_select <- function(.CCLE){
  shiny::fluidRow(
    column(
      width = 12, offset = 1,
      shinydashboard::box(
        width = 10,
        status = "warning",
        solidHeader = TRUE,
        title="Select CCLE Tissues",
        checkboxGroupButtons(
          inputId = "select_mRNA_CCLE",label = "",status = "warning", size = "lg",
          choices = .CCLE,
          checkIcon = list(yes = icon("ok", lib = "glyphicon" ),no = icon("remove",lib = "glyphicon"))
        ),
        shinyjs::hide(switchInput(
          inputId = "select_dataset3", label = "Dataset", value = FALSE,
          onLabel = "All", offLabel = "None", size = "large", offStatus = "danger"
        )))
    )
  )
}

# start analysis widgets --------------------------------------------------
fn_start_analysis <- function(){
  column(
    width = 8, offset = 2, style = "margin-top:20px",
    shinyBS::bsButton(inputId = "analysis", label = "Start Gene Set Analysis", icon = icon("play"), class = "btn-lg"),
    shinyBS::bsButton(inputId = "stop", label = "Stop", icon = icon("pause"), class = "btn-lg danger")
  )
}
