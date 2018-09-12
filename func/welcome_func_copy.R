# sourced by "welcome_server.R"

fn_welcome_msg <- function() {
  column(
    width = 12, offset = 0,
    shiny::tags$h1("GSEXPR offers you a web-based platform for gene, protein or miRNA expression")
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
                       inputId = "input_protein_set",
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
fn_mRNA_HPA_tissue_select <- function(.HPA_tissue){
  shiny::fluidRow(
    column(
      width = 12, offset = 1,
      shinydashboard::box(
        width = 10,
        status = "success",
        solidHeader = TRUE,
        title="Select HPA Tissues",
        checkboxGroupButtons(
          inputId = "select_mRNA_HPA_tissue",label = "",status = "success", size = "lg",
          choices = .HPA_tissue,
          checkIcon = list(yes = icon("ok", lib = "glyphicon" ),no = icon("remove",lib = "glyphicon"))
        ),
        shinyjs::hide(switchInput(
          inputId = "select_dataset4", label = "Dataset", value = FALSE,
          onLabel = "All", offLabel = "None", size = "large", offStatus = "danger"
      )))
    )
  )
}
fn_mRNA_HPA_cellline_select <- function(.HPA_cellline){
  shiny::fluidRow(
    column(
      width = 12, offset = 1,
      shinydashboard::box(
        width = 10,
        status = "info",
        solidHeader = TRUE,
        title="Select HPA celllines",
        checkboxGroupButtons(
          inputId = "select_mRNA_HPA_cellline",label = "",status = "info", size = "lg",
          choices = .HPA_cellline,
          checkIcon = list(yes = icon("ok", lib = "glyphicon" ),no = icon("remove",lib = "glyphicon"))
        ),
        shinyjs::hide(switchInput(
          inputId = "select_dataset5", label = "Dataset", value = FALSE,
          onLabel = "All", offLabel = "None", size = "large", offStatus = "danger"
      )))
    )
  )
}
fn_protein_select <- function(.protein){
  shiny::fluidRow(
    column(
      width = 12, offset = 1,
      shinydashboard::box(
        width = 10,
        status = "primary",
        solidHeader = TRUE,
        title="Select TCGA Cancer Types",
        checkboxGroupButtons(
          inputId = "select_protein_TCGA", label = "",status = "primary", size = "lg", selected = c('ESCA','LIHC','CESC','COAD'),
          choices = .protein
        ),
        shinyjs::hide(switchInput(
          inputId = "select_dataset6", label = "Dataset", value = FALSE,
          onLabel = "All", offLabel = "None", size = "large", offStatus = "danger"
      )))
    )
  )
}
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
# Gene set stat -----------------------------------------------------------
fn_gene_set_stat <- function(input_list_check){
  column(
    width = 8, offset = 2, style = "margin-top:20px",
    downloadLink(
      outputId = "download_total_gene_set", label = NULL, class = NULL,
      valueBox(value = input_list_check$n_total, subtitle = "Total Input Genes", icon = icon("users"), color = "yellow")
    ),
    
    downloadLink(
      outputId = "download_valid_gene_set", label = NULL, class = NULL,
      valueBox(
        value = input_list_check$n_match, subtitle = "Valid Genes", icon = icon("credit-card"),color = "green")
    ),
    downloadLink(
      outputId = "download_input_logs", label = NULL, class = NULL,
      valueBox(
        value = input_list_check$n_non_match, subtitle = "Invalid Genes",icon = icon("line-chart"), color = "red")
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
fn_result <- function(id){
  column(
    width = 12,offset = 0,
    shinydashboard::tabBox(
      id = "expr_plot", title = "", width = 12,
      tabPanel(
        title = "Figure of expression",
        plotOutput(outputId = "expr_bubble_plot") %>% withSpinner(color = "#0dc5c1",size = 0.5, proxy.height = "200px")
      ),
      tabPanel(
        title = "Table of expression",
        DT::dataTableOutput(outputId = "expr_dt_comparison") %>% withSpinner(color = "#0dc5c1",size = 0.5, proxy.height = "200px")
      )
    )
  )
}