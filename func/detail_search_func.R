# sourced by "detail_search_server.R"

fn_welcome_msg <- function() {
  column(
    width = 12, offset = 0,
    shiny::tags$h1("GEDS offers you a web-based platform for gene, protein or miRNA expression")
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
      title="This is introduction of GEDS"
    )
  )
}

# analysis ----------------------------------------------------------------

fn_analysis <-  function(){
  shiny::fluidRow(
  column(
    width = 10,offset =1,
    shinydashboard::tabBox(
      title = "",id = "tabset1", height = "140px",width=12,
          tabPanel("mRNA", 
                  shiny::uiOutput(outputId = "ui_panel_mRNA"),
                  shiny::uiOutput(outputId = "ui_mRNA_select"),
                  shiny::uiOutput(outputId = "ui_mRNA_stat"),
                  shiny::uiOutput(outputId = "ui_start_analysis"),
                  shiny::uiOutput(outputId = "ui_mRNA_result")
                  
                  ),
          tabPanel("protein", 
                  shiny::uiOutput(outputId = "ui_panel_protein"),
                  shiny::uiOutput(outputId = "ui_protein_select"),
                  shiny::uiOutput(outputId = "ui_protein_stat"),
                  shiny::uiOutput(outputId = "ui_protein_result")
          ),
          tabPanel("miRNA", 
                  shiny::uiOutput(outputId = "ui_panel_miRNA"),
                  shiny::uiOutput(outputId = "ui_miRNA_select"),
                  shiny::uiOutput(outputId = "ui_miRNA_stat"),
                  shiny::uiOutput(outputId = "ui_miRNA_result")
          )
        )
  ))
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
