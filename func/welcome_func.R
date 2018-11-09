# sourced by "detail_search_server.R"

fn_welcome_msg <- function() {
  column(
    width = 12, offset = 0,
    shiny::tags$h1("GEDS: Gene Expression Display Server")
  )
}

# analysis ----------------------------------------------------------------

fn_analysis <-  function(){
  shiny::fluidRow(
  column(
    width = 11,offset =1,
    shinydashboard::tabBox(
      title = "",id = "tabset1", height = "140px",width=10,
          tabPanel("mRNA", 
                  shiny::uiOutput(outputId = "ui_panel_mRNA"),
                  shiny::uiOutput(outputId = "ui_mRNA_select"),
                  shiny::uiOutput(outputId = "ui_mRNA_stat"),
                  shiny::uiOutput(outputId = "ui_mRNA_result")
                  
                  ),
          tabPanel("Protein", 
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

# download gene set button ------------------------------------------------

fn_gs_download <- function(data,txt){
  downloadHandler(
    filename = function() {
      glue::glue("{txt}")
    },
    content = function(con) {
      .f <- as.data.frame(data)
      readr::write_tsv(x=.f, con,col_names = FALSE)
    }
  )
}