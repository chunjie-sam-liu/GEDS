# sourced by "detail_search_server.R"

fn_welcome_msg <- function() {
  column(
    width = 12, offset = 0,
    tagList(
      column(width = 2,offset = 1,
        shiny::tags$img(
        src = "./img/logo.png",
        class = "center-block img-responsive",
        style = "height: 150px;"
      )
      ),
      column(width = 9,
        shiny::tags$h1("GEDS: Gene Expression Display Server"),
        shiny::tags$p("GEDS is an integrative platform to show human gene expressions in cancer types, normal tissues and cell lines for user input genes, miRNAs and proteins.")
        )
    )
    
  )
}

# analysis ----------------------------------------------------------------

fn_analysis <-  function(){
  tagList(
    column(
      width = 10, offset = 1, style = "margin-top: 30px; text-align: left",
      shiny::tags$p("Tips: Click mRNA, miRNA, protein to switch panel.")
    ),
    column(
      width = 10, offset = 1, style = "margin-top: -20px",
      shinydashboard::tabBox(
        title = "",id = "tabset1", width=12, 
            tabPanel("mRNA", 
                  shiny::uiOutput(outputId = "ui_panel_mRNA"),
                  #shiny::uiOutput(outputId = "ui_mRNA_select"),
                  shiny::uiOutput(outputId = "ui_mRNA_result")
            ),
            tabPanel("miRNA", 
                 shiny::uiOutput(outputId = "ui_panel_miRNA"),
                 #shiny::uiOutput(outputId = "ui_miRNA_select"),
                 shiny::uiOutput(outputId = "ui_miRNA_result")
            ),
            tabPanel("protein", 
                 shiny::uiOutput(outputId = "ui_panel_protein"),
                 #shiny::uiOutput(outputId = "ui_protein_start"),
                 #shiny::uiOutput(outputId = "ui_protein_stat"),
                 shiny::uiOutput(outputId = "ui_protein_result")
            )
      )
    )
  )
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