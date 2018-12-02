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
        shiny::tags$p("GEDS is an integrative gene expression platform for human cancer tissues, cancer cell lines and normal tissues with mRNA level, protein level and miRNA."),
        shiny::tags$p("On GEDS, when users input a set of names for genes, miRNAs and proteins,it will show their expressions in different cancer types, normal tissues and/or cell lines.")
        )
    )
    
  )
}

# analysis ----------------------------------------------------------------

fn_analysis <-  function(){
  column(
    width = 10,offset =1,
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
}

# download gene set button ------------------------------------------------
fn_showdata <- function(){
  column(
    width = 12,offset = 0,
      shiny::tags$hr(style = "margin-top: 40px"),
      shiny::tags$img(
        src = "./img/combine.png",
        class = "center-block img-responsive" )
  )
}


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