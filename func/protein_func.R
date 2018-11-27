# sourced by "welcome_server.R"

# panel -------------------------------------------------------------------


fn_panel_protein <- function(.choice){
  tagList(
  column(
    width = 12, offset = 0,
    pickerInput(
      inputId = "input_protein_set", choices = .choice$protein, width = "1000px", options = list( `live-search` = TRUE, size = 5, title = "Select interested protein symbol")
    )
  ),
  column(
    width = 10,offset = 1,
    shiny::tags$p(
      style = "font-size: 9pt; color: red",
      "Protein level expression is quantified by reverse phase protein array (RPPA). It includes the cancer related ~200 protein and corresponding phosphorylated status."
    )
  ))
}


# result ------------------------------------------------------------------

fn_protein_single_result <- function(){
  column(
    width = 12,offset = 0,
    shinydashboard::tabBox(
      id = "protein_expr_plot", title = "", width = 12,
      tabPanel("Figure of expression",
               fluidRow(
               column(width=1,
                      download_bt(NS("protein",id=NULL))
               )),
               fluidRow(
               column(width=12,
                plotOutput(outputId = "expr_bubble_plot_protein", height = "100%", width = "100%") %>% 
                withSpinner(color = "#0dc5c1",size = 0.5, proxy.height = "200px")
               ))
      ),
      tabPanel(
        title = "Table of expression",
        DT::dataTableOutput(outputId = "expr_dt_comparison_protein") %>% withSpinner(color = "#0dc5c1",size = 0.5, proxy.height = "200px")
      )
  ))
}

fn_protein_multi_result <- function(list){
    column(
      width = 12, offset = 0,
      shinydashboard::tabBox(
        id = "mutiple_protein_plot_result", title = "", width = 12,
        tabPanel(
          title = "Figure of expression",
          tagList(
            column(
              width = 12, offset = 0,
              shiny::uiOutput(outputId = "plot_multiple_protein")
            ))
        ),
        tabPanel(
          title = "Table of expression",
          tagList(
          column(
          width = 12, offset = 0,
          DT::dataTableOutput(outputId = "expr_dt_comparison_TCGA_protein") %>% withSpinner(color = "#0dc5c1",size = 0.5, proxy.height = "200px")
          ),
          column(
            width = 12, offset = 0,
            DT::dataTableOutput(outputId = "expr_dt_comparison_MCLP_protein") %>% withSpinner(color = "#0dc5c1",size = 0.5, proxy.height = "200px")
          )
        )))
    )
}

fn_plot_multiple_protein <- function(choice){
  tagList(
    fluidRow(
    column(width=1,
           download_bt(NS("protein",id=NULL))
    )),
    fluidRow(
    column(width=12,
      plotOutput(outputId = choice, height = "100%") %>% withSpinner(color = "#0dc5c1",size = 0.5, proxy.height = "200px")
    ))
  )
}