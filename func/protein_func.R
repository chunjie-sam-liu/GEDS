# sourced by "welcome_server.R"

# panel -------------------------------------------------------------------


fn_panel_protein <- function(.choice){
  tagList(
  column(
    width = 12, offset = 0,
    pickerInput(
      inputId = "input_protein_set", choices = .choice$protein, width = "1000px", 
      options = list( `live-search` = TRUE, size = 5, title = "Select interested antibody name"),
      choicesOpt = list(subtext = .choice$anno)
    )
  ),
  column(
    width = 10,offset = 1,
    shiny::tags$p(
      style = "font-size: 15px; color: red",
      "Protein level expression is quantified by reverse phase protein array (RPPA). It includes the cancer related ~300 protein and corresponding phosphorylated status."
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

fn_protein_TCGA <- function(){
  tagList(
    column(
      width = 12, offset = 0,
      downloadButton("expr_dt_download_TCGA_protein", "Download TCGA data of selected protein (CSV)", style = 'font-size: 20px')
    ),
    column(
      width = 12, offset = 0,
      DT::dataTableOutput(outputId = "expr_dt_comparison_TCGA_protein") %>% withSpinner(color = "#0dc5c1",size = 0.5, proxy.height = "200px")
    )
  )
}

fn_protein_MCLP <- function(){
  tagList(
    column(
      width = 12, offset = 0,
      downloadButton("expr_dt_download_MCLP_protein", "Download MCLP data of selected protein (CSV)", style = 'font-size: 20px')
    ),
    column(
      width = 12, offset = 0,
      DT::dataTableOutput(outputId = "expr_dt_comparison_MCLP_protein") %>% withSpinner(color = "#0dc5c1",size = 0.5, proxy.height = "200px")
    )
  )
}

fn_protein_CCLE <- function(){
  tagList(
    column(
      width = 12, offset = 0,
      downloadButton("expr_dt_download_CCLE_protein", "Download CCLE data of selected protein (CSV)", style = 'font-size: 20px')
    ),
    column(
      width = 12, offset = 0,
      DT::dataTableOutput(outputId = "expr_dt_comparison_CCLE_protein") %>% withSpinner(color = "#0dc5c1",size = 0.5, proxy.height = "200px")
    )
  )
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
              width = 12,
              style = 'margin-top: 20px;color: #2196f3',
              shiny::tags$p("Tips: 1. Click the download button to download data in CSV format. 2. Type letters behind search to filter table. 3. Click the arrow in table header to sort the table.")
            ),
            shiny::uiOutput(outputId = "TCGA_protein"),
            shiny::uiOutput(outputId = "MCLP_protein"),
            shiny::uiOutput(outputId = "CCLE_protein")
          )
        )
      )
    )
}

fn_plot_multiple_protein <- function(choice){
  tagList(
    fluidRow(
      column(width = 12,
           shiny::tags$p(shiny::tags$a("Tips: Click this tip to view full name of cancer types in document.", id = "detail3"))
           )
    ),
    fluidRow(
    column(width = 12,
           shiny::uiOutput(outputId = "plot_result_protein")
    ),
    bsModal('protein_boxPopUp_TCGA', '', '', plotlyOutput("protein_hover_TCGA")),
    bsModal('protein_boxPopUp_MCLP', '', '', plotlyOutput("protein_hover_MCLP")),
    bsModal('protein_boxPopUp_CCLE', '', '', plotlyOutput("protein_hover_CCLE"))
    )
  )
}

fn_plot_result_protein1 <- function(){
  tagList(
    fluidRow(
      column(width=12,
             plotlyOutput(outputId = "protein_TCGA", height = "400px",inline=TRUE),
             shiny::tags$hr(),
             plotlyOutput(outputId = "protein_MCLP", height = "400px",inline=TRUE),
             shiny::tags$hr(),
             plotlyOutput(outputId = "protein_CCLE", height = "400px",inline=TRUE)
      )
    )
  )
}
fn_plot_result_protein2 <- function(){
  tagList(
    fluidRow(
      column(width=12,
             plotlyOutput(outputId = "protein_TCGA", height = "400px",inline=TRUE),
             shiny::tags$hr(),
             plotlyOutput(outputId = "protein_MCLP", height = "400px",inline=TRUE)
      )
    )
  )
}
fn_plot_result_protein3 <- function(){
  tagList(
    fluidRow(
      column(width=12,
             plotlyOutput(outputId = "protein_TCGA", height = "400px",inline=TRUE),
             shiny::tags$hr(),
             plotlyOutput(outputId = "protein_CCLE", height = "400px",inline=TRUE)
      )
    )
  )
}
fn_plot_result_protein4 <- function(){
  tagList(
    fluidRow(
      column(width=12,
             plotlyOutput(outputId = "protein_MCLP", height = "400px",inline=TRUE),
             shiny::tags$hr(),
             plotlyOutput(outputId = "protein_CCLE", height = "400px",inline=TRUE)
      )
    )
  )
}
fn_plot_result_protein5 <- function(){
  tagList(
    fluidRow(
      column(width=12,
             plotlyOutput(outputId = "protein_TCGA", height = "400px",inline=TRUE)
      )
    )
  )
}
fn_plot_result_protein6 <- function(){
  tagList(
    fluidRow(
      column(width=12,
             plotlyOutput(outputId = "protein_MCLP", height = "400px",inline=TRUE)
      )
    )
  )
}
fn_plot_result_protein7 <- function(){
  tagList(
    fluidRow(
      column(width=12,
             plotlyOutput(outputId = "protein_CCLE", height = "400px",inline=TRUE)
      )
    )
  )
}
