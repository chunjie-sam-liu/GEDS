# sourced by "welcome_server.R"

# panel -------------------------------------------------------------------

fn_panel_mRNA <- function(){
  tagList(
    column(
      width = 11, offset = 0,
      shinyWidgets::searchInput(
        inputId = "input_mRNA_set",
        label = "",
        placeholder = 'Please input a set of gene official symbols or aliases separated by space or "," or ";".',
        btnSearch = icon("search"),
        btnReset = icon("remove"),
        width = "100%"
      ),
      shiny::uiOutput(outputId = "ui_mRNA_stat")
    ),
    column(
      width = 1,
      shiny::tags$div(
        class = "form-group shiny-input-container",
        shiny::tags$label("for" = "margin"),
        shiny::tags$div(
          class = "input-group search-text",
          shiny::tags$span(
            class = "input-group-btn",
            shinyBS::bsButton(inputId = "mRNA_example", label = "Example", icon = icon(name = "check"))
          )
        )
      )
    )
  )
}


# Gene set stat -----------------------------------------------------------
fn_mRNA_set_stat <- function(){
  column(
    width = 12, offset = 0, 
    verbatimTextOutput("mRNA_invalid")
  )
}

# start analysis widgets --------------------------------------------------

fn_mRNA_single_result <- function(){
    column(
      width = 12,offset = 0,
      shinydashboard::tabBox(
        id = "mRNA_expr_plot", title = "", width = 12,
        tabPanel("Figure of expression",
                 fluidRow(
                 column(width=1,
                        download_bt(NS("mRNA",id=NULL))
                 )),
                 fluidRow(
                 column(width=12,
                  plotOutput(outputId = "expr_bubble_plot_mRNA", height = "100%", width = "100%") %>% 
                  withSpinner(color = "#0dc5c1",size = 0.5, proxy.height = "200px")
                 ))
        ),
        tabPanel(
          title = "Table of expression",
          DT::dataTableOutput(outputId = "expr_dt_comparison_mRNA") %>% withSpinner(color = "#0dc5c1",size = 0.5, proxy.height = "200px")
        )
      )
    )
}

fn_mRNA_multi_result <- function(list){
    tagList(
      column(
        width = 12, offset = 0,
        style = 'color: #2196f3',
        shiny::tags$p("Tips: Click button to view result of other mRNAs.")
      ),
      column(
        width = 12, offset = 0,
        style = 'margin-top: -35px',
        radioGroupButtons(
          inputId = "select_mRNA_result", label = "", status = "primary", size = "lg",
          individual = TRUE, choices = list
        )
      ),
      column(
        width = 12, offset = 0,
        shinydashboard::tabBox(
          id = "mutiple_mRNA_plot_result", title = "", width = 12,
          tabPanel(
            title = "Figure of expression",
              column(
                width = 12, offset = 0,
                shiny::uiOutput(outputId = "plot_multiple_mRNA")
              )
          ),
          tabPanel(
            title = "Table of expression",
            column(
              width = 12, offset = 0,
              shiny::uiOutput(outputId = "table_multiple_mRNA")
            )
          )
        )  
      )
    )
}

fn_plot_multiple_mRNA <- function(choice){
  tagList(
    fluidRow(
    style = 'margin-top: 20px',
    column(width = 2,
           radioGroupButtons(
             inputId = "mRNA_log",#width = "50%" ,
             label = "Scale", size = "sm",
             choices = c("Log", "Linear"),
             checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                              no = icon("remove",lib = "glyphicon"))
           )
    ),
    column(width = 10,
           shiny::tags$p(shiny::tags$a("Tips: Click this tip to view full name of cancer types in document.", id = "detail2"))
           )
    ),
    fluidRow(
    column(width=12,
           #plotlyOutput(outputId = choice, height = "1200px",inline=TRUE)
           shiny::uiOutput(outputId = "plot_result_mRNA")
    ),
    bsModal('mRNA_boxPopUp_TCGA', '', '', plotlyOutput("mRNA_hover_TCGA")),
    bsModal('mRNA_boxPopUp_GTEX', '', '', plotlyOutput("mRNA_hover_GTEX")),
    bsModal('mRNA_boxPopUp_CCLE', '', '', plotlyOutput("mRNA_hover_CCLE"))
    )
  )
}

fn_plot_result_mRNA1 <- function(){
  tagList(
    fluidRow(
      column(width=12,
             plotlyOutput(outputId = "mRNA_TCGA", height = "400px",inline=TRUE),
             shiny::tags$hr(),
             plotlyOutput(outputId = "mRNA_GTEX", height = "400px",inline=TRUE),
             shiny::tags$hr(),
             plotlyOutput(outputId = "mRNA_CCLE", height = "400px",inline=TRUE)
      )
    )
  )
}
fn_plot_result_mRNA2 <- function(){
  tagList(
    fluidRow(
      column(width=12,
             plotlyOutput(outputId = "mRNA_TCGA", height = "400px",inline=TRUE),
             shiny::tags$hr(),
             plotlyOutput(outputId = "mRNA_GTEX", height = "400px",inline=TRUE)
      )
    )
  )
}
fn_plot_result_mRNA3 <- function(){
  tagList(
    fluidRow(
      column(width=12,
             plotlyOutput(outputId = "mRNA_GTEX", height = "400px",inline=TRUE),
             shiny::tags$hr(),
             plotlyOutput(outputId = "mRNA_CCLE", height = "400px",inline=TRUE)
      )
    )
  )
}
fn_plot_result_mRNA4 <- function(){
  tagList(
    fluidRow(
      column(width=12,
             plotlyOutput(outputId = "mRNA_TCGA", height = "400px",inline=TRUE),
             shiny::tags$hr(),
             plotlyOutput(outputId = "mRNA_CCLE", height = "400px",inline=TRUE)
      )
    )
  )
}
fn_plot_result_mRNA5 <- function(){
  tagList(
    fluidRow(
      column(width=12,
             plotlyOutput(outputId = "mRNA_TCGA", height = "400px",inline=TRUE)
      )
    )
  )
}
fn_plot_result_mRNA6 <- function(){
  tagList(
    fluidRow(
      column(width=12,
             plotlyOutput(outputId = "GTEX", height = "400px",inline=TRUE)
      )
    )
  )
}
fn_plot_result_mRNA7 <- function(){
  tagList(
    fluidRow(
      column(width=12,
             plotlyOutput(outputId = "CCLE", height = "400px",inline=TRUE)
      )
    )
  )
}

fn_table_multiple_mRNA <- function(tcga,gtex,ccle,download_tcga,download_gtex,download_ccle){
  fluidRow(
    column(
      width = 12,
      style = 'margin-top: 20px;color: #2196f3',
      shiny::tags$p("Tips: 1. Click the download button to download data in CSV format. 2. Type letters behind search to filter table. 3. Click the arrow in table header to sort the table.")
    ),
    column(
      width = 12,
      downloadButton(download_tcga, "Download TCGA data of this gene (CSV)", style = 'font-size: 20px'),
      downloadButton("expr_dt_comparison_TCGA_mRNA", "Download TCGA data of all mRNA input (CSV)", style = 'font-size: 20px')
    ),
    column(
      width = 12,
      DT::dataTableOutput(outputId = tcga) %>% withSpinner(color = "#0dc5c1",size = 0.5, proxy.height = "200px")
    ),
    column(
      width = 12,
      downloadButton(download_gtex, "Download GTEx data of this gene (CSV)", style = 'font-size: 20px'),
      downloadButton("expr_dt_comparison_GTEX_mRNA", "Download GTEx data of all mRNA input (CSV)", style = 'font-size: 20px')
    ),
    column(
      width = 12,
      DT::dataTableOutput(outputId = gtex) %>% withSpinner(color = "#0dc5c1",size = 0.5, proxy.height = "200px")
    ),
    column(
      width = 12,
      downloadButton(download_ccle, "Download CCLE data of this gene (CSV)", style = 'font-size: 20px'),
      downloadButton("expr_dt_comparison_CCLE_mRNA", "Download CCLE data of all mRNA input (CSV)", style = 'font-size: 20px')
    ),
    column(
      width = 12,
      DT::dataTableOutput(outputId = ccle) %>% withSpinner(color = "#0dc5c1",size = 0.5, proxy.height = "200px")
    )
  )
}
