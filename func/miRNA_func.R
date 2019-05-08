# sourced by "welcome_server.R"

# panel -------------------------------------------------------------------

fn_panel_miRNA <- function(){
  tagList(
  column(
    width = 11, offset = 0,
    shinyWidgets::searchInput(
      inputId = "input_miRNA_set",
      label = "",
      placeholder = 'Please input miRNA set separated by space or " , "or " ; "',
      btnSearch = icon("search"),
      btnReset = icon("remove"),
      width = "100%"
    ),
    shiny::uiOutput(outputId = "ui_miRNA_stat")
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
          shinyBS::bsButton(inputId = "miRNA_example", label = "Example", icon = icon(name = "check"))
        )
      )
    )
  )
  )
}

# dataset seletct ---------------------------------------------------------

fn_miRNA_select <- function(.miRNA){
    column(
      width = 12, offset = 0,
      tabsetPanel(
        tabPanel("Cancer Types",
          tagList(
          column(width = 11,
          checkboxGroupButtons(
          inputId = "select_miRNA_TCGA", label = "", status = "primary",selected = c('ACC','BLCA','BRCA','CESC'),
          individual = TRUE, choices = .miRNA,checkIcon = list(yes = icon("ok", lib = "glyphicon"),no = icon("remove",lib = "glyphicon")))),
          column(width = 1,
            shinyBS::bsButton(inputId = "select_all_miRNA_TCGA", label = "Select all", class = "btn"),
            shinyBS::bsButton(inputId = "unselect_all_miRNA_TCGA", label = "Unselect all", class = "btn")
          ))
       )))
}

fn_miRNA_set_stat <- function(){
  column(
    width = 10, offset = 1,
    verbatimTextOutput("miRNA_invalid")
  )
}
# result ------------------------------------------------------------------

fn_mirna_single_result <- function(){
  column(
    width = 12,offset = 0,
    shinydashboard::tabBox(
      id = "expr_plot", title = "", width = 12,
      tabPanel("Figure of expression",
               fluidRow(
               column(width=1,
                      download_bt(NS("miRNA",id=NULL))
               )),
               fluidRow(
               column(width=12,
                plotOutput(outputId = "expr_bubble_plot_mirna", height = "100%", width = "100%") %>% 
                withSpinner(color = "#0dc5c1",size = 0.5, proxy.height = "200px")
               ))
          ),
      tabPanel(
        title = "Table of expression",
        DT::dataTableOutput(outputId = "expr_dt_comparison_mirna") %>% withSpinner(color = "#0dc5c1",size = 0.5, proxy.height = "200px"))
    ))
}

fn_mirna_multi_result <- function(list){
    tagList(
      column(
        width = 12, offset = 0,
        style = 'color: #2196f3',
        shiny::tags$p("Tips: Click button to view result of other miRNAs.")
      ),
      column(
        width = 12, offset = 0,
        style = 'margin-top: -35px',
        radioGroupButtons(
          inputId = "select_miRNA_result", label = "", status = "primary", size = "lg",
          individual = TRUE, choices = list
        )
      ),
      column(
        width = 12, offset = 0,
        shinydashboard::tabBox(
          id = "mutiple_miRNA_plot_result", title = "", width = 12,
          tabPanel(
            title = "Figure of expression",
              column(
                width = 12, offset = 0,
                shiny::uiOutput(outputId = "plot_multiple_miRNA")
              )
          ),
          tabPanel(
            title = "Table of expression",
            column(
              width = 12, offset = 0,
              shiny::uiOutput(outputId = "table_multiple_miRNA")
            )
          )
        )
      )
    )
}

fn_plot_multiple_miRNA <- function(choice){
  tagList(
    fluidRow(
      style = 'margin-top: 20px',
      column(width = 12,
           shiny::tags$p(shiny::tags$a("Tips: Click this tip to view full name of cancer types in document.", id = "detail"))
      )
    ),
    fluidRow(
      column(width=12,
             plotlyOutput(outputId = choice, height = "400px",inline=TRUE)
      ),
      bsModal('boxPopUp', '', '', plotlyOutput("hover"))
    )
  )
}

fn_table_multiple_miRNA <- function(table,download){
  fluidRow(
    column(
      width = 12,
      style = 'margin-top: 20px; color: #2196f3',
      shiny::tags$p("Tips: 1. Click the download button to download data in CSV format. 2. Type letters behind search to filter table. 3. Click the arrow in table header to sort the table.")
    ),
    column(
      width = 12,
      downloadButton(download, "Download TCGA data of this miRNA (CSV)", style = 'font-size: 20px'),
      downloadButton("expr_dt_comparison_TCGA_mirna", "Download TCGA data of all miRNA input (CSV)", style = 'font-size: 20px')
    ),
    column(
      width = 12,
      DT::dataTableOutput(outputId = table) %>% withSpinner(color = "#0dc5c1",size = 0.5, proxy.height = "200px")
    )
  )
}