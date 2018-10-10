# sourced by "welcome_server.R"

# panel -------------------------------------------------------------------

fn_panel_mRNA <- function(){
  tagList(
  column(
    width = 9, offset = 0,
    shinyWidgets::searchInput(
      inputId = "input_mRNA_set",
      label = "",
      placeholder = 'Please input HGNC symbol gene set separated by space or " , "or " ; "',
      btnSearch = icon("search"),
      btnReset = icon("remove"),
      width = "100%"
    )
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


# dataset seletct ---------------------------------------------------------

fn_mRNA_select <- function(.tcga,.gtex,.ccle){
  shiny::fluidRow(
    column(
      width = 12, offset=0,
      tabsetPanel(id = "select_mRNA",
        tabPanel("Cancer Types",
          checkboxGroupButtons(
          inputId = "select_mRNA_TCGA", label = "",status = "primary", selected = c('ACC','BLCA','BRCA','CESC'), 
          individual = TRUE, choices = .tcga, checkIcon = list(yes = icon("ok", lib = "glyphicon"),no = icon("remove",lib = "glyphicon"))
          ),
          shinyjs::hide(switchInput(
          inputId = "select_dataset1", label = "Dataset", value = FALSE,
          onLabel = "All", offLabel = "None", size = "large", offStatus = "danger"
        ))),
        tabPanel("Tissues",
          checkboxGroupButtons(
          inputId = "select_mRNA_GTEX", label = "",status = "primary", selected = c('adipose_tissue'), 
          individual = TRUE, choices = .gtex, checkIcon = list(yes = icon("ok", lib = "glyphicon"),no = icon("remove",lib = "glyphicon"))
          ),
          shinyjs::hide(switchInput(
          inputId = "select_dataset2", label = "Dataset", value = FALSE,
          onLabel = "All", offLabel = "None", size = "large", offStatus = "danger"
        ))),
        tabPanel("Celllines",
          checkboxGroupButtons(
          inputId = "select_mRNA_CCLE", label = "",status = "primary", selected = c('adrenal_cortex'), 
          individual = TRUE, choices = .ccle, checkIcon = list(yes = icon("ok", lib = "glyphicon"),no = icon("remove",lib = "glyphicon"))
          ),
          shinyjs::hide(switchInput(
          inputId = "select_dataset3", label = "Dataset", value = FALSE,
          onLabel = "All", offLabel = "None", size = "large", offStatus = "danger"
        )))
    ))
  )
}

# Gene set stat -----------------------------------------------------------
fn_mRNA_set_stat <- function(input_list_check){
  shiny::fluidRow(
  column(
    width = 10, offset = 1, 
    downloadLink(
      outputId = "download_total_mRNA_set", label = NULL, class = NULL,
      valueBox(value = input_list_check$n_total, subtitle = "Total Input", icon = icon("users"), color = "yellow")
    ),
    
    downloadLink(
      outputId = "download_valid_mRNA_set", label = NULL, class = NULL,
      valueBox(value = input_list_check$n_match, subtitle = "Valid", icon = icon("credit-card"),color = "green")
    ),
    downloadLink(
      outputId = "download_input_logs", label = NULL, class = NULL,
      valueBox(value = input_list_check$n_non_match, subtitle = "Invalid",icon = icon("line-chart"), color = "red")
    )
  ))
}

# start analysis widgets --------------------------------------------------
fn_start_analysis <- function(){
  shiny::fluidRow(
  column(
    width = 8, offset = 2, 
    shinyBS::bsButton(inputId = "analysis", label = "Start Gene Set Analysis", icon = icon("play"), class = "btn-lg")
  ))
}

fn_mRNA_single_result <- function(){
  shiny::fluidRow(
    column(
      width = 12,offset = 0,
      shinydashboard::tabBox(
        id = "mRNA_expr_plot", title = "", width = 12,
        tabPanel("Figure of expression",
                 plotOutput(outputId = "expr_bubble_plot_mRNA", height = "100%", width = "100%") %>% 
                   withSpinner(color = "#0dc5c1",size = 0.5, proxy.height = "200px")
        ),
        tabPanel(
          title = "Table of expression",
          DT::dataTableOutput(outputId = "expr_dt_comparison_mRNA") %>% withSpinner(color = "#0dc5c1",size = 0.5, proxy.height = "200px")
        )
      )
    ))
}

fn_mRNA_multi_result <- function(list){
  shiny::fluidRow(
    column(
      width = 12, offset = 0,
      shinydashboard::tabBox(
        id = "mutiple_mRNA_plot_result", title = "", width = 12,
        tabPanel(
          title = "Figure of expression",
          tagList(
            column(
              width = 12, offset = 0,
              radioGroupButtons(
                inputId = "select_mRNA_result", label = "", status = "primary", size = "lg",
                individual = TRUE, choices = list
              )),
            column(
              width = 12, offset = 0,
              shiny::uiOutput(outputId = "plot_multiple_mRNA")
            ))
        ),
        tabPanel(
          title = "Table of expression",
          DT::dataTableOutput(outputId = "expr_dt_comparison_mRNA") %>% withSpinner(color = "#0dc5c1",size = 0.5, proxy.height = "200px")
        ))
    ))
}

fn_plot_multiple_mRNA <- function(choice){
  plotOutput(outputId = choice, height = "100%") %>% withSpinner(color = "#0dc5c1",size = 0.5, proxy.height = "200px")
}