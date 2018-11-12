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

fn_miRNA_set_stat <- function(input_list_check){
  column(
    width = 10, offset = 1,
    downloadLink(
      outputId = "download_total_miRNA_set", label = NULL, class = NULL,
      valueBox(value = input_list_check$n_total, subtitle = "Total Input", icon = icon("users"), color = "yellow")
    ),
    
    downloadLink(
      outputId = "download_valid_miRNA_set", label = NULL, class = NULL,
      valueBox(value = input_list_check$n_match, subtitle = "Valid", icon = icon("credit-card"),color = "green")
    ),
    downloadLink(
      outputId = "download_miRNA_input_logs", label = NULL, class = NULL,
      valueBox(value = input_list_check$n_non_match, subtitle = "Invalid",icon = icon("line-chart"), color = "red")
    )
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
    column(
      width = 12, offset = 0,
      shinydashboard::tabBox(
        id = "mutiple_miRNA_plot_result", title = "", width = 12,
        tabPanel(
          title = "Figure of expression",
          tagList(
            column(
              width = 12, offset = 0,
              radioGroupButtons(
                inputId = "select_miRNA_result", label = "", status = "primary", size = "lg",
                individual = TRUE, choices = list
              )),
            column(
              width = 12, offset = 0,
              shiny::uiOutput(outputId = "plot_multiple_miRNA")
            ))
        ),
        tabPanel(
          title = "Table of expression",
          DT::dataTableOutput(outputId = "expr_dt_comparison_mirna") %>% withSpinner(color = "#0dc5c1",size = 0.5, proxy.height = "200px")
        ))
      )
}

fn_plot_multiple_miRNA <- function(choice){
  tagList(
    fluidRow(
    column(width=1,
           download_bt(NS("miRNA",id=NULL))
    )),
    fluidRow(
    column(width=12,
      plotOutput(outputId = choice, height = "100%") %>% withSpinner(color = "#0dc5c1",size = 0.5, proxy.height = "200px")
    )))
}