# sourced by "welcome_server.R"

# panel -------------------------------------------------------------------

fn_panel_mRNA <- function(){
  tagList(
  column(
    width = 10, offset = 0,
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
  ))
}


# dataset seletct ---------------------------------------------------------

fn_mRNA_select <- function(.tcga,.gtex,.ccle){
    column(
      width = 12, offset=0,
      tabsetPanel(id = "select_mRNA",
        tabPanel("Cancer types",
          tagList(
          column(width = 10,
          checkboxGroupButtons(
          inputId = "select_mRNA_TCGA", label = "",status = "primary", selected = c('ACC','BLCA','BRCA','CESC'), 
          individual = TRUE, choices = .tcga, checkIcon = list(yes = icon("ok", lib = "glyphicon"),no = icon("remove",lib = "glyphicon"    )))),
          column(width = 1,
          shinyBS::bsButton(inputId = "select_all_mRNA_TCGA", label = "select all", class = "btn"),
          shinyBS::bsButton(inputId = "unselect_all_mRNA_TCGA", label = "unselect all", class = "btn")
          ))),
          tabPanel("Tissues",
          tagList(
          column(width = 10,
          checkboxGroupButtons(
          inputId = "select_mRNA_GTEX", label = "",status = "primary", selected = c('adipose'), 
          individual = TRUE, choices = .gtex, checkIcon = list(yes = icon("ok", lib = "glyphicon"),no = icon("remove",lib = "glyphicon")))),
          column(width = 1,
            shinyBS::bsButton(inputId = "select_all_mRNA_GTEX", label = "select all", class = "btn"),
            shinyBS::bsButton(inputId = "unselect_all_mRNA_GTEX", label = "unselect all", class = "btn")
          )  )),
          tabPanel("Cell lines",
          tagList(
          column(width = 10,
          checkboxGroupButtons(
          inputId = "select_mRNA_CCLE", label = "",status = "primary", selected = c('adrenal cortex'), 
          individual = TRUE, choices = .ccle, checkIcon = list(yes = icon("ok", lib = "glyphicon"),no = icon("remove",lib = "glyphicon")))),
          column(width = 1,
            shinyBS::bsButton(inputId = "select_all_mRNA_CCLE", label = "select all", class = "btn"),
            shinyBS::bsButton(inputId = "unselect_all_mRNA_CCLE", label = "unselect all", class = "btn")
          )  ))
    ))
}

# Gene set stat -----------------------------------------------------------
fn_mRNA_set_stat <- function(input_list_check){
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
      outputId = "download_unmatched_mRNA_set", label = NULL, class = NULL,
      valueBox(value = input_list_check$n_non_match, subtitle = "Invalid",icon = icon("line-chart"), color = "red")
    )
  )
}

# start analysis widgets --------------------------------------------------

fn_mRNA_single_result <- function(){
    column(
      width = 12,offset = 0,
      shinydashboard::tabBox(
        id = "mRNA_expr_plot", title = "", width = 12,
        tabPanel("Figure of expression",
                 column(width=2,
                        download_bt(NS("mRNA",id=NULL))
                 ),
                 column(width=12,
                  plotOutput(outputId = "expr_bubble_plot_mRNA", height = "100%", width = "100%") %>% 
                  withSpinner(color = "#0dc5c1",size = 0.5, proxy.height = "200px")
                 )
        ),
        tabPanel(
          title = "Table of expression",
          DT::dataTableOutput(outputId = "expr_dt_comparison_mRNA") %>% withSpinner(color = "#0dc5c1",size = 0.5, proxy.height = "200px")
        )
      )
    )
}

fn_mRNA_multi_result <- function(list){
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
    )
}

fn_plot_multiple_mRNA <- function(choice){
  tagList(
    column(width=2,
           download_bt(NS("mRNA",id=NULL))
    ),
    column(width=12,
           plotOutput(outputId = choice, height = "100%") %>% withSpinner(color = "#0dc5c1",size = 0.5, proxy.height = "200px")
    )
  )
}