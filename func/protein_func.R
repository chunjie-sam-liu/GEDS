# sourced by "welcome_server.R"

# panel -------------------------------------------------------------------

fn_panel_protein <- function(){
  tagList(
  column(
    width = 11, offset = 0,
    shinyWidgets::searchInput(
      inputId = "input_protein_set",
      label = "",
      placeholder = 'Please input protein set separated by space or " , "or " ; "',
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
          shinyBS::bsButton(inputId = "protein_example", label = "Example", icon = icon(name = "check"))
        )
      )
    )
  ))
}


# dataset select ----------------------------------------------------------

fn_protein_select <- function(.tcga,.mclp){
    column(
      width = 12, offset = 0,
      tabsetPanel(id = "select_protein",
        tabPanel("Cancer Types",
          tagList(
          column(width = 11,
          checkboxGroupButtons(
          inputId = "select_protein_TCGA", label = "",status = "primary", selected = c('ACC','BLCA','BRCA','CESC'), 
          individual = TRUE, choices = .tcga, checkIcon = list(yes = icon("ok", lib = "glyphicon"),no = icon("remove",lib = "glyphicon")))),
          column(width = 1,
            shinyBS::bsButton(inputId = "select_all_protein_TCGA", label = "Select all", class = "btn"),
            shinyBS::bsButton(inputId = "unselect_all_protein_TCGA", label = "Unselect all", class = "btn")
                 ) )),
        tabPanel("Normal Tissues",
          tagList(
          column(width = 11,
          checkboxGroupButtons(
          inputId = "select_protein_MCLP", label = "",status = "primary", selected = c('bladder'), 
          individual = TRUE, choices = .mclp, checkIcon = list(yes = icon("ok", lib = "glyphicon"),no = icon("remove",lib = "glyphicon")))),
          column(width = 1,
            shinyBS::bsButton(inputId = "select_all_protein_MCLP", label = "Select all", class = "btn"),
            shinyBS::bsButton(inputId = "unselect_all_protein_MCLP", label = "Unselect all", class = "btn")
            ) ))
        )
      )
}

fn_protein_set_stat <- function(input_list_check){
  column(
    width = 10, offset = 1,
    downloadLink(
      outputId = "download_total_protein_set", label = NULL, class = NULL,
      valueBox(value = input_list_check$n_total, subtitle = "Total Input", icon = icon("users"), color = "yellow")
    ),
    downloadLink(
      outputId = "download_valid_protein_set", label = NULL, class = NULL,
      valueBox(value = input_list_check$n_match, subtitle = "Valid", icon = icon("credit-card"),color = "green")
    ),
    downloadLink(
      outputId = "download_protein_input_logs", label = NULL, class = NULL,
      valueBox(value = input_list_check$n_non_match, subtitle = "Invalid",icon = icon("line-chart"), color = "red")
    )
  )
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
              radioGroupButtons(
                inputId = "select_protein_result", label = "", status = "primary", size = "lg",
                individual = TRUE, choices = list
              )),
            column(
              width = 12, offset = 0,
              shiny::uiOutput(outputId = "plot_multiple_protein")
            ))
        ),
        tabPanel(
          title = "Table of expression",
          DT::dataTableOutput(outputId = "expr_dt_comparison_protein") %>% withSpinner(color = "#0dc5c1",size = 0.5, proxy.height = "200px")
        ))
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