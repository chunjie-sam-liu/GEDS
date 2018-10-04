# sourced by "welcome_server.R"

# panel -------------------------------------------------------------------

fn_panel_protein <- function(){
  shiny::fluidRow(
  tagList(
  column(
    width = 10, offset = 0,
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
  )
  ))
}


# dataset select ----------------------------------------------------------

fn_protein_select <- function(.protein){
  shiny::fluidRow(
    column(
      width = 12, offset = 0,
      shinydashboard::box(
        width = 12,
        checkboxGroupButtons(
          inputId = "select_protein_TCGA", label = "",status = "primary", size = "lg", selected = c('ACC','BLCA','BRCA','CESC'), 
          individual = TRUE, choices = .protein
        ),
        shinyjs::hide(switchInput(
          inputId = "select_dataset6", label = "Dataset", value = FALSE,
          onLabel = "All", offLabel = "None", size = "large", offStatus = "danger"
        )))
      )
    )
}

fn_protein_set_stat <- function(input_list_check){
  shiny::fluidRow(
  column(
    width = 10, offset = 1, style = "margin-top:20px",
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
  ))
}

# result ------------------------------------------------------------------

fn_result <- function(id){
  shiny::fluidRow(
  column(
    width = 12,offset = 0,
    shinydashboard::tabBox(
      id = "expr_plot", title = "", width = 12,
      tabPanel(
        title = "Figure of expression",
        plotOutput(outputId = "expr_bubble_plot", height = "100%") %>% withSpinner(color = "#0dc5c1",size = 0.5, proxy.height = "200px")
      ),
      tabPanel(
        title = "Table of expression",
        DT::dataTableOutput(outputId = "expr_dt_comparison") %>% withSpinner(color = "#0dc5c1",size = 0.5, proxy.height = "200px")
      )
    )
  ))
}
