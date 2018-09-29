# sourced by "welcome_server.R"

# panel -------------------------------------------------------------------

fn_panel_miRNA <- function(){
  tagList(
  column(
    width = 10,
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
  shiny::fluidRow(
    column(
      width = 12, offset = 0,
      shinydashboard::box(
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        title="Select TCGA Cancer Types",
        checkboxGroupButtons(
          inputId = "select_miRNA_TCGA", label = "", status = "primary", size = "lg",selected = c('ACC','BLCA','BRCA','CESC'),
          individual = TRUE, choices = .miRNA
        ),
        shinyjs::hide(switchInput(
          inputId = "select_dataset7", label = "Dataset", value = FALSE,
          onLabel = "All", offLabel = "None", size = "large", offStatus = "danger"
        )))
    )
  )
}

# result ------------------------------------------------------------------

fn_mirna_result <- function(){
  column(
    width = 12,offset = 0,
    shinydashboard::tabBox(
      id = "expr_plot", title = "", width = 12,
      tabPanel(
        title = "Figure of expression",
        plotOutput(outputId = "expr_bubble_plot_mirna", height = "100%") %>% withSpinner(color = "#0dc5c1",size = 0.5, proxy.height = "200px")
      ),
      tabPanel(
        title = "Table of expression",
        DT::dataTableOutput(outputId = "expr_dt_comparison_mirna") %>% withSpinner(color = "#0dc5c1",size = 0.5, proxy.height = "200px")
      )
    )
  )
}
