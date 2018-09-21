# sourced by "welcome_server.R"

# panel -------------------------------------------------------------------

fn_panel_protein <- function(){
  column(
    width = 12, offset = 0,
    shinyWidgets::searchInput(
      inputId = "input_protein_set",
      label = "",
      placeholder = 'Please input protein set separated by space or " , "or " ; "',
      btnSearch = icon("search"),
      btnReset = icon("remove"),
      width = "100%"
    )
  )
}


# dataset select ----------------------------------------------------------

fn_protein_select <- function(.protein){
  shiny::fluidRow(
    column(
      width = 12, offset = 1,
      shinydashboard::box(
        width = 10,
        status = "primary",
        solidHeader = TRUE,
        title="Select TCGA Cancer Types",
        checkboxGroupButtons(
          inputId = "select_protein_TCGA", label = "",status = "primary", size = "lg", selected = c('ESCA','LIHC','CESC','COAD'),
          choices = .protein
        ),
        shinyjs::hide(switchInput(
          inputId = "select_dataset6", label = "Dataset", value = FALSE,
          onLabel = "All", offLabel = "None", size = "large", offStatus = "danger"
        )))
    )
  )
}


# result ------------------------------------------------------------------

fn_result <- function(id){
  column(
    width = 12,offset = 0,
    shinydashboard::tabBox(
      id = "expr_plot", title = "", width = 12,
      tabPanel(
        title = "Figure of expression",
        plotOutput(outputId = "expr_bubble_plot") %>% withSpinner(color = "#0dc5c1",size = 0.5, proxy.height = "200px")
      ),
      tabPanel(
        title = "Table of expression",
        DT::dataTableOutput(outputId = "expr_dt_comparison") %>% withSpinner(color = "#0dc5c1",size = 0.5, proxy.height = "200px")
      )
    )
  )
}
