# sourced by "welcome_server.R"

# panel -------------------------------------------------------------------

fn_panel_protein_old <- function(){
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

fn_panel_protein <- function(.choice){
  column(
    width = 12, offset = 0,
    tagList(
      column(width = 10,offset = 0,
    pickerInput(
      inputId = "input_protein_set", choices = .choice$protein, width = "600px", options = list(
        `live-search` = TRUE, size = 5, title = "Select interested protein symbol")
    )),
    column(width = 2,
      shinyBS::bsButton(inputId = "protein_reset", label = "", icon = icon(name = "remove")),
      shinyBS::bsButton(inputId = "protein_search", label = "", icon = icon(name = "search"))
    )
    )
  )
}

# dataset select ----------------------------------------------------------

fn_protein_start <- function(){
    column(
      width = 12, offset = 0,
      shinyBS::bsButton(inputId = "protein_reset", label = "protein_reset", icon = icon(name = "remove")),
      shinyBS::bsButton(inputId = "protein_search", label = "protein_search", icon = icon(name = "search"))
      )
}

fn_protein_set_stat <- function(input_list_check){
  column(
    width = 10, offset = 1,
    verbatimTextOutput("protein_invalid")
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
              shiny::uiOutput(outputId = "plot_multiple_protein")
            ))
        ),
        tabPanel(
          title = "Table of expression",
          tagList(
          column(
          width = 12, offset = 0,
          DT::dataTableOutput(outputId = "expr_dt_comparison_TCGA_protein") %>% withSpinner(color = "#0dc5c1",size = 0.5, proxy.height = "200px")
          ),
          column(
            width = 12, offset = 0,
            DT::dataTableOutput(outputId = "expr_dt_comparison_MCLP_protein") %>% withSpinner(color = "#0dc5c1",size = 0.5, proxy.height = "200px")
          )
        )))
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