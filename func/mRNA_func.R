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
          tagList(
          column(
            width = 12, offset = 0,
            DT::dataTableOutput(outputId = "expr_dt_comparison_TCGA_mRNA") %>% withSpinner(color = "#0dc5c1",size = 0.5, proxy.height = "200px")
          ),
          column(
            width = 12, offset = 0,
            DT::dataTableOutput(outputId = "expr_dt_comparison_GTEX_mRNA") %>% withSpinner(color = "#0dc5c1",size = 0.5, proxy.height = "200px")
          ),
          column(
            width = 12, offset = 0,
            DT::dataTableOutput(outputId = "expr_dt_comparison_CCLE_mRNA") %>% withSpinner(color = "#0dc5c1",size = 0.5, proxy.height = "200px")
          )
          )
        ))
    )
}

fn_plot_multiple_mRNA <- function(choice){
  tagList(
    fluidRow(
    column(width=1,
           download_bt(NS("mRNA",id=NULL))
    )),
    fluidRow(
    column(width=12,
           plotOutput(outputId = choice, height = "100%") %>% withSpinner(color = "#0dc5c1",size = 0.5, proxy.height = "200px")
    ))
  )
}