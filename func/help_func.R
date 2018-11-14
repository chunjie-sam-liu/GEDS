# sourced by "help_server.R"


# load data ---------------------------------------------------------------
stat_data <- readr::read_rds(path = file.path(config$database, 'geds-data-stat.rds.gz'))

# tutorial ----------------------------------------------------------------

fn_tutorial <- function() {
  shiny::tagList(
    shiny::tags$h3(shiny::icon(name = "angle-down"), "Tutorial"),
    
    shiny::tags$img(
      src = "./img/tutorial.jpg",
      class = "center-block img-responsive" ),
    shiny::tags$br(),
    
    shiny::tags$h3(shiny::icon(name = "angle-down"), "Figure and table"),
    
    shiny::tags$img(
      src = "./img/figure.jpg",
      class = "center-block img-responsive" ),
    shiny::tags$p("This figure displayed the expression of the input list in the dataset user selected."),
    shiny::tags$img(
      src = "./img/table.jpg",
      class = "center-block img-responsive" ),
    shiny::tags$p("This table cantained the detail information and the average expression about the input list. User could click the arrow besides column names to change the rank or input on the line to filter. ")
  )
}

# document ----------------------------------------------------------------

help_data_table <- function(source) {
  d <- stat_data[[source]]
  cap <- c(
    'tcga_sample_stat' = '',
    'gtex_mrna_stat' = 'GTEx mRNA Stat.',
    'ccle_mrna_stat' = 'CCLE mRNA Stat.',
    'mclp_protein_stat' = 'MCLP Protein Stat.'
  )
  DT::datatable(
    data = d,
    options = list(
      info = FALSE,
      paging = FALSE,
      searching = FALSE,
      autoWidth = TRUE,
      ordering = FALSE
    ),
    rownames = FALSE,
    colnames = names(d),
    filter = "none",
    style = "bootstrap",
    class = "table-bordered table-condensed",
    caption = shiny::tags$caption(cap[source], style = 'color:black')
  )
}

fn_document <- function() {
  shiny::tagList(
    shiny::tags$p("GEDS is an integrative expression platform for gene mRNA, miRNA expression and protein RPPA expression. The all expression data is from TCGA cancer types, GTEx normal tissues and CCLE cancer cell lines."),
    shiny::tags$dl(
      class = "dl-vertical",
      shiny::tags$dt("TCGA Sample Statistics"),
      shiny::tags$dd(
        shiny::fluidRow(
          shiny::column(
            width = 12, offset = 0,
            DT::dataTableOutput(outputId = 'tcga_data_table') %>% 
              withSpinner(color = "#2196f3",size = 0.5, proxy.height = "200px")
          )
        )
      ),
      shiny::tags$dt("GTEx and Cell Line Statistics"),
      shiny::tags$dd(
        shiny::fluidRow(
          shiny::column(
            width = 4, offset = 0,
            DT::dataTableOutput(outputId = 'gtex_data_table') %>% 
              withSpinner(color = "#2196f3",size = 0.5, proxy.height = "200px")
          ),
          shiny::column(
            width = 4, offset = 0,
            DT::dataTableOutput(outputId = 'ccle_data_table') %>% 
              withSpinner(color = "#2196f3",size = 0.5, proxy.height = "200px")
          ),
        shiny::column(
            width = 4, offset = 0,
            DT::dataTableOutput(outputId = 'mclp_data_table') %>% 
              withSpinner(color = "#2196f3",size = 0.5, proxy.height = "200px")
          )
        )
      )
    )
  )
}

# help content ------------------------------------------------------------

fn_help_content <- function(){
  column(
    width = 10, offset = 1, aling = 'left',
    
    shiny::tags$h3(
      class = "text-left",
      shiny::icon(name = "angle-double-right", class = "fa-fw"),
      "Tutorial and Documentation"
    ),
    
    shiny::tags$hr(),
    
    shiny::tags$div(
      
      # nav tabs
      shiny::tags$ul(
        class = "nav nav-tabs", role = "tablist",
        shiny::tags$li(
          role = "presentation", class = "active",
          shiny::tags$a(
            href = "#ui_tutorial", "aria-controls" = "ui_tutorial", 
            role = "tab", "data-toggle" = "tab", "Tutorial"
          )
        ),
        shiny::tags$li(
          role = "presentation",
          shiny::tags$a(
            href = "#ui_document", "aria-controls" = "ui_document", 
            role = "tab", "data-toggle" = "tab", "Document"
          )
        )
      ),
      
      # tab panes
      shiny::tags$div(
        class = "tab-content",
        shiny::tags$div(
          role = "tabpanel", class = "tab-pane active", id = "ui_tutorial",
          fn_tutorial()
        ),
        shiny::tags$div(
          role = "tabpanel", class = "tab-pane", id = "ui_document", 
          fn_document()
        )
      )
    )
  )
}
