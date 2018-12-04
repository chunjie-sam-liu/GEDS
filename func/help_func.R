# sourced by "help_server.R"


# load data ---------------------------------------------------------------
stat_data <- readr::read_rds(path = file.path(config$database, 'geds-data-stat.rds.gz'))

# tutorial ----------------------------------------------------------------

fn_tutorial <- function() {
  column(
    width = 10,offset = 1,
    shiny::tags$h3(
      class = "text-left",
      shiny::icon(name = "angle-double-right", class = "fa-fw"),
      "Tutorial"
    ),
    
    shiny::tags$hr(),
    
    shiny::tags$h3(shiny::icon(name = "chevron-circle-down"), "Guide", style = 'text-align: left'),
    shiny::tags$img(
      src = "./img/tutorial.jpg",
      class = "center-block img-responsive" ),
    shiny::tags$br(),
    shiny::tags$p("This figure showed the function of each button and the two steps to use GEDS in mRNA and miRNA dataset."),
    shiny::tags$img(
      src = "./img/shouye-protein.jpg",
      class = "center-block img-responsive" ),
    shiny::tags$br(),
    shiny::tags$p("This figure showed the way to use GEDS in protein dataset."),
    
    shiny::tags$h3(shiny::icon(name = "chevron-circle-down"), "Figure and table", style = 'text-align: left'),
    
    shiny::tags$h4("mRNA figure result", style = 'text-align: left'),
    shiny::tags$img(
      src = "./img/figure-mRNA.jpg",
      class = "center-block img-responsive" ),
    shiny::tags$p("This figure displayed the expression of the mRNA in the TCGA, GTEx and CCLES datasets. The boxplot showed the expression in tumor and normal cancer types from TCGA. The barplot showed the expression in normal tissues or cell lines from GTEx and CCLE."),
    
    shiny::tags$h4("miRNA figure result", style = 'text-align: left'),
    shiny::tags$img(
      src = "./img/figure-miRNA.jpg",
      class = "center-block img-responsive" ),
    shiny::tags$p("This figure displayed the expression of the miRNA in the TCGA dataset. The boxplot showed the expression in tumor and normal cancer types from TCGA."),
    
    shiny::tags$h4("Protein figure result", style = 'text-align: left'),
    shiny::tags$img(
      src = "./img/figure-protein.jpg",
      class = "center-block img-responsive" ),
    shiny::tags$p("This figure displayed the expression of the protein in the TCGA and MCLP datasets. The boxplot showed the expression in tumor and normal cancer types from TCGA. The barplot showed the expression in cell lines from MCLP."),
    
    shiny::tags$h4("Table result", style = 'text-align: left'),
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
  column(
    width = 10, offset = 1,
    shiny::tags$h3(
      class = "text-left",
      shiny::icon(name = "angle-double-right", class = "fa-fw"),
      "Document"
    ),
    shiny::tags$hr(),
    shiny::tags$p("GEDS is an integrative gene expression platform for human cancer tissues, cancer cell lines and normal tissues with mRNA level, protein level and miRNA."),
    shiny::tags$li("mRNA level expression is quantified by RNA-seq data and consists of cancer tissue, normal tissue and cancer cell line. They are obtained from",shiny::tags$a("The Cancer Genome Atlas (TCGA)", href = "https://cancergenome.nih.gov/", target = "_blank", style = "color:#008176"), ",", shiny::tags$a("Genotype-Tissue Expression (GTEx)", href = "https://gtexportal.org/home/", target = "_blank", style = "color:#008176"), "and", shiny::tags$a("Cancer Cell Line Encyclopedia (CCLE)", href = "https://portals.broadinstitute.org/ccle", target = "_blank", style = "color:#008176"), "."),
    shiny::tags$li("Protein level expression is quantified by reverse phase protein array (RPPA). It includes the cancer related ~200 protein and corresponding phosphorylated status. The data are from", shiny::tags$a("The Cancer Proteome Atlas (TCPA)", href = "https://tcpaportal.org/tcpa/", target = "_blank", style = "color:#008176"), "and", shiny::tags$a("MD Anderson Cell Lines Project (MCLP)", href = "https://tcpaportal.org/mclp/#/", target = "_blank", style = "color:#008176"), "."),
    shiny::tags$li("miRNA expression is collected from", shiny::tags$a("TCGA", href = "https://cancergenome.nih.gov/", target = "_blank", style = "color:#008176"), "with tumor and normal tissue in several cancer types."),
    shiny::tags$br(),
    shiny::tags$p("Quick search and visualization of specific gene expression in multiple cancer types or normal tissues can help researchers to focus on the certain cancer type, tissue or cell line. Then its protein level expression or phosphorylated protein expression can be examined in the cancer type or cell line. Besides, the miRNA expression can be explored to compare the tumor and normal expression. In a word, GEDS is an open access and intuitive web-based platform for searching, visualizing the mRNA, protein and miRNA expression in cancer tissue, cancer cell line and normal tissue. It is helpful for invetigator without bioinformatics skills."),
    shiny::tags$br(),
    shiny::tags$h4(
      shiny::icon(name = "angle-right", class = "fa-fw"),
      "Cancer types (TCGA)"),
    shiny::tags$img(
      src = "./img/polar.png",
      class = "center-block img-responsive" ),
    shiny::tags$br(),
    shiny::tags$dl(
      class = "dl-vertical",
      shiny::tags$p("TCGA Sample Statistics"),
      shiny::tags$dd(
        shiny::fluidRow(
          shiny::column(
            width = 12, offset = 0,
            DT::dataTableOutput(outputId = 'tcga_data_table') %>% 
              withSpinner(color = "#2196f3",size = 0.5, proxy.height = "200px")
          )
        )
      )),
    shiny::tags$h4(
      shiny::icon(name = "angle-right", class = "fa-fw"),
      "Normal Tissues data (GTEx)"),
    shiny::tags$img(
      src = "./img/humanbody.png",
      class = "center-block img-responsive" ),
    shiny::tags$br(),
    shiny::tags$h4(
      shiny::icon(name = "angle-right", class = "fa-fw"),
      "Cell lines data (CCLE and MCLP)"),
    shiny::tags$img(
      src = "./img/cellline.png",
      class = "center-block img-responsive" ),
    shiny::tags$br(),
    shiny::tags$dl(
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

