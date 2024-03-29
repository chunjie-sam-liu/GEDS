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
      class = "center-block img-responsive"),
    shiny::tags$br(),
    shiny::tags$p("This figure showed the function of each button and the two steps to use GEDS in mRNA and miRNA dataset."),
    shiny::tags$img(
      src = "./img/shouye-protein.jpg",
      class = "center-block img-responsive"),
    shiny::tags$br(),
    shiny::tags$p("This figure showed the way to use GEDS in protein dataset."),
    
    shiny::tags$h3(shiny::icon(name = "chevron-circle-down"), "Figure and table", style = 'text-align: left'),
    
    shiny::tags$h4("mRNA figure result", style = 'text-align: left'),
    shiny::tags$img(
      src = "./img/figure-mRNA.jpg",
      class = "center-block img-responsive" ),
    shiny::tags$p("This figure displayed the expression of the mRNA in the TCGA, GTEx and CCLE datasets. The boxplot showed the expression in tumor and normal cancer types from TCGA and the expression in normal tissues or cell lines from GTEx and CCLE."),
    
    shiny::tags$h4("miRNA figure result", style = 'text-align: left'),
    shiny::tags$img(
      src = "./img/figure-miRNA.jpg",
      class = "center-block img-responsive" ),
    shiny::tags$p("This figure displayed the expression of the miRNA in the TCGA dataset. The boxplot showed the expression in tumor and normal cancer types from TCGA."),
    
    shiny::tags$h4("Protein figure result", style = 'text-align: left'),
    shiny::tags$img(
      src = "./img/figure-protein.jpg",
      class = "center-block img-responsive" ),
    shiny::tags$p("This figure displayed the expression of the protein in the TCGA, MCLP and CCLE datasets. The boxplot showed the expression in tumor cancer types from TCGA and the expression in cell lines from MCLP and CCLE."),
    
    shiny::tags$h4("Sample detail figure result", style = 'text-align: left'),
    shiny::tags$img(
      src = "./img/click_plot.jpg",
      class = "center-block img-responsive" ),
    shiny::tags$p("This figure displayed the detail expression of dataset in cancer type/tissue/cell line. User could hover over the dot to view the detail expression of each sample."),
    
    shiny::tags$h4("Table result", style = 'text-align: left'),
    shiny::tags$img(
      src = "./img/table.jpg",
      class = "center-block img-responsive" ),
    shiny::tags$p("This table cantained the detail information and the mean expression about the input list. User could click the arrow besides column names to change the rank or input on the line to filter and click the download button to download mean expression data of one or all genes. ")
  )
}

# document ----------------------------------------------------------------

help_data_table <- function(source) {
  d <- stat_data[[source]]
  cap <- c(
    'tcga_sample_stat' = '',
    'gtex_mrna_stat' = 'GTEx mRNA Stat.',
    'ccle_mrna_stat' = 'CCLE mRNA Stat.',
    'mclp_protein_stat' = 'MCLP Protein Stat.',
    'ccle_protein_stat' = 'CCLE Protein Stat.'
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
    # Basic description of GEDS.
    shiny::tags$h3(
      class = "text-left",
      shiny::icon(name = "angle-double-right", class = "fa-fw"),
      "Document"
    ),
    shiny::tags$hr(),
    shiny::tags$h4(
      shiny::icon(name = "angle-right", class = "fa-fw"),"Funtion"),
    shiny::tags$li("Choosing specific cell lines for further study according to preliminary experiment."),
    shiny::tags$li("Analyzing gene expression from RNA level and protein level as well as phosphorylated protein across multiple tissues and cell lines."),
    shiny::tags$li("Visualizing miRNA expression between tumors and normal tissues."),
    shiny::tags$br(),
    shiny::tags$h4(
      shiny::icon(name = "angle-right", class = "fa-fw"),"Overview"),
    shiny::tags$p("GEDS is an integrative gene expression platform for human cancer tissues, cancer cell lines and normal tissues with mRNA level, protein level and miRNA level."),
    shiny::tags$li("mRNA level expression is quantified by RNA-seq data of different cancer tissues, normal tissues and cancer cell lines. Data from",shiny::tags$a("The Cancer Genome Atlas (TCGA)", href = "https://cancergenome.nih.gov/", target = "_blank", style = "color:#008176")," are normalized by ",shiny::tags$a("RNA-Seq by Expectation Maximization (RSEM)", href = "https://bmcbioinformatics.biomedcentral.com/articles/10.1186/1471-2105-12-323", target = "_blank", style = "color:#008176"), ". Data from",shiny::tags$a("Genotype-Tissue Expression (GTEx)", href = "https://gtexportal.org/home/", target = "_blank", style = "color:#008176")," are normalized by Transcripts Per Million (TPM)", ". Data from", shiny::tags$a("Cancer Cell Line Encyclopedia (CCLE)", href = "https://portals.broadinstitute.org/ccle", target = "_blank", style = "color:#008176"),"are normalized by",shiny::tags$a("Fragments Per Kilobase of transcript per Million fragments mapped (FPKM)", href = "https://www.nature.com/articles/nbt.1621", target = "_blank", style = "color:#008176"),"."),
    shiny::tags$li("Protein level expression is quantified by reverse phase protein array (RPPA). It includes the cancer related ~300 protein and corresponding phosphorylated status. The data are from", shiny::tags$a("The Cancer Proteome Atlas (TCPA)", href = "https://tcpaportal.org/tcpa/", target = "_blank", style = "color:#008176"), "," , shiny::tags$a("MD Anderson Cell Lines Project (MCLP)", href = "https://tcpaportal.org/mclp/#/", target = "_blank", style = "color:#008176"), "and", shiny::tags$a("Cancer Cell Line Encyclopedia (CCLE)", href = "https://portals.broadinstitute.org/ccle", target = "_blank", style = "color:#008176") , "."),
    shiny::tags$li("miRNA expression is collected from", shiny::tags$a("TCGA", href = "https://cancergenome.nih.gov/", target = "_blank", style = "color:#008176"), "with tumor and normal tissue in several cancer types, and normalized by ",shiny::tags$a("RSEM (RNA-Seq by Expectation Maximization)", href = "https://bmcbioinformatics.biomedcentral.com/articles/10.1186/1471-2105-12-323", target = "_blank", style = "color:#008176"),"."),
    shiny::tags$br(),
    shiny::tags$p("Quick search and visualization of specific gene expression in multiple cancer types or normal tissues can help researchers to focus on the certain cancer type, tissue or cell line. Then its protein level expression or phosphorylated protein expression can be examined in the cancer type or cell line. Besides, the miRNA expression can be explored to compare the tumor and normal expression. These multi-source and multi-platform expression data integrated in GEDS are normalized and removed batch effect across tissue and cell lines by its provider. Therefore the expression level can’t be compared its value directly between tissue and cell lines, but we can arrange tissues according to the expression level and see rank in tissues and cell lines. In a word, GEDS is an open access and intuitive web-based platform for searching, visualizing the mRNA, protein and miRNA expression in cancer tissue, cancer cell line and normal tissue. It is helpful for investigator without bioinformatics skills."),
    shiny::tags$br(),
    # TCGA data
    shiny::tags$h4(
      shiny::icon(name = "angle-right", class = "fa-fw"),"Cancer types (TCGA)"),
    shiny::tags$img(
      src = "./img/polar_low.png",
      class = "center-block img-responsive" ),
    shiny::tags$p("TCGA data summary. Numbers showed the sample number of each cancer types in different groups.",align = "center"),
    shiny::tags$br(),
    #shiny::tags$dl(
      #class = "dl-vertical",
      #shiny::tags$p("TCGA Sample Statistics"),
      #shiny::tags$dd(
      #  shiny::fluidRow(
      #    shiny::column(
      #      width = 12, offset = 0,
      #      DT::dataTableOutput(outputId = 'tcga_data_table') %>% 
      #       withSpinner(color = "#2196f3",size = 0.5, proxy.height = "200px")
      #    )
      #  )
      #)),
    # GTEx data
    shiny::tags$h4(
      shiny::icon(name = "angle-right", class = "fa-fw"),
      "Normal Tissues data (GTEx)"),
    shiny::tags$img(
      src = "./img/humanbody_low.png",
      class = "center-block img-responsive" ),
    shiny::tags$br(),
    shiny::tags$p("GTEx data summary. Numbers showed the sample number of each tissues in GTEx.",align = "center"),
    shiny::tags$br(),
    # Cell line
    shiny::tags$h4(
      shiny::icon(name = "angle-right", class = "fa-fw"),
      "Cell lines data (CCLE and MCLP)"),
    shiny::tags$img(
      src = "./img/cellline_low.png",
      class = "center-block img-responsive" ),
    shiny::tags$br(),
    shiny::tags$p("CCLE data summary. Numbers showed the sample number of each cell lines in different group of CCLE.",align = "center")#,
    #shiny::tags$br(),
    #shiny::tags$dl(
    #  shiny::tags$dd(
    #    shiny::fluidRow(
    #      shiny::column(
    #        width = 6, offset = 0,
    #        DT::dataTableOutput(outputId = 'gtex_data_table') %>% 
    #          withSpinner(color = "#2196f3",size = 0.5, proxy.height = "200px")
    #      ),
    #      shiny::column(
    #        width = 6, offset = 0,
    #        DT::dataTableOutput(outputId = 'ccle_data_table') %>% 
    #          withSpinner(color = "#2196f3",size = 0.5, proxy.height = "200px")
    #      )
    #      ),
    #   shiny::fluidRow(
    #    shiny::column(
    #        width = 6, offset = 0,
    #        DT::dataTableOutput(outputId = 'mclp_data_table') %>% 
    #          withSpinner(color = "#2196f3",size = 0.5, proxy.height = "200px")
    #      ),
    #    shiny::column(
    #      width = 6, offset = 0,
    #      DT::dataTableOutput(outputId = 'ccle_protein_data_table') %>% 
    #        withSpinner(color = "#2196f3",size = 0.5, proxy.height = "200px")
    #    )
    #    )
    #  )
    #)
  )
}

