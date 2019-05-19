# source by server.R
# saved as mRNA_server.R

# Check input gene set ----------------------------------------------------
check_mRNA_set <- function(.s) {
  .s %>%stringr::str_split(pattern = "[ ,;]+", simplify = TRUE) %>%.[1, ] -> .ss
  .ss
}

# Validate gene with TCGA gene symbol -------------------------------------

validate_mRNA_set <- function(.v,  .total_symbol, input_mRNA_check = input_mRNA_check) {
  alias$mRNA = NULL
  .vvv <- .v[.v != ""] %>% unique() %>% sapply(FUN = toupper, USE.NAMES = FALSE)
  tibble::tibble(symbol = .vvv) %>%
    dplyr::mutate(
      expression = purrr::map(
        .x = symbol,
        .f = function(.x) {
          grep(pattern = (paste(",",.x,",") %>% 
              stringr::str_replace_all(' ','')), .total_symbol$alias_match, value = TRUE ) -> a
          .total_symbol %>% dplyr::filter(alias_match %in% a) %>% .$symbol-> b
          .total_symbol %>% dplyr::filter(alias_match %in% a) %>% .$Symbol ->f
          if(length(f)>0){
            al <- paste(f," ","(",.x,")",sep="") %>% toString()
            if(length(alias$mRNA) > 0){
              alias$mRNA <- paste(alias$mRNA,","," ",al,sep = "")
            }
            else{
              alias$mRNA <- al
            }
          }
          grep(pattern = (paste(",",.x,",") %>% 
              stringr::str_replace_all(' ','')), .total_symbol$symbol_match, value = TRUE ) -> c
          .total_symbol %>% dplyr::filter(symbol_match %in% c) %>% .$symbol-> d
          e <- c(b,d)
          if(length(e)>0){e} else{"drop"}
        }
      )
    ) -> .v_dedup
  input_mRNA_check$non_match <- .v_dedup %>% dplyr::filter(expression %in% "drop") %>% .$symbol
  .vvv %in% input_mRNA_check$non_match ->.inter
  input_mRNA_check$match <-  .vvv[!.inter]
  input_mRNA_check$total <- c(input_mRNA_check$match,input_mRNA_check$non_match)
  input_mRNA_check$n_non_match <- length(input_mRNA_check$non_match)
  input_mRNA_check$n_match <- length(.vvv[!.inter])
  input_mRNA_check$n_total <- length(input_mRNA_check$non_match) + length(.vvv[!.inter])
  if (input_mRNA_check$n_match > 0){
    status$mRNA_set <- TRUE
    status$mRNA_valid <- TRUE 
    match$mRNA <- .v_dedup %>% dplyr::filter(symbol %in% .vvv[!.inter]) %>% .$expression %>% unlist() %>% tibble::tibble(x = .) %>% dplyr::distinct() %>% .$x
    if(length(input_mRNA_check$non_match) > 0 && length(alias$mRNA) > 0){
      status$mRNA_invalid <- TRUE
      paste("The name(s) is invalid:", input_mRNA_check$non_match %>% toString()) -> inva
      paste("The name(s) is changed to official symbol:", alias$mRNA) -> cha
      output$mRNA_invalid <- renderText({paste(inva,cha,sep = "\n")})
    }
    else if(length(input_mRNA_check$non_match) > 0){
      status$mRNA_invalid <- TRUE
      output$mRNA_invalid <- renderText({paste("The name(s) is invalid:", input_mRNA_check$non_match %>% toString())})
    }
    else if(length(alias$mRNA) > 0){
      status$mRNA_invalid <- TRUE
      output$mRNA_invalid <- renderText({paste("The name(s) is changed to official symbol:", alias$mRNA)})
    }
    else{
      status$mRNA_invalid <- FALSE
    }
  }
  else {
    status$mRNA_set <- FALSE
    status$mRNA_result <- FALSE
    status$mRNA_valid <- FALSE}
}

# Example -----------------------------------------------------------------

observeEvent(input$mRNA_example, {
  status$mRNA_set <- FALSE
  status$mRNA_result <- FALSE
  closeAlert(session = session, alertId = "guide-alert")
  shinyjs::js$example_mRNA_set(id = "seinput_mRNA_set")
  shinyjs::enable(id = "input_mRNA_set")
})

# Clear input -------------------------------------------------------------

observeEvent(input$input_mRNA_set_reset, {
  shinyjs::reset("input_mRNA_set")
  closeAlert(session = session, alertId = "guide-alert")
  status$mRNA_set <- FALSE
  status$mRNA_result <- FALSE
  status$mRNA_plot <- TRUE
  status$mRNA_valid <- TRUE
  status$mRNA_trigger <- FALSE
  status$mRNA_invalid <- FALSE
  error$mRNA_set <- ""
})

# Monitor search ----------------------------------------------------------

validate_input_mRNA_set <- eventReactive(
  eventExpr = input$input_mRNA_set_search,
  ignoreNULL = TRUE,
  valueExpr = {
    if(reset$mRNA){reset$mRNA <- FALSE} else{reset$mRNA <- TRUE}
    error$mRNA_set <- ""
    if (is.null(input$input_mRNA_set) || input$input_mRNA_set == "") {
      error$mRNA_set <- "Error: Please input gene symbol."
      status$mRNA_trigger <- if (status$mRNA_trigger == TRUE) FALSE else TRUE
      return()
    }
    # check gene
    .v_igs <- check_mRNA_set(.s = input$input_mRNA_set)
    # validate genes
    validate_mRNA_set(.v = .v_igs, .total_symbol = total_mRNA_symbol, input_mRNA_check = input_mRNA_check)
    
  }
)

#####add new
observeEvent(c(reset$mRNA),{
  status$protein_result <- FALSE
  if(status$mRNA_set){
  status$mRNA_plot <- FALSE
  dataset_number$mRNA <- 30
  TCGA_mRNA_result()
  GTEX_mRNA_result()
  CCLE_mRNA_result()
  a <- TCGA_mRNA_plot_result %>% dplyr::select(symbol) %>% dplyr::distinct() %>% .$symbol
  b <- GTEX_mRNA_table_result %>% dplyr::select(symbol) %>% dplyr::distinct() %>% .$symbol
  c <- CCLE_mRNA_table_result %>% dplyr::select(symbol) %>% dplyr::distinct() %>% .$symbol
  c(a,b,c) %>% tibble::tibble(symbol = .) %>% dplyr::distinct() %>% .$symbol -> plot_number$mRNA
  status$mRNA_result <- TRUE
  if(status$mRNA_trigger){status$mRNA_trigger <- FALSE} else{status$mRNA_trigger <- TRUE}
}}
)

TCGA_mRNA_result <- function(){
  TCGA_mRNA %>% 
    dplyr::mutate(
      expr = purrr::map(
        .x = summary,
        .f = function(.x) {
          .x %>%
            dplyr::filter(symbol %in% match$mRNA) %>% dplyr::select(-entrez_id) %>%
            tidyr::gather(key = barcode, value = expr, -c(symbol)) %>% tidyr::unnest()
        }
      )
    ) %>% dplyr::select(-summary) %>% tidyr::unnest() -> a
  a %>% 
    dplyr::mutate(tmp = paste(cancer_types,barcode)) %>% 
    dplyr::select(cancer_types = tmp,site,symbol,expr) %>% 
    dplyr::group_by(cancer_types,site,symbol) %>% 
    dplyr::slice(1:5) %>% tidyr::drop_na() %>% dplyr::ungroup() %>% 
    dplyr::mutate(tmp = log2(expr+1)) %>% 
    dplyr::select(cancer_types,site,symbol,expr = tmp) %>% 
    dplyr::left_join(total_mRNA_symbol,by="symbol") %>%
    dplyr::select(cancer_types,site,symbol=Symbol,expr) ->> TCGA_mRNA_plot_result
  a %>% dplyr::left_join(mRNA_TCGA, by = "cancer_types") %>% 
    dplyr::mutate(tmp=ifelse(barcode == "tumor",cancer,normal)) %>% 
    dplyr::select(cancer_types = Disease_Type, symbol, barcode, expr,count = tmp) %>% 
    dplyr::mutate(tmp = paste(cancer_types,barcode)) %>% 
    dplyr::select(cancer_types = tmp,symbol,count,expr) %>% 
    dplyr::group_by(cancer_types,symbol) %>% 
    dplyr::slice(6) %>% tidyr::drop_na() %>% 
    dplyr::ungroup() %>% 
    dplyr::left_join(total_mRNA_symbol,by="symbol") %>%
    dplyr::select(cancer_types,symbol=Symbol,count,expr) ->> TCGA_mRNA_table_result
    return(TCGA_mRNA_plot_result)
    return(TCGA_mRNA_table_result)
}

GTEX_mRNA_result <- function(){
  GTEX_mRNA %>%
    dplyr::mutate(
      expr = purrr::map(
        .x = summary,
        .f = function(.x) {
          .x %>%
            dplyr::filter(symbol %in% match$mRNA) %>% dplyr::select(-ensembl_gene_id) %>%
            tidyr::unnest()
        }
      )
    ) %>% dplyr::select(-summary) %>% tidyr::unnest() %>% 
    dplyr::rename(cancer_types = SMTS,expr=summary) -> expr_clean 
  expr_clean %>% dplyr::group_by(cancer_types,symbol) %>% 
    dplyr::slice(1:5) %>% tidyr::drop_na() %>% dplyr::ungroup() ->> GTEX_mRNA_plot_result
  expr_clean %>% dplyr::group_by(cancer_types,symbol) %>% 
    dplyr::slice(6) %>% tidyr::drop_na() %>% dplyr::ungroup() %>%
    dplyr::left_join(mRNA_GTEX,by = "cancer_types") %>% 
    dplyr::select(cancer_types,symbol,tissue_num,expr) %>% 
    dplyr::left_join(total_mRNA_symbol,by="symbol") %>%
    dplyr::select(cancer_types,symbol=Symbol,tissue_num,expr) ->> GTEX_mRNA_table_result
  return(GTEX_mRNA_plot_result)
  return(GTEX_mRNA_table_result)
}

CCLE_mRNA_result <- function(){
  CCLE_mRNA  %>% 
    dplyr::mutate(
      expr = purrr::map(
        .x = summary,
        .f = function(.x) {
          .x %>%
            dplyr::filter(symbol %in% match$mRNA) %>% tidyr::unnest()
        }
      )
    ) %>% dplyr::select(-summary) %>% tidyr::unnest() %>% dplyr::rename(cancer_types = tissue,expr=summary) -> expr_clean
  expr_clean %>% dplyr::group_by(cancer_types,symbol) %>% 
    dplyr::slice(1:5) %>% tidyr::drop_na() %>% 
    dplyr::ungroup()  ->> CCLE_mRNA_plot_result
  expr_clean %>% dplyr::group_by(cancer_types,symbol) %>% 
    dplyr::slice(6) %>% tidyr::drop_na() %>% dplyr::ungroup() %>%
    dplyr::left_join(mRNA_CCLE,by = "cancer_types") %>% 
    dplyr::select(cancer_types,symbol,cellline_num,expr) %>%
    dplyr::left_join(total_mRNA_symbol,by="symbol") %>%
    dplyr::select(cancer_types,symbol=Symbol,cellline_num,expr) ->> CCLE_mRNA_table_result
  return(CCLE_mRNA_plot_result)
  return(CCLE_mRNA_table_result)
}
#####add new

# mRNA table_print -------------------------------------------------------------
expr_box_plot_mRNA <-  function(.expr,.type){
  ###add new
  if(.type == "TCGA"){
  ### add new
    .expr %>% dplyr::rename(FPKM = expr) %>%
      tidyr::separate(col = cancer_types, into = c("cancer_types", "types")) %>%
      dplyr::mutate(tmp = paste(site,"(",cancer_types,")")) %>%
      dplyr::select(cancer_types=tmp,types,symbol,FPKM) %>%
      dplyr::mutate(types = stringr::str_to_title(types))  -> t1
    t1 %>% dplyr::filter(types %in% "Tumor") %>% dplyr::group_by(cancer_types) %>% 
      dplyr::slice(3) %>% dplyr::arrange(desc(FPKM)) %>% .$cancer_types -> order
    plot_ly(
      data = t1,
      x = ~ cancer_types,
      y = ~ FPKM,
      type = "box",
      split = ~ types,
      color = ~ types, colors = c("midnightblue", "red3"),
      source = "mRNA_TCGA"
    ) %>% layout(
      title = t1$symbol[1],
      boxmode = "group",
      xaxis = list(
        title = "Cancer Types (TCGA)",
        showticklabels = TRUE,
        tickangle = 295, tickfont = list(size = 12),
        showline = TRUE,
        categoryorder = "array", 
        categoryarray = order
      ),
      yaxis = list(title = "RSEM(log2)" ,showline = TRUE,
                   zeroline = FALSE,hoverformat = '.2f'),
      legend = list(orientation = 'h',x = 0.7, y = 1))
    ###add new
  }
  else{
    .expr %>% dplyr::rename(FPKM = expr) %>%
      dplyr::group_by(symbol) %>% dplyr::arrange(symbol,desc(FPKM)) %>% 
      dplyr::ungroup() %>% dplyr::mutate(tmp = stringr::str_to_title(cancer_types)) %>% 
      dplyr::select(cancer_types = tmp, symbol, FPKM)-> t2
    if(.type == "GTEX"){
      mRNA_GTEX_sd %>% dplyr::filter(symbol %in% t2$symbol[1]) %>% 
        dplyr::mutate(tmp = stringr::str_replace_all(SMTS,pattern="_",replacement=" ") %>% stringr::str_to_title()) %>% 
        dplyr::select(SMTS=tmp,sd) %>% 
        dplyr::rename(cancer_types = SMTS) -> mRNA_GTEX_sd2
      t2 %>% dplyr::left_join(mRNA_GTEX_sd2,by="cancer_types") -> t2
      t2 %>% dplyr::group_by(cancer_types) %>% 
        dplyr::slice(3) %>% dplyr::arrange(desc(FPKM)) %>% .$cancer_types -> order
      plot_ly(
        data = t2, x = ~ cancer_types, y = ~ log2(FPKM+1), type = "box", split = ~ symbol, 
        color = ~ symbol, colors = "#cbb255",source = "mRNA_GTEX", tickfont = list(size = 12),
        name = "GTEX",showlegend = FALSE
      ) %>% layout(
        boxgap = 0,#boxgroupgap=0,
        title = t2$symbol[1],
        xaxis = list(
          title = "Normal Tissues (GTEx)", showticklabels = TRUE,
          tickangle = 295, showline = TRUE, categoryorder = "array", 
          categoryarray = order
        ),
        yaxis = list(title = "FPKM(log2)" ,showline = TRUE,
                     zeroline = FALSE,hoverformat = '.2f'))
      }
    else{
      mRNA_CCLE_sd %>% dplyr::filter(symbol %in% t2$symbol[1]) %>% 
        dplyr::mutate(tmp = stringr::str_replace_all(tissue,pattern="_",replacement=" ") %>% stringr::str_to_title()) %>% 
        dplyr::select(tissue=tmp,sd) %>% 
        dplyr::rename(cancer_types = tissue) -> mRNA_GTEX_sd2
      t2 %>% dplyr::left_join(mRNA_GTEX_sd2,by="cancer_types") -> t2
      t2 %>% dplyr::group_by(cancer_types) %>% 
        dplyr::slice(3) %>% dplyr::arrange(desc(FPKM)) %>% .$cancer_types -> order
      plot_ly(
        data = t2, x = ~ cancer_types, y = ~ log2(FPKM+1), type = "box", split = ~ symbol, 
        color = ~ symbol, colors = "#ffc0cb",source = "mRNA_CCLE", tickfont = list(size = 12),
        name = "CCLE",showlegend = FALSE#,error_y = ~list(array = sd,color = '#000000')
      ) %>% layout(
        title = t2$symbol[1],
        xaxis = list(
          title = "Cell lines (CCLE)", showticklabels = TRUE,
          tickangle = 295, showline = TRUE, categoryorder = "array", 
          categoryarray = order
        ),
        yaxis = list(title = "FPKM(log2)" ,showline = TRUE,
                     zeroline = FALSE,hoverformat = '.2f'))
    }
    ###add new
  }
}
expr_clean_datatable_mRNA <- function(.expr_clean,.title) {
  if(.title == "Cancer Types (TCGA)"){
    name <- "Mean expr. (RSEM)"
    site <- "Cancer Type"
  }
  else if(.title == "Normal Tissues (GTEx)"){
    name <- "Mean expr. (FPKM)"
    site <- "Tissue"
  }
  else{
    name <- "Mean expr. (FPKM)"
    site <- "Cell line Lineage"
  }
  DT::datatable(
    data = .expr_clean,
    options = list(
      pageLength = 10,
      searching = TRUE,
      autoWidth = TRUE,
      dom = "Bfrtip",
      columnDefs = list(list(className = 'dt-center',targets="_all"))
    ),
    caption = shiny::tags$caption(
      .title,
      style = 'font-size: 25px; color: black'
    ),
    rownames = FALSE,
    colnames = c(site, "Symbol", "Sample Statistics", name),
    style = "bootstrap",
    class = "table-bordered table-condensed"
  ) %>% 
    #DT::formatSignif(columns = c("expr"), digits = 2) %>%
    DT::formatRound(columns = c("expr"), 2)
}

click_plot_TCGA_tumor <- function(.expr_clean) {
  .expr_clean %>% stringr::str_split_fixed(pattern = "\\( ",n=2) %>% 
    .[,2] %>% stringr::str_split_fixed(pattern = " \\)",n=2) %>% 
    .[,1] -> cancertypes
  paste("/home/liucj/shiny-data/GEDS/split_file/mRNA/TCGA/",cancertypes,".rds.gz",sep = "") -> file_name
  file <- readr::read_rds(file_name)
  file %>% names %>% .[c(-1,-2)] %>% tibble::tibble(barcode = .) %>% 
    dplyr::mutate(type = stringr::str_sub(string = barcode, start = 14, end = 15)) %>% 
    dplyr::mutate(type = ifelse(type == "11", "Normal", "Tumor")) ->cancertype
  cancertype %>% dplyr::filter(type %in% "Tumor") %>% .$barcode->tumor_barcode
  file %>% dplyr::filter(symbol %in% input$select_mRNA_result) %>%
    dplyr::select(symbol,tumor_barcode) %>% 
    tidyr::gather(key=barcode,value=expr,-c(symbol)) %>%
    dplyr::mutate(type = stringr::str_sub(string = barcode, start = 1, end = 16)) %>%
    dplyr::select(symbol,barcode=type,expr) -> rebuild_file
  rebuild_file %>% dplyr::arrange(expr) %>% .$barcode -> order
  rebuild_file %>% plot_ly(x = ~barcode, y = ~ log2(expr+1),type = "scatter",mode = "markers") %>%
    layout(
      title = paste(cancertypes,"Tumor","n=",length(order)),
      xaxis = list(
        title = "Patient",
        showticklabels = FALSE,
        showline= TRUE,
        categoryorder = "array", 
        categoryarray = order
      ),
      yaxis = list(title = "RSEM(log2)" ,showline = TRUE,hoverformat = '.2f')
    )
}

click_plot_TCGA_normal <- function(.expr_clean) {
  .expr_clean %>% stringr::str_split_fixed(pattern = "\\( ",n=2) %>% 
    .[,2] %>% stringr::str_split_fixed(pattern = " \\)",n=2) %>% 
    .[,1] -> cancertypes
  paste("/home/liucj/shiny-data/GEDS/split_file/mRNA/TCGA/",cancertypes,".rds.gz",sep = "") -> file_name
  file <- readr::read_rds(file_name)
  file %>% names %>% .[c(-1,-2)] %>% tibble::tibble(barcode = .) %>% 
    dplyr::mutate(type = stringr::str_sub(string = barcode, start = 14, end = 15)) %>% 
    dplyr::mutate(type = ifelse(type == "11", "Normal", "Tumor")) ->cancertype
  cancertype %>% dplyr::filter(type %in% "Normal") %>% .$barcode->tumor_barcode
  file %>% dplyr::filter(symbol %in% input$select_mRNA_result) %>%
    dplyr::select(symbol,tumor_barcode) %>% 
    tidyr::gather(key=barcode,value=expr,-c(symbol)) %>%
    dplyr::mutate(type = stringr::str_sub(string = barcode, start = 1, end = 16)) %>%
    dplyr::select(symbol,barcode=type,expr) ->rebuild_file
  rebuild_file %>% dplyr::arrange(expr) %>% .$barcode -> order
  rebuild_file %>% plot_ly(x = ~barcode, y = ~ log2(expr+1),type = "scatter",mode = "markers") %>%
    layout(
      title = paste(cancertypes,"Normal","n=",length(order)),
      xaxis = list(
        title = "Patient",
        showticklabels = FALSE,
        showline= TRUE,
        categoryorder = "array", 
        categoryarray = order
      ),
      yaxis = list(title = "RSEM(log2)" ,showline = TRUE,hoverformat = '.2f')
    )
}

click_plot_GTEX <- function(.expr_clean){
  .expr_clean %>% stringr::str_replace_all(pattern = " ",replacement = "") -> cancertypes
  paste("/home/liucj/shiny-data/GEDS/split_file/mRNA/GTEX/",cancertypes,".rds.gz",sep = "") -> file_name
  file <- readr::read_rds(file_name)
  file %>% dplyr::filter(symbol %in% input$select_mRNA_result) %>%
    dplyr::select(-ensembl_gene_id) %>%
    tidyr::gather(key=barcode,value=expr,-c(symbol)) ->rebuild_file
  rebuild_file %>% dplyr::arrange(expr) %>% .$barcode -> order
  rebuild_file %>% plot_ly(x = ~barcode, y = ~ log2(expr+1),type = "scatter",mode = "markers") %>%
    layout(
      title = paste(.expr_clean,"n=",length(order)),
      xaxis = list(
        title = "Normal Tissues",
        showticklabels = FALSE,
        categoryorder = "array", 
        categoryarray = order
      ),
      yaxis = list(title = "FPKM(log2)" ,showline = TRUE,hoverformat = '.2f')
    )
}

click_plot_CCLE <- function(.expr_clean){
  .expr_clean %>% stringr::str_replace_all(pattern = " ",replacement = "") -> cancertypes
  paste("/home/liucj/shiny-data/GEDS/split_file/mRNA/CCLE/",cancertypes,".rds.gz",sep = "") -> file_name
  file <- readr::read_rds(file_name)
  file %>% dplyr::filter(symbol %in% input$select_mRNA_result) %>%
    tidyr::gather(key=barcode,value=expr,-c(symbol)) ->rebuild_file
  rebuild_file %>% dplyr::arrange(expr) %>% .$barcode -> order
  rebuild_file %>% plot_ly(x = ~barcode, y = ~ log2(expr+1),type = "scatter",mode = "markers") %>%
    layout(
      title = paste(.expr_clean,"n=",length(order)),
      xaxis = list(
        title = "Cell lines",
        showticklabels = FALSE,
        categoryorder = "array", 
        categoryarray = order
      ),
      yaxis = list(title = "FPKM(log2)" ,showline = TRUE,hoverformat = '.2f')
    )
}
# ObserveEvent ------------------------------------------------------------
observeEvent(status$mRNA_trigger, {
  if (error$mRNA_set != "" && !is.null(error$mRNA_set)) {
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Error...",
      text = error$mRNA_set,
      type = "error"
    )
  }
})

observeEvent(status$mRNA_valid, {
  if (status$mRNA_valid == FALSE) {
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Error...",
      text = "No matched symbol, please check",
      type = "error"
    )
  }
})

# observe -----------------------------------------------------------------
observe(validate_input_mRNA_set())
observeEvent(event_data("plotly_click", source = "mRNA_TCGA"), {
  toggleModal(session, modalId = "mRNA_boxPopUp_TCGA", toggle = "toggle")
})
observeEvent(event_data("plotly_click", source = "mRNA_GTEX"), {
  toggleModal(session, modalId = "mRNA_boxPopUp_GTEX", toggle = "toggle")
})
observeEvent(event_data("plotly_click", source = "mRNA_CCLE"), {
  toggleModal(session, modalId = "mRNA_boxPopUp_CCLE", toggle = "toggle")
})
observeEvent(c(input$select_mRNA_result,status$mRNA_trigger), {
  if(length(input$select_mRNA_result)>0 && status$mRNA_set){
    ###add new
    TCGA_mRNA_plot_result %>% 
      dplyr::filter(symbol %in% input$select_mRNA_result) -> TCGA_one_plot
    TCGA_mRNA_table_result %>% 
      dplyr::filter(symbol %in% input$select_mRNA_result) -> TCGA_one_table
    GTEX_mRNA_plot_result %>% 
      dplyr::filter(symbol %in% input$select_mRNA_result) -> GTEX_one_plot
    GTEX_mRNA_table_result %>% 
      dplyr::filter(symbol %in% input$select_mRNA_result) -> GTEX_one_table
    CCLE_mRNA_plot_result %>% 
      dplyr::filter(symbol %in% input$select_mRNA_result) -> CCLE_one_plot
    CCLE_mRNA_table_result %>% 
      dplyr::filter(symbol %in% input$select_mRNA_result) -> CCLE_one_table
    if(length(TCGA_one_plot$cancer_types) + length(GTEX_one_plot$cancer_types) + length(CCLE_one_plot$cancer_types) > 0 ){
      choice$mRNA <- paste(input$select_mRNA_result,status$mRNA_trigger) %>% stringr::str_replace_all(' ','')
      mRNA$TCGA_table <- paste(input$select_mRNA_result,status$mRNA_trigger) %>% stringr::str_replace_all(' ','') %>% paste(.,"TCGA_table",sep = "")
      mRNA$TCGA_download <- paste(input$select_mRNA_result,status$mRNA_trigger) %>% stringr::str_replace_all(' ','') %>% paste(.,"TCGA_download",sep = "")
      mRNA$GTEX_table <- paste(input$select_mRNA_result,status$mRNA_trigger) %>% stringr::str_replace_all(' ','') %>% paste(.,"GTEX_table",sep = "")
      mRNA$GTEX_download <- paste(input$select_mRNA_result,status$mRNA_trigger) %>% stringr::str_replace_all(' ','') %>% paste(.,"GTEX_download",sep = "")
      mRNA$CCLE_table <- paste(input$select_mRNA_result,status$mRNA_trigger) %>% stringr::str_replace_all(' ','') %>% paste(.,"CCLE_table",sep = "")
      mRNA$CCLE_download <- paste(input$select_mRNA_result,status$mRNA_trigger) %>% stringr::str_replace_all(' ','') %>% paste(.,"CCLE_download",sep = "")
      t <- 0
      g <- 0
      c <- 0
      if(length(TCGA_one_plot$cancer_types) > 0){
        TCGA_plot <- expr_box_plot_mRNA(TCGA_one_plot,"TCGA")
        output$mRNA_TCGA <- renderPlotly({TCGA_plot})
        output[[mRNA$TCGA_table]] <- DT::renderDataTable({expr_clean_datatable_mRNA(TCGA_one_table,"Cancer Types (TCGA)")})
        output[[mRNA$TCGA_download]] <- downloadHandler(
          filename = function() {
            paste(Sys.Date(),"TCGA_single_mRNA.csv",sep = "_")
          },
          content = function(file) {
            write.csv(TCGA_one_table, file, row.names = TRUE)
          }
        )
        output$expr_dt_comparison_TCGA_mRNA <- downloadHandler(
          filename = function() {
            paste(Sys.Date(),"TCGA_all_input_mRNA.csv",sep = "_")
          },
          content = function(file) {
            write.csv(TCGA_mRNA_table_result, file, row.names = TRUE)
          }
        )
        t = 1
      }
      if(length(GTEX_one_plot$cancer_types) > 0){
        GTEX_plot <- expr_box_plot_mRNA(GTEX_one_plot,"GTEX")
        output$mRNA_GTEX <- renderPlotly({GTEX_plot})
        output[[mRNA$GTEX_table]] <- DT::renderDataTable({expr_clean_datatable_mRNA(GTEX_one_table,"Normal Tissues (GTEx)")})
        output[[mRNA$GTEX_download]] <- downloadHandler(
          filename = function() {
            paste(Sys.Date(),"GTEx_single_mRNA.csv",sep = "_")
          },
          content = function(file) {
            write.csv(GTEX_one_table, file, row.names = TRUE)
          }
        )
        output$expr_dt_comparison_GTEX_mRNA <- downloadHandler(
          filename = function() {
            paste(Sys.Date(),"GTEx_all_input_mRNA.csv",sep = "_")
          },
          content = function(file) {
            write.csv(GTEX_mRNA_table_result, file, row.names = TRUE)
          }
        )
        g = 1
      }
      if(length(CCLE_one_plot$cancer_types) > 0){
        CCLE_plot <- expr_box_plot_mRNA(CCLE_one_plot,"CCLE")
        output$mRNA_CCLE <- renderPlotly({CCLE_plot})
        output[[mRNA$CCLE_table]] <- DT::renderDataTable({expr_clean_datatable_mRNA(CCLE_one_table,"Cell lines (CCLE)")})
        output[[mRNA$CCLE_download]] <- downloadHandler(
          filename = function() {
            paste(Sys.Date(),"CCLE_single_mRNA.csv",sep = "_")
          },
          content = function(file) {
            write.csv(CCLE_one_table, file, row.names = TRUE)
          }
        )
        output$expr_dt_comparison_CCLE_mRNA <- downloadHandler(
          filename = function() {
            paste(Sys.Date(),"CCLE_all_input_mRNA.csv",sep = "_")
          },
          content = function(file) {
            write.csv(CCLE_mRNA_table_result, file, row.names = TRUE)
          }
        )
        c = 1
      }
      p3 <- plotly_empty(source = "main")
      if (t == 1 && g == 1 && c == 1){
        #subplot(
        #  TCGA_plot, p3,
        #  GTEX_plot, p3,
        #  CCLE_plot,
        #  nrows = 5, titleX = TRUE, titleY = TRUE , heights = c(0.25,0.125,0.25,0.125,0.25)
        #) -> plot_result
        plotmode$mRNA <-  1
        #output[[choice$mRNA]] <- renderPlotly({plot_result})
      }
      else if (t == 1 && g == 1){
        #subplot(
        #  TCGA_plot,p3,GTEX_plot,
        #  nrow = 3, heights = c(0.4,0.2,0.4)
        #) -> plot_result
        plotmode$mRNA <-  2
        #output[[choice$mRNA]] <- renderPlotly({plot_result})
      }
      else if(g == 1 && c == 1){
        #subplot(
        #  GTEX_plot,p3,CCLE_plot,
        #  nrow = 3, heights = c(0.4,0.2,0.4)
        #) -> plot_result
        plotmode$mRNA <-  3
        #output[[choice$mRNA]] <- renderPlotly({plot_result})
      }
      else if(t == 1 && c == 1){
        #subplot(
        #  TCGA_plot,p3,CCLE_plot,
        #  nrow = 3, heights = c(0.4,0.2,0.4)
        #) -> plot_result
        plotmode$mRNA <-  4
        #output[[choice$mRNA]] <- renderPlotly({plot_result})
      }
      else if(t == 1){
        plotmode$mRNA <-  5
        #output[[choice$mRNA]] <- renderPlotly({TCGA_plot})
      }
      else if(g == 1){
        plotmode$mRNA <-  6
        #output[[choice$mRNA]] <- renderPlotly({GTEX_plot})
      }
      else if(c == 1){
        plotmode$mRNA <-  7
        #output[[choice$mRNA]] <- renderPlotly({CCLE_plot})
      }
      output$mRNA_hover_TCGA <- renderPlotly({
        eventdat <- event_data('plotly_click', source="mRNA_TCGA") # get event data from source main
        if(is.null(eventdat) == T) return(NULL)        # If NULL dont do anything
        if(eventdat$curveNumber[1] == 1){
          click_plot_TCGA_tumor(eventdat$x[1])
        }
        else if(eventdat$curveNumber[1] == 0){
          click_plot_TCGA_normal(eventdat$x[1])
        }
      })
      output$mRNA_hover_GTEX <- renderPlotly({
        eventdat <- event_data('plotly_click', source="mRNA_GTEX") # get event data from source main
        if(is.null(eventdat) == T) return(NULL)        # If NULL dont do anything
        click_plot_GTEX(eventdat$x[1])
      })
      output$mRNA_hover_CCLE <- renderPlotly({
        eventdat <- event_data('plotly_click', source="mRNA_CCLE") # get event data from source main
        if(is.null(eventdat) == T) return(NULL)        # If NULL dont do anything
        click_plot_CCLE(eventdat$x[1])
      })
      ###add new
    }
  }
})

