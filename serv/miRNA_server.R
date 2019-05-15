# source by server.R
# saved as miRNA_server.R

# Check input gene set ----------------------------------------------------
check_mirna_set <- function(.s) {
  .s %>%stringr::str_split(pattern = "[ ,;]+", simplify = TRUE) %>%.[1, ]  -> .ss
  .ss
}

# Validate gene with TCGA gene symbol -------------------------------------

validate_miRNA_set <- function(.v,  .total_symbol, input_miRNA_check = input_miRNA_check) {
  .v[.v != ""] %>% unique()  -> .vv
  gsub(pattern = "-",replacement = "", .vv) %>% sapply(FUN = tolower, USE.NAMES = FALSE) %>% 
    grep(pattern="mir|let",.,value=TRUE) -> .vvv
  input_miRNA_check$non_match <- gsub(pattern = "-",replacement = "", .vv) %>% sapply(FUN = tolower, USE.NAMES = FALSE) %>% grep(pattern="mir|let",.,value=TRUE, invert = TRUE)
  tibble::tibble(symbol=.vvv) %>%
    dplyr::mutate(
      expression = purrr::map(
        .x = symbol,
        .f = function(.x) {
          paste(.x,"-") %>% stringr::str_replace(" ",'') %>% grep(pattern = ., .total_symbol$match, value = TRUE) -> a
          .total_symbol %>% dplyr::filter(match %in% a) %>% .$symbol->b
          if(length(a) < 1){
            paste(.x,",") %>% stringr::str_replace(" ",'') %>% grep(pattern = ., .total_symbol$match, value = TRUE) -> a
            .total_symbol %>% dplyr::filter(match %in% a) %>% .$symbol->b
            if(length(a)<1){
              grep(pattern = .x, .total_symbol$match2, value = TRUE) ->a
              .total_symbol %>% dplyr::filter(match2 %in% a) %>% .$symbol->b
            }
          }
          if(length(b)>0){b} else{"drop"}
        }
      )
    ) -> .v_dedup
  ddd <- .v_dedup %>% dplyr::filter(expression %in% "drop") %>% .$symbol
  .vvv %in% ddd -> .inter
  input_miRNA_check$match <-  .vvv[!.inter]
  input_miRNA_check$n_match <- length(.vvv[!.inter])
  input_miRNA_check$total <- c(input_miRNA_check$match,input_miRNA_check$non_match)
  input_miRNA_check$n_non_match <- length(input_miRNA_check$non_match)
  input_miRNA_check$n_total <- length(input_miRNA_check$non_match) + length(.vvv[!.inter])
  if(input_miRNA_check$n_match > 0) {
    status$miRNA_set <- TRUE
    status$miRNA_valid <- TRUE 
    match$miRNA <- .v_dedup %>% dplyr::filter(symbol %in% .vvv[!.inter]) %>% .$expression %>% unlist() %>% tibble::tibble(x = .) %>% dplyr::distinct() %>% .$x
    if(length(input_miRNA_check$non_match) > 0){
      status$miRNA_invalid <- TRUE
      output$miRNA_invalid <- renderText({paste("The list below is invalid:", input_miRNA_check$non_match %>% toString())})
    }
    else{
      status$miRNA_invalid <- FALSE
    }
  } 
  else {
    status$miRNA_set <- FALSE
    status$miRNA_result <- FALSE
    status$miRNA_valid <- FALSE}
}

# Example -----------------------------------------------------------------

observeEvent(input$miRNA_example, {
  status$miRNA_set <- FALSE
  status$miRNA_result <- FALSE
  status$miRNA_trigger <- FALSE
  closeAlert(session = session, alertId = "guide-alert")
  shinyjs::js$example_miRNA_set(id = "seinput_miRNA_set")
  shinyjs::enable(id = "input_miRNA_set")
})

# Clear input -------------------------------------------------------------

observeEvent(input$input_miRNA_set_reset, {
  shinyjs::reset("input_miRNA_set")
  closeAlert(session = session, alertId = "guide-alert")
  status$miRNA_set <- FALSE
  status$miRNA_plot <- TRUE
  status$miRNA_result <- FALSE
  status$miRNA_valid <- TRUE
  status$miRNA_trigger <- FALSE
  output$expr_bubble_plot_mirna <- NULL
  output$expr_dt_comparison_mirna <-  NULL
})


# Monitor search ----------------------------------------------------------

validate_input_miRNA_set <- eventReactive(
  eventExpr = input$input_miRNA_set_search,
  ignoreNULL = TRUE,
  valueExpr = {
    if(reset$miRNA){reset$miRNA <- FALSE} else{reset$miRNA <- TRUE}
    if (is.null(input$input_miRNA_set) || input$input_miRNA_set == "") {
      error$miRNA_set <- "Error: Please input miRNA symbol."
      status$miRNA_trigger <- if (status$miRNA_trigger == TRUE) FALSE else TRUE
      return()
    }
    # check gene
    .v_igs <- check_mirna_set(.s = input$input_miRNA_set)
    # validate genes
    validate_miRNA_set(.v = .v_igs, .total_symbol = total_miRNA_symbol, input_miRNA_check = input_miRNA_check)
  }
)

###add new
observeEvent(c(reset$miRNA),{
  status$protein_result <- FALSE
  if(status$miRNA_set){
    status$miRNA_plot <- FALSE
    dataset_number$miRNA <- 30
    TCGA_miRNA_result()
    plot_number$miRNA <- TCGA_miRNA_plot_result %>% dplyr::select(name) %>% dplyr::distinct() %>% .$name
    if(status$miRNA_trigger){status$miRNA_trigger <- FALSE} else{status$miRNA_trigger <- TRUE}
    status$miRNA_result <- TRUE
    return(TCGA_miRNA_plot_result)
  }}
)

TCGA_miRNA_result <- function(){
  TCGA_miRNA %>%
    dplyr::mutate(
      mirna = purrr::map(
        .x = summary,
        .f = function(.x) {
          .x %>%
            dplyr::filter(name %in% match$miRNA) %>% 
            tidyr::gather(key = barcode, value = expr, -c(gene,name)) %>% tidyr::unnest()
        }
      )
    ) %>% dplyr::select(-summary) %>% tidyr::unnest() -> a
  a %>% 
    dplyr::mutate(tmp = paste(cancer_types,barcode)) %>% 
    dplyr::select(cancer_types=tmp,site,gene,name,expr) %>% 
    dplyr::group_by(cancer_types,gene,name) %>% 
    dplyr::slice(1:5) %>% tidyr::drop_na() %>% 
    dplyr::ungroup() %>% dplyr::mutate(tmp = log2(expr+1)) %>% 
    dplyr::select(cancer_types,site,gene,name,expr = tmp) ->> TCGA_miRNA_plot_result
  a %>% dplyr::left_join(miRNA_TCGA, by = "cancer_types") %>% 
   dplyr::mutate(tmp=ifelse(barcode == "tumor",cancer,normal)) %>%
   dplyr::select(cancer_types = Disease_Type, name, gene, barcode, expr, count=tmp) %>% 
    dplyr::mutate(tmp = paste(cancer_types,barcode)) %>% 
    dplyr::select(cancer_types=tmp, name, gene, count, expr) %>% 
  dplyr::group_by(cancer_types,name) %>% dplyr::slice(6) %>% tidyr::drop_na() %>% dplyr::ungroup() %>% 
    dplyr::mutate(tmp = log2(expr+1)) %>% dplyr::select(cancer_types, name, gene, count, expr = tmp) ->> TCGA_miRNA_table_result
  return(TCGA_miRNA_plot_result)
  return(TCGA_miRNA_table_result)
}
###add new

# miRNA table print -------------------------------------------------------
expr_box_plot_mirna <-  function(.expr){
  .expr %>% dplyr::rename(TPM = expr,symbol = name) %>%
    tidyr::separate(col = cancer_types, into = c("cancer_types", "types")) %>%
    dplyr::mutate(tmp = paste(site,"(",cancer_types,")")) %>%
    dplyr::select(cancer_types=tmp,types,symbol,TPM) %>%
    dplyr::mutate(types = stringr::str_to_title(types))  -> t1
  t1 %>% dplyr::filter(types %in% "Tumor") %>% dplyr::group_by(cancer_types) %>% 
    dplyr::slice(3) %>% dplyr::arrange(desc(TPM)) %>% .$cancer_types -> order
  plot_ly(
    data = t1,
    x = ~ cancer_types,
    y = ~ TPM,
    type = "box",
    split = ~ types,
    color = ~ types, colors = c("midnightblue", "red3"),
    source = "miRNA"
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
    yaxis = list(title = "RSEM(log2)" ,showline = TRUE,hoverformat = '.2f'),
    legend = list(orientation = 'h',x = 0.7, y = 1.2))
}
expr_clean_datatable_mirna <- function(.expr_clean,.title) {
  DT::datatable(
    data = .expr_clean,
    options = list(
      pageLength = 10,
      autoWidth = TRUE,
      searching = TRUE,
      dom = "Bfrtip",
      columnDefs = list(list(className = 'dt-center',targets="_all"))
    ),
    caption = shiny::tags$caption(
      .title,
      style = 'font-size: 20; color: black'
    ),
    rownames = FALSE,
    colnames = c("Cancer Type", "Symbol","Sample Statistics", "TPM (log2) expr."),
    style = "bootstrap",
    class = "table-bordered table-condensed"
  ) %>% 
    #DT::formatSignif(columns = c("expr"), digits = 2) %>%
    DT::formatRound(columns = c("expr"), 2)
}

click_plot_miRNA_TCGA_tumor <- function(.expr_clean) {
  .expr_clean %>% stringr::str_split_fixed(pattern = "\\( ",n=2) %>% 
    .[,2] %>% stringr::str_split_fixed(pattern = " \\)",n=2) %>% 
    .[,1] -> cancertypes
  paste("/home/liucj/shiny-data/GEDS/split_file/miRNA/TCGA/",cancertypes,".rds.gz",sep = "") -> file_name
  file <- readr::read_rds(file_name)
  file %>% dplyr::select(-gene,-name) %>% names %>% tibble::tibble(barcode = .) %>% 
    dplyr::mutate(type = stringr::str_sub(string = barcode, start = 14, end = 15)) %>% 
    dplyr::mutate(type = ifelse(type == "11", "Normal", "Tumor")) ->cancertype
  cancertype %>% dplyr::filter(type %in% "Tumor") %>% .$barcode->tumor_barcode
  file %>% dplyr::filter(name %in% input$select_miRNA_result) %>%
    dplyr::select(name,tumor_barcode) %>% 
    tidyr::gather(key=barcode,value=expr,-c(name)) %>%
    dplyr::mutate(type = stringr::str_sub(string = barcode, start = 1, end = 16)) %>%
    dplyr::select(name,barcode=type,expr) ->rebuild_file
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
      yaxis = list(title = "TPM(log2)" ,showline = TRUE,hoverformat = '.2f')
    )
}

click_plot_miRNA_TCGA_normal <- function(.expr_clean) {
  .expr_clean %>% stringr::str_split_fixed(pattern = "\\( ",n=2) %>% 
    .[,2] %>% stringr::str_split_fixed(pattern = " \\)",n=2) %>% 
    .[,1] -> cancertypes
  paste("/home/liucj/shiny-data/GEDS/split_file/miRNA/TCGA/",cancertypes,".rds.gz",sep = "") -> file_name
  file <- readr::read_rds(file_name)
  file %>% dplyr::select(-gene,-name) %>% names %>% tibble::tibble(barcode = .) %>% 
    dplyr::mutate(type = stringr::str_sub(string = barcode, start = 14, end = 15)) %>% 
    dplyr::mutate(type = ifelse(type == "11", "Normal", "Tumor")) ->cancertype
  cancertype %>% dplyr::filter(type %in% "Normal") %>% .$barcode->tumor_barcode
  file %>% dplyr::filter(name %in% input$select_miRNA_result) %>%
    dplyr::select(name,tumor_barcode) %>% 
    tidyr::gather(key=barcode,value=expr,-c(name)) %>%
    dplyr::mutate(type = stringr::str_sub(string = barcode, start = 1, end = 16)) %>%
    dplyr::select(name,barcode=type,expr) ->rebuild_file
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
      yaxis = list(title = "TPM(log2)" ,showline = TRUE,hoverformat = '.2f')
    )
}

# ObserveEvent ------------------------------------------------------------
observeEvent(status$miRNA_trigger, {
  if (error$miRNA_set != "" && !is.null(error$miRNA_set)) {
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Error...",
      text = error$miRNA_set,
      type = "error"
    )
  }
})

observeEvent(status$miRNA_valid, {
  if (status$miRNA_valid == FALSE) {
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Error...",
      text = "No matched symbol, please check",
      type = "error"
    )
  }
})
# observe -----------------------------------------------------------------
observe(validate_input_miRNA_set())
observeEvent(event_data("plotly_click", source = "miRNA"), {
  toggleModal(session, modalId = "miRNA_boxPopUp", toggle = "toggle")
})
observeEvent(c(input$select_miRNA_result,status$miRNA_trigger), {
  if(length(input$select_miRNA_result) > 0 && status$miRNA_valid){
    choice$miRNA <- total_miRNA_symbol %>% dplyr::filter(symbol %in% input$select_miRNA_result)  %>% .$gene
    miRNA$TCGA_table <- total_miRNA_symbol %>% dplyr::filter(symbol %in% input$select_miRNA_result)  %>% .$gene %>% paste(.,"table",sep = "")
    miRNA$TCGA_download <- total_miRNA_symbol %>% dplyr::filter(symbol %in% input$select_miRNA_result)  %>% .$gene %>% paste(.,"download",sep = "")
    TCGA_miRNA_table_result %>%　dplyr::select(-gene) -> all_table
    TCGA_miRNA_plot_result %>% dplyr::filter(gene %in% choice$miRNA) -> TCGA_one_plot
    TCGA_miRNA_table_result %>% dplyr::filter(gene %in% choice$miRNA) %>% dplyr::select(-gene) -> TCGA_one_table
    if(length(TCGA_one_plot$cancer_types) ){
      output[[choice$miRNA]] <- renderPlotly({TCGA_one_plot %>% expr_box_plot_mirna()})
      output[[miRNA$TCGA_table]] <- DT::renderDataTable({expr_clean_datatable_mirna(TCGA_one_table,"Cancer Types (TCGA)")})
      output[[miRNA$TCGA_download]] <- downloadHandler(
        filename = function() {
          paste(Sys.Date(),"TCGA_single_miRNA.csv",sep = "_")
        },
        content = function(file) {
          write.csv(TCGA_one_table, file, row.names = TRUE)
        }
      )
      output$expr_dt_comparison_TCGA_mirna <- downloadHandler(
        filename = function() {
          paste(Sys.Date(),"TCGA_all_input_miRNA.csv",sep = "_")
        },
        content = function(file) {
          write.csv(all_table, file, row.names = TRUE)
        }
      )
      output$miRNA_hover <- renderPlotly({
        eventdat <- event_data('plotly_click', source="miRNA") # get event data from source main
        if(is.null(eventdat) == T) return(NULL)        # If NULL dont do anything
          if(eventdat$curveNumber[1] == 1){
            click_plot_miRNA_TCGA_tumor(eventdat$x[1])
          }
          else if(eventdat$curveNumber[1] == 0){
            click_plot_miRNA_TCGA_normal(eventdat$x[1])
          }
      })
    }
  }
})

# observeEvent of selectall -----------------------------------------------

observeEvent(input$select_all_miRNA_TCGA, {
  shinyjs::js$TCGAmiRNAselectall()
})

observeEvent(input$unselect_all_miRNA_TCGA, {
  shinyjs::js$TCGAmiRNAunselectall()
  status$miRNA_result <- FALSE
})