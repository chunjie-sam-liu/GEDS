# source by server.R
# saved as protein_server.R

# Clear input -------------------------------------------------------------
###add new
observeEvent(event_data("plotly_click", source = "protein"), {
  toggleModal(session, modalId = "protein_boxPopUp", toggle = "toggle")
})
observeEvent(c(input$input_protein_set),{
    status$protein_result <- FALSE
    dataset_number$protein <- 30
    input$input_protein_set %>% tibble::tibble(protein=.) %>% 
      dplyr::mutate(protein= stringr::str_replace_all(protein,pattern = " |-",replacement = "") %>% toupper()) %>%
      .$protein ->match$protein
    if(match$protein != ""){
    status$protein_plot <- FALSE
    TCGA_protein_result()
    MCLP_protein_result()
    CCLE_protein_result()
    protein$TCGA <- FALSE
    protein$MCLP <- FALSE
    protein$CCLE <- FALSE
    t <- 0
    m <- 0
    c <- 0
    if(length(TCGA_protein_plot_result) > 1){
      TCGA_protein_plot_result -> TCGA_one_plot
      TCGA_plot <- expr_buble_plot_protein(TCGA_one_plot,"TCGA")
      protein$TCGA <- TRUE
      t <- 1
      status$protein_result <- TRUE
    }
    if(length(MCLP_protein_plot_result) > 1){
      MCLP_protein_plot_result -> MCLP_one_plot
      MCLP_plot <- expr_buble_plot_protein(MCLP_one_plot,"MCLP")
      protein$MCLP <- TRUE
      m <- 1
      status$protein_result <- TRUE
    }
    if(length(CCLE_protein_plot_result) > 1){
      CCLE_protein_plot_result -> CCLE_one_plot
      CCLE_plot <- expr_buble_plot_protein(CCLE_one_plot,"CCLE")
      protein$CCLE <- TRUE
      c <- 1
      status$protein_result <- TRUE
    }
    p3 <- plotly_empty(source = "protein")
    if(t == 1 && m == 1 && c == 1){
      subplot(
        TCGA_plot, p3,
        MCLP_plot, p3,
        CCLE_plot,
        nrows = 5, titleX = TRUE, titleY = TRUE , heights = c(0.25,0.125,0.25,0.125,0.25)
      ) -> plot_result
      plotmode <-  1
      output[[match$protein]] <- renderPlotly({plot_result})
    }
    else if(t == 1 && m == 1){
      subplot(
        TCGA_plot,p3,MCLP_plot,
        nrow = 3, heights = c(0.4,0.2,0.4)
      ) -> plot_result
      plotmode <-  2
      output[[match$protein]] <- renderPlotly({plot_result})
    }else if(t == 1 && c == 1){
      subplot(
        TCGA_plot,p3,CCLE_plot,
        nrow = 3, heights = c(0.4,0.2,0.4)
      ) -> plot_result
      plotmode <-  3
      output[[match$protein]] <- renderPlotly({plot_result})
    }
    else if(m == 1 && c == 1){
      subplot(
        MCLP_plot,p3,CCLE_plot,
        nrow = 3, heights = c(0.4,0.2,0.4)
      ) -> plot_result
      plotmode <-  4
      output[[match$protein]] <- renderPlotly({plot_result})
    }
    else if(t == 1){
      pplotmode <-  5
      output[[match$protein]] <- renderPlotly({TCGA_plot})
    }
    else if(m == 1){
      plotmode <-  6
      output[[match$protein]] <- renderPlotly({MCLP_plot})
    }
    else if(c == 1){
      plotmode <-  7
      output[[match$protein]] <- renderPlotly({CCLE_plot})
    }
    output$protein_hover <- renderPlotly({
      eventdat <- event_data('plotly_click', source="protein")
      if(is.null(eventdat) == T) return(NULL)        
      if(plotmode == 1){
        if(eventdat$curveNumber[1] == 0){
          click_plot_protein_TCGA(eventdat$x[1])
        }
        else if(eventdat$curveNumber[1] == 2){
          click_plot_protein_MCLP(eventdat$x[1])
        }
        else if(eventdat$curveNumber[1] == 4){
          click_plot_protein_CCLE(eventdat$x[1])
        }
      }
      else if(plotmode == 2){
        if(eventdat$curveNumber[1] == 0){
          click_plot_protein_TCGA(eventdat$x[1])
        }
        else if(eventdat$curveNumber[1] == 2){
          click_plot_protein_MCLP(eventdat$x[1])
        }
      }
      else if(plotmode == 3){
        if(eventdat$curveNumber[1] == 0){
          click_plot_protein_TCGA(eventdat$x[1])
        }
        else if(eventdat$curveNumber[1] == 2){
          click_plot_protein_CCLE(eventdat$x[1])
        }
      }
      else if(plotmode == 4){
        if(eventdat$curveNumber[1] == 0){
          click_plot_protein_MCLP(eventdat$x[1])
        }
        else if(eventdat$curveNumber[1] == 2){
          click_plot_protein_CCLE(eventdat$x[1])
        }
      }
      else if(plotmode == 5){
        click_plot_protein_TCGA(eventdat$x[1])
      }
      else if(plotmode == 6){
        click_plot_protein_MCLP(eventdat$x[1])
      }
      else if(plotmode == 7){
        click_plot_protein_CCLE(eventdat$x[1])
      }
    })
  }}
)

TCGA_protein_result <- function(){
  TCGA_protein %>%
    dplyr::mutate(
      expr = purrr::map(
        .x = summary,
        .f = function(.x) {
          .x %>% dplyr::mutate(protein = stringr::str_replace_all(pattern = "_",replacement = "",protein) %>% toupper()) %>%
          dplyr::filter(protein %in% match$protein) -> a
          if(length(a$protein) > 0){
            a %>% tidyr::unnest() 
          }
          else{
            a <- "drop"
            a %>% tibble::tibble(symbol=.)
          }
        }
      )
    ) %>% dplyr::select(-summary) %>% tidyr::unnest() %>% dplyr::filter(symbol != "drop")  -> expr_clean
  if(length(expr_clean$cancer_types) > 0){
    expr_clean %>% dplyr::group_by(cancer_types,symbol,protein) %>% dplyr::slice(1:5) %>% 
      tidyr::drop_na() %>% dplyr::ungroup() %>% dplyr::rename(expr = tumor)  ->> TCGA_protein_plot_result
    expr_clean %>% dplyr::group_by(cancer_types,symbol,protein) %>% dplyr::slice(6) %>% 
      tidyr::drop_na() %>% dplyr::ungroup() %>% dplyr::rename(expr = tumor) ->> TCGA_protein_table_result
    TCGA_protein_table_result %>% dplyr::left_join(protein_TCGA,by = "cancer_types") %>% dplyr::select(cancer_types = Disease_Type,symbol,protein,cancer,expr) -> TCGA_protein_table_result
    output$expr_dt_comparison_TCGA_protein <- DT::renderDataTable({expr_clean_datatable_protein(TCGA_protein_table_result,"Cancer Types (TCGA)")})
    output$expr_dt_download_TCGA_protein <- downloadHandler(
      filename = function() {
        paste(Sys.Date(),"TCGA_selected_protein.csv",sep = "_")
      },
      content = function(file) {
        write.csv(TCGA_protein_table_result, file, row.names = TRUE)
      }
    )
  }
  else{
    TCGA_protein_plot_result <<- "blank"
  }
  return(TCGA_protein_plot_result)
}

MCLP_protein_result <- function(){
  MCLP_protein %>%
    dplyr::mutate(
      expr = purrr::map(
        .x = summary,
        .f = function(.x) {
          .x %>% dplyr::mutate(protein = stringr::str_replace_all(pattern = "_",replacement = "",protein) %>% toupper()) %>%
            dplyr::filter(protein %in% match$protein) -> a
          if(length(a$protein) > 0){
            a %>% tidyr::unnest() 
          }
          else{
            a <- "drop"
            a %>% tibble::tibble(symbol = .)
          }
        }
      )
    ) %>% dplyr::select(-summary) %>% tidyr::unnest() %>% dplyr::filter(symbol != "drop") %>%
    dplyr::rename(cancer_types = tis)  -> expr_clean
  if(length(expr_clean$cancer_types) > 0){
    expr_clean %>% dplyr::group_by(cancer_types,symbol,protein) %>% dplyr::slice(1:5) %>% 
      tidyr::drop_na() %>% dplyr::ungroup() %>% dplyr::left_join(protein_MCLP,by="cancer_types") %>% 
      dplyr::select(cancer_types,symbol,protein,cellline_num,expr=summary) ->> MCLP_protein_plot_result
    expr_clean %>% dplyr::group_by(cancer_types,symbol,protein) %>% dplyr::slice(6) %>% 
      tidyr::drop_na() %>% dplyr::ungroup() %>% dplyr::left_join(protein_MCLP,by="cancer_types") %>% 
      dplyr::select(cancer_types,symbol,protein,cellline_num,expr=summary) ->> MCLP_protein_table_result
    output$expr_dt_comparison_MCLP_protein <- DT::renderDataTable({expr_clean_datatable_protein(MCLP_protein_table_result,"Cell lines (MCLP)")})
    output$expr_dt_download_MCLP_protein <- downloadHandler(
      filename = function() {
        paste(Sys.Date(),"MCLP_selected_protein.csv",sep = "_")
      },
      content = function(file) {
        write.csv(MCLP_protein_table_result, file, row.names = TRUE)
      }
    )
  }
  else{
    MCLP_protein_table_result <<- "blank"
  }
  return(MCLP_protein_plot_result)
}

CCLE_protein_result <- function(){
  CCLE_protein %>%
    dplyr::mutate(
      expr = purrr::map(
        .x = summary,
        .f = function(.x) {
          .x %>% dplyr::mutate(protein = stringr::str_replace_all(pattern = "_",replacement = "",protein) %>% toupper()) %>% 
            dplyr::filter(protein %in% match$protein) -> a
          if(length(a$protein) > 0){
            a %>% tidyr::unnest() 
          }
          else{
            a <- "drop"
            a %>% tibble::tibble(symbol = .)
          }
        }
      )
    ) %>% dplyr::select(-summary) %>% tidyr::unnest() %>% dplyr::filter(symbol != "drop" ) %>%
    dplyr::rename(cancer_types = tissue)  -> expr_clean
  if(length(expr_clean$cancer_types)>0){
    expr_clean %>% 
      dplyr::mutate(cancer_types = stringr::str_replace_all(pattern = "_",replacement = " ",cancer_types)) %>% 
      dplyr::group_by(cancer_types,symbol,protein) %>% dplyr::slice(1:5) %>% tidyr::drop_na() %>% 
      dplyr::ungroup() %>% dplyr::left_join(protein_CCLE,by="cancer_types") %>% 
      dplyr::select(cancer_types,symbol,protein,cellline_num,expr) ->> CCLE_protein_plot_result
    expr_clean %>% 
      dplyr::mutate(cancer_types = stringr::str_replace_all(pattern = "_",replacement = " ",cancer_types)) %>% 
      dplyr::group_by(cancer_types,symbol,protein) %>% dplyr::slice(6) %>% tidyr::drop_na() %>% 
      dplyr::ungroup() %>% dplyr::left_join(protein_CCLE,by="cancer_types") %>% 
      dplyr::select(cancer_types,symbol,protein,cellline_num,expr) ->> CCLE_protein_table_result
    output$expr_dt_comparison_CCLE_protein <- DT::renderDataTable({expr_clean_datatable_protein(CCLE_protein_table_result,"Cell lines (CCLE)")})
    output$expr_dt_download_CCLE_protein <- downloadHandler(
      filename = function() {
        paste(Sys.Date(),"CCLE_selected_protein.csv",sep = "_")
      },
      content = function(file) {
        write.csv(CCLE_protein_table_result, file, row.names = TRUE)
      }
    )
  }
  else{
    CCLE_protein_table_result <<- "blank"
  }
  return(CCLE_protein_plot_result)
}

click_plot_protein_TCGA <- function(.expr_clean) {
  .expr_clean %>% stringr::str_split_fixed(pattern = "\\( ",n=2) %>% 
    .[,2] %>% stringr::str_split_fixed(pattern = " \\)",n=2) %>% 
    .[,1] -> cancertypes
  paste("/home/liucj/shiny-data/GEDS/split_file/protein/TCGA/",cancertypes,".rds.gz",sep = "") -> file_name
  file <- readr::read_rds(file_name)
  file %>% names %>% .[c(-1,-2)] %>% tibble::tibble(barcode = .) %>% 
    dplyr::mutate(type = stringr::str_sub(string = barcode, start = 14, end = 15)) %>% 
    dplyr::mutate(type = ifelse(type == "11", "Normal", "Tumor")) ->cancertype
  cancertype %>% dplyr::filter(type %in% "Tumor") %>% .$barcode->tumor_barcode
  file %>% dplyr::filter(protein %in% match$protein) %>%
    dplyr::select(protein,tumor_barcode) %>% 
    tidyr::gather(key=barcode,value=expr,-c(protein)) %>%
    dplyr::mutate(type = stringr::str_sub(string = barcode, start = 1, end = 16)) %>%
    dplyr::select(protein,barcode=type,expr) ->rebuild_file
  rebuild_file %>% dplyr::arrange(expr) %>% .$barcode -> order
  rebuild_file %>% plot_ly(x = ~barcode, y = ~ expr, type = "scatter",mode = "markers") %>%
    layout(
      title = paste(cancertypes,"Tumor","n=",length(order)),
      xaxis = list(
        title = "Patient",
        showticklabels = FALSE,
        showline= TRUE,
        categoryorder = "array", 
        categoryarray = order
      ),
      yaxis = list(title = "Protein expression" ,showline = TRUE,hoverformat = '.2f')
    )
}

click_plot_protein_MCLP <- function(.expr_clean) {
  .expr_clean %>% stringr::str_replace_all(pattern = " ",replacement = "") -> cancertypes
  paste("/home/liucj/shiny-data/GEDS/split_file/protein/MCLP/",cancertypes,".rds.gz",sep = "") -> file_name
  file <- readr::read_rds(file_name)
  file %>% dplyr::filter(protein %in% match$protein) %>%
    tidyr::gather(key=barcode,value=expr,-c(symbol,protein)) ->rebuild_file
  rebuild_file %>% dplyr::arrange(expr) %>% .$barcode -> order
  rebuild_file %>% plot_ly(x = ~barcode, y = ~ expr,type = "scatter",mode = "markers") %>%
    layout(
      title = paste(.expr_clean,"n=",length(order)),
      xaxis = list(
        title = "Cell lines",
        showticklabels = FALSE,
        showline= TRUE,
        categoryorder = "array", 
        categoryarray = order
      ),
      yaxis = list(title = "Protein expression" ,showline = TRUE,hoverformat = '.2f')
    )
}

click_plot_protein_CCLE <- function(.expr_clean) {
  .expr_clean %>% stringr::str_replace_all(pattern = " ",replacement = "") -> cancertypes
  paste("/home/liucj/shiny-data/GEDS/split_file/protein/CCLE/",cancertypes,".rds.gz",sep = "") -> file_name
  file <- readr::read_rds(file_name)
  file %>% dplyr::filter(protein %in% match$protein) ->rebuild_file
  rebuild_file %>% dplyr::arrange(expr) %>% .$Cell_line -> order
  rebuild_file %>% plot_ly(x = ~Cell_line, y = ~ expr,type = "scatter",mode = "markers") %>%
    layout(
      title = paste(.expr_clean,"n=",length(order)),
      xaxis = list(
        title = "Cell lines",
        showticklabels = FALSE,
        showline= TRUE,
        categoryorder = "array", 
        categoryarray = order
      ),
      yaxis = list(title = "Protein expression" ,showline = TRUE,hoverformat = '.2f')
    )
}
###add new

# protein table_print -------------------------------------------------------------
expr_buble_plot_protein <-  function(.expr,.type){
  if(.type == "TCGA"){
  .expr %>% dplyr::rename(FPKM = expr) %>%
    dplyr::mutate(tmp = paste(site,"(",cancer_types,")")) %>%
    dplyr::select(cancer_types = tmp,symbol,protein,FPKM) -> t1
    t1 %>% dplyr::group_by(cancer_types) %>% 
      dplyr::slice(3) %>% dplyr::arrange(desc(FPKM)) %>% .$cancer_types -> order
    plot_ly(
      data = t1,
      x = ~ cancer_types,
      y = ~ FPKM,
      type = "box",
      split = ~ symbol,
      color = ~ symbol, colors = "red3",
      source = "protein",showlegend = FALSE
    ) %>% layout(
      boxgap = 0,#boxgroupgap=0,
      boxmode = "group",
      xaxis = list(
        title = "Cancer Types (TCGA)",
        showticklabels = TRUE,
        tickangle = 295, tickfont = list(size = 12),
        showline = TRUE,
        categoryorder = "array", 
        categoryarray = order
      ),
      yaxis = list(title = "Protein expression" ,showline = TRUE,hoverformat = '.2f')#,
      #legend = list(orientation = 'h',x = 0.7, y = 1.05)
      )
  }
  else{
    .expr %>% dplyr::rename(FPKM = expr) %>%
      dplyr::group_by(protein) %>% 
      dplyr::arrange(symbol,desc(FPKM)) %>% 
      dplyr::ungroup() %>%
      dplyr::mutate(tmp = stringr::str_to_title(cancer_types)) %>% 
      dplyr::select(cancer_types=tmp,symbol,protein,FPKM) -> t2
    if(.type == "MCLP"){
      protein_MCLP_sd %>% dplyr::filter(protein %in% t2$protein[1]) %>% 
        dplyr::mutate(tmp = stringr::str_replace_all(tis,pattern="_",replacement=" ") %>% stringr::str_to_title()) %>% 
        dplyr::select(tis=tmp,sd) %>% 
        dplyr::rename(cancer_types = tis) -> protein_MCLP_sd2
      t2 %>% dplyr::left_join(protein_MCLP_sd2,by="cancer_types") -> t2
      t2 %>% dplyr::group_by(cancer_types) %>% 
        dplyr::slice(3) %>% dplyr::arrange(desc(FPKM)) %>% .$cancer_types -> order
      plot_ly(
        data = t2, x = ~ cancer_types, y = ~ FPKM, type = "box", split = ~ symbol, 
        color = ~ symbol, colors = "#2cdbf9",source = "protein", tickfont = list(size = 12),
        name = "MCLP",showlegend = FALSE#,error_y = ~list(array = sd,color = '#000000')
      ) %>% layout(
        title = paste(t2$symbol[1],"(",t2$protein[1],")"),
        xaxis = list(
          title = "Cell lines (MCLP)", showticklabels = TRUE,
          tickangle = 295, showline = TRUE, categoryorder = "array", 
          categoryarray = order
        ),
        yaxis = list(title = "Protein expression" ,showline = TRUE,hoverformat = '.2f'))
    }
    else{
      protein_CCLE_sd %>% dplyr::filter(protein %in% t2$protein[1]) %>% 
        dplyr::mutate(tmp = stringr::str_replace_all(tissue,pattern="_",replacement=" ") %>% stringr::str_to_title()) %>% 
        dplyr::select(tissue=tmp,sd=expr) %>% 
        dplyr::rename(cancer_types = tissue) -> protein_CCLE_sd2
      t2 %>% dplyr::left_join(protein_CCLE_sd2,by="cancer_types") -> t2
      t2 %>% dplyr::group_by(cancer_types) %>% 
        dplyr::slice(3) %>% dplyr::arrange(desc(FPKM)) %>% .$cancer_types -> order
      plot_ly(
        data = t2, x = ~ cancer_types, y = ~ FPKM, type = "box", split = ~ symbol, 
        color = ~ symbol, colors = "#75a3e7",source = "protein", tickfont = list(size = 12),
        name = "CCLE",showlegend = FALSE#,error_y = ~list(array = sd,color = '#000000')
      ) %>% layout(
        xaxis = list(
          title = "Cell lines (CCLE)", showticklabels = TRUE,
          tickangle = 295, showline = TRUE, categoryorder = "array", 
          categoryarray = order
        ),
        yaxis = list(title = "Protein expression" ,showline = TRUE,hoverformat = '.2f'))
    }
  }
}
expr_clean_datatable_protein <- function(.expr_clean,.title) {
  if(.title == "Cancer Types (TCGA)"){
    site <- "Cancer Type"
  }
  else{
    site <- "Cell line Lineage"
  }
  DT::datatable(
    data = .expr_clean,
    options = list(
      pageLength = 10,
      autoWidth = TRUE,
      searching = TRUE,
      dom = "Bfrtip",
      columnDefs = list(list(className = 'dt-center',targets="_all"))
    ),
    caption = htmltools::tags$caption(
      .title,
      style = 'font-size: 25px; color: black'
    ),
    rownames = FALSE,
    colnames = c(site, "Symbol", "Protein", "Sample Statistics", "Mean expr."),
    style = "bootstrap",
    class = "table-bordered table-condensed"
  ) %>% 
    #DT::formatSignif(columns = c("expr"), digits = 2) %>%
    DT::formatRound(columns = c("expr"), 2)
}

# observe -----------------------------------------------------------------
