library(shiny)
library(plotly)
library(shinyBS)

example <- readr::read_rds("/home/xiamx/file_for_GEDS_test/example.rds.gz")
GTEX_example <- readr::read_rds("/home/xiamx/file_for_GEDS_test/GTEX_example.rds.gz")
CCLE_example <- readr::read_rds("/home/xiamx/file_for_GEDS_test/CCLE_example.rds.gz")

ui <- fluidPage(
  column(6, plotlyOutput("mainplot")),
  bsModal('boxPopUp', '', '', plotlyOutput("hover"))
)

server <- function(input, output,session) {
  output$mainplot <- renderPlotly({
    example %>% dplyr::rename(FPKM = expr) %>%
      tidyr::separate(col = cancer_types, into = c("cancer_types", "types")) %>%
      dplyr::mutate(tmp = paste(site,"(",cancer_types,")")) %>%
      dplyr::select(cancer_types=tmp,types,symbol,FPKM) %>%
      dplyr::mutate(types = stringr::str_to_title(types))  -> t1
    t1 %>% dplyr::filter(types %in% "Tumor") %>% dplyr::group_by(cancer_types) %>% 
      dplyr::slice(3) %>% dplyr::arrange(desc(FPKM)) %>% .$cancer_types -> order
    p1 <- plot_ly(
      data = t1,
      x = ~ cancer_types,
      y = ~ FPKM,
      type = "box",
      split = ~ types,
      color = ~ types, colors = c("midnightblue", "red3"),
      source = "main"
    ) %>% layout(
      title = example$symbol[1],
      boxmode = "group",
      scene = "scene1",
      xaxis = list(
        title = "Cancer Types (TCGA)",
        showticklabels = TRUE,
        tickangle = 295, tickfont = list(size = 10),
        showline = TRUE,
        categoryorder = "array", 
        categoryarray = order
        ),
      yaxis = list(title = "RSEM(log2)" ,showline = TRUE))
    
    GTEX_example %>% dplyr::rename(FPKM = expr) %>%
      dplyr::group_by(symbol) %>% dplyr::arrange(symbol,desc(FPKM)) %>% 
      dplyr::ungroup() %>% dplyr::mutate(tmp = stringr::str_to_title(cancer_types)) %>% 
      dplyr::select(cancer_types = tmp, symbol, FPKM)-> t2
    p2 <- plot_ly(
      data = t2, x = ~ cancer_types, y = ~ FPKM, type = "bar", split = ~ symbol, 
      color = ~ symbol, colors = "#cbb255",source = "main", showlegend = FALSE
    ) %>% layout(
      title = example$symbol[1],
      scene = "scene2",
      xaxis = list(
        title = "Normal Tissues (GTEx)", showticklabels = TRUE,
        tickangle = 295, showline = TRUE, categoryorder = "array", 
        categoryarray = t2$cancer_types,tickfont = list(size = 10)
      ),
      yaxis = list(title = "FPKM" ,showline = TRUE))
    CCLE_example %>% dplyr::rename(FPKM = expr) %>%
      dplyr::group_by(symbol) %>% dplyr::arrange(symbol,desc(FPKM)) %>% 
      dplyr::ungroup() %>% dplyr::mutate(tmp = stringr::str_to_title(cancer_types)) %>% 
      dplyr::select(cancer_types = tmp, symbol, FPKM)-> t3
    p3 <- plot_ly(
      data = t3, x = ~ cancer_types, y = ~ FPKM, type = "bar", split = ~ symbol, 
      color = ~ symbol, colors = "#cbb255",source = "main"
    ) %>% layout(
      title = example$symbol[1],
      scene = "scene2",
      xaxis = list(
        title = "Cell line (CCLE)", showticklabels = TRUE,
        tickangle = 295, showline = TRUE, categoryorder = "array", 
        categoryarray = t3$cancer_types,tickfont = list(size = 10)
      ),
      yaxis = list(title = "FPKM" ,showline = TRUE))
    p4 <- plotly_empty(source = "main")
    
    p <- p3

  })
  observeEvent(event_data("plotly_click", source = "main"), {
    toggleModal(session, modalId = "boxPopUp", toggle = "toggle")
  })
  output$hover <- renderPlotly({
    eventdat <- event_data('plotly_click', source="main") # get event data from source main
    if(is.null(eventdat) == T) return(NULL)        # If NULL dont do anything
    point <- as.numeric(eventdat[['pointNumber']]) # Index of the data point being charted
    print(eventdat)
    # draw plot according to the point number on hover
    dat2 <- data.frame(cond = factor(rep("A", each=200)), rating = rnorm(200))
    ggplot(dat2, aes(x=cond, y=rating)) + geom_point()
  })
}

shinyApp(ui, server)