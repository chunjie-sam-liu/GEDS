# saved as footer.r
fluidRow(
  style = "text-align:center",
  shiny::tags$hr(),
  shiny::tags$p(
    "Copyright ©",
    shiny::tags$a("Guo Lab", href = "http://bioinfo.life.hust.edu.cn/home_page#!/", target = "_blank"),
    ",",
    shiny::tags$a("College of Life Science and Technology", href = "http://life.hust.edu.cn/", target = "_blank"),
    ",",
    shiny::tags$a("HUST", href = "http://www.hust.edu.cn/", target = "_blank"),
    ", China", 
    
    shiny::tags$br(),
    
    "Any comments and suggestions, please contact us."
  )
)