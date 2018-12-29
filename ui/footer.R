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
  ),
  shiny::tags$div(
    class = "row",
    shiny::tags$div(
      class = "col-md-offset-3 col-md-3",
      shiny::HTML(text = '<div style="display:inline-block;width:200px;">
              <script type="text/javascript" src="//ra.revolvermaps.com/0/0/7.js?i=0a8z4i1jsww&amp;m=0&amp;c=ff0000&amp;cr1=ffffff&amp;sx=0" async="async"></script>
                  </div>')
    )
  )
)