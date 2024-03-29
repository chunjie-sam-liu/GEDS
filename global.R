# share the vairiables


# For the global configuration --------------------------------------------
# Store variable in config ------------------------------------------------
config <- list()

# Version -----------------------------------------------------------------
config$version <- "0.0.1"


# Working directory -------------------------------------------------------
# default getwd()
# in development mode, change the config$wd to your working directory


config$wd <- "/home/xiamx/github/GEDS"
#config$wd <- "/data/liucj/github/GEDS"
#config$wd <- "/home/liucj/github/GEDS"

# Databases ---------------------------------------------------------------
#config$database <- "/data/shiny-data/GEDS"
config$database <- '/home/liucj/shiny-data/GEDS'



# Absolute paths ----------------------------------------------------------
# server
config$serv <- file.path(config$wd, "serv")

# ui
config$ui <- file.path(config$wd, "ui")

# functions
config$func <- file.path(config$wd, "func")

# user data
config$usrdat <- file.path(config$wd, "usrdat")

# logs
config$logs <- "logs"

# Path to zip -------------------------------------------------------------
Sys.setenv("R_ZIPCMD" = "/usr/bin/zip")

download_bt <- function(id){
  ns <- NS(id)
  tagList(
    shinyWidgets::dropdownButton(
      tags$h3("Download Options"),
      #prettyRadioButtons(
      #  inputId = ns("pictype"),
      #  label = "Selcet format for your pictur",
      #  choices = list("PDF" = "pdf", "PNG" = "png","EPS"="eps"),
      #  inline = TRUE,
      #  icon = icon("check"),
      #  bigger = TRUE, status = "info",
      #  animation = "jelly"
      #),
      numericInput(
        inputId = ns("d_width"),
        label = "Width",
        value = 16,
        min = 10,
        max = 30
      ),
      
      numericInput(
        inputId = ns("d_height"),
        label = "Height",
        value = 20,
        min = 15,
        max = 40
      ),
      downloadButton(
        outputId = ns("picdownload"),
        label = "Download"
      ),
      circle = TRUE, status = "default",
      right = TRUE,
      icon = icon("download"), width = "300px",
      tooltip = shinyWidgets::tooltipOptions(title = "Click to download")
    )
  )
}


