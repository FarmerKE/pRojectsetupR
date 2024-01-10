#' A function to create directory in project folder
#'
#' @param dir_name Provide a directory name
#'
#' @return
#' @export
#'
#' @examples dir_create_function("R")
dir_create_function <- function(dir_name) {
  if(!dir.exists(dir_name)) {
    dir.create(dir_name)
    list(success = glue::glue("{dir_name} Directory Created Successfully"))
  } else {
    list(error = "Directory Exists")
  }
}

#' A function to create a file in project folder
#'
#' @param file_name Provide a file name
#' @param dest This is the working directory; default set as getwd()
#'
#' @return
#' @export
#'
#' @examples file_create_function("app.R", getwd())
file_create_function <- function(file_name, dest = getwd()) {
  
  file_dest <- glue::glue("{dest}/{file_name}")
  
  if(!file.exists(file_dest)) {
    file.create(file_dest)
    list(success = glue::glue("{file_name} File Created Successfully"))
  } else {
    list(error = "File Exists")
  }
}

#' Create a global.R file
#'
#' @param dest This is the working directory; default set as getwd()
#'
#' @return
#' @export
#'
#' @examples create_global_file(getwd())
create_global_file <- function(dest = getwd()) {
  file_name = "global.R"
  if(!file.exists(file_name)) {
    file.create(file_name)
    cat("library(config)", file = file_name, append = TRUE)
    cat("\n", file = file_name, append = TRUE)
    cat("\nconfig <- config::get(file = 'config.yml')", file = file_name, append = TRUE)
    list(message = "Global.R file Successfully Created")
  } else {
    list(error = "File Exists")
  }
}

#' Create a shiny app default file.
#'
#' @param dest This is the working directory; default set as getwd()
#' @param file_name Provide a file name
#'
#' @return
#' @export
#'
#' @examples create_r_module_file(dest = "R/", file_name = "home")
create_r_module_file <- function(dest = getwd(), file_name) {
  file_name_main <- paste0(glue::glue("{dest}/{file_name}"), '.R')
  if(!file.exists(file_name_main)) {
    file.create(file_name_main)
    cat("# UI", file = file_name_main, append = TRUE)
    cat("\n", file = file_name_main, append = TRUE)
    cat(paste0(file_name, "_ui <- function(id) {"), file = file_name_main, append = TRUE)
    cat("\n  ns <- NS(id)", file = file_name_main, append = TRUE)
    cat("\n  tagList(", file = file_name_main, append = TRUE)
    cat("\n    fluidRow(", file = file_name_main, append = TRUE)
    cat("\n      column(", file = file_name_main, append = TRUE)
    cat("\n        width = 12,", file = file_name_main, append = TRUE)
    cat("\n        shiny::dataTableOutput(ns('mtcars'))", file = file_name_main, append = TRUE)
    cat("\n      )", file = file_name_main, append = TRUE)
    cat("\n    )", file = file_name_main, append = TRUE)
    cat("\n  )", file = file_name_main, append = TRUE)
    cat("\n}", file = file_name_main, append = TRUE)
    cat("\n", file = file_name_main, append = TRUE)
    cat("\n# Server", file = file_name_main, append = TRUE)
    cat("\n", file = file_name_main, append = TRUE)
    cat(paste0(file_name, "_server <- function(id) {"), file = file_name_main, append = TRUE)
    cat("\n  moduleServer(", file = file_name_main, append = TRUE)
    cat("\n    id = id,", file = file_name_main, append = TRUE)
    cat("\n    module = function(input, output, session) {", file = file_name_main, append = TRUE)
    cat("\n      ns <- NS(id)", file = file_name_main, append = TRUE)
    cat("\n      output$mtcars <- shiny::renderDataTable(", file = file_name_main, append = TRUE)
    cat("\n        mtcars", file = file_name_main, append = TRUE)
    cat("\n      )", file = file_name_main, append = TRUE)
    cat("\n    }", file = file_name_main, append = TRUE)
    cat("\n  )", file = file_name_main, append = TRUE)
    cat("\n}", file = file_name_main, append = TRUE)
    list(message = "Module File Successfully Created")
  } else {
    list(error = "File Exists")
  }
}

#' Create a dockerfile template.
#'
#' @param dest This is the working directory; default set as getwd()
#'
#' @return
#' @export
#'
#' @examples create_dockerfile()
create_dockerfile <- function(dest = getwd()) {
  file_name = "Dockerfile"
  if(!file.exists(file_name)) {
    file.create(file_name)
    cat("FROM rocker/rstudio", file = file_name, append = TRUE)
    cat("\n", file = file_name, append = TRUE)
    cat("\nRUN apt-get update && apt-get install -y  git-core libcurl4-openssl-dev libgit2-dev libicu-dev libsodium-dev libssl-dev libxml2-dev make pandoc pandoc-citeproc zlib1g-dev && rm -rf /var/lib/apt/lists/*", file = file_name, append = TRUE)
    cat("\nRUN echo \"options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)\" >> /usr/local/lib/R/etc/Rprofile.site", file = file_name, append = TRUE)
    cat("\n", file = file_name, append = TRUE)
    cat("\nCOPY . .", file = file_name, append = TRUE)
    cat("\n", file = file_name, append = TRUE)
    cat("\nEXPOSE 8000", file = file_name, append = TRUE)
    cat("\n", file = file_name, append = TRUE)
    cat("\nRUN R -e \"install.packages(c('shiny', 'shinyauthr', 'shinyjs', 'glue', 'config'))\"", file = file_name, append = TRUE)
    cat("\n", file = file_name, append = TRUE)
    cat("\nENTRYPOINT [\"Rscript", "runApp.R\"]", file = file_name, append = TRUE)
    list(message = "Dockerfile Successfully Created")
  } else {
    list(error = "File Exists")
  }
}

#' Create shiny app.R file.
#' Holds the logic for UI and Server files
#'
#' @param dest This is the working directory; default set as getwd()
#'
#' @return
#' @export
#'
#' @examples create_app_file()
create_app_file <- function(dest = getwd()) {
  file_name = "app.R"
  if(!file.exists(file_name)) {
    file.create(file_name)
    cat("# Define UI logic required", file = file_name, append = TRUE)
    cat("\nui <- fluidPage(", file = file_name, append = TRUE)
    cat("\n  home_ui('home')", file = file_name, append = TRUE)
    cat("\n)", file = file_name, append = TRUE)
    cat("\n", file = file_name, append = TRUE)
    cat("\n# Define server logic required", file = file_name, append = TRUE)
    cat("\nserver <- function(input, output, session) {", file = file_name, append = TRUE)
    cat("\n  home_server('home')", file = file_name, append = TRUE)
    cat("\n}", file = file_name, append = TRUE)
    cat("\n", file = file_name, append = TRUE)
    cat("\n# Run the application", file = file_name, append = TRUE)
    cat("\nshinyApp(ui = ui, server = server)", file = file_name, append = TRUE)
    list(message = "App.R file Successfully Created")
  } else {
    list(error = "File Exists")
  }
}

#' Create a shiny runApp.R file.
#'
#' @param dest This is the working directory; default set as getwd()
#'
#' @return
#' @export
#'
#' @examples create_run_app_file()
create_run_app_file <- function(dest = getwd()) {
  file_name = "runApp.R"
  if(!file.exists(file_name)) {
    file.create(file_name)
    cat("library(shiny)", file = file_name, append = TRUE)
    cat("\nlibrary(shinyauthr)", file = file_name, append = TRUE)
    cat("\nlibrary(shinyjs)", file = file_name, append = TRUE)
    cat("\nlibrary(glue)", file = file_name, append = TRUE)
    cat("\nlibrary(config)", file = file_name, append = TRUE)
    cat("\n", file = file_name, append = TRUE)
    cat("\n# Run app on port 8000 and host = 0.0.0.0", file = file_name, append = TRUE)
    cat("\nshiny::runApp('app.R', port = 8000, host = '0.0.0.0')", file = file_name, append = TRUE)
    list(message = "runApp.R file Successfully Created")
  } else {
    list(error = "File Exists")
  }
}

#' Shiny App Directory and File creation. 
#' Creates the files in the project directory.
#'
#' @return
#' @export
#'
#' @examples create_shiny_app()
create_shiny_app <- function() {
  dir_create_function("R")
  dir_create_function("js")
  
  file_create_function("config.yml")
  
  create_r_module_file(dest = "R/", file_name = "home")
  create_global_file()
  create_dockerfile()
  create_app_file()
  create_run_app_file()
  list(
    success = "All files created successfully"
  )
}




