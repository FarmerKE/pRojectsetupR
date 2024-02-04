#' Create API Folder Directory
#'
#' @param dest 
#'
#' @return
#' @export
#'
#' @examples api_dir_function()
api_dir_function <- function() {
  dir_name <- "api"
  if(!dir.exists(dir_name)) {
    dir.create(dir_name)
    list(success = glue::glue("{dir_name} Directory Created Successfully"))
  } else {
    list(error = "Directory Exists")
  }
}

#' Create Plumber API route file
#'
#' @param dest 
#'
#' @return
#' @export
#'
#' @examples create_api_route_file(dest = getwd())
create_api_route_file <- function(dest, app_name) {
  file_name = paste0(dest, "api.R")
  if(!file.exists(file_name)) {
    file.create(file_name)
    cat("\n#' @plumber", file = file_name, append = TRUE)
    cat("\nfunction(pr) {", file = file_name, append = TRUE)
    cat("\n  pr %>%", file = file_name, append = TRUE)
    cat(glue::glue("\n    pr_mount('/routes1', plumb('./{app_name}.R'))"), file = file_name, append = TRUE)
    cat("\n}", file = file_name, append = TRUE)
    list(message = "api.R file Successfully Created")
  } else {
    list(error = "File Exists")
  }
}

#' Create Plumber Entrypoint.R File
#'
#' @param dest 
#'
#' @return
#' @export
#'
#' @examples
create_entrypoint_file <- function(dest) {
  file_name = paste0(dest, "entrypoint.R")
  if(!file.exists(file_name)) {
    file.create(file_name)
    cat("library(plumber)", file = file_name, append = TRUE)
    cat("\n", file = file_name, append = TRUE)
    cat('\nr <- plumb("api.R")$run(port = 8080, host = "0.0.0.0")$', file = file_name, append = TRUE)
    cat("\n  setDebug(debug = TRUE)", file = file_name, append = TRUE)
    list(message = "entrypoint.R file Successfully Created")
  } else {
    list(error = "File Exists")
  }
}

#' Create Plumber API File
#'
#' @param dest 
#' @param filename 
#'
#' @return
#' @export
#'
#' @examples create_api_file("api", "route")
create_api_file <- function(dest, filename) {
  file_name = paste0(glue::glue("{dest}/{filename}"), '.R')
  if(!file.exists(file_name)) {
    file.create(file_name)
    cat("\n#* @get /random_numbers", file = file_name, append = TRUE)
    cat("\n#* @param maxn", file = file_name, append = TRUE)
    cat("\nfunction(maxn) {", file = file_name, append = TRUE)
    cat("\n  maxn <- as.numeric(maxn)", file = file_name, append = TRUE)
    cat("\n  runif(1,min=0,max=maxn)", file = file_name, append = TRUE)
    cat("\n}", file = file_name, append = TRUE)
    list(message = glue::glue("{file_name} file Successfully Created"))
  } else {
    list(error = "File Exists")
  }
}

#' Create Plumber App
#'
#' @param route_name 
#'
#' @return
#' @export
#'
#' @examples create_plumber(route_name = "main")
create_plumber <- function(route_name) {
  api_dir_function()
  create_api_file("api", route_name)
  create_api_route_file("api/", app_name = route_name)
  create_entrypoint_file(dest = "api/")
}
