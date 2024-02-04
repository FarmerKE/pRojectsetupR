
#' @plumber
function(pr) {
  pr %>%pr_mount('/routes1', plumb('./main.R'))
}