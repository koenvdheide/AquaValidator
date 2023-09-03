#' Start Aqualysis Validator met extra opties
#'
#' @param ... arguments to pass to shiny::runApp
#'
#' @export
#'
startup <- function(browser = TRUE,...){
  pkgload::load_all(".")
  shiny::runApp("R", launch.browser = browser,...)
}
