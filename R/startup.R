#' Start Aqualysis Validator
#'
#' @param ... arguments to pass to shiny::runApp
#'
#' @export
#'
startup <- function(...){
  shiny::runApp("R/Shiny", ...)
}