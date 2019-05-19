#' suppressAll
#'
#' @param expr an R expression to evaluate
#' @return the resul of expr
#' @export
suppressAll <- function(expr) {
  capture.output(x <- suppressWarnings(suppressMessages(expr)), file = "/dev/null")
  x
}
