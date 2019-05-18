#' suppressAll
#'
#' @param expr an R expression to evaluate
#' @return the resul of expr
#' @export
suppressAll <- function(expr) {
  capture.output(x <- expr, file = "/dev/null")
  x
}
