#' geom_smooth.spline
#' @keywords internal
#' @noRd
geom_smooth.spline <- function(formula,data,...) {
  M <- model.frame(formula, data)
  smooth.spline(x = M[,2], y = M[,1], df = 5)
}
