#' predictdf.smooth.spline
#' @keywords internal
#' @noRd

predictdf.smooth.spline <- function(model, xseq, se, level) {
  pred <- predict(model, xseq)
  data.frame(x = xseq, y = pred$y)
}
