#' print_matrix
#'
#' @param x a matrix
#' @param digits the number of digits to round the matrix
#' @param mat.type "", "b", "B", "p", "v", "V", or "small"
#'
#' @return LaTeX code for printed matrix
#' @export
#' @examples
#' print_matrix(matrix(runif(20), ncol = 4))
print_matrix <- function(x, digits = 2, mat.type = "b", include.colnames = T) {
  # https://tex.stackexchange.com/questions/55418/how-to-print-a-latex-matrix-using-xtable-in-r
  y <- xtable::xtable(x, align = rep("", ncol(x) + 1), digits = digits) # We repeat empty string 6 times
  tabular_environment <- paste0(mat.type, "matrix")
  print(y, floating = FALSE, tabular.environment = tabular_environment,
        hline.after = NULL, include.rownames = FALSE, include.colnames = include.colnames)
}
