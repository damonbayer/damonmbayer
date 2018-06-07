#' The length of a string (in characters).
#'
#' @param x input character vector
#' @return the input string formatted as in an english sentence
#' @export
#' @examples
#' print_vector(letters)
#' print_vector(letters, oxford.comma = F)
print_vector <- function(x, oxford.comma = T) {
  ifelse(test = oxford.comma,
         yes = paste(paste0(c(head(x, -1), "and"), collapse = ", "), tail(x, 1)),
         no = paste(paste0(head(x, -1), collapse = ", "), "and", tail(x, 1))
  )
}
