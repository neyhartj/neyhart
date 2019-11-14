#' Replace character elements and convert to factor.
#' 
#' @description 
#' Replace the elements in a character and convert to a factor. The levels of the factor
#' are the same order as the replacement vector.
#' 
#' @param x A character vector to replace.
#' @param replacement A named character vector with the original character elements as
#' elements and desired replacement elements as names.
#' 
#' @importFrom stringr str_replace_all
#' 
#' @export
#' 
as_replaced_factor <- function(x, replacement) {
  
  ## Class check
  stopifnot(inherits(x, "character"))
  stopifnot(inherits(replacement, "character"))
  
  # Make sure replacement has names
  if (is.null(names(replacement))) stop("replacement must be a named vector.")
  
  x_repl <- str_replace_all(string = x, pattern = replacement)
  factor(x_repl, levels = replacement)
}




