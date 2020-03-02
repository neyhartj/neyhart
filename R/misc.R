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

#' Set a factor with contrasts that sum-to-zero
#' 
#' @description 
#' A wrapper of \code{\link[stats]{contr.sum}}, but the resulting contrasts matrix included column names
#' and is easier for extracting coefficients from a fitted model object
#' 
#' @param x A factor.
#' @param drop.levels Logical. Should unused levels be dropped?
#' 
#' @export
#' @importFrom stats contr.sum contrasts
#' 
fct_contr_sum <- function(x, drop.levels = FALSE) {
  stopifnot(is.factor(x))
  stopifnot(is.logical(drop.levels))
  
  # Drop levels, if called for 
  x1 <- if (drop.levels) droplevels(x = x) else x
  
  # Redefine contrasts as sum-to-zero
  x1_contrasts <- contr.sum(levels(x1))
  colnames(x1_contrasts) <- head(levels(x1), -1)
  contrasts(x1) <- x1_contrasts
  return(x1)
}




