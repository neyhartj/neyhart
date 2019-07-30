#' Assign cores to a data.frame
#' 
#' @description 
#' Assigns the core (integer) to each row of a data.frame, given the desired number of cores.
#' 
#' @param df A data.frame.
#' @param n_core The number of cores (integer) to split the data.frame.
#' 
#' @export
#' 
assign_cores <- function(df, n_core) {
  
  # Test classes
  stopifnot(inherits(df, "data.frame"))
  stopifnot(inherits(n_core, "numeric"))
  
  n_core <- as.integer(n_core)
  
  df$core <- sort(rep(seq(n_core), length.out = nrow(df)))
  return(df)
}

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




