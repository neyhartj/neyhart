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

#' Setting factor contrasts
#' 
#' @description 
#' Functions for manipulating the contrasts of factors.
#' 
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


#' @describeIn fct_contr_sum
#' 
#' @param checks Levels of the factor \code{x} corresponding to "checks"
#' 
#' @export
#' 
fct_contr_check_trt <- function(x, checks, drop.levels = FALSE) {
  # Error
  stopifnot(is.logical(drop.levels))
  
  # Convert x to factor, if not
  x1 <- as.factor(x)
  # Check if checks are levels of x
  checks1 <- intersect(levels(x1), checks)
  
  # Find the non-check levels
  non_checks <- setdiff(levels(x1), checks1)
  
  ## Create a base contrast matrix - diagonal
  contr <- diag(x = nlevels(x1))
  dimnames(contr) <- replicate(2, levels(x1), simplify = FALSE)
  
  ## Add check contrasts
  ## Contrasts must sum to 0 
  ## Compare positive groups with negative groups
  contr[checks1, non_checks] <- - (1 / length(checks1))
  contr[non_checks, checks1] <- - (1 / length(non_checks))
  
  # Add the contrast matrix to the factor vector
  contrasts(x1) <- contr
  # Return x1
  return(x1)
  
}



#' Return the size of objects in the environment
#' 
#' @import dplyr
#' @import purrr
#' 
#' @export
#' 
object_size <- function(unit = "auto") {
  
  objects <- ls(envir = .GlobalEnv)
  size <- map(objects, ~object.size(x = get(.)))
  size_num <- as.numeric(unlist(size))
  size2 <- map_chr(size, ~format(., unit = unit))
  
  tibble(object = objects, size = size2)[order(size_num, decreasing = TRUE),]
  
}
